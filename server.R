library(shiny)
library(shinysky)
library(shinythemes)
library(plotly)
library(DT)

rankings=read.csv("ffa_customrankings2017-0.csv")
rankings <- subset(rankings,position == 'QB' | position == 'DST' | 
                     position == 'RB' | 
                     position == 'WR' | 
                     position == 'TE' | 
                     position == 'K' )
player_ids = as.character(rankings$player)
rankings$player=as.character(rankings$player)
delete_player=function(file, player){
  file[file$player!=player,]
}
get_starter=function(file,position){
  subs=file[file$position==position,]
  ##low risk high floor
  plot_ly(subs, x = ~risk, y = ~lower, color = ~dropoff,type='scatter',
          size = ~points, text = ~paste("Name: ", player,"<br>VOR: ",vor,"<br>Points: ", points),mode = "markers",hoverinfo = 'text')
}

get_bench=function(file,position){
  ##high vor & upper
  subs=file[file$position==position,]
  plot_ly(subs, x = ~vor, y = ~upper, color = ~dropoff,
          size = ~points, text = ~paste("Name: ", player,"<br> VOR: ",vor),type='scatter',mode="markers",hoverinfo="text")
}

  
server=function(input, output, session) {

  team=reactiveValues()
  team$df=as.data.frame(NULL)
  rankings_1=reactiveValues()
  rankings_1$df=rankings
  
  observeEvent(input$button2,{
    rankings_1$df=delete_player(rankings_1$df,as.character(input$delete_pick))
  })
  observeEvent(input$button1,{
    adder=rankings_1$df[rankings_1$df$player==input$add_to_team,]
    rankings_1$df=delete_player(rankings_1$df,as.character(input$add_to_team))
    
    team$df=rbind(team$df,adder)
    team$df$position=factor(team$df$position,levels=c("QB","RB","WR","TE","DST","K"))
    team$df=team$df[order(team$df$position,team$df$points),]
    ##user points try to get to 1500
    my_players =  datatable(team$df,style = 'bootstrap',extensions = 'FixedColumns',
                                 options = list(lengthChange= FALSE,
                                   
                                   scrollX = TRUE,
                                   fixedColumns = TRUE
                                 ),rownames= FALSE)
    output$team_table = DT::renderDataTable(my_players)
    output$sum=renderUI({
      HTML(paste("<b><h5>Total Team Points:",  round(sum(team$df$points),2),"<br><font color='red'>Lower End Team Points: ",round(sum(team$df$lower),2), "</font><br><font color='green'>Upper End Team Points: ",
        round(sum(team$df$upper),2),"</font></h5></b>" ))
    })
   
  })
  observe({
    if(input$starter_type=='Starter'){
      output$QB <- renderPlotly({get_starter(rankings_1$df,"QB") %>% config(displayModeBar = F)})
      output$RB <- renderPlotly({get_starter(rankings_1$df,"RB") %>% config(displayModeBar = F)})
      output$WR <- renderPlotly({get_starter(rankings_1$df,"WR") %>% config(displayModeBar = F)})
      output$TE <- renderPlotly({get_starter(rankings_1$df,"TE") %>% config(displayModeBar = F)})
      output$K <- renderPlotly({get_starter(rankings_1$df,"K") %>% config(displayModeBar = F)})
      output$DST <- renderPlotly({get_starter(rankings_1$df,"DST") %>% config(displayModeBar = F)})
    } else if(input$starter_type=='Bench'){
      output$QB <- renderPlotly({get_bench(rankings_1$df,"QB") %>% config(displayModeBar = F)})
      output$RB <- renderPlotly({get_bench(rankings_1$df,"RB") %>% config(displayModeBar = F)})
      output$WR <- renderPlotly({get_bench(rankings_1$df,"WR") %>% config(displayModeBar = F)})
      output$TE <- renderPlotly({get_bench(rankings_1$df,"TE") %>% config(displayModeBar = F)})
      output$K <- renderPlotly({get_bench(rankings_1$df,"K") %>% config(displayModeBar = F)})
      output$DST <- renderPlotly({get_bench(rankings_1$df,"DST") %>% config(displayModeBar = F)})
    }
    
    all_the_players =  datatable(rankings_1$df,filter = 'top',style = 'bootstrap',extensions = 'FixedColumns',
                                 options = list(pageLength = 30,
                                  
                                   scrollX = TRUE,
                                   fixedColumns = TRUE
                                 ),rownames= FALSE)
    output$tbl = DT::renderDataTable(all_the_players)
  })

  observeEvent(input$button3,{
    write.csv(file=paste0("gaes/",input$teamname,"_projections.csv"),row.names=F,as.data.frame(cbind(teamname=input$teamname,total=round(sum(team$df$points),2),lowend=round(sum(team$df$lower),2),highend=round(sum(team$df$upper),2))))
    })
  observe({
    file_names <- dir('gaes') #where you have your files

    teams <- do.call(rbind,lapply(paste0('gaes/',file_names),read.csv))
    teams_1 =  datatable(teams,style = 'bootstrap',extensions = 'FixedColumns',
                                 options = list(lengthChange= FALSE,
                                  order=list(1,"desc"),
                                   pageLength = 12,
                                   dom = 't',
                                   scrollX = TRUE,
                                   fixedColumns = TRUE
                                 ),rownames= FALSE,colnames = c('Team Name', 'Expected Points', 'Lower End', 'Higher End'))
    output$teams_table = DT::renderDataTable(teams_1)
    x <- list(
        title = "Expected Points"
      )
    y <- list(
      title="",
      tickangle = -25,  
      tickfont = list(size = 6.5,
      color = "grey")
    )

    output$gaes_plot <- renderPlotly({plot_ly(data = teams, 
      x = ~total, 
      y = ~teamname, 
      type = 'scatter', 
      mode = 'markers',
      marker = list(size = 10),
      text = ~paste("Team Name: ", teamname, "<br>Total Expected Points: ", total),
      color=~teamname,
        hoverinfo = 'text',
        error_x = list(              type = "data",
                symmetric = FALSE,
                array = c(abs(teams$highend-teams$total)),
                arrayminus = c(teams$total-teams$lowend)))%>%
      layout(showlegend = FALSE) %>% 
      config(displayModeBar = F) %>%
      layout(xaxis = x, yaxis = y)})
    })
  

  
  
}