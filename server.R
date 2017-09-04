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
      HTML(paste("<b><h5>Total Team Points:",  round(sum(team$df$points),2),"<br>Lower End Team Points: ",round(sum(team$df$lower),2), "<br>Upper End Team Points: ",round(sum(team$df$upper),2),"</h5></b>" ))
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
  

  
  
}