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
          size = ~points, text = ~paste("Name: ", player,"<br>VOR: ",vor,"<br>Points: ", points),mode = "markers")
}

get_bench=function(file,position){
  ##high vor & upper
  subs=file[file$position==position,]
  plot_ly(subs, x = ~vor, y = ~upper, color = ~dropoff,
          size = ~points, text = ~paste("Name: ", player,"<br> VOR: ",vor))
}


ui = fluidPage(theme = shinytheme("yeti"),tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "icomoon.css"),tags$style(
  HTML("
       
       .typeahead, .tt-query, .tt-hint {
       font-size: 14px !important;
       height: 25px !important;
       }
        .no-underline:hover {
        text-decoration: none;
       opacity: 0.5;
       }"))),navbarPage(title = "Fantasy Football Quick Analysis",tabPanel("Main",
    fluidRow(column(width=4,
                    h2("Draft Order Inputs"),
                    hr(),
                    helpText("Drafted Player from other Team"),
                    textInput.typeahead(id="delete_pick",
                                        placeholder="Type players name",
                                        local=data.frame(name=c(player_ids)),
                                        valueKey = "name",
                                        limit=5,
                                        tokens=c(1:length(player_ids)),
                                        template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>")
                    ),
                    actionButton("button2", "Submit"),
                    helpText("My Pick"),
                    textInput.typeahead(id="add_to_team",
                                        placeholder="Type players name",
                                        local=data.frame(name=c(player_ids)),
                                        valueKey = "name",
                                        limit=5,
                                        tokens=c(1:length(player_ids)),
                                        template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>")
                    ),
                    actionButton("button1", "Submit")
  ),column(width=8,h4("My Team"),hr(),
           DT::dataTableOutput('team_table'),
           htmlOutput("sum"))),
  hr(),
  fluidRow(column(width=6,h3("QB"),plotlyOutput("QB")),
           column(width=6,h3("RB"),plotlyOutput("RB"))),
  hr(),
  fluidRow(column(width=6,h3("WR"),plotlyOutput("WR")),
           column(width=6,h3("TE"),plotlyOutput("TE"))),
  hr(),
  fluidRow(column(width=6,h3("K"),plotlyOutput("K")),
           column(width=6,h3("DST"),plotlyOutput("DST")))
  ,hr(),
  
  h3("All Available Players"),
  DT::dataTableOutput('tbl'),
  hr(),
  HTML("<footer>
       <font size= '5px'>
       <a href='https://www.linkedin.com/in/christophvel' target='_blank' class='no-underline'><i class='icon-linkedin'></i></a>
       <a href='https://github.com/velaraptor' target='_blank' class='no-underline'><i class='icon-github'></i></a>
       </font>
       <br>
       <font size='1px'>
       Data used from http://fantasyfootballanalytics.net</font>
       </footer>")
       )
  )
  
  )