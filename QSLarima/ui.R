library(shiny)
library(dygraphs)
library(plotly)
library(timevis)

shinyUI(
  navbarPage(
    "Quit Smoking Line",
    
    tabPanel(
      "Home",
      h2("Intervention time series analysis of the smoking cessation 
                quitline: a 16-years retrospective study in Sweden", 
         align = "center"),
      br(),
      h4("Four public health interventions on the number of phone calls received 
        by the Swedish quitline"),
      timevisOutput("timeline"),
      br(),
      uiOutput("title_dy_ts"),
      checkboxInput("log", "Display counts", FALSE),
      #dygraphOutput("dy_ts"),
      plotlyOutput("pl_ts"),
      br()
  ),
  
  tabPanel("1st Interv",
           h2("Effect of a campaign on passive smoking (Jan 2001)"),
           br(),
           plotlyOutput("pl_1Int"),
           br(),
           h3("Rate Ratio comparing after vs pre intervantion"),
           selectInput("tInt1", "Select month after intervantion:", 
                       choices = 1:c(interval(data_milestone$start[1], data_milestone$wupp[1]) %/% months(1)),
                       selected = 6),
           uiOutput("Effect1")
           ),
  tabPanel("2nd Interv",
           h2("Effect of larger warnings on cigarette packs (Sept 2002)"),
           br(),
           plotlyOutput("pl_2Int"),
           br(),
           h3("Rate Ratio comparing after vs pre intervantion"),
           selectInput("tInt2", "Select month after intervantion:", 
                       choices = 1:c(interval(data_milestone$start[2], data_milestone$wupp[2]) %/% months(1)),
                       selected = 6),
           uiOutput("Effect2")
           ),
  tabPanel("3rd Interv",
           h2("Effect of banning smoking in restaurants (Jun 2005)"),
           br(),
           plotlyOutput("pl_3Int"),
           br(),
           h3("Rate Ratio comparing after vs pre intervantion"),
           selectInput("tInt3", "Select month after intervantion:", 
                       choices = 1:c(interval(data_milestone$start[3], data_milestone$wupp[3]) %/% months(1)),
                       selected = 6),
           uiOutput("Effect3")
           ),
  tabPanel("4th Interv",
           h2("Effect of a 10% tobacco tax increase (Jan 2012)"),
           br(),
           plotlyOutput("pl_4Int"),
           br(),
           h3("Rate Ratio comparing after vs pre intervantion"),
           selectInput("tInt4", "Select month after intervantion:", 
                       choices = 1:c(interval(data_milestone$start[4], data_milestone$wupp[4]) %/% months(1)),
                       selected = 6),
           uiOutput("Effect4")
           ),
  tabPanel("Report",
           includeMarkdown("QTL_Monthly_Log_V4.md")
           )
  )
)