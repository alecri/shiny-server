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
      dygraphOutput("dy_ts"),
      plotlyOutput("pl_ts"),
      br()
  ),
  
  tabPanel("1st Interv",
           h2("Effect of a campaign on passive smoking (Jan 2001)"),
           br(),
           plotlyOutput("pl_1Int")
           ),
  tabPanel("2nd Interv",
           h2("Effect of larger warnings on cigarette packs (Sept 2002)"),
           br(),
           plotlyOutput("pl_2Int")
           ),
  tabPanel("3rd Interv",
           h2("Effect of banning smoking in restaurants (Jun 2005)"),
           br(),
           plotlyOutput("pl_3Int")
           ),
  tabPanel("4th Interv",
           h2("Effect of a 10% tobacco tax increase (Jan 2012)"),
           br(),
           plotlyOutput("pl_4Int")
           ),
  tabPanel("Report"
           #includeHTML("QTL_Monthly_Log_V4.html")
           )
  )
)