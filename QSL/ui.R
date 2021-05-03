library(shiny)
library(dygraphs)
library(timevis)
library(plotly)

shinyUI(fluidPage(
   "Quit Smoking Line",
   h1("Evaluating the Impact of Health Warnings"),
   h4("Designed by ", a("A. Crippa", target="_blank", target = "_blank", 
                        href = "http://alecri.github.io/")),
   h3("Policy Events and News Media in the Tobacco Sector 1998-2016"),
   
   sidebarLayout(
      sidebarPanel(
         h2(uiOutput("initial_message")),
         h4(uiOutput("initial_message2")),
         h4(uiOutput("date_selected")),
         uiOutput("dates"),
         uiOutput("date_range_selected"),
         uiOutput("lin_selected"),
         uiOutput("season_selected")
         ),
      
      mainPanel(
         tabsetPanel(
            type = "tabs", 
            tabPanel(
               "Homepage",
               timevisOutput("timeline"),
               plotlyOutput("qsl_ts"),
               dataTableOutput("effects")
            ),
            tabPanel("Analytical results",
                     conditionalPanel(
                        "input.mod_lin == true",
                        verbatimTextOutput("modlin")
                     ),
                     conditionalPanel(
                        "input.mod_season == true",
                        verbatimTextOutput("modseason")
                     )
            ),
            tabPanel("Tabular data",
                     dataTableOutput("table")
            )
         )
      )
   )
))