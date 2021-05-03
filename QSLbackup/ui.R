library(shiny)
library(dygraphs)
library(timevis)

shinyUI(fluidPage(
   "Quit Smoking Line",
   h1("Evaluating the Impact of Health Warnings"),
   h4("Designed by ", a("A. Crippa", target="_blank", target = "_blank", 
                        href = "http://alecri.github.io/")),
   h3("Policy Events and News Media in the Tobacco Sector 1998-2016"),
   
   sidebarLayout(
      sidebarPanel(
         checkboxInput("mod_lin", label = "Linear trend", value = FALSE),
         checkboxInput("mod_season", label = "Seasonal trend", value = FALSE),
         helpText("Click and drag to zoom in (double click to zoom back out).")
      ),
      
      mainPanel(
         tabsetPanel(
            type = "tabs", 
            tabPanel(
               "Homepage",
               timevisOutput("timeline")
            ),
            tabPanel("Interrupted Time Serie",
                     dygraphOutput("its_graph"),
                     conditionalPanel(
                        "input.mod_lin == true",
                        uiOutput("effect_lin")
                     ),
                     conditionalPanel(
                        "input.mod_season == true",
                        uiOutput("effect_season")
                     )
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
            )
         )
      )
   )
))