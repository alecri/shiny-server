library(shiny)
library(dygraphs)
library(timevis)

shinyUI(navbarPage("Quit Smoking Line",
   #headerPanel("Quit Smoking Line"),
   #h2("Evaluating the Impact of Health Warnings", align = "center"),
   tabPanel(
     "Homepage",
     fluidPage(
       h1("Evaluating the Impact of Health Warnings"),
       h4("Designed by ", a("A. Crippa", target="_blank", target = "_blank", 
                            href = "http://alecri.github.io/"))),
     h3("Policy Events and News Media in the Tobacco Sector 1998-2016"),
     timevisOutput("timeline")
   ),
   
   tabPanel("Time Serie",
   sidebarLayout(
      sidebarPanel(
         numericInput("months", label = "Months to Predict", 
                      value = 72, min = 12, max = 144, step = 12),
         selectInput("interval", label = "Prediction Interval",
                     choices = c("0.80", "0.90", "0.95", "0.99"),
                     selected = "0.95"),
         checkboxInput("showgrid", label = "Show Grid", value = TRUE),
         hr(),
         div(strong("From: "), textOutput("from", inline = TRUE)),
         div(strong("To: "), textOutput("to", inline = TRUE)),
         br(),
         helpText("Click and drag to zoom in (double click to zoom back out).")
      ),
      
      mainPanel(
         dygraphOutput("dygraph")
      )
   )
   )
))
