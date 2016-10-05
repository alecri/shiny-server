library(shiny)
library(dygraphs)

shinyUI(fluidPage(
   headerPanel("Quit Smoking Line"),
   
   h2("Temporary template"),
   
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
         timevisOutput("timeline"),
         dygraphOutput("dygraph")
      )
   )
))
