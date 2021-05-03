# global.R for fluoride shiny app

library(shiny)
library(plotly)

fluidPage(
  
  titlePanel("Learning and Memory: Morris Water Maze"),

  sidebarLayout(
    
    # sidebar for chosing input 
    sidebarPanel(
      
      # only separate for different tags
      radioButtons("tag", h3("Select tag:"), 
                   list("Learning" = "|learning|", "Memory" = "|memory|"),
                   selected = "|learning|"),
      checkboxInput("exclude", "Limite fluoride range", value = FALSE),
      conditionalPanel("input.exclude == true",
                       sliderInput("doselim", 
                                   "Limit the analysis to doses less than",
                                   min = 10, max = max(fluo$dose), value = 30, 
                                   step = 1)
      ),
      selectInput("subgrp_eff", "Select specific effect*:", 
                  list("None" = "None", 
                       "Escape time" = "Escape time", 
                       "Platform crossing" = "Platform crossing",
                       "Time in platform area/location" = "Time in platform area/location",
                       "Time in target quadrant" = "Time in target quadrant"),
                  selected = "None"),
      p("* Results for subgroup analyses are displayed only if results 
        from al least three studies are available (n_studies > 2 in 'Study characteristics') "),
      br(),
      uiOutput("analysis_title")
    ),
    
    # main panel for results
    mainPanel(
      tabsetPanel(
        tabPanel("Descriptives",
                 fluidRow(
                   h3("Forest plot", align = "center"),
                   column(6, plotlyOutput("p_pcm", height = 800)),
                   column(6, plotlyOutput("p_smd", height = 800))
                 ),
                 fluidRow(
                   h3("Histogram of the response variables", align = "center"),
                   column(6, plotlyOutput("hist_pcm", height = 250)),
                   column(6, plotlyOutput("hist_smd", height = 250))
                 ),
                 br(), br(),
                 h3("Study characteristics", align = "center"),
                 br(),
                 uiOutput("descrText"),
                 dataTableOutput("descr")
        ),
        tabPanel("Dose-response",
                 fluidRow(
                   h3("Spaghetti plot", align = "center"),
                   plotlyOutput("spaghetti"),
                   h3("Pooled dose-response curve", align = "center"),
                   plotlyOutput("pooled"),
                   h3("Pooled standarized mean differences for selected fluoride doses",
                      align = "center"),
                   dataTableOutput("pred")
                 )
        ),
        tabPanel("Analytical results",
                 fluidRow(
                   h4("R output for the linear model", align = "center"),
                   verbatimTextOutput("linear"),
                   h4("R output for the spline model", align = "center"),
                   verbatimTextOutput("spline"),
                   h4("vcp plot for heterogeneity", align = "center"),
                   plotlyOutput("vpcPlot")
                 )
        )
      )
    )
  )
)