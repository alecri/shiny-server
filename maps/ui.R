library(shiny)
library(plotly)
library(googleVis)

swe <- getData("GADM", country = "SWE", level = 1)

shinyUI(
   navbarPage(
      "Template for map of Sweden",

      ## Tab for the whole sweden
      tabPanel(
         "Sweden",
         
         sidebarLayout(
            sidebarPanel(
               selectInput("year", label = h3("Choose year"), 
                           choices = list("2012" = "y_2012", "2013" = "y_2013",
                                          "2014" = "y_2014"), selected = "y_2014"),
               selectInput("ind", label = h3("Choose indicator"), 
                           choices = list("ind1" = 1, "ind2" = 2,
                                          "ind3" = 3), selected = 1),
            width = 2),
            
            mainPanel(
               fluidRow(
                  column(6, plotlyOutput("maps", height = 1000, width = 1000*13/26)),
                  column(4, dataTableOutput('data_sweden'))
                  )
            )
         )
      ),
      
      tabPanel(
         "Region",
         
         sidebarLayout(
            sidebarPanel(
               selectizeInput("region", label = h3("Choose region"), 
                           choices = regions,
                           options = list(
                              placeholder = 'Please select an option below',
                              onInitialize = I('function() { this.setValue(""); }')
                           )
               ),
               selectInput("ind", label = h3("Choose indicator"), 
                           choices = list("ind1" = 1, "ind2" = 2,
                                          "ind2" = 3), selected = 1)
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
               fluidRow(
                  column(6, plotlyOutput("maps_region", height = 1000, width = 1000*13/26)),
                  column(4, dataTableOutput('data_sweden_region'))
               )
            )
         )
      ),
      
      tabPanel(
         "Google maps",
         
         sidebarLayout(
            sidebarPanel(
               p("Text")
            ),
            
            mainPanel(
               fluidRow(
                  column(6, htmlOutput("google_maps"))
               )
            )
         )
      )
      
   )
)
