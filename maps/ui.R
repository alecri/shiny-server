library(shiny)
library(sp)
library(leaflet)

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
            width = 2),
            
            mainPanel(
                 leafletOutput("maps", height = 800),
                 br(),
                 dataTableOutput('data_sweden')
            )
         )
      ),
      
      tabPanel(
         "Regions",
         
         sidebarLayout(
            sidebarPanel(
               selectizeInput("region", label = h3("Choose region"), 
                           choices = regions,
                           options = list(
                              placeholder = 'Please select an option below',
                              onInitialize = I('function() { this.setValue(""); }')
                           )
               ),
               width = 2
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
               leafletOutput("maps_region", height = 800),
               br(),
               dataTableOutput('data_sweden_region')
            )
         )
      ),
      
      tabPanel(
         "Cities",
         
         sidebarLayout(
            sidebarPanel(
               checkboxInput("negVal", "Dispaly only negative values", value = FALSE),
               width = 2
            ),
            
            mainPanel(
                  leafletOutput("map_cities", height = 800),
                  br(),
                  dataTableOutput('data_sweden_cities')
            )
         )
      )
      
   )
)