library(shiny)
library(sp)
library(leaflet)
library(data.table)
library(DT)

## load data
load("www/grav_map.Rdata")

shinyUI(
   navbarPage(
      "Graviditet i Sverige",

      ## Tab for the whole sweden
      tabPanel(
         "Karta",
         
         sidebarLayout(
            sidebarPanel(
               selectInput("mått", label = h3("Mått"), choices = unique(grav_map$Mått),
                           selected = "Procent"),
               selectInput("year", label = h3("Year"), 
                           choices = list("2014" = "X2014", "2013" = "X2013", "2012" = "X2012")),
               selectInput("ålder", label = h3("Ålder"), choices = levels(grav_map$Ålder),
                           selected = "år Alla åldrar"),
               uiOutput("VarUI"),
            width = 2),
            
            mainPanel(
               leafletOutput("maps", height = 800)
            )
         )
      ),
      
      tabPanel("data",
               dataTableOutput("map_data")
               )
      
   )
)