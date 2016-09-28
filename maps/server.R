library(shiny)
library(sp)
library(maptools)
library(tidyverse)
library(leaflet)
library(raster)
library(data.table)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output){
   
   # reactive datasets
   sweden_reg <- reactive({
      data <- swe_data2
      if (input$region != "") data <- swe_data2[swe_data2$NAME_1 == input$region, ]
      data
   })

   sweden_cities <- reactive({
      cities_neg <- if (input$negVal){
         cities[cities[[input$year]] < 0, ]
      } else {
         cities
      }
      cities_neg
   })
   text_cities <- reactive({
      paste0("Locations: ", sweden_cities()$Locations, "<BR>", 
             input$year, ": ", sweden_cities()[[input$year]])
   }) 
   
   
   # data output
   output$data_sweden <- renderDataTable({
      data.frame(swe_data1_tab)
   })
   output$data_sweden_region <- renderDataTable({
      data.frame(swe_data2_tab[swe_data2_tab$NAME_1 == input$region, ])
   })
   output$data_sweden_cities <- renderDataTable({
      sweden_cities()[, c("Locations", "y_2012", "y_2013", "y_2014")]
   })

   # basic maps
   output$maps <- renderLeaflet({
     leaflet(swe_data1) %>% 
         addTiles() %>%
         setView(lng = 16.31667, lat = 62.38333, zoom = 5)
   })
   output$maps_region <- renderLeaflet({
      leaflet(swe_data2) %>% 
         addTiles() %>%
         setView(lng = 16.31667, lat = 62.38333, zoom = 5)
   })
   output$map_cities <- renderLeaflet({
      text <- paste0("Locations: ", sweden_cities()$Locations, "<BR>", 
                     input$year, ": ", sweden_cities()[[input$year]])
      leaflet(data = sweden_cities()) %>% 
         addTiles() %>%
         setView(lng = 16.31667, lat = 62.38333, zoom = 5) %>%
         addMarkers(~long, ~lat, popup = text_cities())
   })
   
   ## modifying maps sweden
   observe({
      colorBy <- input$year
      colorData <- swe_data1[[colorBy]]
      pal <- colorNumeric("Blues", colorData)
      text <- paste0("Län: ", swe_data1$NAME_1, "<BR>", input$year, ": ",
                     swe_data1[[input$year]])
      
      leafletProxy("maps", data = swe_data1) %>%
         clearShapes() %>%
         clearControls() %>%
         addPolygons(
            stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
            fillColor = pal(colorData), popup = text
         ) %>%
         addLegend("bottomright", pal = pal, values = colorData, title = colorBy)
   })
   
   ## modifying maps region
   observe({
      colorBy <- input$year
      colorData <- sweden_reg()[[colorBy]]
      pal <- colorNumeric("Blues", colorData)
      text <- paste0("Län: ", sweden_reg()$NAME_1, "<BR>", 
                     "Kommuner: ", sweden_reg()$NAME_2, "<BR>",
                     input$year, ": ", sweden_reg()[[input$year]])
      
      leafletProxy("maps_region", data = sweden_reg()) %>%
         clearShapes() %>%
         clearControls() %>%
         # fitBounds(
         #   lng1 = min(sweden_reg()$coord1), lat1 = min(sweden_reg()$coord2),
         #   lng2 = max(sweden_reg()$coord1), lat2 = max(sweden_reg()$coord2)
         # )
         addPolygons(
            stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
            fillColor = pal(colorData), popup = text
         ) %>%
         addLegend("bottomright", pal = pal, values = colorData, title = colorBy) %>%
         fitBounds(
            lng1 = min(sweden_reg()$coord1), lat1 = min(sweden_reg()$coord2),
            lng2 = max(sweden_reg()$coord1), lat2 = max(sweden_reg()$coord2)
         )
   })
   
   ## modifying maps cities
   observe({
      leafletProxy("map_cities", data = sweden_cities()) %>%
         clearMarkers() %>%
         addMarkers(~long, ~lat, popup = text_cities())
   })
   
})