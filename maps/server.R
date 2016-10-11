library(shiny)
library(sp)
library(maptools)
library(tidyverse)
library(leaflet)
library(raster)
library(data.table)
library(DT)
library(swemaps)

# Define server logic required to draw a histogram
shinyServer(function(input, output){
   
   # reactive datasets
   map_knd <- reactive({
      data <- map_kn
      if (input$region != "") data <- data[data$lnnamn == input$region, ]
      data
   })
   citiesd <- reactive({
      cities_neg <- if (input$negVal){
         cities[cities[[input$year]] < 0, ]
      } else {
         cities
      }
      cities_neg$text <- paste0("Locations: ", cities_neg$Locations, "<BR>", 
                                input$year, ": ", cities_neg[[input$year]])
      cities_neg
   })
   
   
   # data output
   output$data_sweden <- renderDataTable({
      map_ln %>% 
         distinct(lnkod, lnnamn, y_2012, y_2013, y_2014)
   })
   output$data_sweden_region <- renderDataTable({
      map_knd() %>% 
         distinct(knkod, knnamn, lnnamn, y_2012, y_2013, y_2014)
   })
   output$data_sweden_cities <- renderDataTable({
      citiesd()[, c("Locations", "y_2012", "y_2013", "y_2014")]
   })

   # basic maps
   output$maps <- renderLeaflet({
     leaflet(map_ln) %>% 
         addTiles() %>%
         setView(lng = 16.31667, lat = 62.38333, zoom = 5)
   })
   output$maps_region <- renderLeaflet({
      leaflet(map_kn) %>% 
         addTiles() %>%
         setView(lng = 16.31667, lat = 62.38333, zoom = 5)
   })
   output$map_cities <- renderLeaflet({
      leaflet(data = citiesd()) %>% 
         addTiles() %>%
         setView(lng = 16.31667, lat = 62.38333, zoom = 5) %>%
            addMarkers(~long, ~lat, popup = ~text)
   })
   
   ## modifying maps sweden
   observe({
     colorBy <- input$year
     colorData <- map_ln[[colorBy]]
     pal <- colorNumeric("Blues", colorData)
     map_ln$color <- pal(colorData)
     map_ln$text <- paste0("Län: ", map_ln$lnnamn, "<BR>", 
                           input$year, ": ", map_ln[[input$year]])
     
     leafletProxy("maps", data = map_ln) %>%
        clearShapes() %>%
        clearControls()
     ## progress bar
      withProgress(message = 'Loading map', value = 0, {
         for (ln in unique(map_ln$lnkod)){
            incProgress(1/length(unique(map_ln$lnkod)))
            leafletProxy("maps", data = map_ln[map_ln$lnkod == ln, ]) %>%
               addPolygons(~ leaflet_long, ~ leaflet_lat, 
                           color = ~color[[1]], weight = 1,
                           popup = ~text)
         }
      })   
      leafletProxy("maps") %>%
        addLegend("bottomright", pal = pal, values = colorData, title = colorBy)
   })
   
   # modifying maps region
   observe({
      colorBy <- input$year
      colorData <- map_knd()[[colorBy]]
      pal <- colorNumeric("Blues", colorData)
      color <- pal(colorData)
      text <- paste0("Län: ", map_knd()$NAME_1, "<BR>",
                     "Kommuner: ", map_knd()$NAME_2, "<BR>",
                     input$year, ": ", map_knd()[[input$year]])

      if (input$region != ""){
         leafletProxy("maps_region", data = map_knd()) %>%
            clearShapes() %>%
            clearControls()
         ## progress bar
         withProgress(message = 'Loading map', value = 0, {
            for (kn in unique(map_knd()[["knkod"]])){
               incProgress(1/length(unique(map_knd()[["knkod"]])))
               leafletProxy("maps_region", data = map_knd()[map_knd()[["knkod"]] == kn, ]) %>%
                  addPolygons(~leaflet_long, ~leaflet_lat,
                              color = color[[1]], weight = 1,
                              popup = text)
            }
         }) 
         leafletProxy("maps_region", data = map_knd()) %>%
         addLegend("bottomright", pal = pal, values = colorData, title = colorBy) %>%
            fitBounds(
               lng1 = min(map_knd()$leaflet_long), lat1 = min(map_knd()$leaflet_lat),
               lng2 = max(map_knd()$leaflet_long), lat2 = max(map_knd()$leaflet_lat)
            )
      }

   })
 
   ## modifying maps cities
   observe({
      leafletProxy("map_cities", data = citiesd()) %>%
         clearMarkers() %>%
         addMarkers(~long, ~lat, popup = ~text)
   })
   
})