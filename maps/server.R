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
      if (!is.null(input$region)) data <- data[data$lnnamn %in% input$region, ]
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
         group_by(lnkod, lnnamn) %>%
         summarise_each(funs(mean), y_2012, y_2013, y_2014)
   })
   output$data_sweden_region <- renderDataTable({
      map_knd() %>% 
         group_by(knkod, knnamn) %>%
         summarise_each(funs(mean), y_2012, y_2013, y_2014)
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
     
     mapknd <- map_knd()
      colorBy <- input$year
      colorData <- mapknd[[colorBy]]
      pal <- colorNumeric("Blues", colorData)
      mapknd$color <- pal(colorData)
      mapknd$text <- paste0("Län: ", mapknd$lnnamn, "<BR>",
                     "Kommuner: ", mapknd$knnamn, "<BR>",
                     input$year, ": ", mapknd[[input$year]])

      if (!is.null(input$region)){
         leafletProxy("maps_region", data = mapknd) %>%
            clearShapes() %>%
            clearControls()
         ## progress bar
         withProgress(message = 'Loading map', value = 0, {
            for (kn in unique(mapknd[["knkod"]])){
               incProgress(1/length(unique(mapknd[["knkod"]])))
               leafletProxy("maps_region", data = mapknd[mapknd[["knkod"]] == kn, ]) %>%
                  addPolygons(~leaflet_long, ~leaflet_lat,
                              color = ~color[[1]], weight = 1,
                              popup = ~text)
            }
         })
         leafletProxy("maps_region", data = mapknd) %>%
         addLegend("bottomright", pal = pal, values = colorData, title = colorBy) %>%
            fitBounds(
               lng1 = min(mapknd$leaflet_long), lat1 = min(mapknd$leaflet_lat),
               lng2 = max(mapknd$leaflet_long), lat2 = max(mapknd$leaflet_lat)
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