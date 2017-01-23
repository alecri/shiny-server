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
   
   map_datar <- reactive({
      map_data <- filter(grav_map, 
                         Mått == input$mått,
                         Ålder == input$ålder)
      if (input$mått != "Medelvärde"){
         if (input$Variabel %in% c("BMI", "Familjesituation", "Tobaksvanor")){
            map_data <- filter(map_data, Variabel == input$Variabel_cat)
         } else {
            map_data <- filter(map_data, Variabel == input$Variabel)
         }
      } else {
         map_data <- filter(map_data, Variabel == input$Variabel)
      }
      map_data
   })
   
   output$map_data <- renderDataTable({
      map_datar()
   })
   
   ## interactive user interfaces
   output$VarUI <- renderUI({
      if (input$mått == "Medelvärde"){
         selectInput("Variabel", label = h3("Variabel"), 
                     choices = c("Medelålder", "BMI medelvärde"))
      } else {
         tagList(
            selectInput("Variabel", label = h3("Variabel"), 
                        choices = c("BMI", "Familjesituation", "Tobaksvanor", "Preeklampsi", "Graviditetsdiabetes")),
            conditionalPanel("input.Variabel == 'BMI'",
                             selectInput("Variabel_cat", label = h3("BMI"), 
                                         choices = list("BMI < 18.5", "18.5 ≤ BMI < 25", "25 ≤ BMI < 30", "BMI ≥ 30"))),
            conditionalPanel("input.Variabel == 'Familjesituation'",
                             selectInput("Variabel_cat", label = h3("Familjesituation"), 
                                         choices = list("Sammanbor med barnafadern", "Annan familjesituation"))),
            conditionalPanel("input.Variabel == 'Tobaksvanor'",
                             selectInput("Variabel_cat", label = h3("Tobaksvanor"), 
                                         choices = list("Röker tre månader före aktuell graviditet",
                                                        "Röker i tidig graviditet", "Röker i sen graviditet",
                                                        "Snusar tre månader före aktuell graviditet", 
                                                        "Snusar i tidig graviditet", "Snusar i sen graviditet")))
         )
      }
   })
   
   
   ## default for maps
   output$maps <- renderLeaflet({
      leaflet(map_ln) %>% 
         addTiles() %>%
         setView(lng = 16.31667, lat = 62.38333, zoom = 5)
   })
   ## modifying maps sweden
   observe({
      #md <- map_data()
      colorBy <- input$year
      colorData <- map_datar()[[colorBy]]
      # pal <- colorNumeric("Blues", colorData)
      # md$color <- pal(colorData)
      # # map_ln$text <- paste0("Län: ", map_ln$lnnamn, "<BR>", 
      # #                       input$year, ": ", map_ln[[input$year]])
      # 
      # leafletProxy("maps", data = md) %>%
      #    clearShapes() %>%
      #    clearControls()
      # ## progress bar
      # withProgress(message = 'Loading map', value = 0, {
      #    for (ln in unique(md$lnkod)){
      #       incProgress(1/length(unique(md$lnkod)))
      #       leafletProxy("maps", data = md[md$lnkod == ln, ]) %>%
      #          addPolygons(~ leaflet_long, ~ leaflet_lat, 
      #                      color = ~color[[1]], weight = 1)
      #    }
      # })
      # leafletProxy("maps") %>%
      #    addLegend("bottomright", pal = pal, values = colorData, title = colorBy)
   })
   
})