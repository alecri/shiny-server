library(shiny)
library(raster)
library(maptools)
library(rgeos)
library(broom)
library(ggplot2)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
   output$data_sweden <- renderDataTable({
      unique(subset(sweden_plot, select = c("id", "NAME_1", "y_2012", "y_2013", "y_2014")))
   })
   
   output$data_sweden_region <- renderDataTable({
      unique(subset(sweden_plot2, select = c("id", "NAME_2", "y_2012", "y_2013", "y_2014")))
   })
   
   
   output$maps <- renderPlotly({
      p <- ggplot(sweden_plot, aes_string(x = "long", y = "lat", group = "group", 
                                          fill = input$year)) +
         geom_polygon() + coord_quickmap() +
         ggthemes::theme_map() + theme(legend.position = c(.8, .2))
      p
      ggplotly(p)
   })
   
   output$maps_region <- renderPlotly({
      p <- ggplot(subset(sweden_plot2, NAME_1 == input$region), 
                         aes_string(x = "long", y = "lat", group = "group", 
                                          fill = input$year)) +
         geom_polygon() + coord_quickmap() +
         ggthemes::theme_map() + theme(legend.position = c(.8, .2))
      ggplotly(p)
   })
   
})