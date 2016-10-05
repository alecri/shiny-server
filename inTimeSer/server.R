library(shiny)
library(timevis)
library(dygraphs)

## data for pipeline
data <- data.frame(
   id      = 1:4,
   content = c("Item one", "Item two",
               "Ranged item", "Item four"),
   start   = c("2016-01-10", "2016-01-11",
               "2016-01-20", "2016-02-14 15:00:00"),
   end     = c(NA, NA, "2016-02-04", NA)
)

## data for time serie
lungDeaths <- cbind(mdeaths, fdeaths)

shinyServer(function(input, output, session){
   
   ## pipe line
   output$timeline <- renderTimevis({
      timevis(data)
   })
   
   ## time serie
   predicted <- reactive({
      hw <- HoltWinters(ldeaths)
      predict(hw, n.ahead = input$months, 
              prediction.interval = TRUE,
              level = as.numeric(input$interval))
   })
   
   output$dygraph <- renderDygraph({
      dygraph(predicted(), main = "Predicted Deaths/Month") %>%
         dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
         dyOptions(drawGrid = input$showgrid)
   })
   
   output$from <- renderText({
      if (!is.null(input$dygraph_date_window))
         strftime(input$dygraph_date_window[[1]], "%d %b %Y")      
   })
   
   output$to <- renderText({
      if (!is.null(input$dygraph_date_window))
         strftime(input$dygraph_date_window[[2]], "%d %b %Y")
   }) 
   
   
})
