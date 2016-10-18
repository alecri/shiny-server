library(shiny)
library(timevis)
library(dygraphs)

## data for pipeline
data <- data.frame(
   id      = 1:11,
   content = c("Quit Smoking Line started", "Campaign on passive smoking",
               "Larger warnings on cigarette packets + less harmful name prohibits", 
               "Indirect tobacco advertising bans", "Smoke-free restaurants",
               "Prohibition cigarettes selling to minors", "Tax increase on mainly snuff",
               "Tax increase", "Tax increase by 10%", "Tax cuts on tobacco ", "Tax increase"),
   start   = c("1998-09-14", "2001-01-01", "2002-09-30", "2003-01-01", "2005-06-01",
               "2005-07-01", "2007-01-01", "2011-01-01", "2012-01-01", "2014-01-01",
               "2015-01-01"),
   end     = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
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
