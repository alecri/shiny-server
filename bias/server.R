require("shiny")
require("plotly")
require("reshape2")

Sys.setenv("plotly_username"="alecri")
Sys.setenv("plotly_api_key"="nr287vef19")
#load("bias.RData")
load("bias151010.RData")
RR <- 2
CV_beta <- seq(0.01, 1.5, length.out = 50)
cv_s2s <- seq(0.4, 5, length.out = 50)
tau2 <- (CV_beta*log(RR))^2
data.fit <- expand.grid(list(x = CV_beta, y = cv_s2s))


# Define a server for the Shiny app
shinyServer(function(input, output) {
   
   ## Selecting data
   data_lim <- reactive({
      if (input$scale == "bias"){
         subset(bias, S == input$k & R == input$R)
      } else {
         subset(meanMeasure, S == input$k & R == input$R)
      }
   })
   
   ## Interactively changing the main and label of the figures
   label <- reactive({
      ifelse(input$scale == "bias", "Bias (%)", "Mean")
   })
   
   output$lab1 <- renderUI({ 
      h4(strong(withMathJax(paste("$$\\text{", label(),  "as a function of } CV_b 
                                  \\text{ and } CV_{v_i} \\text{ fixing K and R} $$"))))
   })
   output$lab2 <- renderUI({ 
      h4(strong(withMathJax(paste("$$\\text{Averaging", input$scale, "over } CV_b$$"))))
   })

   ## Deciding if presenting the graphs using plot_ly (default: plotly)
   ## & if they should be presented seperately (default: FALSE)
   output$threeD_plot <- renderUI({
      if (input$plotly){
         if (input$overlaid){
            list(
               plotlyOutput("plotlyOverlaid"),
               uiOutput("lab2"),
               plotlyOutput("plot2d")
            )
         } else {
            list(
               plotlyOutput("plotlyRb"),
               plotlyOutput("plotlyI2"),
               plotlyOutput("plotlyRi"),
               uiOutput("lab2"),
               plotlyOutput("plot2d")
            )
         }
      } else {
         list(
            plotOutput("plot3dOld"),
            uiOutput("lab2"),
            plotOutput("plot2dOld")
         )
      }
      
   })
   
      
   zRb <- reactive({
      zRb <- with(data_lim(), matrix(Ristar, ncol = 50, nrow = 50, byrow = T))
      dataRb <- data.frame(x = rep(CV_beta, times = 50), y = rep(cv_s2s, each = 50), z = c(zRb))
      data.loessRb <- loess(z ~ x*y, data = dataRb)
      zRbpred <- predict(data.loessRb, newdata = data.fit)
      if (input$loess) 
         zRbpred
      else
         zRb
   })
   zI2 <- reactive({
      zI2 <- with(data_lim(), matrix(I2, ncol = 50, nrow = 50, byrow = T))
      dataI2 <- data.frame(x = rep(CV_beta, times = 50), y = rep(cv_s2s, each = 50), z = c(zI2))
      data.loessI2 <- loess(z ~ x*y, data = dataI2)
      zI2pred <- predict(data.loessI2, newdata = data.fit)
      if (input$loess) 
         zI2pred
      else
         zI2
   })
   zRi <- reactive({
      zRi <- with(data_lim(), matrix(Ri, ncol = 50, nrow = 50, byrow = T))
      dataRi <- data.frame(x = rep(CV_beta, times = 50), y = rep(cv_s2s, each = 50), z = c(zRi))
      data.loessRi <- loess(z ~ x*y, data = dataRi)
      zRipred <- predict(data.loessRi, newdata = data.fit)
      if (input$loess) 
         zRipred
      else
         zRi
   })
   
   biasMean <- reactive({
      biasMean <- do.call(
         "rbind", 
         lapply(split(data_lim()[c("Ri", "Ristar", "I2")], data_lim()["cv_s2s"]),
                function(x) apply(x, 2, mean)))
      biasMean <- reshape2::melt(biasMean)
      names(biasMean) <- c("cv_s2s", "measure", "bias")
      levels(biasMean$measure)[levels(biasMean$measure) == "Ristar"] <- "Rb"
      biasMean
   })
   
   ## For comparable axes
   rangelim <- reactive({
      #if (input$range)
         c(min(zRb(), zI2(), zRi()), max(zRb(), zI2(), zRi()))
      #else NULL
   })


   ## 3d plot with plotly
   plotlyRb <- reactive({
      plot_ly(x = CV_beta, y = cv_s2s, z = zRb(), name = "R_b", type = "surface")
   })
   plotlyI2 <- reactive({
      plot_ly(x = CV_beta, y = cv_s2s, z = zI2(), name = "I^2", type = "surface")
   })
   plotlyRi <- reactive({
      plot_ly(x = CV_beta, y = cv_s2s, z = zRi(), name = "R_I", type = "surface") 
   })
   
   output$plotlyRb <- renderPlotly({
      plotlyRb() %>%
         layout(title = "R_b",
                scene = list(zaxis = list(title = label(), range = rangelim()),
                             xaxis = list(title = "CV_b", range = range(CV_beta)),
                             yaxis = list(title = "CV_vi", range = range(cv_s2s))
                )
         )
   })
   output$plotlyI2 <- renderPlotly({
      plotlyI2() %>%
         layout(title = "I^2",
                scene = list(zaxis = list(title = label(), range = rangelim()),
                             xaxis = list(title = "CV_b", range = range(CV_beta)),
                             yaxis = list(title = "CV_vi", range = range(cv_s2s))
                )
         )
   })
   output$plotlyRi <- renderPlotly({
      plotlyRi() %>%
         layout(title = "R_I",
                scene = list(zaxis = list(title = label(), range = rangelim()),
                             xaxis = list(title = "CV_b", range = range(CV_beta)),
                             yaxis = list(title = "CV_vi", range = range(cv_s2s))
                )
         )
   })
   
   ## combining 3d plotly in one
   output$plotlyOverlaid <-  renderPlotly({
      subplot(plotlyRb(), plotlyI2(), plotlyRi()) %>%
         layout(scene = list(zaxis = list(title = paste("\n\n" , label()), range = rangelim()),
                             xaxis = list(title = "CV_b", range = range(CV_beta)),
                             yaxis = list(title = "CV_vi", range = range(cv_s2s)))) 
   })


   ## 2d plotly for the percent bias with plotly
   output$plot2d <- renderPlotly({
      p <- ggplot(biasMean()) + geom_line(aes(x=cv_s2s, y=bias, colour=measure)) +
                 theme_minimal() + labs(x = "CV_vi", y = label())
      if (input$overlaid != TRUE){
         p <- p + facet_wrap(~measure)
      }
      ggplotly(p)
   })

   
   ## ------------------------------------------ plot WITHOUT plotly
   
   output$plot3dOld <- renderPlot({
      par(mfrow = c(1, 3))
      persp(CV_beta, cv_s2s, zRb(), phi = 45, theta = 45, main = expression(R[b]),
            xlab = "\n\n CV_b", ylab = "\n\n CV_vi", zlab = paste("\n\n" , label()), 
            ticktype = "detailed", cex.lab = 1.5, cex.main = 3)
      persp(CV_beta, cv_s2s, zI2(), phi = 45, theta = 45, cex.lab = 1.5, cex.main = 3,
            xlab = "\n\n CV_b", ylab = "\n\n CV_vi",  zlab = paste("\n\n" , label()),
            ticktype = "detailed", main =  expression(I^2))
      persp(CV_beta, cv_s2s, zRi(), phi = 45, theta = 45, cex.lab = 1.5, cex.main = 3,
            xlab = "\n\n CV_b", ylab = "\n\n CV_vi",  zlab = paste("\n\n" , label()),
            ticktype = "detailed", main = expression(R[I]))
   })
   
   output$plot2dOld <- renderPlot({
      p <- ggplot(biasMean()) + geom_line(aes(x=cv_s2s, y=bias, colour=measure)) +
         theme_minimal() + labs(x = expression(CV[v[i]]), y = label())
      if (input$overlaid != TRUE){
         p <- p + facet_wrap(~measure)
      }
      p
   })
   
   
   
})

