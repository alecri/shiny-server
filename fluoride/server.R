library(shiny)
library(plotly)
library(tidyverse)
library(gridExtra)
library(dosresmeta)
library(rms)
library(shinymanager)

server <- shinyServer(function(input, output, session) {
  
   # call the server part
   # check_credentials returns a function to authenticate users
   res_auth <- secure_server(
      check_credentials = check_credentials(credentials)
   )
   
  # interactive titles and labels
  output$analysis_title <- renderUI({
    main <- paste("Analysis for studies with tag:", 
                  ifelse(input$tag == "|learning|", "learning", "memory"))
    submain <- if (input$subgrp_eff != "None"){
      paste("Limited to effect subtype:", input$subgrp_eff)
    }
    tagList(
      h3(main, align = "center"),
      h3(submain, align = "center")
    )
  })

   # interactive dataset
   fluor <- reactive({
     fluor <- subset(fluo, tags == input$tag)
     if (input$exclude){
       fluor <- subset(fluo, dose < input$doselim)
       # exclude studies with only referent observation
       id_keep <- names(which(with(fluor, table(`endpoint id`)) > 1))
       fluor <- subset(fluor, `endpoint id` %in% id_keep)
     }
     if (input$subgrp_eff != "None"){
       fluor <- subset(fluor, `effect subtype` == input$subgrp_eff)
     }
     fluor
   })
   
   # avoiding error display in the web app
   error <- reactive({
      if (length(unique(fluor()[["endpoint id"]])) <= 2){
         return(TRUE)
      } else {
         return(FALSE)
      }
   })
   
   # table for descriptives
   fluor_descr <- reactive({
      fluor() %>% 
         filter(dose > 0) %>%
         group_by(tags, `effect subtype`) %>%
         summarise(n_studies = length(unique(`endpoint id`)), n_obs = n())
   })
   output$descrText <- renderUI({ 
      p(paste("Total number of ", sum(fluor_descr()$n_studies), " studies including", sum(fluor_descr()$n_obs), "datapoints"))
   })
   output$descr <- renderDataTable({
      fluor_descr()
   })
   
   # dose-response models (spline + linear)
   lin <- reactive({
      if (error()) return(NULL)
      dosresmeta(formula = response ~ dose, id = `endpoint id`,
                 sd = stdev, n = N, covariance = "smd", data = fluor(),
                 proc = "2stage")
   })
   output$linear <- renderPrint({
      if (error()) return(NULL)
      summary(lin())
   })
   spl <- reactive({
      if (error()) return(NULL)
      k <- with(fluor(), quantile(dose, c(.25, .5, .75)))
      dosresmeta(formula = response ~ rcs(dose, k), id = `endpoint id`,
                     sd = stdev, n = N, covariance = "smd", data = fluor(),
                     proc = "1stage")
   })
   output$spline <- renderPrint({
      if (error()) return(NULL)
      summary(spl())
   })
   # predictions
   output$pred <- renderDataTable({
      if (error()) return(NULL)
      round(data.frame(newdata_tab, predict(spl(), newdata_tab)[, -c(1:2)]), 2)
   })

   
   # rendering plot
   output$p_pcm <- renderPlotly({
     if (error()) return(NULL)
     ggplotly(ggplot(subset(fluor(), `dose index` != 0), 
                     aes(x = id_plot, y = `percent control mean`,
                         ymin =`percent control low`, 
                         ymax = `percent control high`)) + 
                geom_pointrange() + coord_flip() + 
                geom_hline(aes(yintercept = 0), lty = 2) +
                xlab("") + ylab("Percent control mean")
     )
   })
   output$hist_pcm <- renderPlotly({
      if (error()) return(NULL)
      ggplotly(ggplot(subset(fluor(), `dose index` != 0), 
                      aes(x = `percent control mean`)) + geom_histogram())
   })
   output$p_smd <- renderPlotly({
      if (error()) return(NULL)
      ggplotly(ggplot(subset(fluor(), `dose index` != 0), 
                      aes(x = id_plot, y = smd, ymin = smd_low, ymax = smd_upp)) + 
                  geom_pointrange() + coord_flip() + geom_hline(aes(yintercept = 0), lty=2) +
                  xlab("") + ylab("Standardized mean differences")
      )
   })
   output$hist_smd <- renderPlotly({
      if (error()) return(NULL)
      ggplotly(ggplot(subset(fluor(), `dose index` != 0), aes(x = smd)) + 
                 geom_histogram() + xlab("Standardized mean differences"))
   })
   output$spaghetti <- renderPlotly({
      if (error()) return(NULL)
      ggplotly(
         ggplot(fluor(), aes(dose, smd, group = `endpoint id`, 
                             shape = `study name`)) + 
            scale_shape_manual(values = seq_along(unique(fluo$`study name`))) +
            labs(shape = "author") +
            geom_line() + geom_point() + theme_bw() + 
            ylab("Standardized mean differences") + xlab("Fluoride, ppm")
      )
   })
   output$pooled <- renderPlotly({
      if (error()) return(NULL)
         newdata <- data.frame(dose = seq(min(fluor()$dose), max(fluor()$dose), length.out = 100))
         pred <- ggplot(predict(spl(), newdata), 
                        aes(x = newdata$dose, y = pred, ymin = ci.lb, ymax = ci.ub)) +
            geom_line() + geom_ribbon(alpha = .1) + theme_bw() +
            xlab("Fluoride, ppm") + ylab("standardized mean difference") +
            geom_hline(aes(yintercept =  0), lty = 3) + 
            geom_line(data = predict(lin(), newdata), aes(y = pred), lty = 2)
         ggplotly(pred)
   })
   
   output$vpcPlot <- renderPlotly({
      if (error()) return(NULL)
      ggplotly(
         ggplot(data.frame(x = fluor()$dose[fluor()$`dose index` != 0], y = vpc(spl())),
                aes(x = x, y = y)) + geom_point() + geom_smooth(se = F, method = "loess") + 
            theme_bw() + xlab("Fluoride, ppm") + ylab("vpc")
      )
   })
})