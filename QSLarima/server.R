library(shiny)
library(timevis)
library(dygraphs)
library(plotly)
library(xts)
library(ggplot2)
library(stats)

shinyServer(function(input, output) {
  
  ## reactive output
  ## --------------------------------------------------------------------------
  output$title_dy_ts <- renderUI({
    if (input$log == FALSE){
      h4("Monthly calling rate per 100,000 smokers to the Swedish smoking 
         cessation quitline between January 1999 and December 2014")
    } else {
      h4("Monthly calls to the Swedish smoking 
         cessation quitline between January 1999 and December 2014")
    }
  })
  
  ## reactive computations
  ## --------------------------------------------------------------------------
  
  
  ## reactive output
  ## --------------------------------------------------------------------------
  
  ## pipe line for the considered interventions
  output$timeline <- renderTimevis({
    config <- list(
      editable = FALSE,
      multiselect = TRUE
    )
    timevis(data_milestone, options = config)
  })
  
  ## time serie calls
  output$dy_ts <- renderDygraph({
    dataxts <- as.xts(qsl_ts, order.by = qsl_ts$month)[, ifelse(input$log, "n", "rate")]
    dy_ts <- dygraph(dataxts) %>%
      dyAxis("y", label = ifelse(input$log, "Total number of calls", 
                                 "Calling rates per 100,000 smokers")
             ) %>%
      dyOptions(drawPoints = TRUE, pointSize = 3, logscale = (input$log == FALSE))
    if (!is.null(input$timeline_selected)){
      for (i in as.numeric(input$timeline_selected)){
        dy_ts <- dy_ts %>%
          dyEvent(data_milestone$start[data_milestone$id == i], 
                  data_milestone$content[data_milestone$id == i], labelLoc = "top") %>%
          dyShading(from = data_milestone$wlow[data_milestone$id == i], 
                    to = data_milestone$wupp[data_milestone$id == i])        
      }
      }
    dy_ts
  })
  output$pl_ts <- renderPlotly({
    if (input$log == FALSE){
      gg_ts <- ggplot(qsl_ts, aes(month, rate, group = 1,
                                      text = paste("date:", month, "<br>", "rate:", round(rate, 1)))) +
        scale_y_continuous("Calling rates per 100,000 smokers", trans = "log", breaks = c(25, 50, 100, 200, 300))
    } else {
      gg_ts <- ggplot(qsl_ts, aes(month, n, group = 1,
                                      text = paste("date:", month, "<br>", "n:", n))) +
        ylab("Total number of calls")
    }
    gg_ts <- gg_ts + geom_point(col = "blue") + geom_line(col = "blue") +
      scale_x_date("\n", date_labels = "%b %y", date_breaks = "6 months") + 
      theme_classic() + theme(axis.text.x = element_text(angle = 45))
    if (!is.null(input$timeline_selected)){
      for (i in as.numeric(input$timeline_selected)){
        gg_ts <- gg_ts +
          geom_vline(xintercept = as.numeric(data_milestone$start[data_milestone$id == i]),
                     linetype = 4) +
          annotate("rect", fill = "grey", alpha = 0.25, 
                   xmin = data_milestone$wlow[data_milestone$id == i], 
                   xmax = data_milestone$wupp[data_milestone$id == i],
                   ymin = ifelse(input$log, 300, 20), ymax = ifelse(input$log, 2700, 320))
          # geom_rect(xmin = as.numeric(data_milestone$start[data_milestone$id == i]),
          #               xmax = as.numeric(data_milestone$start[data_milestone$id == i]),
          #               ymin = -Inf, ymax = +Inf, fill="red",alpha = 0.5)
      }
    }

    ggplotly(gg_ts, tooltip = "text")
  })
  
  # 1st intervantion
  output$pl_1Int <- renderPlotly({
    gg_1Int <- ggplot(pl_int1_long, aes(x = month, y = val, group = 1,
                                        color = serie_type, pch = serie_type, 
                                        text = paste("date:", month, "<br>", "rate:", round(val, 1)))) +
      geom_point() + geom_line() + geom_vline(xintercept = as.numeric(as.Date("2001-01-01")), linetype = 4) +
      theme_classic() + theme(legend.title = element_blank()) +
      theme(axis.text.x = element_text(angle = 45)) +
      scale_x_date("\n", date_labels = "%b %y", date_breaks = "3 months") + 
      scale_y_continuous("Calling rates per 100,000 smokers", trans = "log", breaks = c(25, 50, 100, 200, 300))
    ggplotly(gg_1Int, tooltip = "text")
  })
  
  # output$Effect1 <- renderUI({
  #   RR1 <- round(exp(Effect_I1[Effect_I1 != 0])[as.double(input$tInt1)], 2)
  #   #h2(paste("linear model: RR: ", est_lin[1], "(95% CI: ", est_lin[2], ", ", est_lin[3], ")", sep = ""))
  #   h3(paste("RR: ", RR1, sep = ""))
  # })
  
  output$RR1 <- renderPlotly({
    rr_1Int <- c(exp(ts(Effect_I1, frequency = 12, start = c(1999, 01))))
    time_1Int <- seq(as.Date("1999-01-01"), as.Date("2002-08-01"), by = "month")
    ggplotly(
      ggplot(data = NULL, 
             aes(time_1Int, rr_1Int
                 #text = paste("date:", time_1Int, "<br>", "Rate Ratio:", round(rr_1Int, 3))
                 )) + 
        geom_line() + labs(x = "Time", y = "Rate Ratio") + ylim(.4, 2.5) +
        theme_classic() +
        geom_ribbon(aes(ymin = .5, ymax = rr_1Int), fill = "lightblue")
      )  
    })

    output$tableRR1 <- renderDataTable({
      data.frame(Date = time_1Int,
                 `Rate Ratio` = round(rr_1Int, 2)
      )[rr_1Int != 1, ]
    })

    
  # 2nd intervantion
  output$pl_2Int <- renderPlotly({
    gg_2Int <- ggplot(pl_int2_long, aes(x = month, y = val, group = 1,
                                        color = serie_type, pch = serie_type, 
                                        text = paste("date:", month, "<br>", "rate:", round(val, 1)))) +
      geom_point() + geom_line() + geom_vline(xintercept = as.numeric(as.Date("2002-09-30")), linetype = 4) +
      theme_classic() + theme(legend.title = element_blank()) +
      theme(axis.text.x = element_text(angle = 45)) +
      scale_x_date("\n", date_labels = "%b %y", date_breaks = "3 months") + 
      scale_y_continuous("Calling rates per 100,000 smokers", trans = "log", breaks = c(25, 50, 100, 200, 300))
    ggplotly(gg_2Int, tooltip = "text")
  })
  
  output$RR2 <- renderPlotly({
    rr_2Int <- c(exp(ts(Effect_I2, frequency = 12, start = c(1999, 01))))
    time_2Int <- seq(as.Date("2001-01-01"), as.Date("2005-05-01"), by = "month")
    ggplotly(
      ggplot(data = NULL, 
             aes(time_2Int, rr_2Int
                 #text = paste("date:", time_1Int, "<br>", "Rate Ratio:", round(rr_1Int, 3))
             )) + 
        geom_line() + labs(x = "Time", y = "Rate Ratio") + ylim(.4, 2.5) +
        theme_classic() +
        geom_ribbon(aes(ymin = .5, ymax = rr_2Int), fill = "lightblue")
    )
  })
  
  output$tableRR2 <- renderDataTable({
    data.frame(Date = time_2Int,
               `Rate Ratio` = round(rr_2Int, 2)
    )[rr_2Int != 1, ]
  })
  
  
  # 3rd intervantion
  output$pl_3Int <- renderPlotly({
    gg_3Int <- ggplot(pl_int3_long, aes(x = month, y = val, group = 1,
                                        color = serie_type, pch = serie_type, 
                                        text = paste("date:", month, "<br>", "rate:", round(val, 1)))) +
      geom_point() + geom_line() + geom_vline(xintercept = as.numeric(as.Date("2005-06-01")), linetype = 4) +
      theme_classic() + theme(legend.title = element_blank()) +
      theme(axis.text.x = element_text(angle = 45)) +
      scale_x_date("\n", date_labels = "%b %y", date_breaks = "4 months") + 
      scale_y_continuous("Calling rates per 100,000 smokers", trans = "log", breaks = c(25, 50, 100, 200, 300))
    ggplotly(gg_3Int, tooltip = "text")
  })
  
  output$RR3 <- renderPlotly({
    rr_3Int <- c(exp(ts(Effect_I3, frequency = 12, start = c(1999, 01))))
    time_3Int <- seq(as.Date("2002-09-01"), as.Date("2008-12-01"), by = "month")
    ggplotly(
      ggplot(data = NULL, 
             aes(time_3Int, rr_3Int
                 #text = paste("date:", time_1Int, "<br>", "Rate Ratio:", round(rr_1Int, 3))
             )) + 
        geom_line() + labs(x = "Time", y = "Rate Ratio") + ylim(.4, 2.5) +
        theme_classic() +
        geom_ribbon(aes(ymin = .5, ymax = rr_3Int), fill = "lightblue")
    )
  })
  
  output$tableRR3 <- renderDataTable({
    data.frame(Date = time_3Int,
               `Rate Ratio` = round(rr_3Int, 2)
    )[rr_3Int != 1, ]
  })
  
  
  # 4th intervantion
  output$pl_4Int <- renderPlotly({
    gg_4Int <- ggplot(pl_int4_long, aes(x = month, y = val, group = 1,
                                        color = serie_type, pch = serie_type, 
                                        text = paste("date:", month, "<br>", "rate:", round(val, 1)))) +
      geom_point() + geom_line() + geom_vline(xintercept = as.numeric(as.Date("2012-01-01")), linetype = 4) +
      theme_classic() + theme(legend.title = element_blank()) +
      theme(axis.text.x = element_text(angle = 45)) +
      scale_x_date("\n", date_labels = "%b %y", date_breaks = "5 months") + 
      scale_y_continuous("Calling rates per 100,000 smokers", trans = "log", breaks = c(25, 50, 100, 200, 300))
    ggplotly(gg_4Int, tooltip = "text")
  })
  
  output$RR4 <- renderPlotly({
    rr_4Int <- c(exp(ts(Effect_I4, frequency = 12, start = c(1999, 01))))
    time_4Int <- seq(as.Date("2009-01-01"), as.Date("2014-12-01"), by = "month")
    ggplotly(
      ggplot(data = NULL, 
             aes(time_4Int, rr_4Int
                 #text = paste("date:", time_1Int, "<br>", "Rate Ratio:", round(rr_1Int, 3))
             )) + 
        geom_line() + labs(x = "Time", y = "Rate Ratio") + ylim(.4, 2.5) +
        theme_classic() +
        geom_ribbon(aes(ymin = .5, ymax = rr_4Int), fill = "lightblue")
    )
  })
  
  output$tableRR4 <- renderDataTable({
    data.frame(Date = time_4Int,
               `Rate Ratio` = round(rr_4Int, 2)
               )[rr_4Int != 1, ]
  })
  
})