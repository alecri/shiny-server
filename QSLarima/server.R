library(shiny)
library(timevis)
library(dygraphs)
library(plotly)
library(xts)
library(ggplot2)
library(stats)
library(lubridate)
library(tidyverse)

## load data
load("www/qls_app_dat.Rdata")
load("www/pop_smk.rda")

smk_age <- data.frame(
   gather(pop_smkage[, c(1:2, 3:5)], sex, num, -c(1:2)),
   smk = gather(pop_smkage[, c(2, 6:8)], sex, smk, -year)$smk,
   pct = gather(pop_smkage[, c(2, 9:11)], sex, pct, -year)$pct
)
smk_age$sex <- factor(smk_age$sex, labels = c("total", "female", "male"))
smk <- smk_age %>% group_by(year, sex) %>%
   summarise(num = sum(num),
             smk = sum(smk)) %>%
   mutate(pct = 100*smk/num)



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
  # output$dy_ts <- renderDygraph({
  #   dataxts <- as.xts(qsl_ts, order.by = qsl_ts$month)[, ifelse(input$log, "n", "rate")]
  #   dy_ts <- dygraph(dataxts) %>%
  #     dyAxis("y", label = ifelse(input$log, "Total number of calls", 
  #                                "Calling rates per 100,000 smokers")
  #            ) %>%
  #     dyOptions(drawPoints = TRUE, pointSize = 3, logscale = (input$log == FALSE))
  #   if (!is.null(input$timeline_selected)){
  #     for (i in as.numeric(input$timeline_selected)){
  #       dy_ts <- dy_ts %>%
  #         dyEvent(data_milestone$start[data_milestone$id == i], 
  #                 data_milestone$content[data_milestone$id == i], labelLoc = "top") %>%
  #         dyShading(from = data_milestone$wlow[data_milestone$id == i], 
  #                   to = data_milestone$wupp[data_milestone$id == i])        
  #     }
  #     }
  #   dy_ts
  # })

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
  
  output$pl_smk <- renderPlotly({
     ggplotly(
        ggplot(smk, aes(x = year, color = sex, group = sex, 
                        y = get(ifelse(input$counts, "smk", "pct")),
                        text = paste("Year:", year, "<br>", 
                                     ifelse(input$counts, "Number of smokers:", "Percentage of smokers:"), 
                                     round(get(ifelse(input$counts, "smk", "pct")))))) +
           ylab(ifelse(input$counts, "Number of smokers", "Percentage of smokers")) + 
           geom_line() + geom_point() +
           theme_classic() + theme(axis.text.x = element_text(angle = 45)),
        tooltip = "text"
     )
  })
  output$pl_smk_age <- renderPlotly({
     ggplotly(
        ggplot(smk_age %>% filter(year >= 2004),
               aes(x = year, color = age_cat, group = age_cat,
                   y = get(ifelse(input$counts, "smk", "pct")),
                   text = paste("Year:", year, "<br>", 
                                ifelse(input$counts, "Number of smokers:", "Percentage of smokers:"), 
                                round(get(ifelse(input$counts, "smk", "pct")))))) +
           ylab(ifelse(input$counts, "Number of smokers", "Percentage of smokers")) + 
           geom_line() +
           theme_classic() + theme(axis.text.x = element_text(angle = 45)) +
           facet_grid(. ~ sex),
        tooltip = "text"
     )
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
     ggplotly(
        ggplot(data = data.frame(time_1Int, rr_1Int), 
               aes(time_1Int, rr_1Int, group = 1,
                   text = paste("date:", time_1Int, "<br>", "Rate Ratio:", round(rr_1Int, 3))
               )) + 
           geom_line() + labs(x = "Time", y = "Rate Ratio") + ylim(.4, 2.5) +
           theme_classic() + theme(axis.text.x = element_text(angle = 45)) +
           scale_x_date("\n", date_labels = "%b %y", date_breaks = "3 months") + 
           geom_segment(aes(xend = time_1Int, yend = .4), col = "lightblue"),
        tooltip = "text"
     ) 
    })

    output$tableRR1 <- renderDataTable({
       rr_tab1
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
     ggplotly(
        ggplot(data = data.frame(time_2Int, rr_2Int), 
               aes(time_2Int, rr_2Int, group = 1,
                   text = paste("date:", time_2Int, "<br>", "Rate Ratio:", round(rr_2Int, 3))
               )) + 
           geom_line() + labs(x = "Time", y = "Rate Ratio") + ylim(.4, 2.2) +
           theme_classic() + theme(axis.text.x = element_text(angle = 45)) +
           scale_x_date("\n", date_labels = "%b %y", date_breaks = "3 months") + 
           geom_segment(aes(xend = time_2Int, yend = .4), col = "lightblue"),
        tooltip = "text"
     )
  })
  
  output$tableRR2 <- renderDataTable({
     rr_tab2
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
     ggplotly(
        ggplot(data = data.frame(time_3Int, rr_3Int), 
               aes(time_3Int, rr_3Int, group = 1,
                   text = paste("date:", time_3Int, "<br>", "Rate Ratio:", round(rr_3Int, 3))
               )) + 
           geom_line() + labs(x = "Time", y = "Rate Ratio") + ylim(.9, 1.2) +
           theme_classic() + theme(axis.text.x = element_text(angle = 45)) +
           scale_x_date("\n", date_labels = "%b %y", date_breaks = "3 months") + 
           geom_segment(aes(xend = time_3Int, yend = .9), col = "lightblue"),
        tooltip = "text"
     )
   })
  
  output$tableRR3 <- renderDataTable({
     rr_tab3
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
     ggplotly(
        ggplot(data = data.frame(time_4Int, rr_4Int), 
               aes(time_4Int, rr_4Int, group = 1,
                   text = paste("date:", time_4Int, "<br>", "Rate Ratio:", round(rr_4Int, 3))
               )) + 
           geom_line() + labs(x = "Time", y = "Rate Ratio") + ylim(.5, 1.2) +
           theme_classic() + theme(axis.text.x = element_text(angle = 45)) +
           scale_x_date("\n", date_labels = "%b %y", date_breaks = "3 months") + 
           geom_segment(aes(xend = time_4Int, yend = .5), col = "lightblue"),
        tooltip = "text"
     )
   })
  
  output$tableRR4 <- renderDataTable({
     rr_tab4
  })
  
})