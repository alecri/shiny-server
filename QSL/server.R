library(shiny)
library(timevis)
library(dygraphs)
library(xts)
library(tsModel)
library(tidyverse)
library(Epi)
library(plotly)
library(lubridate)
require(rms)
library(tidyverse)


load("www/qsl_ts.rda")

## data for pipeline
data_milestone <- data.frame(
  id      = 1:11,
  content = c("Quit Smoking Line Initiation", "Campaign on passive smoking",
              "Larger Warnings on Cigarette Packs + Removal of Terms such as 'Light'", 
              "Banning of Indirect Tobacco Advertisements", "Smoke-Free Restaurants",
              "Prohibition of Cigarettes Selling to Minor", "Tax Increase (Mainly Snuff)",
              "Tax Increase", "Tax Increase (10%)", "Tax Decrease", "Tax Increase"),
  start   = c("1998-09-14", "2001-01-01", "2002-09-30", "2003-01-01", "2005-06-01",
              "2005-07-01", "2007-01-01", "2011-01-01", "2012-01-01", "2014-01-01",
              "2015-01-01"),
  end     = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
)
data_milestone$start <- as.Date(data_milestone$start)

shinyServer(function(input, output, session){
   
   
   ## reactive output
   ## --------------------------------------------------------------------------
   
   output$initial_message <- renderText({
      if (is.null(input$timeline_selected))
         ("Select an intervention*")
   })   
   output$initial_message2 <- renderText({
      if (is.null(input$timeline_selected))
         ("*For multiple events, hold the CTRL (or CMD) key and click the items ")
   })
   output$date_selected <- renderText({
      if (is.null(input$timeline_selected)) return(NULL)
      paste("Date(s) Intervention: ", paste(dateInterv(), collapse = ", "))
      })
   output$date_range_selected <- renderUI({
      if (is.null(input$timeline_selected)) return(NULL)
      valstart <- max(as.Date("1999-01-01"), min(dateInterv() %m-% months(20)))
      valend <- min(as.Date("2015-01-01"), max(dateInterv() %m+% months(10)))
      tagList(
         dateRangeInput("date_range", "Date Range:",
                        start = valstart, end = valend,
                        min = as.Date("1999-01-01"), max = as.Date("2015-01-01")),
         sliderInput("slider", "Slider", min = as.Date("1999-01-01"), max = as.Date("2015-01-01"),
                     value = c(valstart, valend))
      )
      })
   
   ## synchronizing dates input
   observe({
      dates <- input$slider
      updateDateRangeInput(session, "date_range", start = dates[1], end = dates[2])
   })
   observe({
      dates <- input$date_range
      updateSliderInput(session, "slider", value = c(dates[1], dates[2]))
   })
   
   ## model choices
   output$lin_selected <- renderUI({
      if (is.null(input$timeline_selected)) return(NULL)
      checkboxInput("mod_lin", label = "Linear trend", value = FALSE)
   })
   output$season_selected <- renderUI({
      if (is.null(input$timeline_selected)) return(NULL)
      tagList(
      checkboxInput("mod_season", label = "Seasonal trend", value = FALSE),
      conditionalPanel('input.mod_season == true',
                       radioButtons("seas_mod", "Type", c("Fourier", "Splines"), 
                                    select = "Fourier")
                       )
      )
   })

   
   
   ## reactive computations
   ## --------------------------------------------------------------------------

   ## date of intervantion   
   dateInterv <- reactive({
      if (is.null(input$timeline_selected)) return(NULL)
      data_milestone$start[data_milestone$id %in% as.numeric(input$timeline_selected)]
   })
   time_interv <- reactive({
      sapply(dateInterv(), function(d) qsl()$time[which(qsl()$month >= d)[1]])
   })
   
   ## reactive dataset
   qsl <- reactive({
     if (!is.null(input$timeline_selected)){
        qsl_ts <- qsl_ts %>%
           filter(between(month, input$date_range[1], input$date_range[2])) %>%
           mutate(time = seq_along(month) - 1)
        for (date in dateInterv()){
           qsl_ts[[paste0("x", which(dateInterv() == date))]] <- 
              as.numeric(qsl_ts$month >= date)
        }
     }
     qsl_ts
   })
   
   ## linear model
   lin <- reactive({
     if (input$mod_lin == FALSE) return (NULL)
      #ti <- qsl()$time[which.min(qsl()$month >= dateInterv())]
      xb <- paste(c("time", paste0("x", seq_along(dateInterv()), collpase = ""),
                    paste0("I(x", seq_along(dateInterv()), "*(time - ", time_interv() , "))", collpase = "")), 
                  collapse = " + ")
     #glm(n ~ time + x1 + I((x1 - ti)*time), offset = log(total), family = poisson, data = qsl())
     glm(as.formula(paste0("n ~ ", xb)), offset = log(total), family = poisson, data = qsl())
   })
   output$modlin <- renderPrint({
     if (input$mod_lin == FALSE) return (NULL)
     summary(lin())
   })
   season <- reactive({
      if (input$mod_season == FALSE) return (NULL)
      xb <- paste(c("time", paste0("x", seq_along(dateInterv()), collpase = ""),
                    paste0("I(x", seq_along(dateInterv()), "*(time - ", time_interv() , "))", collpase = "")), 
                  collapse = " + ")
      if (input$seas_mod == "Fourier"){
         glm(as.formula(paste0("n ~ ", xb, "+ harmonic(month_val, 2, 12)")), offset = log(total), family = poisson, data = qsl())
      } else if (input$seas_mod == "Splines"){
         glm(as.formula(paste0("n ~ ", xb, "+ rcs(month_val, 3)")), offset = log(total), family = poisson, data = qsl())
      }
   })
   output$modlin <- renderPrint({
      if (input$mod_lin == FALSE) return (NULL)
      summary(lin())
   })
   output$modseason <- renderPrint({
      if (input$mod_season == FALSE) return (NULL)
      summary(season())
   })
   
   ## newdata for prediction
   datanew <- reactive({
      if (is.null(input$timeline_selected)) return(NULL)
      datanew <- data.frame(month = seq(input$date_range[1], input$date_range[2], length.out = 500)) %>%
         mutate(time = (seq_along(month) - 1)*(nrow(qsl()))/(500-1),
                total = mean(qsl()$total),
                x1 = as.numeric(month >= dateInterv()),
                month_val = month(month)
                )
      for (date in dateInterv()){
         datanew[[paste0("x", which(dateInterv() == date))]] <- 
            as.numeric(datanew$month >= date)
      }
      datanew
      
   })
   predlin <- reactive({
      if (is.null(input$timeline_selected)) return(NULL)
      if (input$mod_lin == FALSE) return(NULL)
      cbind(datanew(), pred_lin = predict(lin(), datanew(), type = "response")/mean(qsl()$total)*10^5)
   })
   predseason <- reactive({
      if (is.null(input$timeline_selected)) return(NULL)
      if (input$mod_season == FALSE) return(NULL)
      cbind(datanew(), pred_season = predict(season(), datanew(), type = "response")/mean(qsl()$total)*10^5)
   })
   
   
   ## reactive output
   ## --------------------------------------------------------------------------
   
   ## pipe line
   output$timeline <- renderTimevis({
      config <- list(
         editable = FALSE,
         multiselect = TRUE
      )
      timevis(data_milestone, options = config)
   })
   
   ## table for QSL
   output$table <- renderDataTable({
      qsl()
   })
   
   ## estimated effects
   output$effects <- renderDataTable({
      if (is.null(input$timeline_selected)) return(NULL)
      if (input$mod_lin == TRUE)
         est_lin <- round(ci.lin(lin(), Exp = T), 3)[grep("x", names(coef(lin()))), 5:7]
      if (input$mod_season == TRUE)
         est_season <- round(ci.lin(season(), Exp = T), 3)[grep("x", names(coef(season()))) , 5:7]
      estimates <- if (input$mod_lin == TRUE & input$mod_season == TRUE){
         data.frame(linear = est_lin, season = est_season)
         } else if (input$mod_lin == TRUE){
            data.frame(dates = dateInterv(), linear = est_lin[seq_along(dateInterv()), ])
         } else if (input$mod_season == TRUE){
            data.frame(dates = dateInterv(), season = est_season[seq_along(dateInterv()), ]) 
         }
      estimates
   })
   
   
   ## time serie QSL
   output$qsl_ts <- renderPlotly({
      dates_range <- c(as.Date("1999-01-01"), dateInterv(), as.Date("2015-01-01"))
      tsp <- ggplot(qsl(), aes(month, rate)) + geom_point() +
         labs(x = "", y = "Rate of calls x 10000") +
         scale_y_continuous(trans = 'log', breaks = c(25, 50, 100, 200, 300))
      if (!is.null(input$timeline_selected)){
         tsp <- tsp +
            geom_vline(xintercept = as.numeric(data_milestone$start[data_milestone$id %in% as.numeric(input$timeline_selected)]), linetype = 2)
         if (input$mod_lin == TRUE){
            for (i in seq_along(dates_range)[-1]){
               tsp <- tsp +
                  geom_line(data = subset(predlin(), month > dates_range[i-1] & month < dates_range[i]), 
                            aes(month, pred_lin))
            }
         }
         if (input$mod_season == TRUE){
            for (i in seq_along(dates_range)[-1]){
               tsp <- tsp +
                  geom_line(data = subset(predseason(), month > dates_range[i-1] & month < dates_range[i]), 
                            aes(month, pred_season))
            }
         }
      }
      ggplotly(tsp)
   })
   
})
