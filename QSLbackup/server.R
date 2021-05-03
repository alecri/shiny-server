library(shiny)
library(timevis)
library(dygraphs)
library(xts)
library(tsModel)
library(tidyverse)
library(Epi)

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

## read data from csv file
data <- read.csv("sicily.csv")
#head(data)
## compute the standardized rates
data <- mutate(data,
               rate = aces/stdpop*10^5,
               date = as.Date(paste(year, month, 1, sep = "-")),
               before = ifelse(smokban == 0, rate, NA),
               after = ifelse(smokban == 1, rate, NA))

## data for predictions (same as initial)
newdata <- mutate(data, stdpop = mean(stdpop))
newdata0 <- mutate(newdata, smokban = 0)

## data for time serie
lungDeaths <- cbind(mdeaths, fdeaths)

shinyServer(function(input, output, session){
   
   ## pipe line
   output$timeline <- renderTimevis({
      timevis(data_milestone)
   })
   
   ## interrupted time serie models
   mod_lin <- reactive({
      if (input$mod_lin == FALSE) return (NULL)
      glm(aces ~ offset(log(stdpop)) + smokban + time, 
          family = poisson, data)
   })
   output$modlin <- renderPrint({
      if (input$mod_lin == FALSE) return (NULL)
      summary(mod_lin())
   })
   mod_season <- reactive({
      if (input$mod_season == FALSE) return (NULL)
      glm(aces ~ offset(log(stdpop)) + smokban*time + harmonic(month, 2, 12),
          family = quasipoisson, data)
   })
   output$modseason <- renderPrint({
      if (input$mod_season == FALSE) return (NULL)
      summary(mod_season())
   })
   
   output$effect_lin <- renderUI({
      if (input$mod_lin == FALSE){
         return(NULL)
      }
      est_lin <- round(ci.lin(mod_lin(), Exp = T), 3)["smokban", 5:7]
      h2(paste("linear model: RR: ", est_lin[1], "(95% CI: ", est_lin[2], ", ", est_lin[3], ")", sep = ""))
   })
   output$effect_season <- renderUI({
      if (input$mod_season == FALSE){
         return(NULL)
      }
      est_season <- round(ci.lin(mod_season(), Exp = T), 3)["smokban", 5:7]
      h2(paste("seasonal model: RR: ", est_season[1], "(95% CI: ", est_season[2], ", ", est_season[3], ")", sep = ""))
   })
   
   
   ## adding predictions of the original data for plot
   dataxts <- reactive({
      if (input$mod_lin == TRUE){
         data <- data %>%
            mutate(
               predict_lin0 = ifelse(date <= "2004-12-01", NA, predict(mod_lin(), type = "response", newdata0)/mean(data$stdpop)*10^5),
               predict_lin = predict(mod_lin(), type = "response", newdata)/mean(data$stdpop)*10^5
            )
      }
      if (input$mod_season == TRUE){
         data <- data %>%
            mutate(
               predict_season0 = ifelse(date <= "2004-12-01", NA, predict(mod_season(), type = "response", newdata0)/mean(data$stdpop)*10^5),
               predict_season = predict(mod_season(), type = "response", newdata)/mean(data$stdpop)*10^5
            )
      }
      dataxts <- as.xts(data, order.by = data$date)
      varlist <- c("before", "after")
      if (input$mod_lin == TRUE){
         varlist <- c(varlist, "predict_lin", "predict_lin0")
      }
      if (input$mod_season == TRUE){
         varlist <- c(varlist, "predict_season0", "predict_season")
      }
      dataxts[, varlist]
   })
   
   output$its_graph <- renderDygraph({
      its_graph <- dygraph(dataxts()) %>%
         dyAxis("y", label = "Std rate x 10,000", valueRange = c(0, 300)) %>%
         dyShading(from = "2004-12-01", to = "2006-11-01") %>%
         dyEvent("2004-12-01", "New policy", labelLoc = "bottom") %>%
         dyOptions(drawPoints = TRUE, pointSize = 3, strokeWidth = 0)
      
      if (input$mod_lin == TRUE){
         its_graph <- its_graph %>%
            dySeries("predict_lin", drawPoints = FALSE, strokeWidth = 1) %>%
            dySeries("predict_lin0", drawPoints = FALSE, strokeWidth = 1)
      }
      if (input$mod_season == TRUE){
         its_graph <- its_graph %>%
            dySeries("predict_season", drawPoints = FALSE, strokeWidth = 1) %>%
            dySeries("predict_season0", drawPoints = FALSE, strokeWidth = 1)
         }
      its_graph
   })
   
})
