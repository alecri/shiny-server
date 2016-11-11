library(shiny)
library(timevis)
library(dygraphs)
library(xts)
library(tsModel)
library(tidyverse)
library(Epi)
library(plotly)


load("qsl_ts.rda")

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

