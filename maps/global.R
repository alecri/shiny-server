library(raster)
library(rgeos)
library(tidyverse)
library(broom)

# data managment
swe_data1 <- readRDS("www/SWE_adm1.rds")
swe_data1$y_2012 <- rnorm(nrow(swe_data1))
swe_data1$y_2013 <- rnorm(nrow(swe_data1))
swe_data1$y_2014 <- rnorm(nrow(swe_data1))
swe_s <- rgeos::gSimplify(swe_data1, .01)
sweden <- broom::tidy(swe_s, region = "ID_1")
sweden_plot <- merge(x = sweden, y = swe_data1, by.x = "id", by.y = "ID_1")

# regions
swe_data2 <- readRDS("www/SWE_adm2.rds")
swe_data2$y_2012 <- rnorm(nrow(swe_data2))
swe_data2$y_2013 <- rnorm(nrow(swe_data2))
swe_data2$y_2014 <- rnorm(nrow(swe_data2))
swe_s2 <- rgeos::gSimplify(swe_data2, .01)
sweden_region2 <- broom::tidy(swe_s, region = "ID_2")
sweden_plot2 <- merge(x = sweden_region2, y = swe_data2, by.x = "id", by.y = "ID_2")



# defining choices for inputs
regions <- swe_data1$NAME_1