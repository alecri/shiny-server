library(raster)
library(tidyverse)
library(readxl)
library(sp)

# data managment
swe_data1 <- readRDS("www/SWE_adm1.rds")
swe_data1$y_2012 <- round(rnorm(nrow(swe_data1)), 2)
swe_data1$y_2013 <- round(rnorm(nrow(swe_data1)), 2)
swe_data1$y_2014 <- round(rnorm(nrow(swe_data1)), 2)
# defining choices for inputs
regions <- swe_data1$NAME_1

# regions
swe_data2 <- readRDS("www/SWE_adm2.rds")
swe_data2$y_2012 <- round(rnorm(nrow(swe_data2)), 2)
swe_data2$y_2013 <- round(rnorm(nrow(swe_data2)), 2)
swe_data2$y_2014 <- round(rnorm(nrow(swe_data2)), 2)
coord <- coordinates(swe_data2)
swe_data2$coord1 <- coord[, 1]
swe_data2$coord2 <- coord[, 2]

# google maps
cities <- readxl::read_excel("www/swedish cities.xlsx")
cities$lat <- as.numeric(char2dms(cities$Latitude, chd = "°", chm = "'", chs = "\""))
cities$long <- as.numeric(char2dms(cities$Longitude, chd = "°", chm = "'", chs = "\""))
cities$LatLong <- paste(round(cities$lat, 1), round(cities$long, 1), sep = ":")
cities$y_2012 <- round(rnorm(nrow(cities)), 2)
cities$y_2013 <- round(rnorm(nrow(cities)), 2)
cities$y_2014 <- round(rnorm(nrow(cities)), 2)