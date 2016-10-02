# library(raster)
# library(tidyverse)
# library(readxl)
# library(sp)
# library(rgeos)
# library(broom)
# 
# rm(list=ls())
# 
# # data managment
# swe_data1 <- readRDS("www/SWE_adm1.rds")
# swe_data1$y_2012 <- round(rnorm(nrow(swe_data1)), 2)
# swe_data1$y_2013 <- round(rnorm(nrow(swe_data1)), 2)
# swe_data1$y_2014 <- round(rnorm(nrow(swe_data1)), 2)
# swe_data1_sim <- rgeos::gSimplify(swe_data1, .01, topologyPreserve = TRUE)
# swe_data1 <- SpatialPolygonsDataFrame(swe_data1_sim, data = swe_data1@data)
# swe_data1_sim <- broom::tidy(swe_data1_sim, region = "ID_1")
# swe_data1_tab <- unique(subset(
#    merge(x = swe_data1_sim, y = swe_data1, by.x = "id", by.y = "ID_1"),
#                         select = c("NAME_1", "y_2012", "y_2013", "y_2014")))
# 
# # defining choices for inputs
# regions <- swe_data1$NAME_1
# 
# # regions
# swe_data2 <- readRDS("www/SWE_adm2.rds")
# swe_data2$y_2012 <- round(rnorm(nrow(swe_data2)), 2)
# swe_data2$y_2013 <- round(rnorm(nrow(swe_data2)), 2)
# swe_data2$y_2014 <- round(rnorm(nrow(swe_data2)), 2)
# coord <- coordinates(swe_data2)
# swe_data2$coord1 <- coord[, 1]
# swe_data2$coord2 <- coord[, 2]
# swe_data2_sim <- rgeos::gSimplify(swe_data2, .001, topologyPreserve = TRUE)
# swe_data2 <- SpatialPolygonsDataFrame(swe_data2_sim, data = swe_data2@data)
# swe_data2_sim <- broom::tidy(swe_data2_sim, region = "ID_2")
# swe_data2_tab <- unique(subset(
#    merge(x = swe_data2_sim, y = swe_data2, by.x = "id", by.y = "ID_2"),
#    select = c("NAME_1", "NAME_2", "y_2012", "y_2013", "y_2014")))
# 
# # google maps
# cities <- readxl::read_excel("www/swedish cities.xlsx")
# cities$lat <- as.numeric(char2dms(cities$Latitude, chd = "°", chm = "'", chs = "\""))
# cities$long <- as.numeric(char2dms(cities$Longitude, chd = "°", chm = "'", chs = "\""))
# cities$LatLong <- paste(round(cities$lat, 1), round(cities$long, 1), sep = ":")
# cities$y_2012 <- round(rnorm(nrow(cities)), 2)
# cities$y_2013 <- round(rnorm(nrow(cities)), 2)
# cities$y_2014 <- round(rnorm(nrow(cities)), 2)
# 
# #save(cities, regions, swe_data1, swe_data1_tab, swe_data2, swe_data2_tab,
#      file = "www/dataset.Rdata")
# 
# # load created datasets
load("www/dataset.Rdata")

swe_data1 <- map_ln
swe_data1$y_2012 <- round(rnorm(nrow(swe_data1)), 2)
swe_data1$y_2013 <- round(rnorm(nrow(swe_data1)), 2)
swe_data1$y_2014 <- round(rnorm(nrow(swe_data1)), 2)
