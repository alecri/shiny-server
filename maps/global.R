library(raster)
library(rgeos)
library(tidyverse)
library(broom)
library(readxl)

# data managment
swe_data1 <- readRDS("www/SWE_adm1.rds")
swe_data1$y_2012 <- rnorm(nrow(swe_data1))
swe_data1$y_2013 <- rnorm(nrow(swe_data1))
swe_data1$y_2014 <- rnorm(nrow(swe_data1))
swe_s <- rgeos::gSimplify(swe_data1, .01)
sweden <- broom::tidy(swe_s, region = "ID_1")
sweden_plot <- merge(x = sweden, y = swe_data1, by.x = "id", by.y = "ID_1")
sweden_plot$hover <- with(sweden_plot, paste(VARNAME_1, '<br>', 
                                             "Ind1_2012", round(y_2012, 2), "<br>",
                                         "Ind1_2013", round(y_2013, 2), "<br>",
                                         "Ind1_2014", round(y_2014, 2), "<br>"))

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


# google maps
cities <- readxl::read_excel("www/swedish cities.xlsx")
cities$lat <- as.numeric(char2dms(cities$Latitude, chd = "°", chm = "'", chs = "\""))
cities$long <- as.numeric(char2dms(cities$Longitude, chd = "°", chm = "'", chs = "\""))
cities$LatLong <- paste(round(cities$lat, 1), round(cities$long, 1), sep = ":")
cities$y1 <- rnorm(nrow(cities))
cities$y2 <- rnorm(nrow(cities))
cities$y3 <- rnorm(nrow(cities))
cities$Tip <- with(cities, paste0(Locations, "<BR>", "Ind1=", round(y1, 2), 
                                  "<BR>", "Ind2=", round(y2, 2)))
