library(swemaps)
library(readxl)
library(sp)

## for sthlm län
map_ln <- map_ln
map_ln$y_2012 <- round(rnorm(nrow(map_ln)), 2)
map_ln$y_2013 <- round(rnorm(nrow(map_ln)), 2)
map_ln$y_2014 <- round(rnorm(nrow(map_ln)), 2)

## for sthlm kommun
map_kn <- map_kn
map_kn$y_2012 <- round(rnorm(nrow(map_kn)), 2)
map_kn$y_2013 <- round(rnorm(nrow(map_kn)), 2)
map_kn$y_2014 <- round(rnorm(nrow(map_kn)), 2)

## for sthlm cities
cities <- readxl::read_excel("www/swedish cities.xlsx")
cities$lat <- as.numeric(char2dms(cities$Latitude, chd = "°", chm = "'", chs = "\""))
cities$long <- as.numeric(char2dms(cities$Longitude, chd = "°", chm = "'", chs = "\""))
cities$LatLong <- paste(round(cities$lat, 1), round(cities$long, 1), sep = ":")
cities$y_2012 <- round(rnorm(nrow(cities)), 2)
cities$y_2013 <- round(rnorm(nrow(cities)), 2)
cities$y_2014 <- round(rnorm(nrow(cities)), 2)

regions <- levels(map_ln$lnnamn)[-1]
