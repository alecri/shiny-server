library(raster)
library(rgeos)
library(tidyverse)
library(ggthemes)
library(plotly)
library(broom)

swe_data1 <- raster::getData("GADM", country = "SWE", level = 1)
swe_data1$y <- rnorm(nrow(swe_data1))
# just a test
#swe_data1$y[swe_data1$NAME_1 != "Stockholm"] <- -20
swe_s <- rgeos::gSimplify(swe_data1, .01)
sweden <- broom::tidy(swe_s, region = "ID_1")
sweden_plot <- merge(x = sweden, y = swe_data1, by.x = "id", by.y = "ID_1")


p <- ggplot(sweden_plot, aes(x = long, y = lat, group = group, fill = y)) +
   geom_polygon() + coord_quickmap() +
   ggthemes::theme_map() + theme(legend.position = c(.8, .2))
p

plotly::ggplotly(p)


regions <- swe_data1$NAME_1


# regions
swe_data2 <- readRDS("www/SWE_adm2.rds")
swe_data2$y <- rnorm(nrow(swe_data2))
swe_data2$y[swe_data2$NAME_1 != "Stockholm"] <- -20
swe_s2 <- rgeos::gSimplify(swe_data2, .0001)
sweden_region2 <- broom::tidy(swe_s2, region = "ID_2")
#sweden_region2$id <- as.numeric(sweden_region2$id)
sweden_plot2 <- merge(x = sweden_region2, y = swe_data2, by.x = "id", by.y = "ID_2")

p <- ggplot(sweden_plot2, 
                   aes(x = long, y = lat, group = group, fill = y)) +
   geom_polygon() + coord_quickmap() +
   ggthemes::theme_map() + theme(legend.position = c(.8, .2))
p
