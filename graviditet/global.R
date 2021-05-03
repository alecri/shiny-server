# library(tidyverse)
# library(swemaps)
# 
# ## reading data
# rm(list=ls())
# grav <- read.csv("www/Statistikdatabasen_2016-10-17 13_35_49.csv", skip = 1,
#                  sep = ";", stringsAsFactors = F)
# #head(grav)
# #names(grav)
# 
# ## data cleaning
# #unique(grav$Mått)
# #filter(grav, Mått == "Socialstyrelsens statistikdatabas 2016-10-17")
# grav <- filter(grav, Mått != "Socialstyrelsens statistikdatabas 2016-10-17")
# #unique(grav$Mått)
# 
# 
# ## "Antal"      "Procent"    "Medelvärde"
# #str(grav$X2012)
# grav <- grav %>% mutate(
#    X2012 = as.numeric(gsub("[[:space:]]", "", gsub(",", ".", X2012))),
#    X2013 = as.numeric(gsub("[[:space:]]", "", gsub(",", ".", X2013))),
#    X2014 = as.numeric(gsub("[[:space:]]", "", gsub(",", ".", X2014))))
# 
# ## checking
# # grav %>% filter(is.na(X2012_2)) %>% distinct(X2012)
# # grav %>% filter(is.na(X2013_2)) %>% distinct(X2013)
# # grav %>% filter(is.na(X2014_2)) %>% distinct(X2014)
# #glimpse(grav)
# 
# 
# ## for sthlm län
# map_ln <- swemaps::map_ln
# 
# ## merging
# grav <- grav %>%
#    arrange(Landsting) %>%
#    mutate(Landsting = replace(Landsting, Landsting == "Riket", ""),
#           Landsting = factor(Landsting, labels = sort(levels(map_ln$lnnamn))),
#           Ålder = factor(Ålder, labels = c("24 år", "25-29 år", "30-34 år", "35+", "år Alla åldrar"))
#    )
# 
# ## check
# #distinct(grav, Landsting, Landsting_2)
# grav_map <- merge(map_ln, grav, by.x = "lnnamn", by.y = "Landsting")
# save(grav_map, file = "www/grav_map.Rdata")
load("www/grav_map.Rdata")


grav_map <- subset(grav_map, lnnamn != "") 
