library(pyrfume)
library(tidyverse)
library(RgoogleMaps)
library(maps)
library(diagram)  
library(plotrix)
palette(rainbow(20))

NGS_Small <- read.csv("data/NGS_sample01.csv")

df <- NGS_Small %>%
  filter(SEX != "0" & totcorr !=7 & SMOKE != "0") %>%
  filter(ETHNIC !="0" & ETHNIC != "7") %>% 
  filter(decage !="0") %>%
  filter(ZIP != 0)

#only the first 500 so I dont max out the google api
df <- df[1:500, ]

for(i in 1:nrow(df))
{
  result <- getGeoCode(df$ZIP[i])
  df$lat[i] <- as.numeric(result[1])
  df$lon[i] <- as.numeric(result[2])
}

map('usa', boundary = T, fill = TRUE, col = "lightgray", mar = rep(0, 4))
map('state', boundary = T, fill = T, col = 'lightgray', mar = rep(0,4))

with(df, points(df$lon, df$lat, col='orange', pch=19, cex=rescale(df$totcorr, c(1, 8)))) 