
##installing packages

install.packages("ggpairs")
install.packages("ggmap")
install.packages("zipcodeR")
install.packages("GGally")
install.packages("usmap")
install.packages("maps")


##libraries

library(ggpairs)
library(ggmap)
library(zipcode)
library(zipcodeR)
library(GGally)
library(tidyverse)
library(usmap)
library(maps)

##setting working directory

setwd("../../../Codefest AChemS 2022/")

##subsetting

NGS <- read.csv("NGS_sample01.csv")

Sub_NGS <- NGS[c(1,27:33)]

Sub_NGS_pairs <- ggpairs(NGS[c(1,27:33)])

Sub_NGS_pairs
ggsave(filename = "subNGSpairs.jpeg", plot = Sub_NGS_pairs, height = 10, width = 10,units = "in")


NGS_subset_cln <- Sub_NGS %>% 
  filter(ZIP != 0) %>% 
  mutate(ZIP=clean.zipcodes(ZIP))

sum_state <- zip_NGS %>% 
  group_by(state, SMOKE) %>% 
  summarise( values = n(), avg.age = mean(AGE))


plot_usmap(data = sum_state, values = n) +
  scale_fill_continuous(low = "white", high = "blue", name = "Population", label = scales::comma)

USA_map <- map_data("state")

zip2state <- zip_code_db

colnames(NGS_subset_cln)[4]<-"zipcode"

zip_NGS <- left_join(NGS_subset_cln, zip2state %>%
            select(zipcode, state))




statepop

plot_usmap(data = statepop, values = "pop_2015",color="red")

plot_usmap(data = sum_state, values = "values",color="white") +
  scale_fill_continuous(name="Population", low = "ivory4", high = "green3") +
  ggtitle("NGS: where are they from?")


# send me code plz saundecj@wfu.edu







