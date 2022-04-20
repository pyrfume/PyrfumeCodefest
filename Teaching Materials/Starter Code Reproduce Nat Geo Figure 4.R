#Set up packages
library(pyrfume)
library(tidyverse)

#The environment has a downsampled version of the data. You can load using the following code:
NGS_Small <- read.csv("data/NGS_sample01.csv")

#Look at the data
head(NGS_Small)
summary(NGS_Small)

#This is the standard function for loading directly from Pyrfume. Here we load the data dictionary.
#This describes each variable and what the numerical codes mean.
NGS_DataDictionary <- load_data("nat_geo_1986/DataDictionary.csv")
head(NGS_DataDictionary[,1:10])

#we are removing missing rows for the following data
df <- NGS_Small %>%
  filter(SEX != "0" & totcorr !=7 & SMOKE != "0") %>%
  filter(ETHNIC !="0" & ETHNIC != "7") %>% 
  filter(decage !="0") %>%
  #Now we are averaging by sex and age
  group_by(SEX,decage) %>% 
  summarize(totcorr = mean(totcorr))

#Our goal is to make a plot with the number of correctly identified odors on the 
#y-axis, age (in decades) on the x-axis, and color our points by sex
#You can click the small arrow to hide/show the code below, so you can try on
#your own before seeing our solution.

#We will be using ggplot instead of base graphics to make plots.
#A tutorial is here: http://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html

#Recreate the NGS plot #####
#Now we will recreate the plot. You can see a description of all of the variables at https://github.com/pyrfume/pyrfume-data/blob/main/nat_geo_1986/Data%20dictionary.xlsx
ggplot(df, aes(x = decage, y = totcorr, color=as.factor(SEX)))+
  geom_point()

#Clean up the labels #####
ggplot(df, aes(x = decage, y = totcorr, color=as.factor(SEX)))+
  geom_point()+
  labs(y="Number Correct")+
  scale_color_manual(name="Sex",labels=c("Male","Female"),values=c("blue","red"))+
  scale_x_continuous(name="Age in Decades",breaks=c(1:9),labels=c("Teens","20's","30's","40's","50's","60's","70's","80's","90's"))


