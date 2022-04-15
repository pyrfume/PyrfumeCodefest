## Provided Code (from Joel's google doc) -----

#Read in data with pyrfume package
library(pyrfume)
snitz <- load_data("snitz_2013/molecules.csv")

#Or read in the data manually

snitz <- readr::read_csv('https://raw.githubusercontent.com/pyrfume/pyrfume-data/main/snitz_2013/molecules.csv')
#Set up packages
library(pyrfume)
library(tidyverse)

#The environment has a downsampled version of the data. You can load using the following code:
NGS_Small <- read.csv("data/NGS_sample01.csv")

#we are removing missing rows for the following data
df <- NGS_Small %>%
  filter(SEX != "0" & totcorr !=7 & SMOKE != "0") %>%
  filter(ETHNIC !="0" & ETHNIC != "7") %>% 
  filter(decage !="0") %>%
  #Now we are averaging by sex and age
  group_by(SEX,decage) %>% 
  summarize(totcorr = mean(totcorr))

# Now we will recreate the plot. You can see a description of all of the variables at 
# https://github.com/pyrfume/pyrfume-data/blob/main/nat_geo_1986/Data%20dictionary.xlsx

## Jake's Example NGS plots ----
#### Extra packages ----

# install.packages("ggthemes")
# install.packages("plotly")

library(ggthemes)
library(plotly)

#### Make a graph that looks like the orginal ----

df %>% 
  ggplot(aes(x = decage, y = totcorr, shape=as.factor(SEX))) +
  geom_line()+
  geom_point(size=+5,color="white",shape=15)+
  geom_point(size=+3)+
  # geom_segment()+
  labs(title = " ")+
  labs(y="NUMBER CORRECT" )+
  scale_shape_manual(name="Sex",labels=c("Male","Female"),values=c(15,1)) +
  scale_y_continuous(limits = c(1,4),
                     breaks=c(1,1.5,2,2.5,3,3.5,4),
                     labels = c("1.0","","2.0","","3.0","","4.0"))+
  scale_x_continuous(
    name="AGE IN DECADES",breaks=c(1:9),
    labels=c("TEENS","20's","30's","40's","50's","60's","70's","80's","90's")
  ) +
  annotate("point",x = c(1.2,2.2,1.2,2.2), y=c(1.75,1.75,1.25,1.25),shape=c(1,1,15,15),size=+3)+
  annotate("line",x = c(1.35,2.05), y=c(1.75,1.75))+
  annotate("line",x = c(1.35,2.05), y=c(1.25,1.25))+
  annotate("text",x = c(2.9,2.8), y=c(1.25,1.75),label=c("FEMALES","MALES"))+
  theme_classic() +
  theme(legend.title=element_blank(),axis.ticks.length=unit(-0.1, "cm"), legend.position="none")

ggsave(paste("graph.remake",Sys.Date(),".pdf"),width = 7, height = 5)


####  Sex, Age, Smoking and Olfactory Perception ----

NGS_Small %>% 
  select( AGE, SELF_RATE_SMELL, totcorr, SEX, SMOKE) %>% 
  mutate(
    SMOKE = ifelse(SMOKE == 1,"Smoker",ifelse(SMOKE == 2, "Non-Smoker","Not Reported")),
    SEX = ifelse(SEX == 1,"Male",ifelse(SEX == 2, "Female","Not Reported")) ) %>% 
  group_by(AGE, SELF_RATE_SMELL,SEX, SMOKE) %>% 
  summarise(totcorr= mean(totcorr)) %>% 
  ggplot(aes(x= totcorr, y = AGE, color=SEX,shape=SMOKE)) +
  geom_point(alpha = 0.3,size=+3) +
  facet_grid( SEX ~ SMOKE) +
  ggtitle("Sex, Age, Smoking and Olfactory Perception")+
  xlab("Average Correct Odors")+
  ylab("Age (years)")+
  theme_classic()

ggsave(paste("graph.sex-age-smoking-olf.",Sys.Date(),".pdf"),width = 8.5, height = 7)

### Does SELF_RATE_SMELL reflect accuracy (fancy 3D plot) ----

df2 <- NGS_Small %>% 
  select( decage, SELF_RATE_SMELL, totcorr, SEX, SMOKE) %>% 
  mutate(
    SMOKE = ifelse(SMOKE == 1,"Smoker",ifelse(SMOKE == 2, "Non-Smoker","Not Reported")),
    SEX = ifelse(SEX == 1,"Male",ifelse(SEX == 2, "Female","Not Reported")) ) %>% 
  group_by(decage, SELF_RATE_SMELL,SEX, SMOKE) %>% 
  summarise(totcorr= mean(totcorr))

NGS_Small %>% 
  select( decage, SELF_RATE_SMELL, totcorr, SEX, SMOKE) %>% 
  mutate(
    SMOKE = ifelse(SMOKE == 1,"Smoker",ifelse(SMOKE == 2, "Non-Smoker","Not Reported")),
    SEX = ifelse(SEX == 1,"Male",ifelse(SEX == 2, "Female","Not Reported")) ) %>% 
  group_by(decage, SELF_RATE_SMELL, SEX, SMOKE) %>% 
  summarise(totcorr= mean(totcorr)) %>% 
  pivot_wider(names_from = SELF_RATE_SMELL, values_from = totcorr)

df3D <- NGS_Small %>% 
  select( decage, SELF_RATE_SMELL, totcorr) %>% 
  group_by(decage, SELF_RATE_SMELL) %>% 
  summarise(totcorr= mean(totcorr)) %>% 
  pivot_wider(names_from = SELF_RATE_SMELL, values_from = totcorr) %>% 
  column_to_rownames(var = "decage")

g3D <- plot_ly(z=as.matrix(df3D)) %>% 
  add_surface() %>% 
  layout(scene = list(
    title=list(title="plot title"),
    xaxis=list(title = "Self Rated Acuity "),
    yaxis=list(title = "Decade Age"),
    zaxis=list(title = "# Odors ID")))

htmlwidgets::saveWidget(g3D, paste("3D-plot-",Sys.Date(),".html"))
