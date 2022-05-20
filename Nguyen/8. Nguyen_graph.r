#Set up packages
library(pyrfume)
library(tidyverse)
library(GGally)

# Read in and look at the data - random 1% sample of NGS dataset
NGS <- read.table("data/NGS_sample01.csv", header=T, sep = ",", stringsAsFactors = F)

# 1. Reproduce the NGS plot
df <- NGS %>%
  filter(SEX != "0" & totcorr !=7 & SMOKE != "0") %>%
  filter(ETHNIC !="0" & ETHNIC != "7") %>% 
  filter(decage !="0") %>%
  #Now we are averaging by sex and age
  group_by(SEX,decage) %>% 
  summarize(totcorr = mean(totcorr))

# Create plot with ggplot
ggplot(df, aes(x = decage, y = totcorr,shape=as.factor(SEX)))+
  geom_point(size=4)+
  geom_line()+
  labs(y="NUMBER CORRECT")+
  scale_shape_manual(name="",breaks=c("2","1"),labels=c("FEMALES","MALES"),values=c(1,15))+
  scale_size_manual(values=c(5,5))+
  scale_x_continuous(name="AGE IN DECADES",breaks=c(1:9),labels=c("TEEN'S","20'S","30'S","40'S","50'S","60'S","70'S","80'S","90'S"))+
  #scale_y_continuous(limits=c(1.0,4.0),labels=function(x) format(x,nsmall=1),breaks=seq(1,4,0,0.5))+ # this can reformat the y-axis tick labels to one decimal number
  scale_y_continuous(limits=c(1.0,4.0),breaks=seq(1,4,0.5),labels = c("1.0","","2.0","","3.0","","4.0"))+ # this can provide the same y-axis as in the original NGS plot
  theme_classic()+
  theme(legend.position = c(0.12,0.15))

# 2. The influence of demographics on self-reported ability and experienced smell intensities
## Create a new data set with demographics, self-reported ability and smell intensities
dat_demo_smell<-NGS[,c("SELF_RATE_SMELL","AND_INT","AA_INT","GAL_INT","EUG_INT","MER_INT","ROSE_INT",
                       "DAYS_W_PERF","SMOKE","decage","SEX","ETHNIC" )]
colnames(dat_demo_smell)<-c("Self-reported ability","Androstenone","Amyl_acetate","Galaxolide",
                            "Eugenol","Mercaptans","Rose",
                            "Perfume_use","Smoke","Age in decage","Sex","Ethnic")
dat_demo_smell[dat_demo_smell==0]<-NA # replace 0 with NA
dat_demo_smell<-na.omit(dat_demo_smell) # remove rows with NAs
dat_demo_smell$`Experienced intensity`<-rowMeans(dat_demo_smell[,2:7]) # Compute the composite rating for 6 smell ratings
dat_demo_smell$Sex<-recode(dat_demo_smell$Sex,"1"="Male","2"="Female") # recode Sex to Male and Female
dat_demo_smell[c("Perfume_use","Smoke","Sex","Age in decage","Ethnic")]<-lapply(dat_demo_smell[c("Perfume_use","Smoke","Sex","Age in decage","Ethnic")],factor) # set factor variables

## Correlations between ratings and the influence of Sex, similar for other demographics 
ggpairs(dat_demo_smell[,c(1:7,13,11)],aes(color = Sex,alpha = 0.5)) # Pairs plot for Self-repoted ability and smell intensities, grouping by Sex

## Correlation between demographics and ratings
ggpairs(dat_demo_smell[,c(1,13,10,8,9,11,12)])