#Starter Code Dravnieks Atlas

# Library
library(pyrfume)
library(fmsb)#spider/radar plot
library(plotly)#interactive
library(ggrepel)#labels
library(tidyverse)
#plotting theme
theme_set(theme_light())

#Load the data
#We will look at usage for the moment, but may want to add Applicability or combine the two
atlasUsage <- load_data("dravnieks_1985/behavior_2.csv") %>%
  separate(Stimulus,c("Stimulus","Concentration"),sep="_")
#atlasApplicability <- load_data("dravnieks_1985/behavior_1.csv")

head(atlasUsage)

#Visualize with Principal Components Analysis
pca1<-prcomp(atlasUsage[,-c(1:2)],scale=T)
scores = as.data.frame(pca1$x)
ggplot(data=scores, aes(x=PC1, y=PC2, label=atlasUsage$Stimulus))+geom_point()+ geom_text_repel()

#Make PCA plot where only dots that have lemon >20 are colored
lemon = atlasUsage[[4]]
lemon10rows = which(lemon>10)

#Make an interactive plot--you can mouseover the points to see the odor label
p <- ggplot(data=scores, aes(x=PC1, y=PC2))+
  geom_point()+
  geom_point(data=scores[lemon10rows,],color="#FFFF00")
ggplotly(p)

#Now a couple spider plots
# To format for the fmsb package, I have to add 2 lines to the dataframe: the possible max and min of each descriptor
df.spider=rbind(rep(100,ncol(atlasUsage)) , rep(0,ncol(atlasUsage)) , atlasUsage)
rownames(df.spider)=df.spider$Stimulus
df.spider <- df.spider[,-c(1,2)]
#Set the odor to look at
odorNum=which(rownames(df.spider)=="AllylCaproate")
#Drop descriptors with low usage
over10 <- df.spider[odorNum,] > 10
over10[1]=FALSE #remove the first column, which is odor label
radarchart(df.spider[c(1,2,odorNum),over10],maxmin=TRUE,vlcex=0.5,title=rownames(df.spider)[odorNum],cglty = 1, cglcol = "gray",pcol = 4, plwd = 2,pfcol = rgb(0, 0.4, 1, 0.25))
