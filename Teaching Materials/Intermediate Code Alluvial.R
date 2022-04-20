###############################-
## AChemS Pyrfume Codefest 2022
## Nat Geo Smell Survey
## Exploratory analysis & alluvial plots
## Emily Mayhew
###############################-

##### Table setting #####

# Load packages
library(tidyverse) #clean, reformat, plot data
library(corrplot) #make plots showing correlations
library(ggalluvial) #make alluvial plots

# Read in and look at the data - random 1% sample of NGS dataset
NGS <- read.table("data/NGS_sample01.csv", header=T, sep = ",", stringsAsFactors = F)
head(NGS) #print first 6 rows
summary(NGS) #print summary stats for each column


##### Make some exploratory plots #####

# What is the age distribution of participants? 
# plot histogram of subject ages
ggplot(NGS, aes(x=AGE)) + 
  geom_histogram(binwidth = 10)

# Which were the strongest of the odors?  
# make boxplot of intensity ratings by odorant
NGS %>%
  pivot_longer(AND_INT:ROSE_INT, names_to = "odor", values_to = "intensity")%>% #reshape data so all intensity ratings are in 1 column
  ggplot( aes(x=reorder(odor, intensity), y= intensity)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #rotate axis labels

#Did men or women find the odors to be more intense?
#plot boxplots of intensity for each odorant broken out by sex
NGS %>%
  filter(SEX != 0)%>% #drop non-responses
  pivot_longer(AND_INT:ROSE_INT, names_to = "odor", values_to = "intensity")%>% #reshape data so all intensity ratings are in 1 column
  mutate(SEX = factor(SEX,
                      levels = c(0,1,2),
                      labels = c("", "male", "female"))) %>% #make sex a factor, change numbers to labels 
  ggplot( aes(x=SEX, y= intensity, color = SEX))+
  geom_boxplot()+
  facet_grid(~odor)+ #make a separate panel for each odorant
  scale_color_manual(values = c("royalblue", "coral"))+ #change colors
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #rotate axis labels


# Does sensitivity decline with age?
# plot correlation of intensity with age
corr <- cor(NGS[,c("AGE", "AND_INT", "AA_INT", "GAL_INT", "EUG_INT", "MER_INT", "ROSE_INT")], method = "pearson")
corrplot(corr, method = "circle", type = "lower")


##### Make an alluvial plot #####

# Alluvial plot for perceived intensity and word usage by odorant

# First make a smaller data frame without the unnecessary columns
NGS.ID <- NGS[,c("AND_INT", "AA_INT", "GAL_INT", "EUG_INT", "MER_INT", "ROSE_INT", "AND_DES", "AA_DES", "GAL_DES", "EUG_DES", "MER_DES", "ROSE_DES")]
NGS.ID$ID <- rownames(NGS.ID) #need row id's to spread later
# Gather columns with responses on intensity ratings and descriptor choice for each odor
IntDes.gather <- gather(NGS.ID, key="odor.key", value="rating", AND_INT, AA_INT, GAL_INT, EUG_INT, MER_INT, ROSE_INT, AND_DES, AA_DES, GAL_DES, EUG_DES, MER_DES, ROSE_DES)
IntDes.sep <- separate(IntDes.gather, "odor.key", c("odor", "question"))
IntDes <- spread(IntDes.sep, "question", "rating")

#Rename values in more descriptive way
IntDes$odor[IntDes$odor == "AND"] <- "Androstenone"
IntDes$odor[IntDes$odor == "AA"] <- "Amyl Acetate"
IntDes$odor[IntDes$odor == "GAL"] <- "Galaxolide"
IntDes$odor[IntDes$odor == "EUG"] <- "Eugenol"
IntDes$odor[IntDes$odor == "MER"] <- "Mercaptans"
IntDes$odor[IntDes$odor == "ROSE"] <- "Rose"

IntDes$DES[IntDes$DES == 1] <- "No odor"
IntDes$DES[IntDes$DES == 2] <- "Floral"
IntDes$DES[IntDes$DES == 3] <- "Musky"
IntDes$DES[IntDes$DES == 4] <- "Urine"
IntDes$DES[IntDes$DES == 5] <- "Foul"
IntDes$DES[IntDes$DES == 6] <- "Ink"
IntDes$DES[IntDes$DES == 7] <- "Spicy"
IntDes$DES[IntDes$DES == 8] <- "Woody"
IntDes$DES[IntDes$DES == 9] <- "Fruity"
IntDes$DES[IntDes$DES == 10] <- "Burnt"
IntDes$DES[IntDes$DES == 11] <- "Sweet"
IntDes$DES[IntDes$DES == 12] <- "Other"

# Drop non-responses
IntDes <- IntDes[IntDes$DES != 0,] 
IntDes <- IntDes[IntDes$INT != 0,]

# Change order of factors
IntDes$DES <- factor(IntDes$DES, levels = c("Sweet", "Fruity", "Floral", "Spicy", "Woody", "Musky", "Urine", "Foul", "Burnt", "Ink", "Other", "No odor"))
IntDes$INT <- factor(IntDes$INT, levels = c(5, 4, 3, 2, 1))
IntDes$odor <- factor(IntDes$odor, levels = c("Amyl Acetate", "Rose", "Eugenol", "Galaxolide", "Androstenone", "Mercaptans"))

# Change variable names
colnames(IntDes) <- c("ID", "odor", "descriptor", "intensity")

# Create frequency table
IntDesTable <- xtabs(~odor + descriptor + intensity, data=IntDes)
IntDesAlluv <- as.data.frame(ftable(IntDesTable))

# Generate alluvial plot
ggplot(IntDesAlluv, aes(y = Freq, axis1 = odor, axis2 = descriptor, axis3 = intensity)) +
  geom_alluvium(aes(fill = odor), width = 1/100) +
  geom_stratum(width = 1/20, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("odor", "descriptor","intensity"), expand = c(.1, .1)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  theme_classic() +
  ggtitle("Evaluation of odor character and intensity per odorant")


##### Make a different alluvial plot #####

# Now with even more questions! 
# Memory, Eat, Wear

#subset dataframe
NGS.ID <- NGS[,c("AND_QUAL", "AA_QUAL", "GAL_QUAL", "EUG_QUAL", "MERCAP_QUAL", "ROSE_QUAL",
                 "AND_INT", "AA_INT", "GAL_INT", "EUG_INT", "MER_INT", "ROSE_INT", 
                 "AND_MEM", "AA_MEM", "GAL_MEM", "EUG_MEM", "MER_MEM", "ROSE_MEM", 
                 "AND_EAT", "AA_EAT", "GAL_EAT", "EUG_EAT", "MER_EAT", "ROSE_EAT",
                 "AND_WEAR", "AA_WEAR", "GAL_WEAR", "EUG_WEAR", "MER_WEAR", "ROSE_WEAR")]
names(NGS.ID)[names(NGS.ID) == "MERCAP_QUAL"] <- "MER_QUAL"
NGS.ID$ID <- rownames(NGS.ID)

#reformat data
IntDes.gather <- gather(NGS.ID, key="odor.key", value="rating", -ID)
IntDes.sep <- separate(IntDes.gather, "odor.key", c("odor", "question"))
IntDes <- spread(IntDes.sep, "question", "rating")

#drop non-responses
IntDes <- IntDes[IntDes$QUAL != 0,]
IntDes <- IntDes[IntDes$INT != 0,]
IntDes <- IntDes[IntDes$MEM != 0,]
IntDes <- IntDes[IntDes$EAT != 0,]
IntDes <- IntDes[IntDes$WEAR != 0,]

#Change 1/2 to Yes/No
IntDes$MEM[IntDes$MEM == 1] <- "Yes"
IntDes$MEM[IntDes$MEM == 2] <- "No"
IntDes$EAT[IntDes$EAT == 1] <- "Yes"
IntDes$EAT[IntDes$EAT == 2] <- "No"
IntDes$WEAR[IntDes$WEAR == 1] <- "Yes"
IntDes$WEAR[IntDes$WEAR == 2] <- "No"

#change odor names
IntDes$odor[IntDes$odor == "AND"] <- "Androstenone"
IntDes$odor[IntDes$odor == "AA"] <- "Amyl Acetate"
IntDes$odor[IntDes$odor == "GAL"] <- "Galoxolide"
IntDes$odor[IntDes$odor == "EUG"] <- "Eugenol"
IntDes$odor[IntDes$odor == "MER"] <- "Mercaptans"
IntDes$odor[IntDes$odor == "ROSE"] <- "Rose"

#order factor levels
IntDes$QUAL <- factor(IntDes$QUAL, levels = c(5, 4, 3, 2, 1))
IntDes$INT <- factor(IntDes$INT, levels = c(5, 4, 3, 2, 1))
IntDes$MEM <- factor(IntDes$MEM, levels = c("Yes", "No"))
IntDes$EAT <- factor(IntDes$EAT, levels = c("Yes", "No"))
IntDes$WEAR <- factor(IntDes$WEAR, levels = c("Yes", "No"))
IntDes$odor <- factor(IntDes$odor, levels = c("Amyl Acetate", "Rose", "Eugenol", "Androstenone", "Galoxolide", "Mercaptans"))

#generate frequency table
IntDesTable <- xtabs(~odor + QUAL + INT + EAT + WEAR, data=IntDes)
IntDesAlluv <- as.data.frame(ftable(IntDesTable))

mini <- IntDesAlluv[IntDesAlluv$odor == "Amyl Acetate",]


ggplot(IntDesAlluv, aes(y = Freq, axis1 = odor, axis2 = EAT, axis3 = WEAR, axis5 = QUAL, axis6 = INT)) +
  geom_alluvium(aes(fill = odor), width = 1/360) +
  geom_stratum(width = 1/10, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("odor", "would eat", "would wear", "pleasantness", "intensity"), expand = c(.1, .1)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  theme_classic() +
  ggtitle("How are odors described?")

