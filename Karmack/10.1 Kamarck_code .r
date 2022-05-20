## Rescore the NGS total correct
## This project was motivated by androstenone

#load libraries
library(tidyverse)

#load data
df <- read.csv("data/NGS_sample01.csv")


#It creates labels for all of the factors to make the dataframe easier to read
#(I apologize for these lines of code)
df$AND_DES_name <- factor(df$AND_DES, levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12), labels = c("No Response", "No odor", "Floral", "Musky", "Urine", "Foul", "Ink", "Spicy", "Woody", "Fruity", "Burnt", "Sweet", "Other"))
df$AA_DES_name <- factor(df$AA_DES, levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12), labels = c("No Response", "No odor", "Floral", "Musky", "Urine", "Foul", "Ink", "Spicy", "Woody", "Fruity", "Burnt", "Sweet", "Other"))
df$GAL_DES_name <- factor(df$GAL_DES, levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12), labels = c("No Response", "No odor", "Floral", "Musky", "Urine", "Foul", "Ink", "Spicy", "Woody", "Fruity", "Burnt", "Sweet", "Other"))
df$EUG_DES_name <- factor(df$EUG_DES, levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12), labels = c("No Response", "No odor", "Floral", "Musky", "Urine", "Foul", "Ink", "Spicy", "Woody", "Fruity", "Burnt", "Sweet", "Other"))
df$MER_DES_name <- factor(df$MER_DES, levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12), labels = c("No Response", "No odor", "Floral", "Musky", "Urine", "Foul", "Ink", "Spicy", "Woody", "Fruity", "Burnt", "Sweet", "Other"))
df$ROSE_DES_name <- factor(df$ROSE_DES, levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12), labels = c("No Response", "No odor", "Floral", "Musky", "Urine", "Foul", "Ink", "Spicy", "Woody", "Fruity", "Burnt", "Sweet", "Other"))

###################################

### make my own total correlation score!

####################################
df$Subj <- row.names(df)

## recode answers that we didn't like

#corr1 - androstenone
## AND = "musky"(3) or "Urine"(4)
## I think it could also smell "foul" or even potentially "floral" if they relate it to perfume
#Give a half point for "foul" or "floral"
#Give a quarter point for 'sweet', 'woody'
hist(df$AND_DES)
df <- df %>%
  mutate(AND_MLK_score = case_when(AND_DES %in% c(3,4) ~ 1,
                                   AND_DES %in% c(2,5) ~ 0.5,
                                   AND_DES %in% c(8,11) ~0.25, 
                                   TRUE ~ 0))

#corr2 - AA (amylyl acetate)
hist(df$AA_DES)
## Amylyl acetate was counted as correct if they say it smells "fruity" (9)
## We will also count it as 'sweet' for a half point
df <- df %>%
  mutate(AA_MLK_score = case_when(AA_DES ==9 ~ 1,
                                  AA_DES == 11 ~ 0.5,
                                  TRUE ~ 0))

#corr3 - galaxolide
hist(df$GAL_DES)
#galaxolide = musky (3)
#Give a half point for 'floral' (2), and a quarter point for sweet (11) and woody (8)
df <- df %>%
  mutate(GAL_MLK_score = case_when(GAL_DES ==3 ~ 1,
                                   GAL_DES == 2 ~ 0.5,
                                   GAL_DES %in% c(8,11) ~ 0.25,
                                   TRUE ~ 0))
#corr4 - eugenol
hist(df$EUG_DES)
# eugenol = spicy (7)
#I totally agree here and most people agree too
df <- df %>%
  mutate(EUG_MLK_score = case_when(EUG_DES ==7 ~ 1,
                                   TRUE ~ 0))

#corr5 - mercaptans (gas)
hist(df$MER_DES)
# mercaptans = foul (5)
# seems good - I'll give half a point for burnt becuase people might mix up gas with burning... ( I admit this is arbitrary)
#I was thinking I could also give a quarter of a point for 'urine' becuase its the only other bad descriptor... 
df <- df %>%
  mutate(MER_MLK_score = case_when(MER_DES ==5 ~ 1,
                                   MER_DES == 10 ~ 0.5,
                                   MER_DES == 4 ~ 0.25,
                                   TRUE ~ 0))


#corr 6 - rose
hist(df$ROSE_DES)
#only accepts floral; I think that's also pretty good. 
df <- df %>%
  mutate(ROSE_MLK_score = case_when(ROSE_DES ==2 ~ 1,
                                    TRUE ~ 0))


############################
#### Total MLK score
############################
df$totcorr_MLK_score <- df$AND_MLK_score + df$AA_MLK_score + df$EUG_MLK_score + df$GAL_MLK_score + df$MER_MLK_score + df$ROSE_MLK_score


############################
#### Clean by eliminating people who didn't answer the questions at all
############################
## 1. eliminate people who didn't answer ALL of the SMELL yes/no (this is coded as 0) - eliminated 3062 people
df.answer <- subset(df, !(AND_SMELL == 0 & AMY_SMELL ==0 & GALAX_SMELL ==0 & EUG_SMELL ==0 & MERCAP_SMELL ==0 & ROSE_SMELL ==0))
## 2. eliminate people who didn't anyswer ANY of the descriptors - eliminated 4148 people
df.forcedAnswer <- subset(df.answer, !(AND_DES ==0 & AA_DES ==0 & GAL_DES ==0 & EUG_DES==0 & MER_DES==0 & ROSE_DES ==0))
## 3. eliminate people who answered all of the desciprors as 'other'- only 26 dudes
df.allOther <- subset(df.forcedAnswer, !(AND_DES ==12 & AA_DES ==12 & GAL_DES ==12 & EUG_DES==12 & MER_DES==12 & ROSE_DES ==12))
#rename
df.score <- df.allOther

## look at this
#hist(df.score$totcorr_MLK_score)
#hist(df.score$totcorr)

############################
#### How did we do??
############################

#make tidy
#df.g <- pivot_longer(df.score, c(totcorr, totcorr_MLK_score),names_to="scoreType", values_to = "correct")

#ggplot(df.g, aes(x = correct, color = scoreType))+
#  geom_density()

## unit slope
ggplot(df.score, aes(x=totcorr, y=totcorr_MLK_score))+
  geom_jitter(alpha=0.2)+
  geom_abline(slope=1, intercept=0)+
  xlab("Original 'Total Correct' Score")+
  ylab("Recoded 'Total Correct' Score")+
  theme_bw()

