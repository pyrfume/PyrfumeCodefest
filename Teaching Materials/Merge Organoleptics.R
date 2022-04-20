###
#Add Goodscents
#clean up duplicates--acetoin
#Clean up arctander sparse



library(pyrfume)
library(tidyverse)



#Merge based on CID

#Leffingwell
#leffingwell.raw <- load_data("leffingwell/leffingwell_data.csv")
leffingwell.behavior <- load_data("leffingwell/behavior.csv")
leffingwell.molecules <- load_data("leffingwell/molecules.csv")

# leffingwell.reduced <- leffingwell.behavior[,-2] %>% 
#   filter(!CID==0) %>% 
#   group_by(CID) %>%
#   mutate(description = paste(names(.[-1])[as.logical(cur_data())], collapse = ' ',sep=", ")) %>% 
#   select(CID,description) %>% 
#   merge(leffingwell.molecules[,c(1,5)],.) 

#Concatenate column names where value is 1 to create a "description" column
leffingwell.clean <- leffingwell.behavior[,-2] %>% 
  #Remove odors with no CID
  filter(CID>0) %>% 
  group_by(CID) %>%
  mutate(description = paste(names(.[-1])[as.logical(cur_data())], collapse = ' ',sep=", ")) %>% 
  select("CID","description") %>% 
  rename("Leffingwell description" = "description")

# #Goodscents
goodscents.behavior <- load_data("goodscents/behavior.csv")
goodscents.molecules <- load_data("goodscents/molecules.csv")
goodscents.identifiers <- load_data("goodscents/identifiers.csv")
# 

goodscents.reduced <- goodscents.behavior %>%
  merge(goodscents.identifiers,.) %>%
  select(-TGSC.ID) %>%
  group_by(CID) %>%
  distinct(CID,.keep_all=TRUE) %>% #This grabs one of the descriptions for each CID, but not sure why there are multiple descriptions
  mutate(description = paste(names(.[-1])[as.logical(cur_data())], collapse = ' ')) %>% 
  select(CID,description) %>% 
  rename("Goodscents description" = "description")
# #58 (1037661) is a duplicate, 126 brown is different

# #Dravnieks
# #We will look at usage for the moment, but may want to add Applicability or combine the two
#dravnieks.applicability <- load_data("dravnieks_1985/behavior_1.csv")
dravnieks.molecules <- load_data("dravnieks_1985/molecules.csv")
dravnieks.identifiers <- load_data("dravnieks_1985/identifiers.csv")

dravnieks.usage <- load_data("dravnieks_1985/behavior_2.csv") %>%
  merge(dravnieks.identifiers[,c(1,3)],.) %>% 
  separate(Stimulus,c("Stimulus","Concentration"),sep="_") %>% 
  filter(Concentration=="high") %>% #Some odors were run at two concentrations, here we merge only the high
  filter(CID>0)

dravnieks.usage.atlas <- dravnieks.usage %>%
  pivot_longer(FRUITY.CITRUS:WARM, names_to = "Descriptor", values_to = "Value") %>%
  arrange(CID, -Value) %>% 
  group_by(CID) %>% 
  mutate(rankedDesc = rank(-Value,ties.method = "first")) %>% 
  mutate(Rating = paste(Descriptor,round(Value,1))) %>% 
  select(-c(Value,Descriptor,Concentration,Stimulus)) %>% 
  pivot_wider(names_from = rankedDesc, values_from = Rating) %>%
  select(1:6) %>% #CID and first five descriptors
  rename("A"="1","B"="2","C"="3","D"="4","E"="5") %>% 
  mutate("Dravnieks description" = tolower(paste(as.character(A),as.character(B),as.character(C),as.character(D),as.character(E)))) %>% 
  select(CID,"Dravnieks description")


#IFRA
ifra.behavior <- load_data("ifra_2019/behavior.csv")
ifra.clean <- ifra.behavior %>% 
  mutate("IFRA description" = paste(ifra.behavior$Descriptor.1,ifra.behavior$Descriptor.2,ifra.behavior$Descriptor.3,sep=", ")) %>%
  filter(CID>0) %>% 
  distinct(CID,.keep_all=TRUE) %>% #This grabs one of the descriptions for each CID, but not sure why there are multiple descriptions. Should they be merged?
  select("CID","IFRA description")
#ifra.molecules <- load_data("ifra_2019/molecules.csv")
#ifra <- load_data("ifra_2019/ifra-fragrance-ingredient-glossary---oct-2019.csv")

#Arctander
arctander.identifiers <- load_data("arctander_1960/identifiers.csv")
arctander.behavior <- load_data("arctander_1960/behavior_1_sparse.csv")
arctander.behavior$ChastretteDetails <- str_replace_all(arctander.behavior$ChastretteDetails,"[[:punct:]]", " ")
arctander.behavior$ChastretteDetails <- str_squish(arctander.behavior$ChastretteDetails)
arctander.clean <- arctander.behavior %>% 
  merge(arctander.identifiers) %>% 
  rename("CID" = "new_CID") %>% 
  filter(CID>0) %>% 
  distinct(CID,.keep_all=TRUE) %>% #This grabs one of the descriptions for each CID, but not sure why there are multiple descriptions. Should they be merged?
  rename("Arctander description" = "ChastretteDetails") %>% 
  select("CID","Arctander description")
  
#Sigma-Aldrich
#sigma.identifiers <- load_data("sigma_2014/identifiers.csv")
sigma.clean <- load_data("sigma_2014/behavior.csv") 
sigma.clean$descriptors <- str_replace_all(sigma.clean$descriptors,"[[:punct:]]", " ")
sigma.clean$descriptors <- str_squish(sigma.clean$descriptors)
sigma.clean <- sigma.clean %>% 
  filter(CID>0) %>% 
  distinct(CID,.keep_all=TRUE) %>% #This grabs one of the descriptions for each CID, but not sure why there are multiple descriptions. Should they be merged?
  rename("Sigma description" = "descriptors") %>% 
  select("CID","Sigma description")

  
#AromaDB
#aromadb.identifiers <- load_data("aromadb/molecules.csv")
aromadb.behavior <- load_data("aromadb/behavior.csv") 
aromadb.clean <- aromadb.behavior %>% 
  select("CID","Filtered.Descriptors") %>% 
  filter(CID>0) %>% 
  distinct(CID,.keep_all=TRUE) %>% #This grabs one of the descriptions for each CID, but not sure why there are multiple descriptions. Should they be merged?
  rename("AromaDB description" = "Filtered.Descriptors")

#Add names
master.names <- load_data("molecules/molecules.csv") %>% 
  select("CID","name")
  
#Merge them all!
main <- leffingwell.clean %>% 
  #merge(goodscents.clean,all=TRUE) %>% 
  merge(aromadb.clean,all=TRUE) %>% 
  merge(ifra.clean,all = TRUE) %>% 
  merge(arctander.clean,all=TRUE) %>% 
  merge(goodscents.reduced,all=TRUE) %>% 
  merge(dravnieks.usage.atlas,all=TRUE) %>%
  merge(sigma.clean,all=TRUE) %>%
  rowwise() %>% 
  mutate("Number of datasets with descriptions" = sum(!is.na(cur_data()[,c("AromaDB description", "Leffingwell description", "IFRA description", "Arctander description","Goodscents description","Dravnieks description","Sigma description")]))) %>% 
  merge(master.names,.,all.y=TRUE)


#check duplicates
length(main$CID)-length(unique(main$CID))
