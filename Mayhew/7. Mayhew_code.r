###############################-
## AChemS Pyrfume Codefest 2022
## Exploring odor quality datasets
## Looking for systematic differences in descriptor usage between labelers
## Emily Mayhew / adapted from Joel's shiny code
###############################-


#load packages
library(pyrfume)
library(tidyverse)


#Leffingwell #####
#leffingwell.raw <- load_data("leffingwell/leffingwell_data.csv")
leffingwell.behavior <- load_data("leffingwell/behavior.csv")
leffingwell.molecules <- load_data("leffingwell/molecules.csv")

#Concatenate column names where value is 1 to create a "description" column
leffingwell.clean <- leffingwell.behavior[,-2] %>% 
  #Remove odors with no CID
  filter(CID>0) %>% 
  group_by(CID) %>%
  mutate(description = paste(names(.[-1])[as.logical(cur_data())], collapse = ' ',sep=", ")) %>% 
  select("CID","description") %>% 
  rename("Leffingwell description" = "description")

#IFRA #####
ifra.behavior <- load_data("ifra_2019/behavior.csv")
ifra.clean <- ifra.behavior %>% 
  mutate("IFRA description" = paste(ifra.behavior$Descriptor.1,ifra.behavior$Descriptor.2,ifra.behavior$Descriptor.3,sep=", ")) %>%
  filter(CID>0) %>% 
  select("CID","IFRA description")
#ifra.molecules <- load_data("ifra_2019/molecules.csv")
#ifra <- load_data("ifra_2019/ifra-fragrance-ingredient-glossary---oct-2019.csv")

#Arctander #####
arctander.identifiers <- load_data("arctander_1960/identifiers.csv")
arctander.behavior <- load_data("arctander_1960/behavior_1_sparse.csv")
arctander.behavior$ChastretteDetails <- str_replace_all(arctander.behavior$ChastretteDetails,"[[:punct:]]", " ")
arctander.behavior$ChastretteDetails <- str_squish(arctander.behavior$ChastretteDetails)
arctander.clean <- arctander.behavior %>% 
  #mutate(Stimulus = Stimulus+1) %>% 
  merge(arctander.identifiers) %>% 
  rename("CID" = "new_CID") %>% 
  filter(CID>0) %>% 
  rename("Arctander description" = "ChastretteDetails") %>% 
  select("CID","Arctander description")

#Sigma-Aldrich #####
#sigma.identifiers <- load_data("sigma_2014/identifiers.csv")
sigma.clean <- load_data("sigma_2014/behavior.csv") %>% 
  filter(CID>0) %>% 
  rename("Sigma description" = "descriptors") %>% 
  select("CID","Sigma description")

#AromaDB #####
#aromadb.identifiers <- load_data("aromadb/molecules.csv")
aromadb.behavior <- load_data("aromadb/behavior.csv") 
aromadb.clean <- aromadb.behavior %>% 
  select("CID","Filtered.Descriptors") %>% 
  filter(CID>0) %>% 
  rename("AromaDB description" = "Filtered.Descriptors")

#Add names #####
master.names <- load_data("molecules/molecules.csv") %>% 
  select("CID","IsomericSMILES","name")

#Merge them all! #####
main <- leffingwell.clean %>% 
  #merge(goodscents.clean,all=TRUE) %>% 
  merge(aromadb.clean,all=TRUE) %>% 
  merge(ifra.clean,all = TRUE) %>% 
  merge(arctander.clean,all=TRUE) %>% 
  merge(aromadb.clean,all=TRUE) %>% 
  rowwise() %>% 
  mutate("Number of datasets with descriptions" = sum(!is.na(cur_data()[,c("AromaDB description", "Leffingwell description", "IFRA description", "Arctander description")]))) %>% 
  merge(master.names,.,all.y=TRUE)


### need to clean this data... each dataset has its own separator 
df_clean <- main %>%
  separate(`AromaDB description`, into = c("AromaDB_1", "AromaDB_2", "AromaDB_3"), sep = ";") %>%
  separate(`Leffingwell description`, into = c("Leffingwell_1", "Leffingwell_2", "Leffingwell_3", "Leffingwell_4", "Leffingwell_5", "Leffingwell_6", "Leffingwell_7", "Leffingwell_8", "Leffingwell_9"), sep = " ")%>%
  separate(`IFRA description`, into = c("IFRA_1", "IFRA_2", "IFRA_3"), sep = ", ")%>%
  separate(`Arctander description`, into = c("Arctander_1", "Arctander_2", "Arctander_3", "Arctander_4", "Arctander_5", "Arctander_6", "Arctander_7", "Arctander_8", "Arctander_9", "Arctander_10"), sep = " ")%>%
  pivot_longer(AromaDB_1:Arctander_10, names_to = "DB_index", values_to = "Descriptor")%>%
  drop_na(Descriptor)%>%
  separate(DB_index, into = c("DB", "index", sep = "_"))%>%
  mutate_at( vars(Descriptor), .funs = tolower) 

library(RColorBrewer)

#plot total descriptor usage for top 50 terms for 4 pooled odor quality databases
df_clean %>%
  filter(Descriptor != "")%>%
  group_by(Descriptor)%>%
  summarise(DescriptorCounts = n())%>%
  mutate(DescriptorRank = rank(-DescriptorCounts)) %>%
  filter(DescriptorRank < 51)%>%
  ggplot( aes(reorder(Descriptor, DescriptorRank), DescriptorCounts, fill = DescriptorCounts))+
  geom_histogram(stat = "identity")+
  scale_fill_distiller(palette = 3, direction = 1)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        legend.position = "none",
        axis.title.x = element_blank())+
  ylab("Descriptor use frequency")+
  ggtitle("50 most-used descriptors across 4 odor quality databases")

#plot variations in descriptor usage across 4 odor quality databases
df_clean %>%
  filter(Descriptor != "")%>%
  group_by(DB, Descriptor)%>%
  summarise(DescriptorCounts = n())%>%
  mutate(DescriptorRank = rank(-DescriptorCounts)) %>%
  filter(DescriptorRank < 51)%>%
  ggplot( aes(reorder(Descriptor, DescriptorRank), DescriptorCounts, fill = DB))+
  geom_histogram(stat = "identity")+
  facet_grid(rows = vars(DB))+
  scale_fill_brewer(type = "qual", palette = 2)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        legend.position = "none",
        axis.title.x = element_blank())+
  ylab("Descriptor use frequency")+
  ggtitle("Variations in descriptor usage across 4 odor quality databases")


             