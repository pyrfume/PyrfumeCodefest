library(shiny)
library(pyrfume)
library(tidyverse)
#first we load data sources and match descriptor to CID

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

#Now start the actual Shiny app code:

# UI #####
ui <- fluidPage(
  titlePanel("Pyrfume Odor Lookup"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("name",
                       "Chemical Name:",
                       c("All",
                         unique(as.character(main$name))))
    ),
    column(4,
           selectInput("smiles",
                       "SMILES:",
                       c("All",
                         unique(as.character(main$IsomericSMILES))))
    ),
    column(4,
           selectInput("CID",
                       "PubChemID:",
                       c("All",
                         unique(as.character(main$CID))))
    )
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table")
)

#Server #####
server <- function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- main
    if (input$name != "All") {
      data <- data[data$name == input$name,]
    }
    if (input$smiles != "All") {
      data <- data[data$IsomericSMILES == input$smiles,]
    }
    if (input$CID != "All") {
      data <- data[data$CID == input$CID,]
    }
    data
  }))
  
}

# Run the application
shinyApp(ui = ui, server = server)
