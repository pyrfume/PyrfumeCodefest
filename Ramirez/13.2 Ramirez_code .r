#Install the packages
required_packages = c("maptools", "rgdal", "rgeos", "tidyverse","sf",
                      "zipcodeR")
for (i in required_packages) {
  if (i %in% rownames(installed.packages()) == FALSE) {
    install.packages(i)
  }
  else {
    NULL
  }
}

# Load what we need
require(maptools)
require(rgdal)
require(rgeos)
require(tidyverse)
require(sf)
require(zipcodeR)

# Read shapefile into R
ztca <- readOGR("tl_2010_us_zcta500/tl_2010_us_zcta500.shp", verbose=TRUE)

# Reformat to sf
reformat_sf<-st_as_sf(ztca)

# Do you think memory grows on trees? We keep only what we need
rm(ztca)

# Load in NGSS Dataset
NGS<-read_csv("NGS.csv")

# Select only those in the US with a valid zipcode
Selected_NGS<-NGS %>% filter(COUNTRY==84 & ZIP!=0) %>%  select(...1,ZIP)

# Get it out of memory
rm(NGS)

# Fix zipcodes starting with 0
Selected_NGS$Zipcode<-zipcodeR::normalize_zip(Selected_NGS$ZIP)

# Count the number of times each zipcode is present
ZipcodeCounts<-Selected_NGS %>% count(Zipcode)

#Join your counts with the spatial data
joined_final<-left_join(reformat_sf,ZipcodeCounts,by=c("ZCTA5CE00"="Zipcode"))

# Release your memory
# Keep only what is needed
rm(reformat_sf)
rm(ZipcodeCounts)
rm(Selected_NGS)

# Save your data here so we only have to clean once
save.image("ACHEMS.RData")



# Load your data if you are in a new session
# load("ACHEMS.RData")

# Include only zipcodes which have at least one respondent
joined_final_<-joined_final %>% filter(n>=1)

# Create a spatial file for the US MAP
states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

# Make sure both have the same projection
states_transformed<-st_transform(states,crs=4269)


# Make the plot
ggplot() + geom_sf(data=states_transformed) + geom_point(data=joined_final_, aes(y=as.numeric(INTPTLAT00),x=as.numeric(INTPTLON00),color=log(n)),alpha=0.4,size=0.2) +
  scale_color_viridis_c() +
  xlim(-125,-65)+ylim(25,50) +
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.line.x = element_blank()) +
  theme(axis.line.y = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  labs(color="ln(N)") +
  ggtitle("Geographic Distribution of National Geographic Smell Survey Respondents in the USA",subtitle = "Hawaii, Alaska, and Puerto Rico omitted.") 
  




