####  Program name:     BeeSummaries_perStatusCell_20230718.r
####  Program location: ./Programs/Bioindicator_Chesshire2021_checklists_easternHalfUSA/Extract_BeeBird_data
####  Program goal:     Count number of bees and/or bee richness per grid cell
####  Created by:       Josee Rousseau 
####  Last modified:    October 17, 2023

### Libraries
library(here)
library(raster)
library(stars)
library(spData)
library(sf)
library(dplyr)
library(purrr)
library(ggplot2)
library(scam)
library(stringr)

'%!in%' <- function(x,y)!('%in%'(x,y))

### Import bee records
bees <- read.csv(here("Data", "BioIndicator", "bees_Chesshire2021_USA_20231011.csv"))
# 1,991,840 records

### Select years of interest
bees.yr <- bees[which(bees$year >= 2007 & bees$year <= 2021), ] # 690,096 records

### Import the bird status grid
grid <- st_read("/Users/jsr293/Documents/GIS_Data/eBird_StatusTrend_Grids/statusGrid_USA.shp")
grid.crs <- crs(grid)

### Extract USA region 
region <- st_transform(us_states, crs = 4326)

### Delineate Eastern USA region
EastUSA <- region[region$NAME %in% c("West Virginia", "Virginia", "Kentucky", "Tennessee", "North Carolina", "South Carolina", "Georgia",
                                     "Alabama", "Maryland", "Mississippi", "Connecticut", "Delaware", "Massachusetts", "New Hampshire",
                                     "New Jersey", "New York", "Pennsylvania", "Rhode Island", "Vermont", "District of Columbia",
                                     "North Dakota", "South Dakota", "Nebraska", "Kansas", "Minnesota", "Iowa", "Missouri", "Wisconsin", 
                                     "Illinois","Michigan", "Indiana", "Ohio", "Arkansas", "Oklahoma"), ]
  
### Dissolve the state boundaries of the polygon
EastUSA <- EastUSA %>%
  st_union() %>%
  st_sf() 

### Project the polygon to projection used by bird status and buffer slightly (3 km)
EastUSA <- EastUSA %>%
  st_transform(crs = grid.crs) %>%
  st_buffer(dist = 3000)
# st_write(EastUSA2,
# 				 dsn = here("Data", "BioIndicator", "EastUSA2_20231017.shp"),
# 				 layer = "EastUSA2_20231017",
# 				 driver="ESRI Shapefile")

### Crop the status grid to our region of interest and export layer
grid.EastUSA <- grid[lengths(st_intersects(grid, EastUSA)) == 1,]  # if use == 0 then select grids outside area of interest
st_write(grid.EastUSA2,
				 dsn = here("Data", "BioIndicator", "birdStatusGrid_4326_EastUSA_3kmGridAndBuffer_20231017.shp"),
				 layer = "birdStatusGrid_4326_EastUSA_3kmGridAndBuffer_20231017",
				 driver="ESRI Shapefile")
saveRDS(grid.EastUSA, here("Data", "BioIndicator", "birdStatusGrid_4326_EastUSA_3kmGridAndBuffer_20231017.rds"))

### Read the status grid to eastern USA (3x3 km square grid cells)
# grid.EastUSA <- readRDS(here("Data", "BioIndicator", "birdStatusGrid_4326_EastUSA_3kmGridAndBuffer_20231017.rds"))

### Same USGS as spatial object
bees.yr$Long <- bees.yr$finalLongitude
bees.yr$Lat <- bees.yr$finalLatitude
bees.sf <- st_as_sf(x = bees.yr,
           coords = c("Long", "Lat"),
           crs = 4326) %>%
  st_transform(crs = grid.crs)
# st_write(bees.sf,
# 				 dsn = here("Data", "BioIndicator", "allBees_20231017.shp"),
# 				 layer = "allBees_20231017",
# 				 driver="ESRI Shapefile")

### Extract bees in region of interest
beesInStates <- st_filter(bees.sf, grid.EastUSA) 
# 482,101 records for 2007 to 2021

### Identify the grid in which each bee is located
bees.grid <- sf::st_join(beesInStates, grid.EastUSA, join = st_intersects) 

### Reduce size of dataframe by selecting some columns
bees.grid2 <- bees.grid[, c("idCells", "institutionCode", "collectionCode", "finalName", "individualCount", "year", "month", 
                           "day", "finalLongitude", "finalLatitude", "finalCoordinateUncertaintyInMeters", "stateProvince",
                           "samplingProtocol", "samplingEffort", "identifiedBy", "order", "family", "genus",
                           "specificEpithet", "infraspecificEpithet", "taxonRank", "occurrenceID", "catalogNumber", "id",
                           "basisOfRecord", "Source", "geometry")]

### Save the list of bees and associated grid cell ID
st_write(bees.grid2,
				 dsn = here("Data", "BioIndicator", "allBees_statusGrid_EastHalfUSA_2007to2021_20231017.shp"),
				 layer = "allBees_statusGrid_EastHalfUSA_2007to2021_20231017",
				 driver="ESRI Shapefile")
saveRDS(bees.grid2, here("Data", "BioIndicator", "allBees_statusGrid_EastHalfUSA_2007to2021_20231017.rds")) # 482,101 records

###########################
### Summarize the bees ####
###########################

### Import file from above
allBees <- readRDS(here("Data", "BioIndicator", "allBees_statusGrid_EastHalfUSA_2007to2021_20231017.rds")) # 482,101 records

### Check what years are included
quantile(allBees$year)
#   0%  25%  50%  75% 100% 
# 2007 2011 2014 2018 2021 
  
### Plot histogram of years
ggplot(allBees, aes(year)) +
  geom_histogram(position=position_identity(), fill="blue", color = "black", alpha=0.25, breaks=seq(2006, 2022, by = 1)) +    
	xlim(c(2006, 2022)) + 
	labs(x="year", y = "Number of records") +
	theme_bw() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))

### Select range of years if not 2007 to 2021
allBees.yr <- allBees[which(allBees$year >= 2007 & allBees$year <= 2021),] # 482,101 bee records

### Check the meter uncertainty associated with the bee locations
summary(allBees.yr$finalCoordinateUncertaintyInMeters)
# For 2007 to 2021
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   #  1.0     5.0    22.0   309.9   139.0 14848.0  440262
    
### Plot histogram of uncertainty
ggplot(allBees.yr, aes(finalCoordinateUncertaintyInMeters)) +
  geom_histogram(position=position_identity(), fill="blue", color = "black", alpha=0.25, bins = 50) +    # breaks=seq(2006, 2022, by = 1)
	# xlim(c(2006, 2022)) + 
	labs(x="Location Uncertainty", y = "Number of records") +
	theme_bw() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))

### Select meter uncertainty cut off
allBees.yr.m <- allBees.yr[(allBees.yr$finalCoordinateUncertaintyInMeters <= 3000 | is.na(allBees.yr$finalCoordinateUncertaintyInMeters)),]
# For 2007 to 2021: 480,908 records for 3 km

### Remove surveys that do not have date info, i.e., missing day or month
allBees.yr.m <- allBees.yr.m[which(allBees.yr.m$day > 0),] # 476,586 records
allBees.yr.m <- allBees.yr.m[which(allBees.yr.m$month > 0),] # 476,584 records
     
### If column 'individual count' is empty or equal to 0, replace by 1
allBees.yr.m$individualCount[allBees.yr.m$individualCount == 0] <- 1
allBees.yr.m$individualCount[is.na(allBees.yr.m$individualCount)] <- 1

### Keep only needed variables
allBees.yr.m.col <- allBees.yr.m[, c("idCells", "year", "month", "day", "finalLatitude", "finalLongitude", "finalName", "individualCount")]  
st_geometry(allBees.yr.m.col) <- NULL

### Summarize
length(unique(allBees.yr.m.col$idCells)) # 26,673 grid cells
dim(allBees.yr.m.col)[1] # 476,584 bee records
length(unique(allBees.yr.m.col$finalName)) # 792 species of bees
# beeSpp <- data.frame(table(allBees.yr.m.col$finalName))

##########################################################
### Calculate standardized number of species per grid ----
##########################################################

### Calculate the standardized number of species, by first calculating the number of species per survey
bees.per.event <- allBees.yr.m.col %>%
  group_by(idCells, year, month, day, finalLatitude, finalLongitude) %>%  # to summarize per grid cell and survey effort (date, location, protocol)
  summarise(nBeesPerEvent = sum(individualCount),
            nSppPerEvent = n_distinct(finalName),
            .groups = "drop") # 133,628 events

####################################################################
### Identify the 1 bee surveys and remove them from the dataset ####
####################################################################

### Remove the records with one bee per survey
bee.surveys <- bees.per.event[which(bees.per.event$nBeesPerEvent > 1),] # 22,656 surveys

### Associate these bee surveys to the bee records and select those
bees.min2 <- merge(allBees.yr.m.col, bee.surveys, by = c("idCells", "year", "month", "day", "finalLatitude", "finalLongitude"), all.x = TRUE) 
bees.min2 <- bees.min2[which(!is.na(bees.min2$nBeesPerEvent)),] # 365,612 records
length(unique(bees.min2$idCells)) # 6,000 locations

### Keep only needed columns
bees.min2 <- bees.min2[, c("idCells", "year", "month", "day", "finalLatitude", "finalLongitude", "finalName", "individualCount")]  

### Export 'final' bee dataset
write.csv(bees.min2, here("Data", "BioIndicator", "beeDF_min2surveys_mayIncludeNonBirdLocs_20231017.csv"), row.names = FALSE)

#######################################################################################################
### Calculate standardized number of species per grid, keeping only surveys with more than one bee ----
#######################################################################################################

### Calculate the standardized number of species, by first calculating the number of species per survey, then averaging number of species/survey across each grid cell
bees.per.event.min2 <- bees.min2 %>%
  group_by(idCells, year, month, day, finalLatitude, finalLongitude) %>%  # to summarize per grid cell and survey effort (date, location, protocol)
  summarise(nBeesPerEvent = sum(individualCount),
            nSppPerEvent = n_distinct(finalName),
            .groups = "drop") # 22,656 events
bees.per.grid.mean.min2 <- bees.per.event.min2 %>%
  group_by(idCells) %>%
  summarise(nBees = sum(nBeesPerEvent),
            nSurveys = n_distinct(year, month, day, finalLatitude, finalLongitude), 
            nYrs = n_distinct(year), 
            nLocs = n_distinct(finalLatitude, finalLongitude),
            nSurveys = n(),
            nBeesMean = mean(nBeesPerEvent),
            nSppMean = mean(nSppPerEvent),
            .groups = "drop") # 6,000 grid cells

### Combine the standardized captures with the grid info
spp.grid.min2 <- merge(grid.EastUSA, bees.per.grid.mean.min2, by = "idCells", all = TRUE) # 427,959 grid cells
spp.grid.min2[is.na(spp.grid.min2)] <- 0

### Summary of bee dataset
nrow(spp.grid.min2[which(spp.grid.min2$nBees > 0),]) # 6,000 locations
sum(spp.grid.min2$nBees) # 382,318 bees (which in practice could be more because we assumed 1 bee to all records missing a count)
sum(spp.grid.min2$nSurveys) # 22,656 surveys

### Export results
st_write(spp.grid.min2,
				 dsn = here("Data", "BioIndicator", "bees_standardized_meanPerSurvey_no1beeSurv_2007to2021_20231017.shp"),
				 layer = "bees_standardized_meanPerSurvey_no1beeSurv_2007to2021_20231017",
				 driver="ESRI Shapefile")
saveRDS(spp.grid.min2, here("Data", "BioIndicator", "bees_standardized_meanPerSurvey_no1beeSurv_2007to2021_20231017.rds")) 

##########################################
### Assess number of specialist bees  ####
##########################################

### Import, if needed, final bee dataset
bees.min2 <- read.csv(here("Data", "BioIndicator", "beeDF_min2surveys_mayIncludeNonBirdLocs_20231017.csv")) 

### Import the list of specialist bees from Sam Droege's website
sp.bees <- read.csv(here("Data", "BioIndicator", "SpecialistBees_NamesFromGBIF_20230914.csv"))
sp.bees <- sp.bees[, c("verbatimScientificName", "species", "matchType", "confidence", "status", "rank")]

### GBIF did not accept all species names, lets create a new field and assume they are all good
sp.bees$scientificName <- paste(word(sp.bees$verbatimScientificName, 1), word(sp.bees$verbatimScientificName,-1))

### Merge
bees.status <- merge(bees.min2, sp.bees, by.x = "finalName", by.y = "scientificName", all.x = TRUE)
bees.status$specialization <- ifelse(!is.na(bees.status$verbatimScientificName), "specialist", "generalist")

### Summarize number of specialist bee species in dataset
numSpecialists <- bees.status %>%
  group_by(finalName, specialization) %>%
  summarize(nBees = n(), .groups = "drop")

numSpecialistSppBees <- numSpecialists %>%
  group_by(specialization) %>%
  summarise(nSpp = n_distinct(finalName), nBees = sum(nBees), .groups = "drop")
# specialization  nSpp   nBees
# generalist       591   347257
# specialist       151    18355

### Percent of species that are specialist
round(numSpecialistSppBees$nSpp[2] / (numSpecialistSppBees$nSpp[2] + numSpecialistSppBees$nSpp[1]) * 100, 2) # 20.35%
