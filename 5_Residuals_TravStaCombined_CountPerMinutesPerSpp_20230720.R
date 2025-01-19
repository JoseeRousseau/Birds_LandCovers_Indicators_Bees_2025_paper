####  Program name:     Residuals_TravStaCombined_CountPerMinutesPerSpp_20230720.r
####  Program location: ./Programs/BioIndicator/Standardize
####  Program goal:     Extract the eBird records that are in the final selection of grid cells
####                    Assess gam / scam curve per species, to later use the residuals as a standardization
####  Created by:       Josee Rousseau 
####  Last modified:    October 17, 2023

### Libraries
library(here)
library(dplyr)
library(purrr)
library(mgcv)
citation("mgcv")
library(scam)
library(stringr)
library(tictoc)

'%notin%' <- Negate('%in%')

### Import bird prevalence files
prevalences.30 <- read.csv(here("Data", "BioIndicator", "prevalencesBirds_byLocations_byStudyArea_30beeGrids_2007to2021_20231017.csv"))

### Create list of species for prevalence minimums, selecting only species with at least 20% prevalence in surveyed locations and 40% prevalence in study area
spp.2007.30.prev <- prevalences.30$species.2007.30[which(prevalences.30$prev >= 20 & prevalences.30$pct.area >= 40 | prevalences.30$species.2007.30 %in% c("Dickcissel", "Grasshopper Sparrow"))] 
# 88 species

### Remove flyover species that are listed in prevalences
flyovers <- c("American Kestrel", "Bald Eagle", "Broad-winged Hawk", "Chimney Swift", "Common Nighthawk", 
              "Cooper's Hawk", "Red-shouldered Hawk", "Red-tailed Hawk", "Turkey Vulture")
spp.2007.30.prev <- spp.2007.30.prev[spp.2007.30.prev %notin% flyovers] # 79 species

###############################################
### Function to run gam models per species ----
###############################################

countPerEffort <- function(species) {
# species <- "American Goldfinch"
# species <- "Acadian Flycatcher"
  
  print(species)
  
  ### Import file
  sppDF <- read.csv(paste0("/Volumes/BirdsBees/Data/BioIndicator/eBirdObsSampl_2007to2021_min30beesCells/eBirdObsSampl_10prev_zf_", species, "_atGridWith30Bees2Surv_EastUSA_2007to2021_20231017.csv"))

  ### Create sub dataframes
  # sppDF <- birds[which(birds$scientific_name == species & !is.na(birds$observation_count) & !is.na(birds$duration_minutes)), ]
  # sppDF <- sppDF[which(sppDF$protocol_code %in% c("P21", "P22")), ]
  
  sppDF$protocol_code <- factor(sppDF$protocol_code)
  
  ### Run gam model for stationary counts and identify the duration at which we observe the highest abundance
  if(sum(sppDF$observation_count > 0) & nrow(sppDF) > 30) {
    # gamDur <- gam(observation_count ~ s(duration_minutes, k = 3), data = sppDF)
    # gamDur <- gam(observation_count ~ s(duration_minutes, k = 3) + s(protocol_type, bs = "re"), data = sppDF)
    # gamDur <- gam(observation_count ~ s(duration_minutes, k = 3) + s(protocol_type), data = sppDF)
    gamDur <- gam(observation_count ~ s(duration_minutes, k = 3) + protocol_code, data = sppDF)    
    # summary(gamDur)
    # plot(gamDur)
    maxY <- max(gamDur$fitted.values)
    x.atMaxY <- approx(x = gamDur$fitted.values, y = sppDF$duration_minutes, xout = maxY)$y
    # yval <- predict(gamDur, data.frame(duration_minutes = x.atMaxY))
    # plot(gamDur, main = paste0(species, " - Stationary & Traveling"), xlab = "Duration in minutes", ylab = "Observation count")
  } else {
    x.atMaxY <- NA
  }

  ### List of surveys for which the duration is longer than the max duration dictated by at least one of its species
  if(x.atMaxY == 300) {
    sampling.out = NA
  } else {
    sampling.out <- sppDF$sampling_event_identifier[which(sppDF$duration_minutes > x.atMaxY) ]
  }
  sampling.out <- sppDF$sampling_event_identifier[which(sppDF$duration_minutes > x.atMaxY) ]
  
  df <- data.frame(species = species, surveyMaxDuration = max(sppDF$duration_minutes), durAtMaxCount = x.atMaxY)
  
  results <- list(df, sampling.out)
   
  return(results)
  
}

allSppModels <- map(spp.2007.30.prev, countPerEffort)
saveRDS(allSppModels, here("Results", "BioIndicator", "allSppModelsMaxDuration_all30BeesCells_EasternUSA_2007to2021_20231017.rds"))

##############################################################################################################################
### For each species and protocol, truncate the survey duration to the duration at which had maximum count or 300 minutes ----
### whatever is shorter      
### Then run an additive gam model per protocol and save file
##############################################################################################################################

### Import list of species, if needed
# allSppModels_old <- readRDS(here("Results", "BioIndicator", "Archives", "allSppModelsMaxDuration_EasternUSA_2007to2021_20230720.rds"))
allSppModels <- readRDS(here("Results", "BioIndicator", "allSppModelsMaxDuration_all30BeesCells_EasternUSA_2007to2021_20231017.rds"))

### Extract the first element and second element of each list 
allSppModels.maxDur <- lapply(allSppModels, function(l) l[[1]])
allSppModels.maxDur <- bind_rows(allSppModels.maxDur) # Shortest duration is 184 minutes (Black-capped Chickadee max dur for peak abundance)

shorterSurvey.scam.fct <- function(species, durAtMaxCount) {
# species <- "Acadian Flycatcher"; durAtMaxCount <- 300
# species <- "Rose-breasted Grosbeak"; durAtMaxCount <- 300

  print(species)
  
  ### Determine the survey duration with the highest observation count. Select either max 300 minutes or a shorter survey length when appropriate.
  shortestDuration <- min(durAtMaxCount, 300)

  df <- data.frame(species = species, maxDuration = shortestDuration)

  ### Import file
  sppDF <- read.csv(paste0("/Volumes/BirdsBees/Data/BioIndicator/eBirdObsSampl_2007to2021_min30beesCells/eBirdObsSampl_10prev_zf_", species, "_atGridWith30Bees2Surv_EastUSA_2007to2021_20231017.csv"))

  ### Create sub dataframes
  sppDF <- sppDF[which(sppDF$duration_minutes <= shortestDuration), ]
  sppDF$protocol_code <- factor(sppDF$protocol_code)
  sppDF$observer_id <- as.factor(sppDF$observer_id)
  sppDF$time_observations_started2 <- as.numeric(chron::times(sppDF$time_observations_started))
  sppDF <- sppDF %>%
    mutate(effort_distance_km2 = case_when(
    protocol_code == "P21" ~ 0,
    protocol_code == "P22" ~ effort_distance_km))
  sppDF <- sppDF[!is.na(sppDF$effort_distance_km2),] 
  sppDF <- sppDF[!is.na(sppDF$time_observations_started2), ]

  ### Format species name for filename
  species <- str_to_title(species) 
  species <- str_replace_all(species, fixed(" "), "")
  
  ### Run various gam models and plot results
  if(sum(sppDF$observation_count) > 0 & nrow(sppDF) > 30) {
    # gamDur <- scam(observation_count ~ s(duration_minutes, bs = "mpi") + protocol_code, data = sppDF, family = poisson(link="log"))
    gamDur <- bam(observation_count ~ s(duration_minutes) + 
                    s(effort_distance_km2) +
                    s(time_observations_started2) + 
                    protocol_code, # +
                    # s(observer_id, bs= "re"),
                  data = sppDF, 
                  family = poisson(link="log"),
                  discrete = TRUE)
    sppDF$residuals <- residuals(gamDur)
    results <- list(sppDF, gamDur)

    saveRDS(results, paste0("/Volumes/BirdsBees/Results/BioIndicator/Standardize_2007to2021_min30BeesCells/CountResidual_EastUSA_perSpecies", species, "_gamModel_TravStaCombined_20231017.rds"))
  }
  
  # remove(df, results, gamDur, sppDF, durAtMaxCount, shortestDuration, species)
  
  return(df)
  
}

results <- pmap_dfr(list(allSppModels.maxDur$species, allSppModels.maxDur$durAtMaxCount), shorterSurvey.scam.fct)

