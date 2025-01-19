####  Program name:     StandardizeBirdObs_EastHalfUSA_20230720.r
####  Program location: ./Programs/BioIndicator/Standardize
####  Program goal:     Standardize bird datasets
####  Created by:       Josee Rousseau 
####  Last modified:    October 17, 2023

### Libraries
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

'%notin%' <- Negate('%in%')

################################
#### Import eBird residuals ----
################################

### List all the eBird observations files (one per bird species) and associated standardized residual values
allResidFiles <- list.files(path = "/Volumes/BirdsBees/Results/BioIndicator/Standardize_2007to2021_min30BeesCells/", pattern = "_gamModel_TravStaCombined_20231017.rds", recursive = TRUE) # 79 files (one per species)

### Import maximum duration at which abundance peaks for each species
allSppModels <- readRDS(here("Results", "BioIndicator", "allSppModelsMaxDuration_all30BeesCells_EasternUSA_2007to2021_20231017.rds"))

### Extract the second element of each list. This is the list of surveys for which the length is shorter than 300 minutes but longer than the max duration at whch abundance peaks
surveys.out <- lapply(allSppModels, function(l) l[[2]])
surveys.out <- unlist(surveys.out)
surveys.out <- unique(surveys.out)


####################################################################
#### Extract each species and standardize at a grid cells level ----
####################################################################

meanResid.fct <- function(sppFileName) {
# sppFileName <- "CountResidual_EastUSA_perSpeciesAcadianFlycatcher_gamModel_TravStaCombined_20231017.rds"

  ### Import species's file
  df <- readRDS(paste0("/Volumes/BirdsBees/Results/BioIndicator/Standardize_2007to2021_min30BeesCells/", sppFileName))
  df <- df[[1]]
  
  ### Remove checklists for which the duration is higher than the max duration (see residuals) of at least one species 
  ### (e.g. if checklist has American GoldFinch, we should throw it out if the checklist is longer than x minutes)
  df <- df[which(df$sampling_event_identifier %notin% surveys.out) ,] 
  
  ### Extract species name
  # species <- str_sub(sppFileName, 33, -53) # Use for the cells with birds
  # species <- str_sub(sppFileName, 33, -54) # Use for the 10 bee cells
  # species <- str_sub(sppFileName, 33, -52) # Use for 1 bee cells
  species <- str_sub(sppFileName, 33, -39) # Use for 30 bee cells (39 right?)
  
  ### Calculate mean bird species residual per grid cell
  meanResid <- df %>%
    mutate(idCells = as.character(idCells)) %>%
    group_by(idCells) %>%
    summarise(mBirdResid = mean(residuals, na.rm = TRUE), .groups = "drop") 
  colnames(meanResid) <- c("idCells", species)
  
  return(meanResid)
  
}

results <- map(allResidFiles, meanResid.fct) %>%
  reduce(full_join, by = "idCells")

### Save file using years 2007 to 2021 and 75 species (40% study area coverage and 20% location prevalence, removing flyovers, adding a few grassland species)
write.csv(results, here("Results", "BioIndicator", "birdSpp_20prev_meanResidPerCell_2007to2021_min30beesCells_20231017.csv"), row.names = FALSE)
# 2,585 grid cells with at least 30 bee records or 2 surveys and 79 species
