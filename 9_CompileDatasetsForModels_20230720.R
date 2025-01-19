####  Program name:     CompileDatasetsForModels_20230720.r
####  Program location: ./Programs/BioIndicatorA/Modelling
####  Program goal:     Selects bees, birds, lanc cover data with 20% prevalence
####  Created by:       Josee Rousseau 
####  Last modified:    October 17, 2023

### Libraries
library(here)
library(raster)
library(purrr)
library(sf)
library(dplyr)
library(stringr)

'%notin%' <- Negate('%in%')

### Import bee richness, and bird status summaries
birds <- read.csv(here("Results", "BioIndicator", "birdSpp_20prev_meanResidPerCell_2007to2021_min30beesCells_20231017.csv")) # 2,501 locations
bees <- readRDS(here("Data", "BioIndicator", "bees_standardized_meanPerSurvey_no1beeSurv_2007to2021_20231017.rds"))
bees <- bees[which(bees$nBees >= 30 | bees$nSurveys >= 2), ] # 3,076 locations
st_geometry(bees) <- NULL

### Update bird common names to remove extra character and fit other common names
names(birds) <- gsub(x = colnames(birds), pattern = "\\.", replacement = "")

### Import and format percentage prevalence per bird species
prevalences.30 <- read.csv(here("Data", "BioIndicator", "prevalencesBirds_byLocations_byStudyArea_30beeGrids_2007to2021_20231017.csv"))

### Create list of species for each year range, making sure some grassland species are kept
spp.prev.30 <- prevalences.30$species.2007.30[which(prevalences.30$prev >= 20 & prevalences.30$pct.area >= 40 | prevalences.30$species.2007.30 %in% c("Dickcissel", "Grasshopper Sparrow"))] 
# 88 species

### Remove flyover species that are listed in prevalences
flyovers <- c("American Kestrel", "Bald Eagle", "Broad-winged Hawk", "Chimney Swift", "Common Nighthawk", 
              "Cooper's Hawk", "Red-shouldered Hawk", "Red-tailed Hawk", "Turkey Vulture")
spp.prev.30 <- spp.prev.30[spp.prev.30 %notin% flyovers] # 79 spp left for 2007 to 2021

### Edit the common names in prevalence file so that it matches the names used in the bird file
spp.prev.30 <- gsub("\\s", "", spp.prev.30)
spp.prev.30 <- gsub("(-.)","\\U\\1", spp.prev.30, perl = TRUE)
spp.prev.30 <- str_replace_all(spp.prev.30, "'", "")
spp.prev.30 <- str_replace_all(spp.prev.30, "-", "")

###################################################
### Identify grid cells with bee and bird data ####
###################################################

### Locations with bees and birds species, use locations with a minimum of 30 bees or min 2 surveys
cells.bb.30 <- merge(bees, birds, by = "idCells") 
cells.bb.30 <- cells.bb.30[which(cells.bb.30$nBees >= 30 | cells.bb.30$nSurveys > 1), ] 
# 2,585 grid cells for 2007 to 2021
cellsID.bb.30 <- cells.bb.30$idCells

#########################################################################################
### Assess which land covers have prevalence threshold per bee data quantity dataset ####
#########################################################################################

### Import land cover percentage per cell and prevalence within whole eastern half of USA region
allCells.LandCovers <- readRDS(here("Data", "BioIndicator_USGS_SCAN_BirdStatus", "pctLandCovers_CropScape2021_EastHalfUSA_20230120.rds"))  # 530,141 locations (cells)
allCells.LandCovers <- allCells.LandCovers[complete.cases(allCells.LandCovers),]  # 530,131 locations
# write.csv(allCells.LandCovers, here("Data", "BioIndicator", "pctLandCovers_CropScape2021_EastHalfUSA_20230120.csv"), row.names = FALSE)
# land.prev <- read.csv(here("Data", "BioIndicator_USGS_SCAN_BirdStatus", "prevalence_CropScape2021_landCovers_EastHalfUSA_20230214.csv"))

### Extract the land cover data at locations where we have bees and birds
lc.bb.30 <- allCells.LandCovers[which(allCells.LandCovers$idCells %in% cellsID.bb.30),]

### Assess prevalence of each land cover for the cells with bee and bird data
lc.bb.30.col2toEnd <- lc.bb.30[, c(2:(dim(lc.bb.30)[2]))]
lc.bb.30.prev <- round(colSums(lc.bb.30.col2toEnd > 0) / nrow(lc.bb.30.col2toEnd) * 100, 2)
lc.bb.30.prev <- data.frame("LandCover" = names(lc.bb.30.col2toEnd), "LandPrev" = lc.bb.30.prev)

### Identify land covers with 20% prevalence at locations with each minimum of bees
lc.30.20prev <- lc.bb.30.prev$LandCover[which(lc.bb.30.prev$LandPrev >= 20)] 
# 21 land covers for 2007 to 2021

#############################################################################################################################
### Create datasets with land covers of 20% prevalence, bird prevalence of 20%, and bee data of diff. minimum quantities ####
#############################################################################################################################

### Datasets for land covers and birds of 20% prevalence, for a minimum of 10 bees per grid cells
bb.lc.30 <- cells.bb.30[, c("idCells", "nSppMean", spp.prev.30)]
nCols <- dim(bb.lc.30)[2]
bb.lc.30 <- merge(bb.lc.30, lc.bb.30, by = "idCells", all = TRUE)
bb.lc.30 <- cbind(bb.lc.30[,c(1:nCols)], bb.lc.30[, c(lc.30.20prev)])
# 2,585 grid cells and 102 variables if min bees 30 or min surveys 2 (for bird residuals)

### Export dataframe
write.csv(bb.lc.30, here("Data", "BioIndicator", "BeesMin30_Birds20pct_LandCovers20pct_2007to2021_no1beeSurv_Resid30BeesCells_20231017.csv"), row.names = FALSE)
