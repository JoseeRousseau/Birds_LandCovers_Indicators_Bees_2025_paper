####  Program name:     Calculate_pctLandCovers_perGridCell_20231017.r
####  Program location: ./Programs/BioIndicator_USGSabund_KohAbund_Status/ReCreate_KohModel
####  Program goal:     Calculate the percentage of 45 land cover categories in each bird status cell
####  Created by:       Josee Rousseau 
####  Last modified:    October 17, 2023

### Much information about cropscape and how to extract and process the data is in the program
### C:\Files\Cornell\BioD_Pollination\Analysis\Programs\Clean_Explore_NewPollinatorData\Combine_CropScapeLayers_20210713.r

### Libraries
library(here)
library(raster)
library(spData)
library(sf)
library(purrr)
library(ggplot2)
library(mapview)

### Import the CropScape raster file: one per year, covering the whole USA
cropScape <- raster(here("Data", "CropScape", "1_Yearly_USA_CropScapeLayers_CropInfo", "CropScape_USA_2008_20230814.tif"))
crs.cropScape <- crs(cropScape)

######## FOR 2007 UPDATE REGION TO LATEST VERSION: SEE BeeSummaries_perStatusCell_20230718.r
# grid.EastUSA <- readRDS(here("Data", "BioIndicator", "birdStatusGrid_4326_EastUSA_3kmGridAndBuffer_20231017.rds"))

# ### Import the bird status grid
# grid <- st_read("/Users/jsr293/Documents/GIS_Data/eBird_StatusTrend_Grids/statusGrid_USA.shp")
# grid.crs <- crs(grid)
# 
# ### Extract USA region 
# region <- st_transform(us_states, crs = 4326)
# 
# ### Delineate Eastern USA region
# EastUSA <- region[region$NAME %in% c("West Virginia", "Virginia", "Kentucky", "Tennessee", "North Carolina", "South Carolina", "Georgia",
#                                      "Alabama", "Maryland", "Mississippi", "Connecticut", "Delaware", "Massachusetts", "New Hampshire",
#                                      "New Jersey", "New York", "Pennsylvania", "Rhode Island", "Vermont", "District of Columbia",
#                                      "North Dakota", "South Dakota", "Nebraska", "Kansas", "Minnesota", "Iowa", "Missouri", "Wisconsin", 
#                                      "Illinois","Michigan", "Indiana", "Ohio", "Arkansas", "Oklahoma"), ]
#   
# ### Dissolve the state boundaries of the polygon
# EastUSA <- EastUSA %>%
#   st_union() %>%
#   st_sf() 
# 
# ### Project the polygon to projection used by bird status
# EastUSA <- EastUSA %>%
#   st_transform(crs = grid.crs)
# 
# ### Crop the status grid to our region of interest
# bbox_EastUSA <- st_bbox(EastUSA)
# grid.EastUSA <- st_crop(grid, bbox_EastUSA)

### Import grid covering eastern half of USA
grid.EastUSA <- readRDS(here("Data", "BioIndicator", "birdStatusGrid_4326_EastUSA_3kmGridAndBuffer_20231017.rds"))

### Project to crs of cropscape
grid.EastUSA <- st_transform(grid.EastUSA, crs = crs.cropScape)

### Identify the cells for which we need percentage of land cover (all cells in eastern half of USA)
listCells <- grid.EastUSA$idCells

### Clean up
# remove(bbox_EastUSA, EastUSA, region, grid, grid.crs, crs.cropScape)

##################################################################################################################
### Functions that identify different land cover pixels and calculate the percent per land cover, per hexagon ----
##################################################################################################################

coversPerCell.fct <- function(cellID) {
# cellID <- 1; yearlyRaster <- cropScape; statusGrid <- grid.EastUSA
# cellID <- 333333; yearlyRaster <- cropScape; statusGrid <- grid.EastUSA
  
  print(cellID)
  # print(str(yearlyRaster))
  # print(str(hexaGrid))
  # print("Done with one loop")
  
  oneCell <- grid.EastUSA[which(grid.EastUSA$idCells == cellID),]

  if(tryCatch(!is.null(crop(cropScape, extent(oneCell))), error=function(e) return(FALSE)) == TRUE ) {
    
    ### Clip the cropScape layer based on the hexagons
    cover.cell <- crop(cropScape, extent(oneCell))
    cover.cell <- mask(cover.cell, oneCell) # contains NAs outside the hexagon
    # raster::freq(cover.cell)
    # plot(cover.cell)
    # print("worked")
  
    ### Calculate the number of pixel in hexagon
    numPixels <- length(cover.cell[!is.na(cover.cell)])

    ### Assign a crop name to each cropscape value to recreate the land categories retained by Koh et al. 2016
    matrix_46gr <- c(0, 1, "corn", 
                  1, 2, "cotton",
                  2, 3, "grain",
                  3, 4, "corn",
                  4, 5, "bean",
                  5, 6, "flowers",
                  9, 10, "bean",
                  10, 11, "tobacco",
                  11, 12, "corn",
                  12, 13, "corn",
                  13, 14, "herbs",
                  20, 25, "grain",
                  25, 26, "doubleCrop",
                  26, 30, "grain",
                  30, 32, "oilseed",
                  32, 33, "flowers",
                  33, 35, "oilseed",
                  35, 36, "alfalfa",
                  36, 37, "grassPasture",
                  37, 38, "oilseed",
                  38, 39, "buckwheat",
                  40, 41, "rootVegetables",
                  41, 42, "bean",
                  42, 43, "solanums",
                  43, 44, "otherCrops",
                  44, 45, "grain",
                  45, 46, "solanums",
                  46, 47, "vegsFruits",
                  47, 48, "watermelons",
                  48, 49, "rootVegetables",
                  49, 50, "melons",
                  50, 51, "vegetables",
                  51, 52, "grain",
                  52, 53, "vegetables",
                  53, 54, "solanums",
                  54, 55, "berries",
                  55, 56, "grain",
                  56, 57, "herbs",
                  57, 58, "wildflowers",
                  58, 60, "grass",
                  60, 61, "idleCropland",
                  61, 62, "grass",
                  65, 68, "orchard",
                  68, 69, "grapes",
                  69, 70, "christmasTrees",
                  70, 71, "treeCrops",
                  71, 72, "citrus",
                  73, 74, "nuts",
                  74, 75, "orchard",
                  75, 76, "nuts",
                  76, 77, "orchard",
                  91, 92, "openWater", # aquaculture not listed in Koh
                  110, 111, "openWater",
                  111, 112, "nonListed", # Ice/Snow not listed in Koh
                  120, 121, "devOpenSpace",
                  121, 122, "devLowUrban",
                  122, 123, "devMedUrban",
                  123, 124, "devHighUrban",
                  130, 131, "barren",
                  140, 141, "deciForest",
                  141, 142, "coniForest",
                  142, 143, "mixedForest",
                  151, 152, "shrubland",
                  175, 176, "grassPasture",
                  189, 190, "woodyWetland",
                  194, 195, "herbWetland",
                  203, 204, "nuts",
                  204, 205, "grain",
                  205, 206, "rootVegetables",
                  206, 207, "asparagus",
                  207, 208, "rootVegetables",
                  208, 209, "melons",
                  209, 210, "orchard",
                  210, 211, "olives",
                  211, 212, "citrus",
                  212, 213, "melons",
                  213, 214, "vegetables",
                  215, 216, "solanums",
                  216, 218, "orchard",
                  218, 219, "vegetables",
                  219, 220, "orchard",
                  220, 221, "strawberries",
                  221, 222, "cucurbits",
                  222, 223, "orchard",
                  223, 224, "bean",
                  224, 226, "doubleCrop",
                  226, 227, "vegetables",
                  228, 229, "cucurbits",
                  229, 241, "doubleCrop",
                  241, 242, "berries",
                  242, 245, "vegetables",
                  245, 247, "rootVegetables",
                  247, 248, "solanums",
                  248, 249, "cucurbits", #squashes
                  249, 250, "berries",
                  253, 254, "doubleCrop")  
    matrix_46gr <- matrix(matrix_46gr, ncol = 3, byrow = TRUE)
    matrix_46gr_df <- as.data.frame(matrix_46gr)
    # matrix_46gr_df$diff <- matrix_46gr_df$V2 - matrix_46gr_df$V1
    matrix_46gr_df$V4 <- as.numeric(factor(matrix_46gr_df$V3, levels = unique(matrix_46gr_df$V3))) + 13
    matrix_46gr_df2 <- matrix_46gr_df[, c("V1", "V2", "V4")]
    matrix_46gr_df2$V1 <- as.numeric(as.character(matrix_46gr_df2$V1))
    matrix_46gr_df2$V2 <- as.numeric(as.character(matrix_46gr_df2$V2))
    matrix_46gr <- as.matrix(matrix_46gr_df2)
    cover.cell.46gr <- reclassify(cover.cell, matrix_46gr)
    # raster::freq(cover.cell.46gr)
  
    ### Calculate the number of pixel per Koh land cover in each grid cell.
    numCorn <- length(cover.cell.46gr[cover.cell.46gr == 14])           # corn
    numCotton <- length(cover.cell.46gr[cover.cell.46gr == 15])         # cotton
    numGrain <- length(cover.cell.46gr[cover.cell.46gr == 16])          # grain
    numBean <- length(cover.cell.46gr[cover.cell.46gr == 17])           # bean
    numFlowers <- length(cover.cell.46gr[cover.cell.46gr == 18])        # flowers
    numTobacco <- length(cover.cell.46gr[cover.cell.46gr == 19])        # tobacco
    numHerbs <- length(cover.cell.46gr[cover.cell.46gr == 20])          # herbs
    numDoubleCrop <- length(cover.cell.46gr[cover.cell.46gr == 21])     # doubleCrop
    numOilseed <- length(cover.cell.46gr[cover.cell.46gr == 22])        # oilseed
    numAlfalfa <- length(cover.cell.46gr[cover.cell.46gr == 23])        # alfalfa
    numGrassPasture <- length(cover.cell.46gr[cover.cell.46gr == 24])   # grassPasture
    numBuckwheat <- length(cover.cell.46gr[cover.cell.46gr == 25])      # buckwheat
    numRootVegetables <- length(cover.cell.46gr[cover.cell.46gr == 26]) # rootVegetables
    numSolanums <- length(cover.cell.46gr[cover.cell.46gr == 27])       # solanums
    numOtherCrops <- length(cover.cell.46gr[cover.cell.46gr == 28])     # otherCrops
    numVegsFruits <- length(cover.cell.46gr[cover.cell.46gr == 29])     # vegsFruits
    numWatermelons <- length(cover.cell.46gr[cover.cell.46gr == 30])    # watermelons
    numMelons <- length(cover.cell.46gr[cover.cell.46gr == 31])         # melons
    numVegetables <- length(cover.cell.46gr[cover.cell.46gr == 32])     # vegetables
    numBerries <- length(cover.cell.46gr[cover.cell.46gr == 33])        # berries
    numWildflowers <- length(cover.cell.46gr[cover.cell.46gr == 34])    # wildflowers
    numGrass <- length(cover.cell.46gr[cover.cell.46gr == 35])          # grass
    numIdleCropland <- length(cover.cell.46gr[cover.cell.46gr == 36])   # idleCropland
    numOrchard <- length(cover.cell.46gr[cover.cell.46gr == 37])        # orchard
    numGrapes <- length(cover.cell.46gr[cover.cell.46gr == 38])         # grapes
    numChristmasTrees <- length(cover.cell.46gr[cover.cell.46gr == 39]) # christmasTrees
    numTreeCrops <- length(cover.cell.46gr[cover.cell.46gr == 40])      # treeCrops
    numCitrus <- length(cover.cell.46gr[cover.cell.46gr == 41])         # citrus
    numNuts <- length(cover.cell.46gr[cover.cell.46gr == 42])           # nuts
    numOpenWater <- length(cover.cell.46gr[cover.cell.46gr == 43])      # openWater
    numNonListed <- length(cover.cell.46gr[cover.cell.46gr == 44])      # nonListed
    numDevOpenSpace <- length(cover.cell.46gr[cover.cell.46gr == 45])   # devOpenSpace
    numDevLowUrban <- length(cover.cell.46gr[cover.cell.46gr == 46])    # devLowUrban
    numDevMedUrban <- length(cover.cell.46gr[cover.cell.46gr == 47])    # devMedUrban
    numDevHighUrban <- length(cover.cell.46gr[cover.cell.46gr == 48])   # devHighUrban
    numBarren <- length(cover.cell.46gr[cover.cell.46gr == 49])         # barren
    numDeciForest <- length(cover.cell.46gr[cover.cell.46gr == 50])     # deciForest
    numConiForest <- length(cover.cell.46gr[cover.cell.46gr == 51])     # coniForest
    numMixedForest <- length(cover.cell.46gr[cover.cell.46gr == 52])    # mixedForest
    numShrubland <- length(cover.cell.46gr[cover.cell.46gr == 53])      # shrubland
    numWoodyWetland <- length(cover.cell.46gr[cover.cell.46gr == 54])   # woodyWetland
    numHerbWetland <- length(cover.cell.46gr[cover.cell.46gr == 55])    # herbWetland
    numAsparagus <- length(cover.cell.46gr[cover.cell.46gr == 56])      # asparagus
    numOlives <- length(cover.cell.46gr[cover.cell.46gr == 57])         # olives
    numStrawberries <- length(cover.cell.46gr[cover.cell.46gr == 58])   # strawberries
    numCucurbits <- length(cover.cell.46gr[cover.cell.46gr == 59])      # cucurbits
  
    # numCorn + numCotton + numGrain + numBean + numFlowers + numTobacco + numHerbs + numDoubleCrop + 
    # numOilseed + numAlfalfa + numGrassPasture + numBuckwheat + numRootVegetables + numSolanums + 
    # numOtherCrops + numVegsFruits + numWatermelons + numMelons + numVegetables + numBerries + 
    # numWildflowers + numGrass + numIdleCropland + numOrchard + numGrapes + numChristmasTrees + 
    # numTreeCrops + numCitrus + numNuts + numOpenWater + numNonListed + numDevOpenSpace + 
    # numDevLowUrban + numDevMedUrban + numDevHighUrban + numBarren + numDeciForest + 
    # numConiForest + numMixedForest + numShrubland + numWoodyWetland + numHerbWetland + 
    # numAsparagus + numOlives + numStrawberries + numCucurbits 
    # test <- as.data.frame(table(matrix_46gr_df$V3, matrix_46gr_df$V4)) 
    # test <- test[which(test$Freq > 0),]
  
    ### Calculate the percentage per cover type
    pctCorn <- round(numCorn / numPixels * 100, 2)
    pctCotton <- round(numCotton / numPixels * 100, 2)
    pctGrain <- round(numGrain / numPixels * 100, 2)
    pctBean <- round(numBean / numPixels * 100, 2)
    pctFlowers <- round(numFlowers / numPixels * 100, 2)
    pctTobacco <- round(numTobacco / numPixels * 100, 2)
    pctHerbs <- round(numHerbs / numPixels * 100, 2)
    pctDoubleCrop <- round(numDoubleCrop / numPixels * 100, 2)
    pctOilseed <- round(numOilseed / numPixels * 100, 2)
    pctAlfalfa <- round(numAlfalfa / numPixels * 100, 2)
    pctGrassPasture <- round(numGrassPasture / numPixels * 100, 2)
    pctBuckwheat <- round(numBuckwheat / numPixels * 100, 2)
    pctRootVegetables <- round(numRootVegetables / numPixels * 100, 2)
    pctSolanums <- round(numSolanums / numPixels * 100, 2)
    pctOtherCrops <- round(numOtherCrops / numPixels * 100, 2)
    pctVegsFruits <- round(numVegsFruits / numPixels * 100, 2)
    pctWatermelons <- round(numWatermelons / numPixels * 100, 2)
    pctMelons <- round(numMelons / numPixels * 100, 2)
    pctVegetables <- round(numVegetables / numPixels * 100, 2)
    pctBerries <- round(numBerries / numPixels * 100, 2)
    pctWildflowers <- round(numWildflowers / numPixels * 100, 2)
    pctGrass <- round(numGrass / numPixels * 100, 2)
    pctIdleCropland <- round(numIdleCropland / numPixels * 100, 2)
    pctOrchard <- round(numOrchard / numPixels * 100, 2)
    pctGrapes <- round(numGrapes / numPixels * 100, 2)
    pctChristmasTrees <- round(numChristmasTrees / numPixels * 100, 2)
    pctTreeCrops <- round(numTreeCrops / numPixels * 100, 2)
    pctCitrus <- round(numCitrus / numPixels * 100, 2)
    pctNuts <- round(numNuts / numPixels * 100, 2)
    pctOpenWater <- round(numOpenWater / numPixels * 100, 2)
    pctNonListed <- round(numNonListed / numPixels * 100, 2)
    pctDevOpenSpace <- round(numDevOpenSpace / numPixels * 100, 2)
    pctDevLowUrban <- round(numDevLowUrban / numPixels * 100, 2)
    pctDevMedUrban <- round(numDevMedUrban / numPixels * 100, 2)
    pctDevHighUrban <- round(numDevHighUrban / numPixels * 100, 2)
    pctBarren <- round(numBarren / numPixels * 100, 2)
    pctDeciForest <- round(numDeciForest / numPixels * 100, 2)
    pctConiForest <- round(numConiForest / numPixels * 100, 2)
    pctMixedForest <- round(numMixedForest / numPixels * 100, 2)
    pctShrubland <- round(numShrubland / numPixels * 100, 2)
    pctWoodyWetland <- round(numWoodyWetland / numPixels * 100, 2)
    pctHerbWetland <- round(numHerbWetland / numPixels * 100, 2)
    pctAsparagus <- round(numAsparagus / numPixels * 100, 2)
    pctOlives <- round(numOlives / numPixels * 100, 2)
    pctStrawberries <- round(numStrawberries / numPixels * 100, 2)
    pctCucurbits <- round(numCucurbits / numPixels * 100, 2)

    ### Save a few numbers in a dataframe
    # numBeeSpp <- hexaGrid$nSppTTL[which(hexaGrid$hID == hID)]
    df <- data.frame(idCells = cellID, 
                   # numPixels = numPixels, 
                   pctAlfalfa = pctAlfalfa,
                   pctAsparagus = pctAsparagus,
                   pctBarren = pctBarren,
                   pctBean = pctBean,
                   pctBerries = pctBerries, 
                   pctBuckwheat = pctBuckwheat,  
                   pctChristmasTrees = pctChristmasTrees, 
                   pctCitrus = pctCitrus, 
                   pctConiForest = pctConiForest,
                   pctCorn = pctCorn,
                   pctCotton = pctCotton,
                   pctCucurbits = pctCucurbits,
                   pctDeciForest = pctDeciForest,
                   pctDevHighUrban = pctDevHighUrban,
                   pctDevLowUrban = pctDevLowUrban,
                   pctDevMedUrban = pctDevMedUrban,
                   pctDevOpenSpace = pctDevOpenSpace,
                   pctDoubleCrop = pctDoubleCrop,
                   pctFlowers = pctFlowers,
                   pctGrain = pctGrain,
                   pctGrapes = pctGrapes,
                   pctGrass = pctGrass,
                   pctGrassPasture = pctGrassPasture,
                   pctHerbs = pctHerbs,
                   pctHerbWetland = pctHerbWetland,
                   pctIdleCropland = pctIdleCropland,
                   pctMelons = pctMelons,
                   pctMixedForest = pctMixedForest,
                   pctNonListed = pctNonListed,
                   pctNuts = pctNuts,
                   pctOilseed = pctOilseed,
                   pctOlives = pctOlives,
                   pctOpenWater = pctOpenWater,
                   pctOrchard = pctOrchard,
                   pctOtherCrops = pctOtherCrops,
                   pctRootVegetables = pctRootVegetables,
                   pctShrubland = pctShrubland,
                   pctSolanums = pctSolanums,
                   pctStrawberries = pctStrawberries,
                   pctTobacco = pctTobacco,
                   pctTreeCrops = pctTreeCrops,
                   pctVegetables = pctVegetables,
                   pctVegsFruits = pctVegsFruits,
                   pctWatermelons = pctWatermelons,
                   pctWildflowers = pctWildflowers,
                   pctWoodyWetland = pctWoodyWetland)  
  
    return(df)
  }
}

allCells.LandCovers <- map_dfr(listCells, coversPerCell.fct)

saveRDS(allCells.LandCovers, here("Data", "BioIndicator_USGS_SCAN_BirdStatus", "pctLandCovers_CropScape2008_EastHalfUSA_20231017.rds"))
allCells.LandCovers <- readRDS(here("Data", "BioIndicator_USGS_SCAN_BirdStatus", "pctLandCovers_CropScape2021_EastHalfUSA_20230120.rds"))


#############################################
### Assess prevalence of the land covers ####
#############################################

### Consider only the cells that have land cover data
allCells.LandCovers <- allCells.LandCovers[complete.cases(allCells.LandCovers),]

### Calculate prevalence for the Eastern region, i.e., number of cell with abundance > 0 divided by total number of cells
# allCells.LandCovers.col1 <- allCells.LandCovers[, c(1)]
allCells.LandCovers.col2toEnd <- allCells.LandCovers[ ,c(2:(dim(allCells.LandCovers)[2]))]
land.prev <- round(colSums(allCells.LandCovers.col2toEnd > 0) / nrow(allCells.LandCovers.col2toEnd) * 100, 2)
land.prev <- data.frame("LandCover" = names(allCells.LandCovers.col2toEnd), "LandPrev" = land.prev)

### Export land cover prevalence
write.csv(land.prev, here("Data", "BioIndicator_USGS_SCAN_BirdStatus", "prevalence_CropScape2021_landCovers_EastHalfUSA_20230214.csv"), row.names = FALSE)
