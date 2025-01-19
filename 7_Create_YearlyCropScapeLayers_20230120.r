####  Program name:     Create_YearlyCropScapeLayers_20230120.r
####  Program location: C:\Files\Cornell\BioD_Pollination\Analysis\Programs\BioIndicator_USGSabund_KohAbund_Status/Extract_BeeBird_data
####  Program goal:     Combine CropScape state layers for each year
####  Created by:       Josee Rousseau 
####  Last modified:    January 20, 2023

### Resources
# Section 9.2: Downloading Cropland Data Layer (CDL) such as CropScape using the CropScapeR package
# - https://tmieno2.github.io/R-as-GIS-for-Economists/CropScapeR.html
# - This package may have more problems with Mac: https://www.rdocumentation.org/packages/CropScapeR/versions/1.1.2
# Tutorial to use cdlTools R package to perform simple analysis of CDL
# - http://meanmean.me/agriculture/r/2016/07/16/The-cdlTools-R-package.html
# General information about CropScape, including accuracy, legend, etc
# https://www.nass.usda.gov/Research_and_Science/Cropland/sarsfaqs2.php#Section1_7.0
# - The 2008 crop year is the first year that the entire Continental United States is covered by the CDL. 
# - CropScape and CDL data use USA Contiguous Albers Equal Area Conic USGS Version with a spheroid of GRS 1980 and datum of NAD83. 

### Libraries
library(here)
library(cdlTools)
library(raster)
library(purrr)

### Identify the year(s) and state(s) to download CropScape data
states <- c("West Virginia", "Virginia", "Kentucky", "Tennessee", "North Carolina", "South Carolina", "Georgia",
            "Alabama", "Maryland", "Mississippi", "Connecticut", "Delaware", "Massachusetts", "New Hampshire",
            "New Jersey", "New York", "Pennsylvania", "Rhode Island", "Vermont", "District of Columbia",
            "North Dakota", "South Dakota", "Nebraska", "Kansas", "Minnesota", "Iowa", "Missouri", "Wisconsin",
            "Illinois","Michigan", "Indiana", "Ohio", "Arkansas", "Oklahoma")
years <- 2021

########################################################################
### Function to download the CropScape data for each state and year ####
########################################################################

CDL.ID.agr <- function(state, year) {

  print(state)

  cdl.st <- getCDL(state, year, location = "/Volumes/BirdsBees/Data/CropScape/2021")

}

allStatesYears <- map2(.x = states, .y = years, CDL.ID.agr)

###################################################################################
### Function to mosaic the different states together to get one layer per year ----
###################################################################################

# While this below works, it takes a very long time to complete. 
# A much more efficient way is to use gdal through the terminal

# Here is how to install and run gdal:
# - https://sandbox.idre.ucla.edu/sandbox/general/how-to-install-and-run-gdal

# Open computer terminal and type the following
# ag-clo-jsr293:~ jsr293$ export PATH=/Library/Frameworks/GDAL.framework/Programs:$PATH

# Test if gdal is installed properly:
# ag-clo-jsr293:~ jsr293$ gdalinfo --version
# You should get something like: GDAL 3.2.2, released 2021/03/05

# The following codes were based on Tom Auer's codes

# First navigate to the file on the C drive where the yearly state CDL layers are saved. Make use of ls and cd functions.
# Here I navigated to /Users/jsr293/Documents/Cornell/BioD_Pollination/Analysis/Data/CropScape
# From the folder, create a .vrt file. I think all CDL files need to be on the computer (not ext. hard drive)
# gdalbuildvrt -hidenodata -srcnodata 0 1_Yearly_USA_CropScapeLayers_CropInfo/CropScape_USA_2021_20230120.vrt StateYear/*.tif

# To translate the .vrt file into .tif
# gdal_translate -co "compress=LZW" -co BIGTIFF=YES --config GDAL_CACHEMAX 2048 1_Yearly_USA_CropScapeLayers_CropInfo/CropScape_USA_2021_20230120.vrt 1_Yearly_USA_CropScapeLayers_CropInfo/CropScape_USA_2021_20230120.tif

# The result is a one-year raster layer in the folder /Users/jsr293/Documents/Cornell/BioD_Pollination/Analysis/Data/CropScape/1_Yearly_USA_CropScapeLayers_CropInfo



