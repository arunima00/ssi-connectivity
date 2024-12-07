# Load packages
library(terra)
library(tidyverse)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

# Get file names for all output files
files <- list.files(path = paste0(proj_path,"Omniscape/Output"),
                    pattern = ".tif$",
                    full.names = TRUE,
                    recursive = TRUE)

# Exclude unwanted files
files <- files[! grepl("flow_potential",files)]

# Loop to process each output map
for (i in files) {
  
  # Read raster
  r <- rast(i)
  
  # Convert raster to dataframe
  r_df <- as.data.frame(r,xy = TRUE,na.rm = FALSE) 
  
  # Bin raster values into 100 bins
  binned <- ntile(r_df[[3]],100)
  
  # Rasterize dataframe with binned values
  r_binned <- rast(cbind(r_df[,c("x","y")],binned),crs = "epsg:4326")
  
  # Write binned raster to TIF file
  writeRaster(r_binned,
              filename = sub(".tif","_binned.tif",i),
              overwrite = TRUE)
}

# Loop for each species
for (spec in c("SHAL","SHMA","MOFA","MOCA","ANNI")) {
  
  # Set species-specific region
  if (spec %in% c("SHAL","MOFA")) {
    region <- "pahw1400"
  }
  
  if (spec %in% c("SHMA","MOCA")) {
    region <- "nil1400"
  }
  
  if (spec == "ANNI") {
    region <- "swg1400"
  }
  
  # Read past and present SDM prediction output rasters
  past <- rast(paste0(proj_path,"SDM/Output/",spec,"/Past PA map/RF_",spec,"_",region,".tif"))
  present <- rast(paste0(proj_path,"SDM/Output/",spec,"/PA map/RF_",spec,"_",region,".tif"))
  
  # Create empty raster
  r <- rast(past)
  
  # Fill values in empty raster from past and present rasters
  r[past[] == 1 & present[] == 1] <- 1
  r[past[] == 1 & present[] == 0] <- 2
  r[past[] == 0 & present[] == 1] <- 3
  r[past[] == 0 & present[] == 0] <- 4
  r[is.na(past[]) | is.na(present[])] <- NA
  
  # Name categories in new combined raster
  levels(r) <- data.frame(id = 1:4,class = c("Unchanged","Only past","Only present","Unsuitable"))
  
  # Reproject to WGS84 EPSG:4326 CRS and write to TIF file
  r <- project(r,
               y = "epsg:4326",
               filename = paste0(proj_path,"SDM/Output/",spec,"/past_present.tif"),
               overwrite = TRUE)
}

## Visualisation and plotting done in QGIS
