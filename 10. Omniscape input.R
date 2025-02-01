# Load packages
library(tidyverse)
library(terra)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

# Get list of raster output file names
files <- list.files(path = paste0(proj_path,"SDM/Output"),
                    pattern = ".tif$",
                    full.names = TRUE,
                    recursive = TRUE)

# Create loop to process input data for all Omniscape models
for (y in c("1995","2017")) {
  
  # Filter output files for past or present scenarios
  if (y == "1995") {
    files_fil <- files[grepl(".*/Past prediction/.*", files)]
  }
  
  if (y == "2017") {
    files_fil <- files[grepl(".*/Prediction/.*", files)]
  }
  
  # Transform all SDM predictions to resistance layers and save as source layers
  for (i in files_fil) {
    
    # Read raster
    r1 <- rast(i)
    
    # Transform prediction maps to resistance layers using three different formulae
    r2 <- 100 - (99 * ((1 - exp(-2 * r1))/(1 - exp(-2))))
    r3 <- 100 - (99 * ((1 - exp(-8 * r1))/(1 - exp(-8))))
    
    # Extract file name
    j <- strsplit(i,"/")[[1]][length(strsplit(i,"/")[[1]])]
    j <- sub(".tif","",j)
    
    # Create output folder in directory if does not exist
    if (! dir.exists(paste0(proj_path,"Omniscape/Input/",y,"/Resistance layers"))){
      dir.create(paste0(proj_path,"Omniscape/Input/",y,"/Resistance layers"),
                 recursive = TRUE)
    }
    
    # Write resistance layers to TIF files
    writeRaster(r2,
                filename = paste0(proj_path,"Omniscape/Input/",y,"/Resistance layers/",j,"_c2.tif"),
                overwrite = TRUE)
    writeRaster(r3,
                filename = paste0(proj_path,"Omniscape/Input/",y,"/Resistance layers/",j,"_c8.tif"),
                overwrite = TRUE)
    
    # Split filename to extract species name and region
    k <- strsplit(j, "_")[[1]]
    
    # Read corresponding land cover raster
    landcov <- rast(paste0(proj_path,"SDM/Input/",k[3],"_",k[4],"/predictors_all/landcov_",y,".tif"))
    
    # Extract species-specific source pixels from land cover raster
    if (k[1] != "ANNI") {
      s <- subst(landcov,from = c(1,3:8),to = NA,raw = TRUE)
    }
    
    if (k[1] == "ANNI") {
      s <- subst(landcov,from = 2:8,to = NA,raw = TRUE)
    }
    
    # Mask all non-source pixels in prediction raster 
    r4 <- mask(r1,mask = s)
    
    # Create output folder in directory if does not exist
    if (! dir.exists(paste0(proj_path,"Omniscape/Input/",y,"/Source layers"))){
      dir.create(paste0(proj_path,"Omniscape/Input/",y,"/Source layers"),
                 recursive = TRUE)
    }
    
    # Write source layer to TIF file
    writeRaster(r4,
                filename = paste0(proj_path,"Omniscape/Input/",y,"/Source layers/",j,".tif"),
                overwrite = TRUE)
  }
}