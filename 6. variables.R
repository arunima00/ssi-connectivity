# Load packages
library(terra)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

# Read tree cover rasters
treecov_2000 <- rast(paste0(proj_path,"GIS/Canopy/Tree Cover/treecov_2000_1ha.tif"))
treecov_2020 <- rast(paste0(proj_path,"GIS/Canopy/Tree Cover/treecov_2020_1ha.tif"))

# Read canopy height rasters
ch_2000 <- rast(paste0(proj_path,"GIS/Canopy/Canopy height/2000/canopy height_2000_1ha.tif"))
ch_2020 <- rast(paste0(proj_path,"GIS/Canopy/Canopy height/2020/canopy height_2020_1ha.tif"))

# Read land cover raster
lc_2017 <- rast(paste0(proj_path,"GIS/Land cover/2017D/2017D_rast_1ha.tif"))
lc_1995 <- rast(paste0(proj_path,"GIS/Land cover/1995D/1995D_rast_1ha.tif"))

# Read stack of topographic variable layers
files_topo <- list.files(paste0(proj_path,"GIS/Derived rasters/Topo variables"),
                         pattern = ".tif$",
                         full.names = TRUE)
topo <- rast(files_topo)

# Read stack of proximity variable layers
files_prox <- list.files(paste0(proj_path,"GIS/Derived rasters/Proximity"),
                         pattern = ".tif$",
                         full.names = TRUE)
prox <- rast(files_prox)

# Read climate zones raster
clim_zone <- rast(paste0(proj_path,"GIS/Climate zones/clim_zone_1ha.tif"))

# Create loop to clip and mask variables for both regions
for (region in c("nil1400","pahw1400","swg1400")) {
  # Read shapefiles based on region
  if (region == "nil1400") {
    shp <- vect(paste0(proj_path,"GIS/Shapefiles/Nilgiri1400m/Nilgiri1400m.shp")
  }
  
  if (region == "pahw1400") {
    shp <- vect(paste0(proj_path,"GIS/Shapefiles/PA_HW_1400m/PA_HW_1400m.shp"))
  }
  
  if (region == "swg1400") {
    shp <- vect(paste0(proj_path,"GIS/Shapefiles/SWG1400m/SWG1400m.shp"))
  }
  
  # Reproject shapefile to reference CRS
  shp <- project(shp,y = crs(clim_zone))
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Input/",region,"_1ha/predictors_all"))){
    dir.create(paste0(proj_path,"SDM/Input/",region,"_1ha/predictors_all"),
               recursive = TRUE)
  }
  
  # Crop and mask tree cover layers to specified region and write to TIF files
  treecov_2000_clip <- crop(x = treecov_2000,
                            y = shp,
                            mask = TRUE,
                            filename = paste0(proj_path,"SDM/Input/",region,"_1ha/predictors_all/treecov_2000.tif"),
                            overwrite = TRUE)
  
  treecov_2020_clip <- crop(x = treecov_2020,
                            y = shp,
                            mask = TRUE,
                            filename = paste0(proj_path,"SDM/Input/",region,"_1ha/predictors_all/treecov_2020.tif"),
                            overwrite = TRUE)
  
  # Crop and mask canopy height layers to specified region and write to TIF files
  ch_2000_clip <- crop(x = ch_2000,
                       y = shp,
                       mask = TRUE,
                       filename = paste0(proj_path,"SDM/Input/",region,"_1ha/predictors_all/canopyheight_2000.tif"),
                       overwrite = TRUE)
  
  ch_2020_clip <- crop(x = ch_2020,
                       y = shp,
                       mask = TRUE,
                       filename = paste0(proj_path,"SDM/Input/",region,"_1ha/predictors_all/canopyheight_2020.tif"),
                       overwrite = TRUE)
  
  # Crop and mask land cover layers to specified region and write to TIF files
  lc_2017_clip <- crop(x = lc_2017,
                       y = shp,
                       mask = TRUE,
                       filename = paste0(proj_path,"SDM/Input/",region,"_1ha/predictors_all/landcov_2017.tif"),
                       overwrite = TRUE)
  lc_1995_clip <- crop(x = lc_1995,
                       y = shp,
                       mask = TRUE,
                       filename = paste0(proj_path,"SDM/Input/",region,"_1ha/predictors_all/landcov_1995.tif"),
                       overwrite = TRUE)
  
  # Create vector of output file names for final topographic variable layers
  output_paths <- c()
  for (i in names(topo)) {
    output_paths <- c(output_paths,
                      paste0(proj_path,"SDM/Input/",region,"_1ha/predictors_all/",i,".tif"))
  }
  
  # Crop and mask topographic variables raster stack to specified region
  topo_clip <- crop(x = topo,y = shp,mask = TRUE)
  
  # Write cropped and masked layers to output TIF files
  writeRaster(topo_clip,filename = output_paths,overwrite = TRUE)
  
  # Create vector of output file names for final proximity variable layers
  output_paths <- c()
  for (i in 1:length(files_prox)){
    j <- strsplit(files_prox[i],split = "/")[[1]][10]
    k <- strsplit(j,split = "_1ha")[[1]][1]
    output_paths <- c(output_paths,
                      paste0(proj_path,"SDM/Input/",region,"_1ha/predictors_all/",k,".tif"))
  }
  
  # Crop and mask proximity variables raster stack to specified region
  prox_clip <- crop(x = prox,y = shp,mask = TRUE)
  
  # Write cropped and masked layers to output TIF files
  writeRaster(prox_clip,filename = output_paths,overwrite = TRUE)
  
  # Crop and mask climate zone layer to specified region and write to TIF file
  clim_zone_clip <- crop(x = clim_zone,
                         y = shp,
                         mask = TRUE,
                         filename = paste0(proj_path,"SDM/Input/",region,"_1ha/predictors_all/clim_zone.tif"),
                         overwrite = TRUE)
  
  # Create raster stacks for past and present for forest species variables
  if (region %in% c("nil1400","pahw1400")) {
    predictors_present_f <- c(treecov_2020_clip,
                              ch_2020_clip,
                              lc_2017_clip,
                              topo_clip,
                              prox_clip[[c(4,6,8)]],
                              clim_zone_clip)
    
    predictors_past_f <- c(treecov_2000_clip,
                           ch_2000_clip,
                           lc_1995_clip,
                           topo_clip,
                           prox_clip[[c(3,5,7)]],
                           clim_zone_clip)
    
    # Save raster stacks to output folder
    writeRaster(predictors_present_f,
                filename = paste0(proj_path,"SDM/Input/",region,"_1ha/",region,"_predictors_present.tif"),
                overwrite = TRUE)
    
    writeRaster(predictors_past_f,
                filename = paste0(proj_path,"SDM/Input/",region,"_1ha/",region,"_predictors_past.tif"),
                overwrite = TRUE)
  }
  
  # Create and save raster stacks for grassland species variables
  if (region == "swg1400"){
    
    # Read grassland cover percentage raster
    ptcover_gland_2017 <- rast(paste0(proj_path,"SDM/Input/nil1400_1ha/predictors_all/gland_cover_2017.tif"))
    ptcover_gland_1995 <- rast(paste0(proj_path,"SDM/Input/nil1400_1ha/predictors_all/gland_cover_1995.tif"))
    
    # Create raster stacks
    predictors_present_g <- c(prox_clip[[2]],
                              topo_clip,
                              clim_zone_clip,
                              ptcover_gland_2017)
    
    predictors_past_g <- c(prox_clip[[1]],
                           topo_clip,
                           clim_zone_clip,
                           ptcover_gland_1995)
    
    # Save stacks
    writeRaster(predictors_present_g,
                filename = paste0(proj_path,"SDM/Input/",region,"_1ha/",region,"_predictors_present.tif"),
                overwrite = TRUE)
    
    writeRaster(predictors_past_g,
                filename = paste0(proj_path,"SDM/Input/",region,"_1ha/",region,"_predictors_past.tif"),
                overwrite = TRUE)
  }
}