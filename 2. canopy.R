# Load package
library(terra)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

# Read canopy height (ch) and canopy cover (cc) raster tiles for past and present
ch_2000_1 <- rast(paste0(proj_path,"GIS/Canopy/Canopy height/2000/10N_070E.tif"))
ch_2000_2 <- rast(paste0(proj_path,"GIS/Canopy/Canopy height/2000/20N_070E.tif"))
ch_2020_1 <- rast(paste0(proj_path,"GIS/Canopy/Canopy height/2020/10N_070E.tif"))
ch_2020_2 <- rast(paste0(proj_path,"GIS/Canopy/Canopy height/2020/20N_070E.tif"))

cc_2000_1 <- rast(paste0(proj_path,"GIS/Canopy/Tree Cover/Hansen_GFC2015_treecover2000_10N_070E.tif"))
cc_2000_2 <- rast(paste0(proj_path,"GIS/Canopy/Tree Cover/Hansen_GFC2015_treecover2000_20N_070E.tif"))
cc_2020_1 <- rast(paste0(proj_path,"GIS/Canopy/Tree Cover/10N_070E_0.5ha.tif"))
cc_2020_2 <- rast(paste0(proj_path,"GIS/Canopy/Tree Cover/20N_070E_0.5ha.tif"))

# Merge tiles for each variable
ch_2000 <- merge(ch_2000_1,ch_2000_2)
ch_2020 <- merge(ch_2020_1,ch_2020_2)

cc_2000 <- merge(cc_2000_1,cc_2000_2)
cc_2020 <- merge(cc_2020_1,cc_2020_2)

# Rename layers
names(ch_2000) <- "canopyheight"
names(ch_2020) <- "canopyheight"

names(cc_2000) <- "treecov"
names(cc_2020) <- "treecov"

# Write merged rasters to TIF files
writeRaster(ch_2000,
            filename = paste0(proj_path,"GIS/Canopy/Canopy height/2000/canopy height 2000.tif"),
            overwrite = TRUE)
writeRaster(ch_2020,
            filename = paste0(proj_path,"GIS/Canopy/Canopy height/2020/canopy height 2020.tif"),
            overwrite = TRUE)

writeRaster(cc_2000,
            filename = paste0(proj_path,"GIS/Canopy/Tree Cover/treecov_2000.tif"),
            overwrite = TRUE)
writeRaster(cc_2020,
            filename = paste0(proj_path,"GIS/Canopy/Tree Cover/treecov_2020.tif"),
            overwrite = TRUE)

# Read reference 1ha resolution raster
rast_1ha <- rast(paste0(proj_path,"GIS/1ha grids.tif"))

# Reproject all rasters to reference raster and write to TIF files
ch_2000_1ha <- project(x = ch_2000,
                       y = rast_1ha,
                       method = "med",
                       filename = paste0(proj_path,"GIS/Canopy/Canopy height/2000/canopy height_2000_1ha.tif"),
                       overwrite = TRUE)
ch_2020_1ha <- project(x = ch_2020,
                       y = rast_1ha,
                       method = "med",
                       filename = paste0(proj_path,"GIS/Canopy/Canopy height/2020/canopy height_2020_1ha.tif"),
                       overwrite = TRUE)
cc_2000_1ha <- project(x = cc_2000,
                       y = rast_1ha,
                       method = "med",
                       filename = paste0(proj_path,"GIS/Canopy/Tree Cover/treecov_2000_1ha.tif"),
                       overwrite = TRUE)
cc_2020_1ha <- project(x = cc_2020,
                       y = rast_1ha,
                       method = "med",
                       filename = paste0(proj_path,"GIS/Canopy/Tree Cover/treecov_2020_1ha.tif"),
                       overwrite = TRUE)