# Load package
library(terra)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

# Read canopy height raster tiles for past and present
ch_2000_1 <- rast(paste0(proj_path,"GIS/Canopy/Canopy height/2000/10N_070E.tif"))
ch_2000_2 <- rast(paste0(proj_path,"GIS/Canopy/Canopy height/2000/20N_070E.tif"))
ch_2020_1 <- rast(paste0(proj_path,"GIS/Canopy/Canopy height/2020/10N_070E.tif"))
ch_2020_2 <- rast(paste0(proj_path,"GIS/Canopy/Canopy height/2020/20N_070E.tif"))

# Merge tiles
ch_2000 <- merge(ch_2000_1,ch_2000_2)
ch_2020 <- merge(ch_2020_1,ch_2020_2)

names(ch_2000) <- "canopyheight"
names(ch_2020) <- "canopyheight"

# Write merged rasters to TIF files
writeRaster(ch_2000,
            filename = paste0(proj_path,"GIS/Canopy/Canopy height/2000/canopy height 2000.tif"),
            overwrite = TRUE)
writeRaster(ch_2020,
            filename = paste0(proj_path,"GIS/Canopy/Canopy height/2020/canopy height 2020.tif"),
            overwrite = TRUE)

# Read reference 1ha resolution raster
rast_1ha <- rast(paste0(proj_path,"occupancy data/Jobin/1500 1ha grids/1ha grids.tif"))

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

# Read reference 25ha raster
clim_zone <- rast(paste0(proj_path,"GIS/Climate zones/clim_zone_25ha.tif"))

# Reproject canopy height layer to 25ha and write to to TIF file
ch_2000_25ha <- project(x = ch_2000,
                        y = clim_zone,
                        method = "med",
                        filename = paste0(proj_path,"GIS/Canopy/Canopy height/2000/canopy height_2000_25ha.tif"),
                        overwrite = TRUE)
ch_2020_25ha <- project(x = ch_2020,
                        y = clim_zone,
                        method = "med",
                        filename = paste0(proj_path,"GIS/Canopy/Canopy height/2020/canopy height_2020_25ha.tif"),
                        overwrite = TRUE)