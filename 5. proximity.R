# Load package
library(terra)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

## To derive proximity rasters in QGIS

# Load land cover layer (make sure pixel units of layer CRS are in metres)
# Raster => Analysis => Proximity (Raster Distance)
# 1. Select land cover raster as Input
# 2. Enter values of land cover categories to measure proximity/distance to 
#    in the target pixel values blank, separated by commas
# 3. Select georeferenced coordinates as distance units
# 4. Save output file to directory

# Read rasters
prox_shola_2017_temp <- rast(paste0(proj_path,"GIS/Derived rasters/Proximity/Extra/prox_shola_2017_30m_temp.tif"))
prox_shola_1995_temp <- rast(paste0(proj_path,"GIS/Derived rasters/Proximity/Extra/prox_shola_1995_30m_temp.tif"))

prox_shola_2017_inv <- rast(paste0(proj_path,"GIS/Derived rasters/Proximity/Extra/prox_shola_2017_30m_inv.tif"))
prox_shola_1995_inv <- rast(paste0(proj_path,"GIS/Derived rasters/Proximity/Extra/prox_shola_1995_30m_inv.tif"))

# Set proximity to shola boundary from inside shola patches to negative values
prox_shola_2017_inv <- (0 - prox_shola_2017_inv)
prox_shola_1995_inv <- (0 - prox_shola_1995_inv)

# Mask values outside forest patch to NA for inverted raster
prox_shola_2017_inv <- mask(prox_shola_2017_inv,
                            mask = prox_shola_2017_temp,
                            maskvalues = 0,
                            inverse = TRUE,
                            updatevalue = NA)
prox_shola_1995_inv <- mask(prox_shola_1995_inv,
                            mask = prox_shola_1995_temp,
                            maskvalues = 0,
                            inverse = TRUE,
                            updatevalue = NA)

# Merge both rasters to get proximity to Shola forest boundary raster
prox_shola_2017 <- merge(x = prox_shola_2017_inv,
                         y = prox_shola_2017_temp,
                         first = TRUE,
                         na.rm = TRUE)
prox_shola_1995 <- merge(x = prox_shola_1995_inv,
                         y = prox_shola_1995_temp,
                         first = TRUE,
                         na.rm = TRUE)

names(prox_shola_2017) <- "prox_shola"
names(prox_shola_1995) <- "prox_shola"

# Write final raster to TIF file
writeRaster(prox_shola_2017,
            filename = paste0(proj_path,"GIS/Derived rasters/Proximity/Extra/prox_shola_2017_30m.tif"),
            overwrite = TRUE)
writeRaster(prox_shola_1995,
            filename = paste0(proj_path,"GIS/Derived rasters/Proximity/Extra/prox_shola_1995_30m.tif"),
            overwrite = TRUE)

## Do the same for proximity to woodland

prox_woodland_2017_temp <- rast(paste0(proj_path,"GIS/Derived rasters/Proximity/Extra/prox_woodland_2017_30m_temp.tif"))
prox_woodland_1995_temp <- rast(paste0(proj_path,"GIS/Derived rasters/Proximity/Extra/prox_woodland_1995_30m_temp.tif"))

prox_woodland_2017_inv <- rast(paste0(proj_path,"GIS/Derived rasters/Proximity/Extra/prox_woodland_2017_30m_inv.tif"))
prox_woodland_1995_inv <- rast(paste0(proj_path,"GIS/Derived rasters/Proximity/Extra/prox_woodland_1995_30m_inv.tif"))

prox_woodland_2017_inv <- (0 - prox_woodland_2017_inv)
prox_woodland_1995_inv <- (0 - prox_woodland_1995_inv)

prox_woodland_2017_inv <- mask(prox_woodland_2017_inv,
                               mask = prox_woodland_2017_temp,
                               maskvalues = 0,
                               inverse = TRUE,
                               updatevalue = NA)
prox_woodland_1995_inv <- mask(prox_woodland_1995_inv,
                               mask = prox_woodland_1995_temp,
                               maskvalues = 0,
                               inverse = TRUE,
                               updatevalue = NA)

prox_woodland_2017 <- merge(x = prox_woodland_2017_inv,
                            y = prox_woodland_2017_temp,
                            first = TRUE,
                            na.rm = TRUE)
prox_woodland_1995 <- merge(x = prox_woodland_1995_inv,
                            y = prox_woodland_1995_temp,
                            first = TRUE,
                            na.rm = TRUE)

names(prox_woodland_2017) <- "prox_woodland"
names(prox_woodland_1995) <- "prox_woodland"

writeRaster(prox_woodland_2017,
            filename = paste0(proj_path,"GIS/Derived rasters/Proximity/Extra/prox_woodland_2017_30m.tif"),
            overwrite = TRUE)
writeRaster(prox_woodland_1995,
            filename = paste0(proj_path,"GIS/Derived rasters/Proximity/Extra/prox_woodland_1995_30m.tif"),
            overwrite = TRUE)

# Read proximity to settlements and grassland rasters
prox_smts_2017 <- rast(paste0(proj_path,"GIS/Derived rasters/Proximity/Extra/prox_settlements_2017_30m.tif"))
prox_smts_1995 <- rast(paste0(proj_path,"GIS/Derived rasters/Proximity/Extra/prox_settlements_1995_30m.tif"))

prox_grassland_2017 <- rast(paste0(proj_path,"GIS/Derived rasters/Proximity/Extra/prox_grassland_2017_30m_temp.tif"))
prox_grassland_1995 <- rast(paste0(proj_path,"GIS/Derived rasters/Proximity/Extra/prox_grassland_1995_30m_temp.tif"))

names(prox_smts_2017) <- "prox_settlements"
names(prox_smts_1995) <- "prox_settlements"

names(prox_grassland_2017) <- "prox_grassland"
names(prox_grassland_1995) <- "prox_grassland"

# Read reference 1ha resolution raster
rast_1ha <- rast(paste0(proj_path,"occupancy data/Jobin/1500 1ha grids/1ha grids.tif"))

# Reproject all rasters and write to TIF files
prox_shola_2017_1ha <- project(x = prox_shola_2017,
                               y = rast_1ha,
                               method = "med",
                               filename = paste0(proj_path,"GIS/Derived rasters/Proximity/prox_shola_2017_1ha.tif"),
                               overwrite = TRUE)
prox_shola_1995_1ha <- project(x = prox_shola_1995,
                               y = rast_1ha,
                               method = "med",
                               filename = paste0(proj_path,"GIS/Derived rasters/Proximity/prox_shola_1995_1ha.tif"),
                               overwrite = TRUE)

prox_woodland_2017_1ha <- project(x = prox_woodland_2017,
                                  y = rast_1ha,
                                  method = "med",
                                  filename = paste0(proj_path,"GIS/Derived rasters/Proximity/prox_woodland_2017_1ha.tif"),
                                  overwrite = TRUE)
prox_woodland_1995_1ha <- project(x = prox_woodland_1995,
                                  y = rast_1ha,
                                  method = "med",
                                  filename = paste0(proj_path,"GIS/Derived rasters/Proximity/prox_woodland_1995_1ha.tif"),
                                  overwrite = TRUE)

prox_grassland_2017_25ha <- project(x = prox_grassland_2017,
                                    y = crs(rast_1ha),
                                    method = "med",
                                    res = 500,
                                    filename = paste0(proj_path,"GIS/Derived rasters/Proximity/prox_grassland_2017_25ha.tif"),
                                    overwrite = TRUE)
prox_grassland_1995_25ha <- project(x = prox_grassland_1995,
                                    y = crs(rast_1ha),
                                    method = "med",
                                    res = 500,
                                    filename = paste0(proj_path,"GIS/Derived rasters/Proximity/prox_grassland_1995_25ha.tif"),
                                    overwrite = TRUE)

prox_smts_2017_1ha <- project(x = prox_smts_2017,
                              y = rast_1ha,
                              method = "med",
                              filename = paste0(proj_path,"GIS/Derived rasters/Proximity/prox_settlements_2017_1ha.tif"),
                              overwrite = TRUE)
prox_smts_1995_1ha <- project(x = prox_smts_1995,
                              y = rast_1ha,
                              method = "med",
                              filename = paste0(proj_path,"GIS/Derived rasters/Proximity/prox_settlements_1995_1ha.tif"),
                              overwrite = TRUE)