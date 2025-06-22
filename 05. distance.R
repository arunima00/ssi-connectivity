# Load package
library(terra)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

## To derive distance rasters in QGIS

# Load land cover layer (make sure pixel units of layer CRS are in metres)
# Raster => Analysis => Proximity (Raster Distance)
# 1. Select land cover raster as Input
# 2. Enter values of land cover categories to measure distance to 
#    in the "target pixel values" field, separated by commas
# 3. Select georeferenced coordinates as distance units, 
#    make sure they are in metres
# 4. Save output file to directory

# Read rasters
dist_shola_2017_temp <- rast(paste0(proj_path,"GIS/Derived rasters/Distance/Extra/dist_shola_2017_30m_temp.tif"))
dist_shola_1995_temp <- rast(paste0(proj_path,"GIS/Derived rasters/Distance/Extra/dist_shola_1995_30m_temp.tif"))

dist_shola_2017_inv <- rast(paste0(proj_path,"GIS/Derived rasters/Distance/Extra/dist_shola_2017_30m_inv.tif"))
dist_shola_1995_inv <- rast(paste0(proj_path,"GIS/Derived rasters/Distance/Extra/dist_shola_1995_30m_inv.tif"))

# Convert distance to shola edge from inside shola patches to negative values
dist_shola_2017_inv <- (0 - dist_shola_2017_inv)
dist_shola_1995_inv <- (0 - dist_shola_1995_inv)

# Mask values outside forest patch to NA for inverted rasters
dist_shola_2017_inv <- mask(dist_shola_2017_inv,
                            mask = dist_shola_2017_temp,
                            maskvalues = 0,
                            inverse = TRUE,
                            updatevalue = NA)
dist_shola_1995_inv <- mask(dist_shola_1995_inv,
                            mask = dist_shola_1995_temp,
                            maskvalues = 0,
                            inverse = TRUE,
                            updatevalue = NA)

# Merge both rasters to get distance to Shola forest edge raster
dist_shola_2017 <- merge(x = dist_shola_2017_inv,
                         y = dist_shola_2017_temp,
                         first = TRUE,
                         na.rm = TRUE)
dist_shola_1995 <- merge(x = dist_shola_1995_inv,
                         y = dist_shola_1995_temp,
                         first = TRUE,
                         na.rm = TRUE)

names(dist_shola_2017) <- "dist_shola"
names(dist_shola_1995) <- "dist_shola"

# Write final rasters to TIF files
writeRaster(dist_shola_2017,
            filename = paste0(proj_path,"GIS/Derived rasters/Distance/Extra/dist_shola_2017_30m.tif"),
            overwrite = TRUE)
writeRaster(dist_shola_1995,
            filename = paste0(proj_path,"GIS/Derived rasters/Distance/Extra/dist_shola_1995_30m.tif"),
            overwrite = TRUE)

## Do the same for distance to woodland edge and distance to grassland edge

dist_woodland_2017_temp <- rast(paste0(proj_path,"GIS/Derived rasters/Distance/Extra/dist_woodland_2017_30m_temp.tif"))
dist_woodland_1995_temp <- rast(paste0(proj_path,"GIS/Derived rasters/Distance/Extra/dist_woodland_1995_30m_temp.tif"))

dist_woodland_2017_inv <- rast(paste0(proj_path,"GIS/Derived rasters/Distance/Extra/dist_woodland_2017_30m_inv.tif"))
dist_woodland_1995_inv <- rast(paste0(proj_path,"GIS/Derived rasters/Distance/Extra/dist_woodland_1995_30m_inv.tif"))

dist_woodland_2017_inv <- (0 - dist_woodland_2017_inv)
dist_woodland_1995_inv <- (0 - dist_woodland_1995_inv)

dist_woodland_2017_inv <- mask(dist_woodland_2017_inv,
                               mask = dist_woodland_2017_temp,
                               maskvalues = 0,
                               inverse = TRUE,
                               updatevalue = NA)
dist_woodland_1995_inv <- mask(dist_woodland_1995_inv,
                               mask = dist_woodland_1995_temp,
                               maskvalues = 0,
                               inverse = TRUE,
                               updatevalue = NA)

dist_woodland_2017 <- merge(x = dist_woodland_2017_inv,
                            y = dist_woodland_2017_temp,
                            first = TRUE,
                            na.rm = TRUE)
dist_woodland_1995 <- merge(x = dist_woodland_1995_inv,
                            y = dist_woodland_1995_temp,
                            first = TRUE,
                            na.rm = TRUE)

names(dist_woodland_2017) <- "dist_woodland"
names(dist_woodland_1995) <- "dist_woodland"

writeRaster(dist_woodland_2017,
            filename = paste0(proj_path,"GIS/Derived rasters/Distance/Extra/dist_woodland_2017_30m.tif"),
            overwrite = TRUE)
writeRaster(dist_woodland_1995,
            filename = paste0(proj_path,"GIS/Derived rasters/Distance/Extra/dist_woodland_1995_30m.tif"),
            overwrite = TRUE)

dist_grassland_2017_temp <- rast(paste0(proj_path,"GIS/Derived rasters/Distance/Extra/dist_grassland_2017_30m_temp.tif"))
dist_grassland_1995_temp <- rast(paste0(proj_path,"GIS/Derived rasters/Distance/Extra/dist_grassland_1995_30m_temp.tif"))

dist_grassland_2017_inv <- rast(paste0(proj_path,"GIS/Derived rasters/Distance/Extra/dist_grassland_2017_30m_inv.tif"))
dist_grassland_1995_inv <- rast(paste0(proj_path,"GIS/Derived rasters/Distance/Extra/dist_grassland_1995_30m_inv.tif"))

dist_grassland_2017_inv <- (0 - dist_grassland_2017_inv)
dist_grassland_1995_inv <- (0 - dist_grassland_1995_inv)

dist_grassland_2017_inv <- mask(dist_grassland_2017_inv,
                               mask = dist_grassland_2017_temp,
                               maskvalues = 0,
                               inverse = TRUE,
                               updatevalue = NA)
dist_grassland_1995_inv <- mask(dist_grassland_1995_inv,
                               mask = dist_grassland_1995_temp,
                               maskvalues = 0,
                               inverse = TRUE,
                               updatevalue = NA)

dist_grassland_2017 <- merge(x = dist_grassland_2017_inv,
                            y = dist_grassland_2017_temp,
                            first = TRUE,
                            na.rm = TRUE)
dist_grassland_1995 <- merge(x = dist_grassland_1995_inv,
                            y = dist_grassland_1995_temp,
                            first = TRUE,
                            na.rm = TRUE)

names(dist_grassland_2017) <- "dist_grassland"
names(dist_grassland_1995) <- "dist_grassland"

writeRaster(dist_grassland_2017,
            filename = paste0(proj_path,"GIS/Derived rasters/Distance/Extra/dist_grassland_2017_30m.tif"),
            overwrite = TRUE)
writeRaster(dist_grassland_1995,
            filename = paste0(proj_path,"GIS/Derived rasters/Distance/Extra/dist_grassland_1995_30m.tif"),
            overwrite = TRUE)

# Read distance to settlement rasters
dist_smts_2017 <- rast(paste0(proj_path,"GIS/Derived rasters/Distance/Extra/dist_settlements_2017_30m.tif"))
dist_smts_1995 <- rast(paste0(proj_path,"GIS/Derived rasters/Distance/Extra/dist_settlements_1995_30m.tif"))

names(dist_smts_2017) <- "dist_settlements"
names(dist_smts_1995) <- "dist_settlements"

# Read reference 1ha resolution raster
rast_1ha <- rast(paste0(proj_path,"occupancy data/1500 1ha grids/1ha grids.tif"))

# Reproject all rasters and write to TIF files
dist_shola_2017_1ha <- project(x = dist_shola_2017,
                               y = rast_1ha,
                               method = "med",
                               filename = paste0(proj_path,"GIS/Derived rasters/Distance/dist_shola_2017_1ha.tif"),
                               overwrite = TRUE)
dist_shola_1995_1ha <- project(x = dist_shola_1995,
                               y = rast_1ha,
                               method = "med",
                               filename = paste0(proj_path,"GIS/Derived rasters/Distance/dist_shola_1995_1ha.tif"),
                               overwrite = TRUE)

dist_woodland_2017_1ha <- project(x = dist_woodland_2017,
                                  y = rast_1ha,
                                  method = "med",
                                  filename = paste0(proj_path,"GIS/Derived rasters/Distance/dist_woodland_2017_1ha.tif"),
                                  overwrite = TRUE)
dist_woodland_1995_1ha <- project(x = dist_woodland_1995,
                                  y = rast_1ha,
                                  method = "med",
                                  filename = paste0(proj_path,"GIS/Derived rasters/Distance/dist_woodland_1995_1ha.tif"),
                                  overwrite = TRUE)

dist_grassland_2017_1ha <- project(x = dist_grassland_2017,
                                   y = rast_1ha,
                                   method = "med",
                                   filename = paste0(proj_path,"GIS/Derived rasters/Distance/dist_grassland_2017_1ha.tif"),
                                   overwrite = TRUE)
dist_grassland_1995_1ha <- project(x = dist_grassland_1995,
                                   y = rast_1ha,
                                   method = "med",
                                   filename = paste0(proj_path,"GIS/Derived rasters/Distance/dist_grassland_1995_1ha.tif"),
                                   overwrite = TRUE)

dist_smts_2017_1ha <- project(x = dist_smts_2017,
                              y = rast_1ha,
                              method = "med",
                              filename = paste0(proj_path,"GIS/Derived rasters/Distance/dist_settlements_2017_1ha.tif"),
                              overwrite = TRUE)
dist_smts_1995_1ha <- project(x = dist_smts_1995,
                              y = rast_1ha,
                              method = "med",
                              filename = paste0(proj_path,"GIS/Derived rasters/Distance/dist_settlements_1995_1ha.tif"),
                              overwrite = TRUE)