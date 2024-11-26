# Load package
library(terra)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

# Read land cover rasters
lc_2017 <- rast(paste0(proj_path,"GIS/Land cover/WG Landscape change raster layers/2017d.img"))
lc_1995 <- rast(paste0(proj_path,"GIS/Land cover/WG Landscape change raster layers/1995d.img"))

# Drop unused categories
lc_2017 <- droplevels(lc_2017)
lc_1995 <- droplevels(lc_1995)

# Set background values to NA
lc_2017 <- subst(lc_2017,from = 0,to = NA,raw = TRUE)
lc_1995 <- subst(lc_1995,from = 0,to = NA,raw = TRUE)

# Name land cover categories
lc_df <- data.frame(value = 1:8,
                    class = c("Shola Grassland",
                              "Shola Forest",
                              "Timber Plantations",
                              "Tea Plantations",
                              "Ochlandra",
                              "Settlements",
                              "Agricultural Land",
                              "Water bodies"))


levels(lc_2017) <- lc_df
levels(lc_1995) <- lc_df

# Rename layer
names(lc_2017) <- "landcov"
names(lc_1995) <- "landcov"

# Write processed raster to TIF file at original resolution
writeRaster(lc_2017,
            filename = paste0(proj_path,"GIS/Land cover/2017D/2017D_rast_30m.tif"),
            overwrite = TRUE)
writeRaster(lc_1995,
            filename = paste0(proj_path,"GIS/Land cover/1995D/1995D_rast_30m.tif"),
            overwrite = TRUE)

# Read reference 1ha resolution raster
rast_1ha <- rast(paste0(proj_path,"GIS/1ha grids.tif"))

# Project land cover raster to reference raster and write to TIF file
lc_2017_1ha <- project(x = lc_2017,
                       y = rast_1ha,
                       method = "near",
                       filename = paste0(proj_path,"GIS/Land cover/2017D/2017D_rast_1ha.tif"),
                       overwrite = TRUE)
lc_1995_1ha <- project(x = lc_1995,
                       y = rast_1ha,
                       method = "near",
                       filename = paste0(proj_path,"GIS/Land cover/1995D/1995D_rast_1ha.tif"),
                       overwrite = TRUE)

# Crop all rasters to extent of reference raster
lc_2017 <- crop(lc_2017,ext(rast_1ha))
lc_1995 <- crop(lc_1995,ext(rast_1ha))

# Polygonise rasters
lc_2017_vect <- as.polygons(lc_2017,round = FALSE,dissolve = TRUE)
lc_1995_vect <- as.polygons(lc_1995,round = FALSE)

# Get fraction cover for each land cover class for each 1ha pixel
ptcover_2017_rast <- rasterize(lc_2017_vect,
                               rast_1ha,
                               field = "landcov",
                               fun = "mean",
                               cover = TRUE,
                               update = TRUE,
                               by = "landcov")

ptcover_1995_rast <- rasterize(lc_1995_vect,
                               rast_1ha,
                               field = "landcov",
                               fun = "mean",
                               cover = TRUE,
                               update = TRUE,
                               by = "landcov")

# Convert to percentage
ptcover_2017_rast <- ptcover_2017_rast * 100
ptcover_1995_rast <- ptcover_1995_rast * 100

# Extract percentage grassland cover layer and rename
ptcover_gland_2017 <- ptcover_2017_rast$`Shola Grassland`
names(ptcover_gland_2017) <- "gland_cover"

ptcover_gland_1995 <- ptcover_1995_rast$`Shola Grassland`
names(ptcover_gland_1995) <- "gland_cover"

# Set background pixels to 0
ptcover_gland_2017[is.na(ptcover_gland_2017)] <- 0
ptcover_gland_1995[is.na(ptcover_gland_1995)] <- 0

# Mask to shapefile and write to TIF file
ptcover_gland_2017_nil <- mask(ptcover_gland_2017,
                               mask = shp,
                               filename = paste0(proj_path,"SDM/Input/nil1400_1ha/predictors_all/gland_cover_2017.tif"),
                               overwrite = TRUE)

ptcover_gland_1995_nil <- mask(ptcover_gland_1995,
                               mask = shp,
                               filename = paste0(proj_path,"SDM/Input/nil1400_1ha/predictors_all/gland_cover_1995.tif"),
                               overwrite = TRUE)