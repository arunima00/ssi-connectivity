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

# Polygonise rasters
lc_2017_vect <- as.polygons(lc_2017,round = FALSE)
lc_1995_vect <- as.polygons(lc_1995,round = FALSE)

# Extract grassland polygons
lc_2017_gland <- lc_2017_vect[lc_2017_vect$landcov == "Shola Grassland"]
lc_1995_gland <- lc_1995_vect[lc_1995_vect$landcov == "Shola Grassland"]

# Get fraction cover for each land cover class for each 1ha pixel
ptcover_gland_2017 <- rasterize(lc_2017_gland,
                                rast_1ha,
                                field = "landcov",
                                fun = "mean",
                                cover = TRUE,
                                update = TRUE)

ptcover_gland_1995 <- rasterize(lc_1995_gland,
                                rast_1ha,
                                field = "landcov",
                                fun = "mean",
                                cover = TRUE,
                                update = TRUE)

# Convert to percentage
ptcover_gland_2017 <- ptcover_gland_2017 * 100
ptcover_gland_1995 <- ptcover_gland_1995 * 100

# Set background pixels to 0
ptcover_gland_2017[is.na(ptcover_gland_2017)] <- 0
ptcover_gland_1995[is.na(ptcover_gland_1995)] <- 0

# Rename layer
names(ptcover_gland_2017) <- "gland_cover"
names(ptcover_gland_1995) <- "gland_cover"

# Write rasters to TIF file
writeRaster(ptcover_gland_2017,
            filename = paste0(proj_path,"GIS/Derived rasters/gland_cover_2017.tif"),
            overwrite = TRUE)

writeRaster(ptcover_gland_1995,
            filename = paste0(proj_path,"GIS/Derived rasters/gland_cover_1995.tif"),
            overwrite = TRUE)