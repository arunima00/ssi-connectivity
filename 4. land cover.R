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
rast_1ha <- rast(paste0(proj_path,"occupancy data/Jobin/1500 1ha grids/1ha grids.tif"))

# Read reference 25ha raster
clim_zone <- rast(paste0(proj_path,"GIS/Climate zones/clim_zone_25ha.tif"))

# Project land cover raster to 1ha and 25ha resolution and write to TIF files
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

lc_2017_25ha <- project(x = lc_2017,
                        y = clim_zone,
                        method = "near",
                        filename = paste0(proj_path,"GIS/Land cover/2017D/2017D_rast_25ha.tif"),
                        overwrite = TRUE)
lc_1995_25ha <- project(x = lc_1995,
                        y = clim_zone,
                        method = "near",
                        filename = paste0(proj_path,"GIS/Land cover/1995D/1995D_rast_25ha.tif"),
                        overwrite = TRUE)

# Polygonise rasters
lc_2017_vect <- as.polygons(lc_2017,round = FALSE)
lc_1995_vect <- as.polygons(lc_1995,round = FALSE)

# Save polygonised land cover shapefile to directory
if (! dir.exists(paste0(proj_path,"GIS/Land cover/2017D/2017D vectorised"))) {
  dir.create(paste0(proj_path,"GIS/Land cover/2017D/2017D vectorised"),recursive = TRUE)
}

if (! dir.exists(paste0(proj_path,"GIS/Land cover/1995D/1995D vectorised"))) {
  dir.create(paste0(proj_path,"GIS/Land cover/1995D/1995D vectorised"),recursive = TRUE)
}

writeVector(lc_2017_vect,
            filename = paste0(proj_path,"GIS/Land cover/2017D/2017D vectorised/2017D.shp"),
            overwrite = TRUE)

writeVector(lc_1995_vect,
            filename = paste0(proj_path,"GIS/Land cover/1995D/1995D vectorised/1995D.shp"),
            overwrite = TRUE)

lc_2017_woodland <- lc_2017_vect[lc_2017_vect$landcov %in% c("Shola Forest","Timber Plantations")]
lc_1995_woodland <- lc_1995_vect[lc_1995_vect$landcov %in% c("Shola Forest","Timber Plantations")]

# Get fraction cover for each land cover class for each 1ha and 25ha pixel
cover_2017_1ha <- rasterize(lc_2017_vect,
                            lc_2017_1ha,
                            field = "landcov",
                            fun = "mean",
                            cover = TRUE,
                            update = TRUE,
                            by = "landcov")

cover_1995_1ha <- rasterize(lc_1995_vect,
                            lc_1995_1ha,
                            field = "landcov",
                            fun = "mean",
                            cover = TRUE,
                            update = TRUE,
                            by = "landcov")

wland_2017_1ha <- rasterize(lc_2017_woodland,
                            lc_2017_1ha,
                            field = "landcov",
                            fun = "mean",
                            cover = TRUE,
                            update = TRUE)

wland_1995_1ha <- rasterize(lc_1995_woodland,
                            lc_1995_1ha,
                            field = "landcov",
                            fun = "mean",
                            cover = TRUE,
                            update = TRUE)

cover_2017_25ha <- rasterize(lc_2017_vect,
                             lc_2017_25ha,
                             field = "landcov",
                             fun = "mean",
                             cover = TRUE,
                             update = TRUE,
                             by = "landcov")

cover_1995_25ha <- rasterize(lc_1995_vect,
                             lc_1995_25ha,
                             field = "landcov",
                             fun = "mean",
                             cover = TRUE,
                             update = TRUE,
                             by = "landcov")

# Convert to percentage
cover_2017_1ha <- cover_2017_1ha * 100
cover_1995_1ha <- cover_1995_1ha * 100

wland_2017_1ha <- wland_2017_1ha * 100
wland_1995_1ha <- wland_1995_1ha * 100

cover_2017_25ha <- cover_2017_25ha * 100
cover_1995_25ha <- cover_1995_25ha * 100

# Set background pixels to 0
cover_2017_1ha[is.na(cover_2017_1ha)] <- 0
cover_1995_1ha[is.na(cover_1995_1ha)] <- 0

wland_2017_1ha[is.na(wland_2017_1ha)] <- 0
wland_1995_1ha[is.na(wland_1995_1ha)] <- 0

cover_2017_25ha[is.na(cover_2017_25ha)] <- 0
cover_1995_25ha[is.na(cover_1995_25ha)] <- 0

# Create list of file names
output_paths_2017_1ha <- c()
output_paths_1995_1ha <- c()
output_paths_2017_25ha <- c()
output_paths_1995_25ha <- c()

names(wland_2017_1ha) <- "Woodland"
names(wland_1995_1ha) <- "Woodland"

if (! dir.exists(paste0(proj_path,"GIS/Derived rasters/cover_2017_1ha"))) {
  dir.create(paste0(proj_path,"GIS/Derived rasters/cover_2017_1ha"),recursive = TRUE)
}

if (! dir.exists(paste0(proj_path,"GIS/Derived rasters/cover_1995_1ha"))) {
  dir.create(paste0(proj_path,"GIS/Derived rasters/cover_1995_1ha"),recursive = TRUE)
}

if (! dir.exists(paste0(proj_path,"GIS/Derived rasters/cover_2017_25ha"))) {
  dir.create(paste0(proj_path,"GIS/Derived rasters/cover_2017_25ha"),recursive = TRUE)
}

if (! dir.exists(paste0(proj_path,"GIS/Derived rasters/cover_1995_25ha"))) {
  dir.create(paste0(proj_path,"GIS/Derived rasters/cover_1995_25ha"),recursive = TRUE)
}

for (i in names(cover_2017_1ha)){
  output_paths_2017_1ha <- c(output_paths_2017_1ha,
                             paste0(proj_path,"GIS/Derived rasters/cover_2017_1ha/",i,".tif"))
  output_paths_1995_1ha <- c(output_paths_1995_1ha,
                             paste0(proj_path,"GIS/Derived rasters/cover_1995_1ha/",i,".tif"))
  output_paths_2017_25ha <- c(output_paths_2017_25ha,
                              paste0(proj_path,"GIS/Derived rasters/cover_2017_25ha/",i,".tif"))
  output_paths_1995_25ha <- c(output_paths_1995_25ha,
                              paste0(proj_path,"GIS/Derived rasters/cover_1995_25ha/",i,".tif"))
}

# Write rasters to TIF files
writeRaster(cover_2017_1ha,
            filename = output_paths_2017_1ha,
            overwrite = TRUE)

writeRaster(cover_1995_1ha,
            filename = output_paths_1995_1ha,
            overwrite = TRUE)

writeRaster(wland_2017_1ha,
            filename = paste0(proj_path,"GIS/Derived rasters/cover_2017_1ha/Shola Forest_Timber.tif"),
            overwrite = TRUE)

writeRaster(wland_1995_1ha,
            filename = paste0(proj_path,"GIS/Derived rasters/cover_1995_1ha/Shola Forest_Timber.tif"),
            overwrite = TRUE)

writeRaster(cover_2017_25ha,
            filename = output_paths_2017_25ha,
            overwrite = TRUE)

writeRaster(cover_1995_25ha,
            filename = output_paths_1995_25ha,
            overwrite = TRUE)