# Load package
library(terra)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

# Read land cover rasters
lc_2017 <- rast(paste0(proj_path,"GIS/Land cover/WG Landscape change raster layers/2017d.img"))
lc_1995 <- rast(paste0(proj_path,"GIS/Land cover/WG Landscape change raster layers/1995d.img"))

# Reproject rasters to WGS84 EPSG:4326
lc_2017 <- project(lc_2017,y = "epsg:4326")
lc_1995 <- project(lc_1995,y = "epsg:4326")

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
            paste0(proj_path,"GIS/Land cover/2017D/2017D_rast_30m.tif"),
            overwrite = TRUE)
writeRaster(lc_1995,
            paste0(proj_path,"GIS/Land cover/1995D/1995D_rast_30m.tif"),
            overwrite = TRUE)

# Read reference 1ha resolution raster
clim_zone_res <- rast(paste0(proj_path,"GIS/Climate zones/clim_zone_1ha.tif"))

# Create empty 1ha resolution raster
res <- rast(nrows = nrow(clim_zone_res),
            ncols = ncol(clim_zone_res),
            ext = ext(clim_zone_res),
            crs = crs(clim_zone_res))

# Resample land cover raster to 1ha resolution and write to TIF file
lc_2017_res <- resample(x = lc_2017,
                        y = res,
                        method = "near",
                        filename = paste0(proj_path,"GIS/Land cover/2017D/2017D_rast_1ha.tif"),
                        overwrite = TRUE)
lc_1995_res <- resample(x = lc_1995,
                        y = res,
                        method = "near",
                        filename = paste0(proj_path,"GIS/Land cover/1995D/1995D_rast_1ha.tif"),
                        overwrite = TRUE)