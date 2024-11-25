# Load package
library(terra)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

# Read climate zone raster
clim_zone <- rast(paste0(proj_path,"GIS/Climate zones/ClassifiedSumSWGPrecandTas.tif"))

# Convert into categorical raster and set levels
levels(clim_zone) <- data.frame(id = 1:37,zone = 11:47)

# Read reference 1ha resolution raster
rast_1ha <- rast(paste0(proj_path,"GIS/1ha grids.tif"))

# Reproject climate zone raster to reference CRS
clim_zone <- project(clim_zone,y = crs(rast_1ha))

# Create empty ~1ha resolution raster for resampling
res <- rast(nrows = nrow(rast_1ha),
            ncols = ncol(rast_1ha),
            crs = crs(rast_1ha),
            ext = ext(rast_1ha))

# Resample to 1ha resolution raster
clim_zone_res <- resample(x = clim_zone,
                          y = res,
                          method = "near")

# Rename layer
names(clim_zone_res) <- "clim_zone"

# Write resampled raster to TIF file
writeRaster(clim_zone_res,
            filename = paste0(proj_path,"GIS/Climate zones/clim_zone_1ha.tif"),
            overwrite = TRUE)
