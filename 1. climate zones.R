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

# Create empty ~1ha resolution raster from original 1km x 1km raster
res <- rast(nrows = nrow(clim_zone) * 9,
            ncols = ncol(clim_zone) * 9,
            crs = crs(clim_zone),
            ext = ext(clim_zone))

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
