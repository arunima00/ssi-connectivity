# Load package
library(terra)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

# Read climate zone raster
clim_zone <- rast(paste0(proj_path,"GIS/Climate zones/ClassifiedSumSWGPrecandTas.tif"))

# Specify from-to values for reclassification
m <- c(17, 1,
       27, 2,
       37, 3,
       47, 4,
       16, 5, 
       26, 6,
       36, 7,
       46, 8,
       15, 9,
       25, 10,
       35, 11,
       45, 12,
       14, 13,
       24, 14,
       34, 15,
       44, 16,
       13, 17,
       23, 18,
       33, 19,
       43, 20,
       12, 21,
       22, 22,
       32, 23,
       42, 24,
       11, 25,
       21, 26,
       31, 27,
       41, 28)

# Convert to matrix
rclmat <- matrix(m, ncol = 2, byrow = TRUE)

# Reclassify raster
clim_zone <- classify(clim_zone, rcl = rclmat)

# Convert to categorical raster and setlevels
levels(clim_zone) <- data.frame(id = 1:28,zone = 1:28)

# Read reference 1ha resolution raster
rast_1ha <- rast(paste0(proj_path,"GIS/1ha grids.tif"))

# Create empty ~1ha resolution raster for resampling
res <- rast(nrows = nrow(rast_1ha),
            ncols = ncol(rast_1ha),
            crs = crs(rast_1ha),
            ext = ext(rast_1ha))

# Reproject climate zone raster to reference CRS
clim_zone <- project(clim_zone,y = crs(rast_1ha))

# Rename layer
names(clim_zone) <- "clim_zone"

# Resample to 1ha resolution raster and write to TIF file
clim_zone_res <- resample(x = clim_zone,
                          y = res,
                          method = "near",
                          filename = paste0(proj_path,"GIS/Climate zones/clim_zone_1ha.tif"),
                          overwrite = TRUE)