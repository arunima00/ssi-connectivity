# Load package
library(terra)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

## To derive Topographic Wetness Index (TWI) in QGIS

# Install SAGA Next Gen plugin
# Load DEM raster layer
# Processing => Toolbox => SAGA Next Gen => terrain_analysis => 
# Topographic wetness index (one step) 
# 1. Select DEM as elevation layer 
# 2. Select Deterministic 8 as flow distribution
# 3. Save output file to directory

# Read DEM and TWI rasters
dem <- rast(paste0(proj_path,"GIS/DEM/WGhats_30m_DEM_SRTM.tif"))
twi <- rast(paste0(proj_path,"GIS/Derived rasters/Topo variables/Extra/TWI merged 30m.tif"))

names(dem) <- "elevation"
names(twi) <- "TWI"

# Read reference 1ha resolution raster
rast_1ha <- rast(paste0(proj_path,"occupancy data/1500 1ha grids/1ha grids.tif"))

# Create loop to derive all topographic variables
for (i in c("aspect","slope","TPI","TRI","roughness")) {
  
  # Derive topographic variable from DEM
  topo <- terrain(dem,v = i)
  
  # Reproject to 1ha raster and write to TIF files
  topo_1ha <- project(x = topo,
                      y = rast_1ha,
                      method = "med",
                      filename = paste0(proj_path,"GIS/Derived rasters/Topo variables/1ha/",i,"_1ha.tif"),
                      overwrite = TRUE)
}

# Reproject DEM and TWI to 1ha resolution and write to TIF files
dem_1ha <- project(x = dem,
                   y = rast_1ha,
                   method = "med",
                   filename = paste0(proj_path,"GIS/Derived rasters/Topo variables/1ha/elevation_1ha.tif"),
                   overwrite = TRUE)

twi_1ha <- project(x = twi,
                   y = rast_1ha,
                   method = "med",
                   filename = paste0(proj_path,"GIS/Derived rasters/Topo variables/1ha/TWI_1ha.tif"),
                   overwrite = TRUE)