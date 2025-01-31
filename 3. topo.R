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
rast_1ha <- rast(paste0(proj_path,"occupancy data/Jobin/1500 1ha grids/1ha grids.tif"))

# Read reference 25ha raster
clim_zone <- rast(paste0(proj_path,"GIS/Climate zones/clim_zone_25ha.tif"))

# Create loop to derive all topographic variables from DEM and reproject
for (i in c("aspect","slope","TPI","TRI","roughness")) {
  
  # Derive topographic variable "i"
  topo <- terrain(dem,v = i)
  
  # Reproject to 1ha and 25ha rasters and write to TIF file
  topo_1ha <- project(x = topo,
                      y = rast_1ha,
                      method = "med",
                      filename = paste0(proj_path,"GIS/Derived rasters/Topo variables/1ha/",i,"_1ha.tif"),
                      overwrite = TRUE)
  
  topo_25ha <- project(x = topo,
                       y = clim_zone,
                       method = "med",
                       filename = paste0(proj_path,"GIS/Derived rasters/Topo variables/25ha/",i,"_25ha.tif"),
                       overwrite = TRUE)
}

# Reproject DEM to 1ha and 25ha write to TIF file
dem_1ha <- project(x = dem,
                   y = rast_1ha,
                   method = "med",
                   filename = paste0(proj_path,"GIS/Derived rasters/Topo variables/1ha/elevation_1ha.tif"),
                   overwrite = TRUE)

dem_25ha <- project(x = dem,
                    y = clim_zone,
                    method = "med",
                    filename = paste0(proj_path,"GIS/Derived rasters/Topo variables/25ha/elevation_25ha.tif"),
                    overwrite = TRUE)

# Reproject TWI 1ha and 25ha resolutions and write to TIF files
twi_1ha <- project(x = twi,
                   y = rast_1ha,
                   method = "med",
                   filename = paste0(proj_path,"GIS/Derived rasters/Topo variables/1ha/TWI_1ha.tif"),
                   overwrite = TRUE)

twi_25ha <- project(x = twi,
                    y = clim_zone,
                    method = "med",
                    filename = paste0(proj_path,"GIS/Derived rasters/Topo variables/25ha/TWI_25ha.tif"),
                    overwrite = TRUE)