# Load packages
library(terra)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

# Create loop to clip and mask variables to all regions
for (region in c("nil1400_1ha","pa1400_1ha","nilpa1400_1ha")) {
  
  # Read shapefiles based on region
  if (region == "nil1400_1ha") {
    shp <- vect(paste0(proj_path,"GIS/Shapefiles/Nilgiri1400m/Nilgiri1400m.shp"))
  }
  
  if (region == "pa1400_1ha") {
    shp <- vect(paste0(proj_path,"GIS/Shapefiles/PalaniAanamalai1400m/PalaniAanamalai1400m.shp"))
  }
  
  if (region == "nilpa1400_1ha") {
    shp1 <- vect(paste0(proj_path,"GIS/Shapefiles/Nilgiri1400m/Nilgiri1400m.shp"))
    shp2 <- vect(paste0(proj_path,"GIS/Shapefiles/PalaniAanamalai1400m/PalaniAanamalai1400m.shp"))
    
    shp <- rbind(shp1,shp2)
    
    if (! dir.exists(paste0(proj_path,"GIS/Shapefiles/Nilgiri_PA_1400m"))) {
      dir.create(paste0(proj_path,"GIS/Shapefiles/Nilgiri_PA_1400m"),recursive = TRUE)
    }
    
    writeVector(shp,
                filename = paste0(proj_path,"GIS/Shapefiles/Nilgiri_PA_1400m/Nilgiri_PA_1400m.shp"),
                overwrite = TRUE)
  }
  
  # Read canopy height rasters
  ch_2000 <- rast(paste0(proj_path,"GIS/Canopy/Canopy height/2000/canopy height_2000_1ha.tif"))
  ch_2020 <- rast(paste0(proj_path,"GIS/Canopy/Canopy height/2020/canopy height_2020_1ha.tif"))
  
  # Read stack of topographic variable layers
  files_topo <- list.files(paste0(proj_path,"GIS/Derived rasters/Topo variables/1ha"),
                           pattern = ".tif$",
                           full.names = TRUE)
  topo <- rast(files_topo)
  
  # Read stack of distance variable layers
  dist_2017 <- c(rast(paste0(proj_path,"GIS/Derived rasters/Distance/dist_settlements_2017_1ha.tif")),
                 rast(paste0(proj_path,"GIS/Derived rasters/Distance/dist_shola_2017_1ha.tif")),
                 rast(paste0(proj_path,"GIS/Derived rasters/Distance/dist_woodland_2017_1ha.tif")),
                 rast(paste0(proj_path,"GIS/Derived rasters/Distance/dist_grassland_2017_1ha.tif")))
  
  dist_1995 <- c(rast(paste0(proj_path,"GIS/Derived rasters/Distance/dist_settlements_1995_1ha.tif")),
                 rast(paste0(proj_path,"GIS/Derived rasters/Distance/dist_shola_1995_1ha.tif")),
                 rast(paste0(proj_path,"GIS/Derived rasters/Distance/dist_woodland_1995_1ha.tif")),
                 rast(paste0(proj_path,"GIS/Derived rasters/Distance/dist_grassland_1995_1ha.tif")))
  
  # Read climate zones raster
  clim_zone <- rast(paste0(proj_path,"GIS/Climate zones/clim_zone_1ha.tif"))
  
  # Read percentage land cover raster stacks
  landcov_2017 <- c(rast(paste0(proj_path,"GIS/Derived rasters/Land cover/cover_2017_1ha/Shola_Forest_cover.tif")),
                    rast(paste0(proj_path,"GIS/Derived rasters/Land cover/cover_2017_1ha/Timber_Plantations_cover.tif")),
                    rast(paste0(proj_path,"GIS/Derived rasters/Land cover/cover_2017_1ha/Woodland_cover.tif")),
                    rast(paste0(proj_path,"GIS/Derived rasters/Land cover/cover_2017_1ha/Montane_Grassland_cover.tif")))
  landcov_1995 <- c(rast(paste0(proj_path,"GIS/Derived rasters/Land cover/cover_1995_1ha/Shola_Forest_cover.tif")),
                    rast(paste0(proj_path,"GIS/Derived rasters/Land cover/cover_1995_1ha/Timber_Plantations_cover.tif")),
                    rast(paste0(proj_path,"GIS/Derived rasters/Land cover/cover_1995_1ha/Woodland_cover.tif")),
                    rast(paste0(proj_path,"GIS/Derived rasters/Land cover/cover_1995_1ha/Montane_Grassland_cover.tif")))
  
  # Read categorical land cover rasters
  landcov_2017_cat <- rast(paste0(proj_path,"GIS/Land cover/2017D/2017D_rast_1ha.tif"))
  landcov_1995_cat <- rast(paste0(proj_path,"GIS/Land cover/1995D/1995D_rast_1ha.tif"))
  
  # Reproject shapefile to project CRS
  shp <- project(shp,y = crs(clim_zone))
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Input/",region,"/predictors_all"))){
    dir.create(paste0(proj_path,"SDM/Input/",region,"/predictors_all"),
               recursive = TRUE)
  }
  
  # Crop and mask canopy height layers to specified region and write to TIF files
  ch_2000_clip <- crop(x = ch_2000,
                       y = shp,
                       mask = TRUE,
                       filename = paste0(proj_path,"SDM/Input/",region,"/predictors_all/canopyheight_2000.tif"),
                       overwrite = TRUE)
  
  ch_2020_clip <- crop(x = ch_2020,
                       y = shp,
                       mask = TRUE,
                       filename = paste0(proj_path,"SDM/Input/",region,"/predictors_all/canopyheight_2020.tif"),
                       overwrite = TRUE)
  
  # Crop and mask topographic variables raster stack to specified region
  topo_clip <- crop(x = topo,y = shp,mask = TRUE)
  
  # Write cropped and masked layers to TIF files
  output_paths <- c()
  for (i in names(topo)) {
    output_paths <- c(output_paths,
                      paste0(proj_path,"SDM/Input/",region,"/predictors_all/",i,".tif"))
  }
  
  writeRaster(topo_clip,filename = output_paths,overwrite = TRUE)
  
  # Crop and mask distance variables raster stack to specified region
  dist_2017_clip <- crop(x = dist_2017,y = shp,mask = TRUE)
  dist_1995_clip <- crop(x = dist_1995,y = shp,mask = TRUE)
  
  # Write cropped and masked layers to TIF files
  output_paths_2017 <- c()
  for (i in names(dist_2017)){
    output_paths_2017 <- c(output_paths_2017,
                           paste0(proj_path,"SDM/Input/",region,"/predictors_all/",i,"_2017.tif"))
  }
  
  output_paths_1995 <- c()
  for (i in names(dist_1995)){
    output_paths_1995 <- c(output_paths_1995,
                           paste0(proj_path,"SDM/Input/",region,"/predictors_all/",i,"_1995.tif"))
  }
  
  writeRaster(dist_2017_clip,filename = output_paths_2017,overwrite = TRUE)
  writeRaster(dist_1995_clip,filename = output_paths_1995,overwrite = TRUE)
  
  # Crop and mask climate zone layer to specified region and write to TIF file
  clim_zone_clip <- crop(x = clim_zone,
                         y = shp,
                         mask = TRUE,
                         filename = paste0(proj_path,"SDM/Input/",region,"/predictors_all/clim_zone.tif"),
                         overwrite = TRUE)
  
  # Crop and mask land cover raster stack to specified region
  landcov_2017_clip <- crop(x = landcov_2017,y = shp,mask = TRUE)
  landcov_1995_clip <- crop(x = landcov_1995,y = shp,mask = TRUE)
  
  # Write cropped and masked layers to TIF files
  output_paths_2017 <- c()
  for (i in names(landcov_2017)){
    output_paths_2017 <- c(output_paths_2017,
                           paste0(proj_path,"SDM/Input/",region,"/predictors_all/",i,"_2017.tif"))
  }
  
  output_paths_1995 <- c()
  for (i in names(landcov_1995)){
    output_paths_1995 <- c(output_paths_1995,
                           paste0(proj_path,"SDM/Input/",region,"/predictors_all/",i,"_1995.tif"))
  }
  
  writeRaster(landcov_2017_clip,filename = output_paths_2017,overwrite = TRUE)
  writeRaster(landcov_1995_clip,filename = output_paths_1995,overwrite = TRUE)
  
  # Write cropped and masked categorical land cover rasters to TIF files
  landcov_2017_cat_clip <- crop(x = landcov_2017_cat,
                                y = shp,
                                mask = TRUE,
                                filename = paste0(proj_path,"SDM/Input/",region,"/predictors_all/landcov_2017.tif"),
                                overwrite = TRUE)
  
  landcov_1995_cat_clip <- crop(x = landcov_1995_cat,
                                y = shp,
                                mask = TRUE,
                                filename = paste0(proj_path,"SDM/Input/",region,"/predictors_all/landcov_1995.tif"),
                                overwrite = TRUE)
  
  # Create raster stacks for past and present for forest and grassland species and save
  predictors_present_f <- c(ch_2020_clip,
                            subset(topo_clip,"elevation",negate = TRUE),
                            subset(dist_2017_clip,"dist_grassland",negate = TRUE),
                            clim_zone_clip,
                            subset(landcov_2017_clip,"Montane_Grassland_cover",negate = TRUE))
  
  predictors_past_f <- c(ch_2000_clip,
                         subset(topo_clip,"elevation",negate = TRUE),
                         subset(dist_1995_clip,"dist_grassland",negate = TRUE),
                         clim_zone_clip,
                         subset(landcov_1995_clip,"Montane_Grassland_cover",negate = TRUE))
  
  writeRaster(predictors_present_f,
              filename = paste0(proj_path,"SDM/Input/",region,"/predictors_present_forest.tif"),
              overwrite = TRUE)
  
  writeRaster(predictors_past_f,
              filename = paste0(proj_path,"SDM/Input/",region,"/predictors_past_forest.tif"),
              overwrite = TRUE)
  
  if (region == "nilpa1400_1ha") {
    predictors_present_g <- c(ch_2020_clip,
                              topo_clip,
                              dist_2017_clip[["dist_grassland"]],
                              clim_zone_clip,
                              landcov_2017_clip[["Montane_Grassland_cover"]])
    
    predictors_past_g <- c(ch_2000_clip,
                           topo_clip,
                           dist_1995_clip[["dist_grassland"]],
                           clim_zone_clip,
                           landcov_1995_clip[["Montane_Grassland_cover"]])
    
    writeRaster(predictors_present_g,
                filename = paste0(proj_path,"SDM/Input/",region,"/predictors_present_grassland.tif"),
                overwrite = TRUE)
    
    writeRaster(predictors_past_g,
                filename = paste0(proj_path,"SDM/Input/",region,"/predictors_past_grassland.tif"),
                overwrite = TRUE)
  }
}