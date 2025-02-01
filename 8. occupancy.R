# Load packages
library(tidyverse)
library(terra)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

# Read occupancy data for forest species to dataframe
df <- read.csv(paste0(proj_path,"occupancy data/new dataset forest species.csv"))

# Convert to points layer
df_vect <- vect(df,geom = c("Longitude","Latitude"),crs = "epsg:4326")

# Read reference 1ha resolution raster
rast_1ha <- rast(paste0(proj_path,"occupancy data/1500 1ha grids/1ha grids.tif"))

# Reproject to reference CRS
df_vect <- project(df_vect,y = crs(rast_1ha))

# Convert back to dataframe
df <- as.data.frame(df_vect,row.names = NULL,geom = "XY")

# Create loop to separate datasets for each species
for (i in c("SHAL","SHMA","MOFA","MOCA")) {
  
  ## White-bellied Sholakili
  if (i == "SHAL"){
    # Filter data for Sholakilis
    df_sel <- cbind(df[,c("Site","x","y")],
                    df %>% select(ends_with("SHKL")))
    
    # Read shapefile for Palani-Anamalai-Highwavies 1400m contour
    shp <- vect(paste0(proj_path,"GIS/Shapefiles/PA_HW_1400m/PA_HW_1400m.shp"))
  }
  
  ## Palani Laughingthrush
  if (i == "MOFA"){
    # Filter data for Laughingthrushes
    df_sel <- cbind(df[,c("Site","x","y")],
                    df %>% select(ends_with("LATH")))
    
    # Read shapefile for Palani-Anamalai-Highwavies 1400m contour polygon
    shp <- vect(paste0(proj_path,"GIS/Shapefiles/PA_HW_1400m/PA_HW_1400m.shp"))
  }
  
  ## Nilgiri Sholakili
  if (i == "SHMA"){
    # Filter data for Sholakilis
    df_sel <- cbind(df[,c("Site","x","y")],
                    df %>% select(ends_with("SHKL")))
    
    # Read shapefile for Nilgiris 1400m contour polygon
    shp <- vect(paste0(proj_path,"GIS/Shapefiles/Nilgiri1400m/Nilgiri1400m.shp"))
  }
  
  ## Nilgiri Laughingthrush
  if (i == "MOCA"){
    # Filter data for Laughingthrushes
    df_sel <- cbind(df[,c("Site","x","y")],
                    df %>% select(ends_with("LATH")))
    
    # Read shapefile for Nilgiris 1400m contour polygon
    shp <- vect(paste0(proj_path,"GIS/Shapefiles/Nilgiri1400m/Nilgiri1400m.shp"))
  }
  
  # Reproject shapefile to reference CRS
  shp <- project(shp,y = crs(rast_1ha))
  
  # Filter data for species-specific region
  df_sel <- filter(df_sel,
                   x >= xmin(shp),
                   x <= xmax(shp),
                   y >= ymin(shp),
                   y <= ymax(shp))
  
  # Remove all rows with NAs
  df_sel <- df_sel %>% na.omit()
  
  # Convert individual counts across 4 replicates per grid to 1/0 presence-absence
  pres<-c()
  for (j in 1:nrow(df_sel)){
    if (sum(df_sel[j,4:7]) != 0){
      pres[j] <- 1
    }
    if (sum(df_sel[j,4:7]) == 0){
      pres[j] <- 0
    } 
  }
  
  ## Calculate detection probability for each grid
  
  # Convert individual count for each replicate to 1/0 presence/absence
  df_sel2 <- df_sel[,c(4:7)]
  df_sel2[df_sel2 >= 1] <- 1
  
  # Average 1/0 values across replicates to get detection probability per grid
  det_prob <- rowMeans(df_sel2)

  # Calculate average species abundance for each grid
  avg <- rowMeans(df_sel[,c(4:7)])

  # Create new dataframe with grid lat-long, species presence/absence, 
  # detection probability and average abundance
  df_final <- cbind(df_sel[c("x","y")],
                    pres,
                    det_prob,
                    avg)
  
  # Rename columns
  colnames(df_final) <- c("Longitude",
                          "Latitude",
                          "Presence",
                          "Detection probability",
                          "Abundance avg.")

  # Write final occupancy data for each species to CSV files
  write.csv(df_final,
            file = paste0(proj_path,"occupancy data/Filtered/",i,".csv"),
            row.names = FALSE)
}

## Nilgiri Pipit

# Read Nilgiri Pipit dataset
df_anni <- read.csv(paste0(proj_path,"occupancy data/dataset for pipit analysis.csv"))

# Extract relevant columns
df_sel <- cbind(df_anni[,c("Site","Longitude","Latitude")],
                df_anni %>% select(ends_with("ANNI")))

# Convert individual counts across 4 replicates per grid to 1/0 presence-absence
pres<-c()
for (j in 1:nrow(df_sel)){
  if (any(df_sel[j,4:7] != 0,na.rm = TRUE)){
    pres[j] <- 1
  }
  if (all(df_sel[j,4:7] == 0,na.rm = TRUE)){
    pres[j] <- 0
  } 
}

# Create new dataframe with grid lat-long and presence/absence
df_final <- cbind(df_sel[c("Longitude","Latitude")],pres)

# Convert to points layer
df_vect <- vect(df_final,geom = c("Longitude","Latitude"),crs = "epsg:4326")

# Reproject to reference CRS
df_vect <- project(df_vect,y = crs(rast_1ha))

# Convert back to dataframe
df <- as.data.frame(df_vect,row.names = NULL,geom = "XY")

colnames(df) <- c("Presence","Longitude","Latitude")

write.csv(df,
          file = "occupancy data/Filtered/ANNI.csv",
          row.names = FALSE)