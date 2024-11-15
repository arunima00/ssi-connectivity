# Load packages
library(tidyverse)
library(terra)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

# Read occupancy data for forest species to dataframe
df <- read.csv(paste0(proj_path,"occupancy data/Jobin/clipped_areas_added_w_sites1.csv"))

# Create loop to separate datasets for each species
for (i in c("SHAL","SHMA","MOFA","MOCA")) {
  
  ## White-bellied Sholakili
  if (i == "SHAL"){
    # Filter data for Sholakilis
    df_sel <- cbind(df[,c("Site","Longitude","Latitude")],
                    df %>% select(ends_with("SHKL")))
    
    # Read shapefile for Palani-Anamalai-Highwavies 1400m contour
    shp <- vect(paste0(proj_path,"GIS/Shapefiles/PA_HW_1400m/PA_HW_1400m.shp"))
  }
  
  ## Palani Laughingthrush
  if (i == "MOFA"){
    # Filter data for Laughingthrushes
    df_sel <- cbind(df[,c("Site","Longitude","Latitude")],
                    df %>% select(ends_with("LATH")))
    
    # Read shapefile for Palani-Anamalai-Highwavies 1400m contour polygon
    shp <- vect(paste0(proj_path,"GIS/Shapefiles/PA_HW_1400m/PA_HW_1400m.shp"))
  }
  
  ## Nilgiri Sholakili
  if (i == "SHMA"){
    # Filter data for Sholakilis
    df_sel <- cbind(df[,c("Site","Longitude","Latitude")],
                    df %>% select(ends_with("SHKL")))
    
    # Read shapefile for Nilgiris 1400m contour polygon
    shp <- vect(paste0(proj_path,"GIS/Shapefiles/Nilgiri1400m/Nilgiri1400m.shp"))
  }
  
  ## Nilgiri Laughingthrush
  if (i == "MOCA"){
    # Filter data for Laughingthrushes
    df_sel <- cbind(df[,c("Site","Longitude","Latitude")],
                    df %>% select(ends_with("LATH")))
    
    # Read shapefile for Nilgiris 1400m contour polygon
    shp <- vect(paste0(proj_path,"GIS/Shapefiles/Nilgiri1400m/Nilgiri1400m.shp"))
  }
  
  ## Nilgiri Pipit
  if (i == "ANNI"){
    # Read nilgiri pipit dataset
    df <- read.csv(paste0(proj_path,"/occupancy data/Jobin/dataset for pipit analysis.csv"))
    
    # Extract relevant columns only
    df_sel <- cbind(df[,c("Site","Longitude","Latitude")],
                    df %>% select(ends_with("ANNI")))
    
    # Read shapefile for Nilgiris 1400m contour polygon
    shp <- vect(paste0(proj_path,"GIS/Shapefiles/Nilgiri1400m/Nilgiri1400m.shp"))
  }
  
  # Filter data for species-specific region
  df_sel <- filter(df_sel,
                   Longitude >= xmin(shp),
                   Longitude <= xmax(shp),
                   Latitude >= ymin(shp),
                   Latitude <= ymax(shp))
  
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
  df_final <- cbind(df_sel[c("Longitude","Latitude")],
                    pres,
                    det_prob,
                    avg)
  
  # Rename columns
  colnames(df_final)[3:5] <- c("Presence",
                               "Detection probability",
                               "Abundance avg.")

  # Write final occupancy data for each species to CSV files
  write.csv(df_final,
            file = paste0(proj_path,"occupancy data/Jobin/Filtered/",i,".csv"),
            row.names = FALSE)
}