# Load packages
library(tidyverse)
library(terra)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

# Loop for each scenario - past and present
for (y in c("1995","2017")) {
  
  # Get file names for all output files
  files <- list.files(path = paste0(proj_path,"Omniscape/Output/",y),
                      pattern = ".tif$",
                      full.names = TRUE,
                      recursive = TRUE)
  
  # Loop to plot each map
  for (i in files){
    
    # Read raster
    r <- rast(i)
    
    # Plot cumulative current flow and flow potential maps 
    if (! grepl("normalized_cum_currmap",i,fixed = TRUE)) {
      
      # Get quantiles of raster values
      quantiles <- quantile(values(r), 
                            probs = seq(0, 1, length.out = 5), 
                            na.rm = TRUE)
      
      # Plot the raster with quantile breaks and save as PNG
      png(filename=sub(".tif",".png",i))
      plot(r, 
           breaks = quantiles,
           col = map.pal("viridis",4), 
           mar = c(3.1,3.1,2.1,7.7))
      dev.off()
    }
    
    # Plot normalised cumulative current flow maps and save as PNG
    if (grepl("normalized_cum_currmap",i,fixed = TRUE)) {
      png(filename=sub(".tif",".png",i))
      plot(r, 
           breaks = c(0,0.7,1.3,1.7,max(values(r),na.rm = TRUE)),
           col = map.pal("viridis",4), 
           mar = c(3.1,3.1,2.1,7.7))
      dev.off()
    }
  }
}
