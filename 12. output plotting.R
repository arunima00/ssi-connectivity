# Load packages
library(terra)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

# Get file names for all output files
files <- list.files(path = paste0(proj_path,"Omniscape/Output"),
                    pattern = ".tif$",
                    full.names = TRUE,
                    recursive = TRUE)

# Loop to plot each output map
for (i in files) {
  
  # Read raster
  r <- rast(i)
  
  # Convert raster to dataframe
  r_df <- as.data.frame(r,xy = TRUE,na.rm = FALSE) 
  
  # Bin raster values into 100 bins
  binned <- ntile(r_df[[3]],100)
  
  # Rasterize dataframe with binned values
  r_binned <- rast(cbind(r_df[,c("x","y")],binned),crs = "epsg:4326")
  
  # Plot binned raster and save as PNG
  png(filename = sub(".tif",".png",i))
  plot(r_binned,col = map.pal("inferno",100))
  dev.off()
}
