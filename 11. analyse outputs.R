# Load packages
library(tidyverse)
library(ggh4x)
library(terra)
library(cowplot)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

# Read all Omniscape output files
files_1995 <- list.files(path = paste0(proj_path,"Omniscape/Output/1995"),
                         pattern = ".tif$",
                         recursive = TRUE,
                         full.names = TRUE)

files_2017 <- list.files(path = paste0(proj_path,"Omniscape/Output/2017"),
                         pattern = ".tif$",
                         recursive = TRUE,
                         full.names = TRUE)

files <- c(files_1995,files_2017)
files <- files[! grepl("percentile",files) & ! grepl("std_stretch",files)]

# Create loop to plot all output maps
for (i in files) {
  
  # Read raster
  r <- rast(i)
  
  if (names(r) == "flow_potential") {
    names(r) <- "FP"
  }
  
  if (names(r) == "normalized_cum_currmap") {
    names(r) <- "NCF"
  }
  
  if (names(r) == "cum_currmap") {
    names(r) <- "CCF"
  }
  
  r <- project(r,"epsg:4326")
  
  # Convert raster to dataframe
  r_df <- as.data.frame(r,xy = TRUE)
  
  if (names(r) != "NCF") {
    
    # Define color scale
    colors <- scales::pal_viridis()(100)
    value_breaks <- quantile(r_df[,names(r)],probs = seq(0,1,by = 0.01))
    
    # Plot raster on percentile colour scale and save PNG
    ggplot(r_df,aes(x,y,fill = .data[[names(r)]])) +
      geom_tile() +
      scale_fill_gradientn(name = names(r),
                           colors = colors,
                           values = scales::rescale(value_breaks),
                           guide = guide_colorbar(barheight = 10)) +
      theme_classic() +
      theme(legend.position = "right",
            axis.title = element_blank()) +
      coord_fixed()
    
    ggsave(filename = sub(".tif",".png",i),
           width = 4500,
           height = 4500,
           units = "px",
           dpi = 600)
  }
  
  # Bin NCF values into percentiles
  if (names(r) == "NCF") {
    
    # Compute percentiles
    r_df <- r_df %>%
      mutate(percentile = ecdf(NCF)(NCF))
    
    # Rasterise dataframe with percentile values and write to TIF file
    r <- rast(x = r_df[,c("x","y","percentile")],
              crs = crs(r))
    
    writeRaster(x = r,
                filename = sub(".tif","_percentile.tif",i),
                overwrite = TRUE)
    
    # Define color scale
    colors <- scales::pal_viridis()(100)
    value_breaks <- quantile(r_df[,"NCF"], probs = seq(0,1,by = 0.01))
    
    # Plot raster on percentile colour scale and save PNG
    ggplot(r_df,aes(x,y,fill = NCF)) +
      geom_tile() +
      scale_fill_gradientn(colors = colors,
                           values = scales::rescale(value_breaks),
                           breaks = c(value_breaks[1],1,value_breaks[101]),
                           labels = round(c(value_breaks[1],1,value_breaks[101]), 2),
                           guide = guide_colorbar(barheight = 10)) +
      theme_classic() +
      theme(legend.position = "right",
            axis.title = element_blank()) +
      coord_fixed()
    
    ggsave(filename = sub(".tif",".png",i),
           width = 4500,
           height = 4500,
           units = "px",
           dpi = 600)
  }
  
  # Convert raw CCF values to standard deviation stretched values
  if (names(r) == "CCF") {
    
    # Calculate standard deviation and mean of raw CCF values
    std <- sd(values(r,na.rm = TRUE))
    avg <- mean(values(r,na.rm = TRUE))
    
    # Transform raw CCF values and save raster with transformed values
    r <- (r - avg)/std
    
    writeRaster(x = r,
                filename = sub(".tif","_std_stretch.tif",i),
                overwrite = TRUE)
    
    # Convert raster to dataframe
    r_df <- as.data.frame(r,xy = TRUE)
    
    # Define color scale
    colors <- scales::pal_viridis()(100)
    value_breaks <- quantile(r_df[,"CCF"], probs = seq(0,1,by = 0.01))
    
    # Plot raster on percentile colour scale and save PNG
    ggplot(r_df, aes(x,y,fill = CCF)) +
      geom_tile() +
      scale_fill_gradientn(name = "CCF (std stretched)",
                           colors = colors,
                           values = scales::rescale(value_breaks),
                           breaks = c(value_breaks[1],0,value_breaks[101]),
                           labels = round(c(value_breaks[1],0,value_breaks[101]), 2),
                           guide = guide_colorbar(barheight = 10)) +
      theme_classic() +
      theme(legend.position = "right",
            axis.title = element_blank()) +
      coord_fixed()
    
    ggsave(filename = sub(".tif","_std_stretch.png",i),
           width = 4500,
           height = 4500,
           units = "px",
           dpi = 600)
  }
}

## Create change in CCF maps for each model

# Loop over species
for (spec in c("SHAL","SHMA","MOFA","MOCA","ANNI","EUAL","FINI")) {
  
  # Loop for resistance layers
  for (c in c("0.25","2","8")) {
    
    # Loop for species-specific dispersal distances
    if (spec %in% c("SHAL","SHMA","MOFA","MOCA","FINI")) {
      d_list <- c("0.2km","0.5km","1km")
    }
    
    if (spec == "EUAL") {
      d_list <- c("0.5km","1km","1.5km")
    }
    
    if (spec == "ANNI") {
      d_list <- c("1km","3km","5km")
    }
    
    for (d in d_list) {
        
      # Set species-specific region
      if (spec %in% c("SHAL","MOFA")) {
        region <- "pa1400_1ha"      
      }
      
      if (spec %in% c("SHMA","MOCA")) {
        region <- "nil1400_1ha"
      }
      
      if (spec %in% c("EUAL","FINI","ANNI")) {
        region <- "nilpa1400_1ha"
      }
      
      # Read past and present Omniscape output rasters
      past <- rast(paste0(proj_path,"Omniscape/Output/1995/",spec,"_RF_",region,"_c",c,"/",d,"/cum_currmap.tif"))
      present <- rast(paste0(proj_path,"Omniscape/Output/2017/",spec,"_RF_",region,"_c",c,"/",d,"/cum_currmap.tif"))
      
      # Calculate difference between past and present current flow for each pixel
      r <- present - past
      
      if (! dir.exists(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/",spec,"_RF_",region,"_c",c,"_",d))) {
        dir.create(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/",spec,"_RF_",region,"_c",c,"_",d),recursive = TRUE)
      }
      
      # Convert to dataframe
      r_df <- as.data.frame(r,xy = TRUE)
      
      # Define color scale
      colors <- scales::pal_viridis()(100)
      value_breaks <- quantile(r_df[,"cum_currmap"],probs = seq(0,1,by = 0.01))
      
      # Plot difference in current flow raster with a diverging colour scale
      ggplot(r_df, aes(x,y,fill = cum_currmap)) +
        geom_tile() +
        scale_fill_gradientn(colors = colors,
                             values = scales::rescale(value_breaks),
                             breaks = c(value_breaks[1],0,value_breaks[101]),
                             labels = round(c(value_breaks[1],0,value_breaks[101]),2),
                             guide = guide_colorbar(barheight = 10)) +
        theme_classic() +
        theme(legend.position = "right",
              legend.title = element_blank()) +
        coord_fixed()
      
      ggsave(filename = paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/",spec,"_RF_",region,"_c",c,"_",d,"/plot.png"),
             width = 4500,
             height = 4500,
             units = "px",
             dpi = 600)
      
      # Write change in CCF rasters to TIF files
      writeRaster(x = r,
                  filename = paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/",spec,"_RF_",region,"_c",c,"_",d,"/raster.tif"),
                  overwrite = TRUE)
      
      # Calculate standard deviation of change in CCF values
      std <- sd(values(r,na.rm = TRUE))
      
      # Transform change in CCF values
      r <- r/std
      
      # Convert to dataframe
      r_df <- as.data.frame(r,xy = TRUE)
      
      # Define color scale
      colors <- scales::pal_viridis()(100)
      value_breaks <- quantile(r_df[,"cum_currmap"],probs = seq(0,1,by = 0.01))
      
      # Plot transformed change in CCF raster with a diverging colour scale
      p <- ggplot(r_df, aes(x,y,fill = cum_currmap)) +
        geom_tile() +
        scale_fill_gradientn(colors = colors,
                             values = scales::rescale(value_breaks),
                             breaks = c(value_breaks[1],0,value_breaks[101]),
                             labels = round(c(value_breaks[1],0,value_breaks[101]),2),
                             guide = guide_colorbar(barheight = 10)) +
        theme_classic() +
        theme(legend.position = "right",
              legend.title = element_blank()) +
        coord_fixed()
      
      ggsave(filename = paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/",spec,"_RF_",region,"_c",c,"_",d,"/plot_std_stretch.png"),
             plot = p,
             width = 4500,
             height = 4500,
             units = "px",
             dpi = 600)
      
      legend <- get_legend(p)
      
      p_legend <- plot_grid(legend)
      
      ggsave(filename = paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/",spec,"_RF_",region,"_c",c,"_",d,"/plot_std_stretch_legend.png"),
             dpi = 600)
      
      # Write transformed change in CCF rasters to TIF files
      writeRaster(x = r,
                  filename = paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/",spec,"_RF_",region,"_c",c,"_",d,"/raster_std_stretch.tif"),
                  overwrite = TRUE)
    }
  }
}

# Create west-east linegraphs for all species for
# change in CCF and present-day CCF

# Read shapefiles for each region
nil <- vect(paste0(proj_path,"GIS/Shapefiles/Nilgiri1400m/Nilgiri1400m.shp"))
pa <- vect(paste0(proj_path,"GIS/Shapefiles/PalaniAanamalai1400m/PalaniAanamalai1400m.shp"))

# Set colour for each model
cols <- c("#deebf7", "#9ecae1", "#3182bd",
          "#fee6ce", "#fdae6b", "#e6550d",
          "#e5f5e0", "#a1d99b", "#31a354")

# Loop over both types of graphs (change in CCF and present-day CCF)
for (type in c("cc","pc")) {
  
  # Loop over species
  for (spec in c("SH","MO","FINI","EUAL","ANNI")) {
    
    if (type == "pc") {
      
      # Read raster files
      files <- list.files(path = paste0(proj_path,"Omniscape/Output/2017"),
                          pattern = "cum_currmap_std_stretch.tif$",
                          recursive = TRUE,
                          full.names = TRUE)
      
      # Subset files for one species
      files_spec <- files[grepl(spec,files)]
      
      # Extract model name (c value x dispersal distance) from filenames 
      models <- lapply(files_spec,function(x) {
        
        y <- strsplit(x,"/")[[1]]
        c <- strsplit(y[length(y) - 2],"_")[[1]][length(strsplit(y[length(y) - 2],"_")[[1]])]
        d <- y[length(y) - 1]
        
        y <- paste(c,d,sep = "_")
        
        y
      })
      
      type_name <- "CCF"
    }
    
    if (type == "cc") {
      
      # Read raster files
      files <- list.files(path = paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap"),
                          pattern = "_std_stretch.tif$",
                          recursive = TRUE,
                          full.names = TRUE)
      
      # Subset files for one species
      files_spec <- files[grepl(spec,files)]
      
      # Extract model name (c value x dispersal distance) from filenames
      models <- lapply(files_spec,function(x) {
        
        y <- strsplit(x,"/")[[1]]
        z <- strsplit(y[length(y) - 1],"_")[[1]]
        c <- z[length(z) - 1]
        d <- z[length(z)]
        
        y <- paste(c,d,sep = "_")
        
        y
      })
      
      type_name <- "Change in CCF"
    }
    
    # Read raster stacks for each individual region
    if (grepl("nilpa1400",files_spec[1])) {
      st <- rast(files_spec)
      
      names(st) <- models
      
      st <- project(st,"epsg:4326")
      
      st_nil <- crop(st,nil,mask = TRUE)
      st_pa <- crop(st,pa,mask = TRUE)
    }
    
    if (! grepl("nilpa1400",files_spec[1])) {
      
      files_spec_nil <- files_spec[grepl("nil1400",files_spec)]
      st_nil <- rast(files_spec_nil)
      names(st_nil) <- models[match(files_spec_nil,files_spec)]
      
      st_nil <- project(st_nil,"epsg:4326")
      
      files_spec_pa <- files_spec[grepl("pa1400",files_spec)]
      st_pa <- rast(files_spec_pa)
      names(st_pa) <- models[match(files_spec_pa,files_spec)]
      
      st_pa <- project(st_pa,"epsg:4326")
    }
    
    # Loop over both regions
    st_both <- list("Nilgiris" = st_nil,"Palani-Anamalai" = st_pa)
    
    data <- c()
      
    for (region in c("Nilgiris","Palani-Anamalai")) {
      
      # Read raster stack of all models for one region
      st <- st_both[[region]]
      
      df <- c()
      row_names <- c()
      
      # Loop over each individual model
      for (i in 1:nlyr(st)) {
        
        # Extract raster for individual model from stack
        r <- st[[i]]
        
        # Convert raster to dataframe
        r_df <- as.data.frame(r,xy = TRUE)
        
        # Set bin size of each longitudinal slice
        bin_size <- (max(r_df$x) - min(r_df$x))/50
        
        sum_val <- c()
        longs <- c()
        
        # Loop over each slice
        for (j in 1:50) {
          
          # Get values for each slice
          b <- filter(r_df,x >= min(r_df$x) + (j - 1) * bin_size,x < min(r_df$x) + j * bin_size)[,names(r)]
          
          # Calculate median of values in each slice
          c <- median(b,na.rm = TRUE)
          
          # Get list of medians for all slices
          sum_val <- c(sum_val,c)
          
          # Get list of longitude positions of all slices
          longs <- c(longs,min(r_df$x) + (j - 1) * bin_size)
        }
        
        # Get dataframe for medians of slices for each model
        df <- rbind(df,sum_val)
        row_names <- c(row_names,names(r))
      }
        
      row.names(df) <- row_names
      colnames(df) <- longs
      
      # Reshape dataframe for plotting
      data_region <- reshape2::melt(df) %>% 
        na.omit() %>% 
        mutate(Region = region)
      
      colnames(data_region) <- c("Model","Longitude",type_name,"Region")
      
      # Set x-axis breaks for each region
      if (region == "Nilgiris") {
        breaks_nil <- seq(min(data_region$Longitude),max(data_region$Longitude),by = bin_size * 10)
      }
        
      if (region == "Palani-Anamalai") {
        breaks_pa <- seq(min(data_region$Longitude),max(data_region$Longitude),by = bin_size * 10)
      }
      
      # Combine dataframe for both regions
      data <- rbind(data,data_region)
    }
    
    # Reorder species-specific models for plotting
    if (spec %in% c("SH","MO","FINI")) {
      lvls <- c("c0.25_0.2km",
                "c0.25_0.5km",
                "c0.25_1km",
                "c2_0.2km",
                "c2_0.5km",
                "c2_1km",
                "c8_0.2km",
                "c8_0.5km",
                "c8_1km")
    }

    if (spec == "EUAL") {
      lvls <- c("c0.25_0.5km",
                "c0.25_1km",
                "c0.25_1.5km",
                "c2_0.5km",
                "c2_1km",
                "c2_1.5km",
                "c8_0.5km",
                "c8_1km",
                "c8_1.5km")
    }

    if (spec == "ANNI") {
      lvls <- c("c0.25_1km",
                "c0.25_3km",
                "c0.25_5km",
                "c2_1km",
                "c2_3km",
                "c2_5km",
                "c8_1km",
                "c8_3km",
                "c8_5km")
    }
    
    # Plot linegraph for each species for both regions using GAM smoothing
    ggplot(data,aes(x = Longitude,y = .data[[type_name]],fill = Model)) +
      geom_smooth(aes(color = Model),method = "gam") +
      scale_color_manual(values = cols,
                         breaks = lvls) +
      scale_fill_manual(values = cols,
                        breaks = lvls) +
      theme_minimal() + 
      geom_hline(yintercept = 0,colour = "darkgrey") +
      facet_wrap2(~ Region,
                  ncol = 2,nrow = 1,
                  scales = "free_x",
                  strip = strip_vanilla()) +
      theme(axis.line.x = element_blank(),
            panel.grid.major.y = element_blank(), 
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_line(colour = "darkgrey"),
            text = element_text(size = 22),
            legend.key.spacing.y = unit(3,"mm"),) +
      facetted_pos_scales(x = list(Region == "Nilgiris" ~ scale_x_continuous(breaks = breaks_nil,
                                                                             labels = round(breaks_nil,2)),
                                   Region == "Palani-Anamalai" ~ scale_x_continuous(breaks = breaks_pa,
                                                                                    labels = round(breaks_pa,2)))) + 
      scale_y_continuous(limits = function(x) {c(-max(abs(x)), max(abs(x)))},
                        breaks = function(x) {pretty(c(-max(abs(x)), max(abs(x))))})
    
    # Save plots
    ggsave(filename = paste0(proj_path,"Presentation/Paper figures/",spec,"_",type,"_linegraph.png"),
           width = 560,
           height = 150,
           units = "mm",
           dpi = 600)
  }
}

# Loop over both types of graphs (change in CCF and present-day CCF)
for (type in c("pc","cc")) {
  
  # Read rasters for each type
  if (type == "pc") {
    
    shal <- rast(paste0(proj_path,"Omniscape/Output/2017/SHAL_RF_pa1400_1ha_c8/0.5km/cum_currmap_std_stretch.tif"))
    shma <- rast(paste0(proj_path,"Omniscape/Output/2017/SHMA_RF_nil1400_1ha_c8/0.5km/cum_currmap_std_stretch.tif"))
    mofa <- rast(paste0(proj_path,"Omniscape/Output/2017/MOFA_RF_pa1400_1ha_c8/0.5km/cum_currmap_std_stretch.tif"))
    moca <- rast(paste0(proj_path,"Omniscape/Output/2017/MOCA_RF_nil1400_1ha_c8/0.5km/cum_currmap_std_stretch.tif"))
    anni <- rast(paste0(proj_path,"Omniscape/Output/2017/ANNI_RF_nilpa1400_1ha_c0.25/3km/cum_currmap_std_stretch.tif"))
    eual <- rast(paste0(proj_path,"Omniscape/Output/2017/EUAL_RF_nilpa1400_1ha_c8/1km/cum_currmap_std_stretch.tif"))
    fini <- rast(paste0(proj_path,"Omniscape/Output/2017/FINI_RF_nilpa1400_1ha_c8/0.5km/cum_currmap_std_stretch.tif"))
    
    type_name <- "CCF"
  }
  
  if (type == "cc") {
    shal <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/SHAL_RF_pa1400_1ha_c8_0.5km/raster_std_stretch.tif"))
    shma <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/SHMA_RF_nil1400_1ha_c8_0.5km/raster_std_stretch.tif"))
    mofa <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/MOFA_RF_pa1400_1ha_c8_0.5km/raster_std_stretch.tif"))
    moca <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/MOCA_RF_nil1400_1ha_c8_0.5km/raster_std_stretch.tif"))
    anni <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/ANNI_RF_nilpa1400_1ha_c0.25_3km/raster_std_stretch.tif"))
    eual <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/EUAL_RF_nilpa1400_1ha_c8_1km/raster_std_stretch.tif"))
    fini <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/FINI_RF_nilpa1400_1ha_c8_0.5km/raster_std_stretch.tif"))
    
    type_name <- "Change in CCF"
  }
  
  # Loop over both regions
  data <- c()
  lc_df <- c()
  
  for (region in c("Nilgiris","Palani-Anamalai")) {
    
    # Load shapefiles and species list for each region
    if (region == "Nilgiris") {
      shp <- vect(paste0(proj_path,"GIS/Shapefiles/Nilgiri1400m/Nilgiri1400m.shp"))
      maps <- list(anni,eual,moca,fini,shma)
      species <- c("ANNI","EUAL","MOCA","FINI","SHMA")
    }
    
    if (region == "Palani-Anamalai") {
      shp <- vect(paste0(proj_path,"GIS/Shapefiles/PalaniAanamalai1400m/PalaniAanamalai1400m.shp"))
      maps <- list(anni,eual,mofa,fini,shal)
      species <- c("ANNI","EUAL","MOFA","FINI","SHAL")
    }
    
    # Loop over species
    df <- c()
    
    for (i in seq_along(species)) {
      
      # Extract raster for each species from list
      r <- maps[[i]]
      names(r) <- "std_stretch"
      
      # Reproject raster to EPSG:4326 WGS84 CRS
      r <- project(r,"epsg:4326")
      
      # Crop raster to each region
      r_clip <- crop(r,shp,mask = TRUE)
      
      # Convert raster to dataframe
      r_df <- as.data.frame(r_clip,xy = TRUE)
      
      # Set bin size of each longitudinal slice
      bin_size <- (max(r_df$x) - min(r_df$x))/50
      
      sum_val <- c()
      longs <- c()
      
      # Loop over each slice
      for (j in 1:50) {
        
        # Get values for each slice
        b <- filter(r_df,x >= min(r_df$x) + (j - 1) * bin_size,x < min(r_df$x) + j * bin_size)[,3]
        
        # Calculate median of values in each slice
        c <- median(b,na.rm = TRUE)
        
        # Get list of medians for all slices
        sum_val <- c(sum_val,c)
        
        # Get list of longitude positions of all slices
        longs <- c(longs,min(r_df$x) + (j - 1) * bin_size)
      }
      
      # Get dataframe for medians of slices for each species
      df <- rbind(df,sum_val)
    }
    
    row.names(df) <- species
    colnames(df) <- longs
    
    # Reshape dataframe for plotting
    data_region <- reshape2::melt(df) %>% 
      na.omit() %>% 
      mutate(Region = region)
    
    colnames(data_region) <- c("Species","Longitude",type_name,"Region")
    
    # Set x-axis breaks for each region
    if (region == "Nilgiris") {
      breaks_nil <- seq(min(data_region$Longitude),max(data_region$Longitude),by = bin_size * 10)
    }
    
    if (region == "Palani-Anamalai") {
      breaks_pa <- seq(min(data_region$Longitude),max(data_region$Longitude),by = bin_size * 10)
    }
    
    # Combine dataframe for both regions
    data <- rbind(data,data_region)
  }
  
  # Set colour for plotting for each species
  spec_cols <- c("#74c476",
                 "#9ecae1",
                 "#4292c6",
                 "#084594",
                 "#fc9272",
                 "#fb6a4a",
                 "#ef3b2c")
  spec_names <- c("ANNI",
                  "EUAL",
                  "MOCA",
                  "MOFA",
                  "FINI",
                  "SHMA",
                  "SHAL")
  
  # Plot linegraphs
  ggplot(data,aes(x = Longitude,y = .data[[type_name]],fill = Species))+
    geom_smooth(aes(color = Species),method = "gam") +
    scale_color_manual(values = spec_cols,
                       breaks = spec_names) +
    scale_fill_manual(values = spec_cols,
                      breaks = spec_names) +
    theme_minimal() + 
    geom_hline(yintercept = 0,colour = "black") +
    facet_wrap2(~ Region,ncol = 2,nrow = 1,
                scales = "free_x",
                strip = strip_vanilla()) +
    theme(axis.line.x = element_blank(),
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line(colour = "black"),
          legend.title = element_blank(),
          strip.text = element_blank(),
          legend.key.spacing.y = unit(3,"mm"),
          text = element_text(size = 22,face = "bold"),
          legend.box.spacing = unit(100,"pt")          ) +
    facetted_pos_scales(x = list(Region == "Nilgiris" ~ scale_x_continuous(breaks = breaks_nil,
                                                                           labels = round(breaks_nil,2)),
                                 Region == "Palani-Anamalai" ~ scale_x_continuous(breaks = breaks_pa,
                                                                                  labels = round(breaks_pa,2)))) + 
    scale_y_continuous(limits = function(x) {c(- max(abs(x)),max(abs(x)))},
                       breaks = function(x) {pretty(c(-max(abs(x)), max(abs(x))))})
  
  # Save linegraphs
  ggsave(filename = paste0(proj_path,"Presentation/Paper figures/",type,"_std_stretch_line.png"),
         width = 13678,
         height = 3500,
         units = "px",
         dpi = 600)
}

## Compare change in CCF with landscape change

# Read land cover rasters from present and past
lc_2017 <- rast(paste0(proj_path,"GIS/Land cover/2017D/2017D_rast_1ha.tif"))
lc_1995 <- rast(paste0(proj_path,"GIS/Land cover/1995D/1995D_rast_1ha.tif"))

# Create empty raster
change <- rast(lc_1995)

# Classify landscape change into 8 categories

levels(lc_2017)[[1]]

#   value            landcov
# 1     1  Montane Grassland
# 2     2       Shola Forest
# 3     3 Timber Plantations
# 4     4    Tea Plantations
# 5     5          Ochlandra
# 6     6        Settlements
# 7     7  Agricultural Land
# 8     8       Water bodies

change[lc_1995[] %in% 4:8 & lc_2017[] %in% 4:8] <- 0
change[lc_1995[] %in% 2:3 & lc_2017[] %in% 4:8] <- 1
change[lc_1995[] == 1 & lc_2017[] %in% 4:8] <- 2
change[lc_1995[] %in% 2:8 & lc_2017[] == 1] <- 3
change[lc_1995[] == 1 & lc_2017[] %in% c(2,3)] <- 4
change[lc_1995[] %in% 4:8 & lc_2017[] %in% c(2,3)] <- 5
change[lc_1995[] == 1 & lc_2017[] == 1] <- 6
change[lc_1995[] %in% c(2,3) & lc_2017[] %in% c(2,3)] <- 7

levels(change) <- data.frame(value = 0:7,
                             class = c("Historical human-use and water bodies",
                                       "Woodland to human-use",
                                       "Grassland to human-use",
                                       "Grassland gain",
                                       "Grassland to woodland",
                                       "Human-use to woodland",
                                       "Stable grassland",
                                       "Stable woodland"))

# Save landscape change raster
writeRaster(x = change,
            filename = paste0(proj_path,"GIS/Land cover/grassland_woodland_change_1ha.tif"),
            overwrite = TRUE)

# Load change in CCF rasters for one model for all species
shal <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/SHAL_RF_pa1400_1ha_c8_0.5km/raster_std_stretch.tif"))
shma <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/SHMA_RF_nil1400_1ha_c8_0.5km/raster_std_stretch.tif"))
mofa <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/MOFA_RF_pa1400_1ha_c8_0.5km/raster_std_stretch.tif"))
moca <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/MOCA_RF_nil1400_1ha_c8_0.5km/raster_std_stretch.tif"))
anni <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/ANNI_RF_nilpa1400_1ha_c0.25_3km/raster_std_stretch.tif"))
eual <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/EUAL_RF_nilpa1400_1ha_c8_1km/raster_std_stretch.tif"))
fini <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/FINI_RF_nilpa1400_1ha_c8_0.5km/raster_std_stretch.tif"))

# Create species list
specs <- list(shal,shma,mofa,moca,anni,fini,eual)
specs_names <- c("SHAL","SHMA","MOFA","MOCA","ANNI","FINI","EUAL")

# Load landscape change raster
lc_change <- rast(paste0(proj_path,"GIS/Land cover/grassland_woodland_change_1ha.tif"))

# Loop over species
r_df <- data.frame()

for (i in 1:7) {
  
  # Extract change in CCF raster for each species
  cc <- specs[[i]]
  
  names(cc) <- "std_stretch"
  
  # Crop landscape change raster to the same extent
  lc_change_clip <- crop(lc_change,cc)
  
  # Stack landscape change raster with change in CCF raster
  r <- c(cc,lc_change_clip)
  
  # Convert stack to dataframe
  r_df_i <- as.data.frame(r) %>%
    mutate(Species = specs_names[i]) %>%
    na.omit()
  
  # Combine dataframes for all species
  r_df <- rbind(r_df,r_df_i)
}

# Reorder species names
r_df$Species <- factor(r_df$Species,levels = c("ANNI","EUAL","MOCA","MOFA","FINI","SHMA","SHAL"))

# Plot boxplot for change in CCF versus landscape change with outliers
bxplot <- ggplot(r_df,aes(x = class,y = std_stretch,fill = Species)) +
  geom_boxplot(outliers = TRUE,outlier.size = 0.5) +
  theme_classic() +
  scale_fill_manual(values = c("#74c476",
                               "#9ecae1",
                               "#4292c6",
                               "#084594",
                               "#fc9272",
                               "#fb6a4a",
                               "#ef3b2c")) +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  ylab(label = "Change in CCF") +
  theme(axis.title.x = element_blank(),
        panel.grid.major.y = element_line(colour = "darkgrey"),
        axis.line.x = element_blank(),
        text = element_text(size = 15))

# Save boxplot with outliers
ggsave(filename = paste0(proj_path,"Presentation/Paper figures/lc_change_bxplot_complete.png"),
       plot = bxplot,
       width = 10000,
       height = 4000,
       units = "px",
       dpi = 600)

# Remove irrelevant landscape change categories
r_df <- filter(r_df,! (class %in% c("Historical human-use and water bodies","Stable grassland","Stable woodland")))

# Plot boxplot without outliers and with relevant landscape change categories
bxplot <- ggplot(r_df,aes(x = class,y = std_stretch,fill = Species)) +
  geom_boxplot(outliers = FALSE) +
  theme_classic() +
  scale_fill_manual(values = c("#74c476",
                               "#9ecae1",
                               "#4292c6",
                               "#084594",
                               "#fc9272",
                               "#fb6a4a",
                               "#ef3b2c")) +
  ylab(label = "Change in CCF") +
  theme(axis.title.x = element_blank(),
        panel.grid.major.y = element_line(colour = "darkgrey"),
        axis.line.x = element_blank()) 

# Crop and mask landscape change raster for each region
lc_change_pa <- crop(lc_change,shal,mask = TRUE)
lc_change_nil <- crop(lc_change,shma,mask = TRUE)

# Get area covered by each landscape change category for all regions
lc_change_pa_pct <- expanse(lc_change_pa,byValue = TRUE) %>%
  mutate(layer = "Palani-Anamalai",
         area = area * 100 / sum(area)) # Convert to percentage

colnames(lc_change_pa_pct) <- c("Region","Class","Area")

lc_change_nil_pct <- expanse(lc_change_nil,byValue = TRUE) %>%
  mutate(layer = "Nilgiris",
         area = area * 100 / sum(area)) # Convert to percentage

colnames(lc_change_nil_pct) <- c("Region","Class","Area")

# Get similar dataframe for both regions combined
lc_change_both_pct <- merge(lc_change_nil_pct,lc_change_pa_pct,by = "Class") %>%
  mutate(Area = (Area.x + Area.y) * 100 / sum(Area.x,Area.y))

lc_change_both_pct <- data.frame(Region = rep("Both",nrow(lc_change_both_pct)),
                                 Class = lc_change_both_pct$Class,
                                 Area = lc_change_both_pct$Area)

# Merge all dataframes
lc_change_pct <- rbind(lc_change_nil_pct,lc_change_pa_pct,lc_change_both_pct)

# Plot piechart for fraction of area covered by landscape change categories 
piec <- ggplot(lc_change_pct, aes("", Area, fill = Class)) + 
  geom_bar(stat = "identity", color = "white", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ Region, ncol = 3, nrow = 1) +
  scale_fill_manual(values = c("#bcb1b1",
                               "#ca69b2",
                               "#dc1010",
                               "#1f78b4",
                               "#f3d009",
                               "#040404",
                               "#45d43b",
                               "#174613"),
                    breaks = lc_change_nil_pct$Class,
                    labels = function(x) str_wrap(x,width = 23)) +
  theme_void() +
  theme(legend.title = element_blank(),
        strip.clip = "off",
        strip.text = element_text(size = 11)) 

# Plot boxplot and piechart in the same window
ggdraw() +
  draw_plot(bxplot, 0, .5, 1, 0.5) +
  draw_plot(piec, 0.1, 0, 0.8, 0.4) +
  draw_plot_label(c("(A)", "(B)"), c(0, 0), c(1, 0.4), size = 11)

# Save final plot
ggsave(filename = paste0(proj_path,"Presentation/Paper figures/lc_change.png"),
       width = 6000,
       height = 3500,
       units = "px",
       dpi = 600)

# Load filenames for all rasters for change in CCF
files <- list.files(path = paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap"),
                    pattern = "std_stretch.tif$",
                    recursive = TRUE,
                    full.names = TRUE)

# List species
specs_names <- c("SHAL","SHMA","MOFA","MOCA","ANNI","FINI","EUAL")

# Read landscape change raster
lc_change <- rast(paste0(proj_path,"GIS/Land cover/grassland_woodland_change_1ha.tif"))

# Set colours for each model for plotting
cols <- c("#deebf7", "#9ecae1", "#3182bd",
          "#fee6ce", "#fdae6b", "#e6550d",
          "#e5f5e0", "#a1d99b", "#31a354")

# Loop over species
for (spec in specs_names) {
  
  # Subset raster files for each species
  files_spec <- files[grepl(spec,files)]
  
  # Create raster stack for each species
  st <- rast(files_spec)
  
  # Extract model name (c value x dispersal distance) from filenames
  names(st) <- lapply(files_spec,function(x) {
    
    y <- strsplit(x,"/")[[1]]
    z <- strsplit(y[length(y) - 1],"_")[[1]]
    c <- z[length(z) - 1]
    d <- z[length(z)]
    
    y <- paste(c,d,sep = "_")
    
    y
  })
  
  # Crop landscape change raster to same extent
  lc_change_clip <- crop(lc_change,st)
  
  # Stack landscape change raster with change in CCF raster
  r <- c(st,lc_change_clip)
  
  # Convert stack to dataframe
  r_df <- as.data.frame(r) %>% na.omit()
  
  # Reorder model names for plotting
  if (spec %in% c("SHMA","SHAL","MOCA","MOFA","FINI")) {
    lvls <- c("c0.25_0.2km",
              "c0.25_0.5km",
              "c0.25_1km",
              "c2_0.2km",
              "c2_0.5km",
              "c2_1km",
              "c8_0.2km",
              "c8_0.5km",
              "c8_1km")
  }
  
  if (spec == "EUAL") {
    lvls <- c("c0.25_0.5km",
              "c0.25_1km",
              "c0.25_1.5km",
              "c2_0.5km",
              "c2_1km",
              "c2_1.5km",
              "c8_0.5km",
              "c8_1km",
              "c8_1.5km")
  }
  
  if (spec == "ANNI") {
    lvls <- c("c0.25_1km",
              "c0.25_3km",
              "c0.25_5km",
              "c2_1km",
              "c2_3km",
              "c2_5km",
              "c8_1km",
              "c8_3km",
              "c8_5km")
  }
  
  # Reshape dataframe for plotting
  data <- reshape::melt(r_df)
  
  colnames(data) <- c("class","Model","Change in CCF")
  
  # Plot boxplot with outliers for all models for each species
  p_outliers <- ggplot(data,aes(x = class,y = .data[["Change in CCF"]],fill = Model)) +
    geom_boxplot(outliers = TRUE,
                 outlier.size = 0.5) +
    theme_classic() +
    scale_fill_manual(values = cols,
                      breaks = lvls) +
    scale_x_discrete(labels = scales::label_wrap(15)) +
    theme(axis.title.x = element_blank(),
          panel.grid.major.y = element_line(colour = "darkgrey"),
          axis.line.x = element_blank(),
          text = element_text(size = 17))
  
  # Plot boxplot without outliers for all models for each species
  p <- ggplot(data,aes(x = class,y = .data[["Change in CCF"]],fill = Model)) +
    geom_boxplot(outliers = FALSE) +
    theme_classic() +
    scale_fill_manual(values = cols,
                      breaks = lvls) +
    theme(axis.title.x = element_blank(),
          panel.grid.major.y = element_line(colour = "darkgrey"),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          text = element_text(size = 17))
  
  # Plot both boxplots in the same window
  ggdraw() +
    draw_plot(p, 0, .5, 1, 0.5) +
    draw_plot(p_outliers, 0, 0, 1, 0.5) +
    draw_plot_label(c("(A)", "(B)"), c(0, 0), c(1, 0.5), size = 13)
  
  # Save boxplots
  ggsave(filename = paste0(proj_path,"Presentation/Paper figures/lc_change_",spec,"_bxplots.png"),
         width = 10000,
         height = 8000,
         units = "px",
         dpi = 600)
}