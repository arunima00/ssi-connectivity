# Load packages
library(tidyverse)
library(terra)

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

# Create loop to plot all output maps
for (i in files) {
  
  # Read raster
  r <- rast(i)
  
  if (names(r) == "flow_potential") {
    names(r) <- "fp[]"
  }
  
  if (names(r) == "normalized_cum_currmap") {
    names(r) <- "NCF"
  }
  
  if (names(r) == "cum_currmap") {
    names(r) <- "CCF"
  }
  
  # Convert raster to dataframe
  r_df <- as.data.frame(r,xy = TRUE)
  
  # Compute percentiles
  r_df <- r_df %>%
    mutate(percentile = ecdf(.data[[names(r)]])(.data[[names(r)]]))
  
  # Define color scale
  colors <- scales::pal_viridis()(100)
  value_breaks <- quantile(r_df[,names(r)], probs = seq(0,1,by = 0.01))
  
  # Plot raster on percentile colour scale and save PNG
  ggplot(r_df, aes(x, y, fill = .data[[names(r)]])) +
    geom_tile() +
    scale_fill_gradientn(
      name = names(r),
      colors = colors,
      values = scales::rescale(value_breaks),
      guide = guide_colorbar(barheight = 10)
    ) +
    theme_classic() +
    theme(legend.position = "right") +
    coord_fixed()
  
  ggsave(filename = sub(".tif",".png",i),
         dpi = 300)
}

# Set bivariate legend colour palette 
col.matrix <- biscale::bi_pal(pal = "DkViolet2",
                              dim = 3,
                              preview = FALSE)

# Loop for species
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
      d_list <- c("1km","3km")
    }
    
    for (d in d_list) {
      
      # Loop for both NCF and CCF
      for (con in c("cum_currmap","normalized_cum_currmap")) {
        
        # Set species-specific region
        if (spec %in% c("SHAL","MOFA")) {
          region <- "pa1400_1ha"      }
        
        if (spec %in% c("SHMA","MOCA")) {
          region <- "nil1400_1ha"
        }
        
        if (spec %in% c("EUAL","FINI","ANNI")) {
          region <- "nilpa1400_1ha"
        }
        
        # Read past and present Omniscape output rasters
        past <- rast(paste0(proj_path,"Omniscape/Output/1995/",spec,"_RF_",region,"_c",c,"/",d,"/",con,".tif"))
        present <- rast(paste0(proj_path,"Omniscape/Output/2017/",spec,"_RF_",region,"_c",c,"/",d,"/",con,".tif"))
        
        # Calculate difference between past and present current flow for each pixel
        r <- present - past
        
        if (! dir.exists(paste0(proj_path,"Omniscape/Output/Change/Difference_",con))) {
          dir.create(paste0(proj_path,"Omniscape/Output/Change/Difference_",con),recursive = TRUE)
        }
        
        # Convert to dataframe
        r_df <- as.data.frame(r,xy = TRUE)
        
        # Compute percentiles
        r_df <- r_df %>%
          mutate(percentile = ecdf(.data[[con]])(.data[[con]]))
        
        # Define color scale
        colors <- scales::pal_viridis()(3)
        value_breaks <- quantile(r_df[,con], probs = seq(0,1,by = 0.01))
        
        # Set plot legend label
        if (con == "normalized_cum_currmap") {
          t <- "NCF change"
        }
        
        if (con == "cum_currmap") {
          t <- "CCF change"
        }
        
        # Plot difference in current flow raster with a diverging colour scale
        ggplot(r_df, aes(x, y, fill = .data[[con]])) +
          geom_tile() +
          scale_fill_gradientn(
            name = t,
            colors = colors,
            values = scales::rescale(value_breaks),
            breaks = c(value_breaks[1],0,value_breaks[101]),
            labels = round(c(value_breaks[1],0,value_breaks[101]), 2)
          ) +
          theme_classic() +
          theme(legend.position = "right") +
          coord_fixed()
        
        ggsave(filename = paste0("Omniscape/Output/Change/Difference_",con,"/",spec,"_RF_",region,"_c",c,"_",d,"/plot.png"),
               path = proj_path,
               dpi = 300)
        
        # Write difference in current flow rasters to TIF files
        writeRaster(r,
                    filename = paste0(proj_path,"Omniscape/Output/Change/Difference_",con,"/",spec,"_RF_",region,"_c",c,"_",d,"/raster.tif"),
                    overwrite = TRUE)
        
        # Create bivariate past and present maps for CCF
        if (con == "cum_currmap") {
          
          if (! dir.exists(paste0(proj_path,"Omniscape/Output/Change/Bivariate_",con))) {
            dir.create(paste0(proj_path,"Omniscape/Output/Change/Bivariate_",con),recursive = TRUE)
          }
          
          # Convert rasters to dataframes
          past_df <- as.data.frame(past,xy = TRUE,na.rm = FALSE)
          present_df <- as.data.frame(present,xy = TRUE,na.rm = FALSE) 
          
          # Bin raster values into terciles
          past_df_binned <- ntile(past_df[,con],3)
          present_df_binned <- ntile(present_df[,con],3)
          
          # Convert dataframes with binned values to rasters
          past_binned <- rast(cbind(past_df[,c("x","y")],past_df_binned),
                              crs = crs(past))
          present_binned <- rast(cbind(present_df[,c("x","y")],present_df_binned),
                                 crs = crs(present))
          
          # Create empty raster
          r <- rast(past)
          
          # Fill values in empty raster from past and present binned rasters
          r[past_binned[] == 1 & present_binned[] == 1] <- 1
          r[past_binned[] == 1 & present_binned[] == 2] <- 2
          r[past_binned[] == 1 & present_binned[] == 3] <- 3
          r[past_binned[] == 2 & present_binned[] == 1] <- 4
          r[past_binned[] == 2 & present_binned[] == 2] <- 5
          r[past_binned[] == 2 & present_binned[] == 3] <- 6
          r[past_binned[] == 3 & present_binned[] == 1] <- 7
          r[past_binned[] == 3 & present_binned[] == 2] <- 8
          r[past_binned[] == 3 & present_binned[] == 3] <- 9
          r[is.na(past_binned[]) | is.na(present_binned[])] <- NA
          
          # Plot bivariate map
          png(filename = paste0(proj_path,"Omniscape/Output/Change/Bivariate_",con,"/",spec,"_RF_",region,"_c",c,"_",d,".png"),
              height = 3000,
              width = 3000,
              res = 300)
          
          plot(r,
               frame.plot = FALSE, 
               axes = FALSE, 
               box = FALSE,
               legend = FALSE, 
               col = col.matrix,
               mar = c(0.7,0.7,0.7,0.7))
          
          dev.off()
          
          # Write bivariate raster to TIF file
          writeRaster(r,
                      filename = paste0(proj_path,"Omniscape/Output/Change/Bivariate_",con,"/",spec,"_RF_",region,"_c",c,"_",d,".tif"),
                      overwrite = TRUE)
        }
      }
    }
  }
}

shal <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/SHAL_RF_pa1400_1ha_c8_0.5km/raster.tif"))
shma <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/SHMA_RF_nil1400_1ha_c8_0.5km/raster.tif"))
mofa <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/MOFA_RF_pa1400_1ha_c8_0.5km/raster.tif"))
moca <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/MOCA_RF_nil1400_1ha_c8_0.5km/raster.tif"))
anni <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/ANNI_RF_nilpa1400_1ha_c0.25_3km/raster.tif"))
eual <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/EUAL_RF_nilpa1400_1ha_c8_1km/raster.tif"))
fini <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/FINI_RF_nilpa1400_1ha_c8_0.5km/raster.tif"))

specs <- list(shal,shma,mofa,moca,anni,fini,eual)
specs_names <- c("SHAL","SHMA","MOFA","MOCA","ANNI","FINI","EUAL")

r_df <- data.frame()

for (i in 1:7) {
  cc <- specs[[i]]
  
  cc_df <- as.data.frame(cc,xy = TRUE)
  
  std <- sd(cc_df$cum_currmap)
  
  cc_df <- cc_df %>%
    mutate(std_stretch = cum_currmap/std)
  
  cc <- rast(cc_df[,c("x","y","std_stretch")],type = "xyz",crs = crs(cc))
  
  lc_change <- rast(paste0(proj_path,"GIS/Land cover/grassland_woodland_change_1ha.tif"))
  
  lc_change <- crop(lc_change,cc)
  
  r <- c(cc,lc_change)
  
  r_df_i <- as.data.frame(r) %>%
    mutate(Species = specs_names[i]) %>%
    na.omit()
  
  r_df <- rbind(r_df,r_df_i)
}

r_df$Species <- factor(r_df$Species,levels = c("ANNI","EUAL","MOCA","MOFA","FINI","SHMA","SHAL"))

r_df <- filter(r_df,! (class %in% c("Historical human-use and water bodies","Grassland gain")))

ggplot(r_df,aes(x = class,y = std_stretch,fill = Species)) +
  geom_boxplot(outliers = FALSE) +
  scale_fill_manual(values = c("#74c476",
                               "#9ecae1",
                               "#4292c6",
                               "#084594",
                               "#fc9272",
                               "#fb6a4a",
                               "#ef3b2c")) +
  ylab(label = "Change in CCF") +
  theme(axis.text.x = element_text(hjust = 1,vjust = 1,angle = 45),
        axis.title.x = element_blank())

if (! dir.exists(paste0(proj_path,"Omniscape/Output/Final plots"))) {
  dir.create(paste0(proj_path,"Omniscape/Output/Final plots"))
}

ggsave(paste0("Omniscape/Output/Final plots/combined_boxplot_lc_change_std_stretch.png"),
       dpi = 600,
       bg = "white")



for (region in c("nil","pa")) {
  
  for (type in c("cc","pc")) {
    
    if (type == "pc") {
      shal <- rast(paste0(proj_path,"Omniscape/Output/2017/SHAL_RF_pa1400_1ha_c8/0.5km/cum_currmap_std_stretch.tif"))
      shma <- rast(paste0(proj_path,"Omniscape/Output/2017/SHMA_RF_nil1400_1ha_c8/0.5km/cum_currmap_std_stretch.tif"))
      mofa <- rast(paste0(proj_path,"Omniscape/Output/2017/MOFA_RF_pa1400_1ha_c8/0.5km/cum_currmap_std_stretch.tif"))
      moca <- rast(paste0(proj_path,"Omniscape/Output/2017/MOCA_RF_nil1400_1ha_c8/0.5km/cum_currmap_std_stretch.tif"))
      anni <- rast(paste0(proj_path,"Omniscape/Output/2017/ANNI_RF_nilpa1400_1ha_c0.25/3km/cum_currmap_std_stretch.tif"))
      eual <- rast(paste0(proj_path,"Omniscape/Output/2017/EUAL_RF_nilpa1400_1ha_c8/1km/cum_currmap_std_stretch.tif"))
      fini <- rast(paste0(proj_path,"Omniscape/Output/2017/FINI_RF_nilpa1400_1ha_c8/0.5km/cum_currmap_std_stretch.tif"))
    }
    
    if (type == "cc") {
      shal <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/SHAL_RF_pa1400_1ha_c8_0.5km/raster_std_stretch.tif"))
      shma <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/SHMA_RF_nil1400_1ha_c8_0.5km/raster_std_stretch.tif"))
      mofa <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/MOFA_RF_pa1400_1ha_c8_0.5km/raster_std_stretch.tif"))
      moca <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/MOCA_RF_nil1400_1ha_c8_0.5km/raster_std_stretch.tif"))
      anni <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/ANNI_RF_nilpa1400_1ha_c0.25_3km/raster_std_stretch.tif"))
      eual <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/EUAL_RF_nilpa1400_1ha_c8_1km/raster_std_stretch.tif"))
      fini <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/FINI_RF_nilpa1400_1ha_c8_0.5km/raster_std_stretch.tif"))
    }
    
    if (region == "nil") {
      shp <- vect(paste0(proj_path,"GIS/Shapefiles/Nilgiri1400m/Nilgiri1400m.shp"))
      maps <- list(anni,eual,moca,fini,shma)
      species <- c("ANNI","EUAL","MOCA","FINI","SHMA")
      cols <- c("#74c476",
                "#9ecae1",
                "#4292c6",
                "#fc9272",
                "#fb6a4a")
    }
    
    if (region == "pa") {
      shp <- vect(paste0(proj_path,"GIS/Shapefiles/PalaniAanamalai1400m/PalaniAanamalai1400m.shp"))
      maps <- list(anni,eual,mofa,fini,shal)
      species <- c("ANNI","EUAL","MOFA","FINI","SHAL")
      cols <- c("#74c476",
                "#9ecae1",
                "#084594",
                "#fc9272",
                "#ef3b2c")
    }
    
    df <- c()
    
    for (i in seq_along(species)) {
      
      r <- maps[[i]]
      names(r) <- "std_stretch"
      r <- project(r,"epsg:4326")
      
      if (type == "cc") {
        type_name <- "Change in CCF"
      }
      
      if (type == "pc") {
        type_name <- "CCF"
      }
      
      r_clip <- crop(r,shp,mask = TRUE)
      
      r_df <- as.data.frame(r_clip,xy = TRUE)
      
      bin_size <- (max(r_df$x) - min(r_df$x))/100
      
      sum_val <- c()
      longs <- c()
      
      for (j in 1:100) {
        b <- filter(r_df,x >= min(r_df$x) + (j - 1) * bin_size,x < min(r_df$x) + j * bin_size)[,3]
        c <- median(b,na.rm = TRUE)
        sum_val <- c(sum_val,c)
        longs <- c(longs,min(r_df$x) + (j - 1) * bin_size)
      }
      
      df <- rbind(df,sum_val)
    }
    
    row.names(df) <- species
    colnames(df) <- longs
    
    data <- reshape2::melt(df) %>% na.omit()
    
    colnames(data) <- c("Species","Longitude",type_name)
    
    p <- ggplot(data,aes(x = Longitude,y = .data[[type_name]],fill = Species))+
      geom_smooth(aes(color = Species),method = "gam") +
      scale_color_manual(values = cols) +
      scale_fill_manual(values = cols) +
      theme_minimal() + 
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            axis.line.x = element_blank(),
            panel.grid.major.y = element_blank(), 
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_line(colour = "black"),
            legend.title = element_blank(),
            legend.key.spacing.y = unit(3,"mm"),
            text = element_text(size = 22,face = "bold")) +
      scale_y_continuous(
        limits = function(x) {
          max_val <- max(abs(x))
          c(-max_val, max_val)
        },
        breaks = function(x) {
          pretty(c(-max(abs(x)), max(abs(x))))
        }
      ) +
      scale_x_continuous(breaks = seq(min(data$Longitude),max(data$Longitude),by = bin_size * 20))
    
    if (region == "pa") {
      p <- p + theme(axis.title.y = element_blank())
    }
    
    plot(p)
    
    ggsave(paste0(proj_path,"Omniscape/Output/Final plots/heatmap_",region,"_",type,"_std_stretch_line.png"),
           bg = "white",
           width = 280,
           height = 150,
           units = "mm",
           dpi = 600)
  }
}

##################################################

col.matrix <- biscale::bi_pal(pal = "DkViolet2",
                              dim = 3,
                              preview = FALSE)

for (spec in c("SHKL","LATH","ANNI","EUAL","FINI")) {
  
  if (spec %in% c("SHKL","LATH")) {
    
    if (spec == "SHKL") {
      spec_nil <- "SHMA"
      spec_pa <- "SHAL"
    }
    
    if (spec == "LATH") {
      spec_nil <- "MOCA"
      spec_pa <- "MOFA"
    }
    
    # Read past and present SDM prediction output rasters
    past_nil <- rast(paste0(proj_path,"Omniscape/Output/1995/",spec_nil,"_RF_nil1400_1ha_c8/0.5km/cum_currmap.tif"))
    present_nil <- rast(paste0(proj_path,"Omniscape/Output/2017/",spec_nil,"_RF_nil1400_1ha_c8/0.5km/cum_currmap.tif"))
    
    # Convert raster to dataframe
    past_nil_df <- as.data.frame(past_nil,xy = TRUE,na.rm = FALSE)
    present_nil_df <- as.data.frame(present_nil,xy = TRUE,na.rm = FALSE) 
    
    # Bin raster values into terciles
    past_nil_df_binned <- ntile(past_nil_df[[3]],3)
    present_nil_df_binned <- ntile(present_nil_df[[3]],3)
    
    # Rasterize dataframe with binned values
    past_nil_binned <- rast(cbind(past_nil_df[,c("x","y")],past_nil_df_binned),
                        crs = crs(past_nil))
    present_nil_binned <- rast(cbind(present_nil_df[,c("x","y")],present_nil_df_binned),
                           crs = crs(present_nil))
    
    # Create empty raster
    r_nil <- rast(past_nil)
    
    # Fill values in empty raster from past_nil and present_nil rasters
    r_nil[past_nil_binned[] == 1 & present_nil_binned[] == 1] <- 1
    r_nil[past_nil_binned[] == 1 & present_nil_binned[] == 2] <- 2
    r_nil[past_nil_binned[] == 1 & present_nil_binned[] == 3] <- 3
    r_nil[past_nil_binned[] == 2 & present_nil_binned[] == 1] <- 4
    r_nil[past_nil_binned[] == 2 & present_nil_binned[] == 2] <- 5
    r_nil[past_nil_binned[] == 2 & present_nil_binned[] == 3] <- 6
    r_nil[past_nil_binned[] == 3 & present_nil_binned[] == 1] <- 7
    r_nil[past_nil_binned[] == 3 & present_nil_binned[] == 2] <- 8
    r_nil[past_nil_binned[] == 3 & present_nil_binned[] == 3] <- 9
    
    r_nil[is.na(past_nil_binned[]) | is.na(present_nil_binned[])] <- NA
    
    # Read past and present SDM prediction output rasters
    past_pa <- rast(paste0(proj_path,"Omniscape/Output/1995/",spec_pa,"_RF_pa1400_1ha_c8/0.5km/cum_currmap.tif"))
    present_pa <- rast(paste0(proj_path,"Omniscape/Output/2017/",spec_pa,"_RF_pa1400_1ha_c8/0.5km/cum_currmap.tif"))
    
    # Convert raster to dataframe
    past_pa_df <- as.data.frame(past_pa,xy = TRUE,na.rm = FALSE)
    present_pa_df <- as.data.frame(present_pa,xy = TRUE,na.rm = FALSE) 
    
    # Bin raster values into terciles
    past_pa_df_binned <- ntile(past_pa_df[[3]],3)
    present_pa_df_binned <- ntile(present_pa_df[[3]],3)
    
    # Rasterize dataframe with binned values
    past_pa_binned <- rast(cbind(past_pa_df[,c("x","y")],past_pa_df_binned),
                            crs = crs(past_pa))
    present_pa_binned <- rast(cbind(present_pa_df[,c("x","y")],present_pa_df_binned),
                               crs = crs(present_pa))
    
    # Create empty raster
    r_pa <- rast(past_pa)
    
    # Fill values in empty raster from past_pa and present_pa rasters
    r_pa[past_pa_binned[] == 1 & present_pa_binned[] == 1] <- 1
    r_pa[past_pa_binned[] == 1 & present_pa_binned[] == 2] <- 2
    r_pa[past_pa_binned[] == 1 & present_pa_binned[] == 3] <- 3
    r_pa[past_pa_binned[] == 2 & present_pa_binned[] == 1] <- 4
    r_pa[past_pa_binned[] == 2 & present_pa_binned[] == 2] <- 5
    r_pa[past_pa_binned[] == 2 & present_pa_binned[] == 3] <- 6
    r_pa[past_pa_binned[] == 3 & present_pa_binned[] == 1] <- 7
    r_pa[past_pa_binned[] == 3 & present_pa_binned[] == 2] <- 8
    r_pa[past_pa_binned[] == 3 & present_pa_binned[] == 3] <- 9
    
    r_pa[is.na(past_pa_binned[]) | is.na(present_pa_binned[])] <- NA
  }
  
  if (spec %in% c("FINI","EUAL","ANNI")) {
    
    if (spec == "FINI") {
      region <- "nilpa1400_1ha"
      d <- "0.5km"
    }
    
    if (spec == "EUAL") {
      region <- "nilpa1400_1ha"
      d <- "1km"
    }
    
    if (spec == "ANNI") {
      region <- "nilpa1400_25ha"
      d <- "1km"
    }
    
    # Read past and present SDM prediction output rasters
    past <- rast(paste0(proj_path,"Omniscape/Output/1995/",spec,"_RF_",region,"_c8/",d,"/cum_currmap.tif"))
    present <- rast(paste0(proj_path,"Omniscape/Output/2017/",spec,"_RF_",region,"_c8/",d,"/cum_currmap.tif"))
    
    # Convert raster to dataframe
    past_df <- as.data.frame(past,xy = TRUE,na.rm = FALSE)
    present_df <- as.data.frame(present,xy = TRUE,na.rm = FALSE) 
    
    # Bin raster values into terciles
    past_df_binned <- ntile(past_df[[3]],3)
    present_df_binned <- ntile(present_df[[3]],3)
    
    # Rasterize dataframe with binned values
    past_binned <- rast(cbind(past_df[,c("x","y")],past_df_binned),
                        crs = crs(past))
    present_binned <- rast(cbind(present_df[,c("x","y")],present_df_binned),
                           crs = crs(present))
    
    # Create empty raster
    r <- rast(past)
    
    # Fill values in empty raster from past and present rasters
    r[past_binned[] == 1 & present_binned[] == 1] <- 1
    r[past_binned[] == 1 & present_binned[] == 2] <- 2
    r[past_binned[] == 1 & present_binned[] == 3] <- 3
    r[past_binned[] == 2 & present_binned[] == 1] <- 4
    r[past_binned[] == 2 & present_binned[] == 2] <- 5
    r[past_binned[] == 2 & present_binned[] == 3] <- 6
    r[past_binned[] == 3 & present_binned[] == 1] <- 7
    r[past_binned[] == 3 & present_binned[] == 2] <- 8
    r[past_binned[] == 3 & present_binned[] == 3] <- 9
    
    r[is.na(past_binned[]) | is.na(present_binned[])] <- NA
    
    nil <- project(vect(paste0(proj_path,"GIS/Shapefiles/Nilgiri1400m/Nilgiri1400m.shp")),crs(r))
    pa <- project(vect(paste0(proj_path,"GIS/Shapefiles/PalaniAanamalai1400m/PalaniAanamalai1400m.shp")),crs(r))
    
    r_nil <- crop(r,nil,mask = TRUE)
    r_pa <- crop(r,pa,mask = TRUE)
  }
  
  # Save plot as PNG
  png(filename = paste0(proj_path,"Presentation/NSAB 2025/",spec,"_bivariate.png"),
      height = 5000,
      width = 5000,
      res = 500,
      bg = "#f4f0f0")
  
  par(mfrow = c(2,1),
      bg = "#f4f0f0")
  
  # Plot bivariate map
  plot(r_nil,
       frame.plot = FALSE, 
       axes = FALSE, 
       box = FALSE,
       legend = FALSE, 
       col = col.matrix,
       mar = c(0,0,0,9))
  
  plot(r_pa,
       frame.plot = FALSE, 
       axes = FALSE, 
       box = FALSE,
       legend = FALSE, 
       col = col.matrix,
       mar = c(0,6,0,0))
  
  dev.off()
}

library(ggplot2)

# Define the bivariate color matrix (3x3 example)
bivar_colors <- col.matrix

# Convert to matrix for layout
color_matrix <- matrix(bivar_colors, nrow = 3, byrow = FALSE)

# Create a data frame for plotting
legend_df <- expand.grid(x = 1:3, y = 1:3)
legend_df$color <- as.vector(color_matrix)

# Create the plot
legend_plot <- ggplot(legend_df, aes(x, y, fill = color)) +
  geom_tile() +
  scale_fill_identity() +  # Use manual colors
  theme_minimal(base_size = 14) +  # Keeps layout structure
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),  # Remove grid lines
    plot.background = element_rect(fill = "#f4f0f0", color = NA),  # Background color
  ) +
  labs(x = "Past CCF →", y = "Present CCF →") +
  theme(
    axis.title.x = element_text(size = 17,vjust = -1),
    axis.title.y = element_text(size = 17,vjust = 2)
  ) +
  coord_fixed()  # Keep square proportions

legend_plot

# Save as PNG
ggsave("Presentation/NSAB 2025/bivariate_legend.png", legend_plot, width = 3, height = 3, dpi = 500, bg = "#f4f0f0")


landcov_2017 <- rast("GIS/Land cover/2017D/2017D_rast_30m.tif")
landcov_1995 <- rast("GIS/Land cover/1995D/1995D_rast_30m.tif")

r<-rast(landcov_1995)

r[landcov_2017[] != landcov_1995[]] <- 1
r[landcov_2017[] == landcov_1995[]] <- 0

landcov<-project(r,"epsg:4326")

# Set row and column names for model paramaters matrix
col <- c("nil_block1",
         "nil_block2",
         "nil_block3",
         "nil_block4",
         "nil_block5",
         "pa_block1",
         "pa_block2",
         "pa_block3",
         "pa_block4",
         "pa_block5")

spec_list <- c("Land cover change",
               "SHKL_pc",
               "LATH_pc",
               "EUAL_pc",
               "FINI_pc",
               "ANNI_pc",
               "SHKL_cc",
               "LATH_cc",
               "EUAL_cc",
               "FINI_cc",
               "ANNI_cc")

# Create empty matrix
df <- matrix(nrow = length(spec_list),
             ncol = length(col),
             dimnames = list(spec_list,col))

r_clip_nil <- crop(landcov,nil,mask=TRUE)

r_df_nil<-as.data.frame(r_clip_nil,xy=TRUE)

bin_size_nil<-(max(r_df_nil$x) - min(r_df_nil$x))/5

sum_val_nil <- c()
for (i in 1:5) {
  b<-filter(r_df_nil,x >= min(r_df_nil$x) + (i - 1) * bin_size_nil,x < min(r_df_nil$x) + i * bin_size_nil)[,3]
  c<-sum(b) * 100 / length(b)
  sum_val_nil<-c(sum_val_nil,c)
}

r_clip_pa <- crop(landcov,pa,mask=TRUE)

r_df_pa<-as.data.frame(r_clip_pa,xy=TRUE)

bin_size_pa<-(max(r_df_pa$x) - min(r_df_pa$x))/5

sum_val_pa <- c()
for (i in 1:5) {
  b<-filter(r_df_pa,x >= min(r_df_pa$x) + (i - 1) * bin_size_pa,x < min(r_df_pa$x) + i * bin_size_pa)[,3]
  c<-sum(b) * 100 / length(b)
  sum_val_pa<-c(sum_val_pa,c)
}

df[1,] <- c(sum_val_nil,sum_val_pa)

if (! dir.exists(paste0(proj_path,"Omniscape/Output/Heatmaps"))) {
  dir.create(paste0(proj_path,"Omniscape/Output/Heatmaps"),recursive = TRUE)
}

for (region in c("nil","pa")) {
  
  for (type in c("cc","pc")) {
    
    if (type == "pc") {
      shal <- rast(paste0(proj_path,"Omniscape/Output/2017/SHAL_RF_pa1400_1ha_c8/0.5km/cum_currmap.tif"))
      shma <- rast(paste0(proj_path,"Omniscape/Output/2017/SHMA_RF_nil1400_1ha_c8/0.5km/cum_currmap.tif"))
      mofa <- rast(paste0(proj_path,"Omniscape/Output/2017/MOFA_RF_pa1400_1ha_c8/0.5km/cum_currmap.tif"))
      moca <- rast(paste0(proj_path,"Omniscape/Output/2017/MOCA_RF_nil1400_1ha_c8/0.5km/cum_currmap.tif"))
      anni <- rast(paste0(proj_path,"Omniscape/Output/2017/ANNI_RF_nilpa1400_25ha_c0.25/3km/cum_currmap.tif"))
      eual <- rast(paste0(proj_path,"Omniscape/Output/2017/EUAL_RF_nilpa1400_1ha_c8/1km/cum_currmap.tif"))
      fini <- rast(paste0(proj_path,"Omniscape/Output/2017/FINI_RF_nilpa1400_1ha_c8/0.5km/cum_currmap.tif"))
    }
    
    if (type == "cc") {
      shal <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/SHAL_RF_pa1400_1ha_c8_0.5km.tif"))
      shma <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/SHMA_RF_nil1400_1ha_c8_0.5km.tif"))
      mofa <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/MOFA_RF_pa1400_1ha_c8_0.5km.tif"))
      moca <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/MOCA_RF_nil1400_1ha_c8_0.5km.tif"))
      anni <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/ANNI_RF_nilpa1400_25ha_c0.25_3km.tif"))
      eual <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/EUAL_RF_nilpa1400_1ha_c8_1km.tif"))
      fini <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/FINI_RF_nilpa1400_1ha_c8_0.5km.tif"))
    }
    
    if (region == "nil") {
      shp <- vect(paste0(proj_path,"GIS/Shapefiles/Nilgiri1400m/Nilgiri1400m.shp"))
      maps <- list(shma,moca,fini,eual,anni)
      species <- c("SHMA","MOCA","FINI ","EUAL","ANNI")
    }
    
    if (region == "pa") {
      shp <- vect(paste0(proj_path,"GIS/Shapefiles/PalaniAanamalai1400m/PalaniAanamalai1400m.shp"))
      maps <- list(shal,mofa,fini,eual,anni)
      species <- c("SHAL","MOFA","FINI ","EUAL","ANNI")
    }
    
    plot_list <- list()
    
    df <- c()
    
    for (i in seq_along(species)) {
      
      r <- maps[[i]]
      
      r <- project(r,"epsg:4326")
      
      r_clip <- crop(r,shp,mask = TRUE)
      
      r_df <- as.data.frame(r_clip,xy=TRUE)
      
      bin_size<-(max(r_df$x) - min(r_df$x))/5
      
      sum_val <- c()
      
      for (j in 1:5) {
        b <- filter(r_df,x >= min(r_df$x) + (j - 1) * bin_size,x < min(r_df$x) + j * bin_size)[,3]
        c <- mean(b)
        sum_val <- c(sum_val,c)
      }
      
      data <- reshape2::melt(sum_val)
      
      data <- cbind(data, Slice = 1:5, Species = species[i])
      
      x <- ggplot(data, aes(x = Slice, y = Species, fill = value))+
        geom_tile() +
        theme_minimal() +
        theme(legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_text(size = 20,face = "bold"))  
      
      if (i == 5) {
        x <- x + theme(axis.text.x = element_text(size = 20,face = "bold"))  
      } else {
        x <- x + theme(axis.text.x = element_blank())  
      }
      
      if (type == "cc") {
        x <- x + scale_fill_gradient2()  
      } else {
        x <- x + scale_fill_gradient(low = "white",high = "darkgreen")
      }
      
      plot_list[[i]] <- x
      
      df <- rbind(df,sum_val)
    }
    
    cowplot::plot_grid(plotlist = plot_list,ncol = 1)
    
    ggsave(paste0(proj_path,"Omniscape/Output/Heatmaps/heatmap_",region,"_",type,".png"),
           bg = "white",
           width = 230,
           height = 150,
           units = "mm",
           dpi = 500)
    
    row.names(df) <- species
    colnames(df) <- 1:5
    
    write.csv(df,paste0(proj_path,"Omniscape/Output/Heatmaps/heatmap_",region,"_",type,".csv"))
  }
}

lc_2017 <- rast(paste0(proj_path,"GIS/Land cover/2017D/2017D_rast_30m.tif"))
lc_1995 <- rast(paste0(proj_path,"GIS/Land cover/1995D/1995D_rast_30m.tif"))

change <- rast(lc_1995)

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

# r <- crop(change,c(77,78,10,10.5))
# plot(r)

writeRaster(change,"GIS/Land cover/grassland_woodland_change.tif",overwrite = TRUE)

pa_nil <- vect("GIS/Shapefiles/Nilgiri_PA_1400m/Nilgiri_PA_1400m.shp")
pa <- vect("GIS/Shapefiles/PalaniAanamalai1400m/PalaniAanamalai1400m.shp")
nil <- vect("GIS/Shapefiles/Nilgiri1400m/Nilgiri1400m.shp")

change_clip <- crop(change,pa_nil,mask = TRUE)
change_pa <- crop(change,pa,mask=T)
change_nil <- crop(change,nil,mask=T)

r_sprc <- sprc(change,change_clip,change_pa,change_nil)

df <- levels(change)[[1]][2]

for (i in 1:4) {
  x <- terra::freq(r_sprc[i])[,c(2,3)]
  x <- x %>%
    mutate(percentage = count * 100 / sum(x$count))
  
  df <- cbind(df,x$percentage)
}

colnames(df) <- c("class","SWG","PA_nil","PA","Nil")

write.csv(df,"GIS/Land cover/grassland_woodland_change.csv",row.names = FALSE)

for (region in c("pa","nil")) {
  
  if (region == "nil") {
    poly <- vect(paste0(proj_path,"GIS/Shapefiles/Nilgiri1400m/Nilgiri1400m.shp"))
  }
  
  if (region == "pa") {
    poly <- vect(paste0(proj_path,"GIS/Shapefiles/PalaniAanamalai1400m/PalaniAanamalai1400m.shp"))
  }
  
  # Compute slice width
  slice_width <- (xmax(poly) - xmin(poly)) / 5
  
  # Create slice polygons
  slices <- list()
  for (i in 1:5) {
    x1 <- xmin(poly) + (i - 1) * slice_width
    x2 <- xmin(poly) + i * slice_width
    
    # Define slice as a polygon
    slice_coords <- list(matrix(c(x1,ymin(poly),
                                  x2,ymin(poly),
                                  x2,ymax(poly),
                                  x1,ymax(poly),
                                  x1,ymin(poly)),
                                ncol = 2, byrow = TRUE))
    
    slices[[i]] <- vect(slice_coords,type = "polygons",crs = "epsg:4326")
  }
  
  # Combine slices into a single multipolygon layer
  sliced_polygons <- do.call(rbind,slices)
  
  # Intersect slices with the original polygon to keep only overlapping parts
  sliced_result <- intersect(sliced_polygons, poly)
  
  # Add an ID column to differentiate slices
  sliced_result$ID <- 1:nrow(sliced_result)
  
  if (! dir.exists(paste0(proj_path,"GIS/Shapefiles/Slices"))) {
    dir.create(paste0(proj_path,"GIS/Shapefiles/Slices"),recursive = TRUE)
  }
  
  writeVector(sliced_result,paste0(proj_path,"GIS/Shapefiles/Slices/",region,".shp"),overwrite = TRUE)
}


lc_2017 <- rast(paste0(proj_path,"GIS/Land cover/2017D/2017D_rast_1ha.tif"))
lc_1995 <- rast(paste0(proj_path,"GIS/Land cover/1995D/1995D_rast_1ha.tif"))

change <- rast(lc_1995)

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

# r <- crop(change,c(77,78,10,10.5))
# plot(r)

writeRaster(change,"GIS/Land cover/grassland_woodland_change_1ha.tif",overwrite = TRUE)

slices_pa <- vect("GIS/Shapefiles/Slices/pa.shp")
slices_nil <- vect("GIS/Shapefiles/Slices/nil.shp")

rast_25ha <- rast("Omniscape/Output/2017/ANNI_RF_nilpa1400_25ha_c0.25/0.5km/cum_currmap.tif")
rast_1ha <- rast("Omniscape/Output/2017/EUAL_RF_nilpa1400_1ha_c0.25/0.5km/cum_currmap.tif")

slices_pa <- project(slices_pa,crs(rast_1ha))
slices_nil <- project(slices_nil,crs(rast_1ha))

pa_25ha <- crop(rast_25ha,slices_pa)
nil_25ha <- crop(rast_25ha,slices_nil)

pa_1ha <- crop(rast_1ha,slices_pa)
nil_1ha <- crop(rast_1ha,slices_nil)

slices_pa_1ha <- rasterize(slices_pa,pa_1ha,field = "ID",fun = "max")
slices_pa_25ha <- rasterize(slices_pa,pa_25ha,field = "ID",fun = "max")

slices_nil_1ha <- rasterize(slices_nil,nil_1ha,field = "ID",fun = "max")
slices_nil_25ha <- rasterize(slices_nil,nil_25ha,field = "ID",fun = "max")

writeRaster(slices_pa_1ha,"GIS/slices_pa_1ha.tif",overwrite = TRUE)
writeRaster(slices_pa_25ha,"GIS/slices_pa_25ha.tif",overwrite = TRUE)
writeRaster(slices_nil_1ha,"GIS/slices_nil_1ha.tif",overwrite = TRUE)
writeRaster(slices_nil_25ha,"GIS/slices_nil_25ha.tif",overwrite = TRUE)

shal <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/SHAL_RF_pa1400_1ha_c8_0.5km.tif"))
shma <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/SHMA_RF_nil1400_1ha_c8_0.5km.tif"))
mofa <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/MOFA_RF_pa1400_1ha_c8_0.5km.tif"))
moca <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/MOCA_RF_nil1400_1ha_c8_0.5km.tif"))
anni <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/ANNI_RF_nilpa1400_25ha_c0.25_3km.tif"))
eual <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/EUAL_RF_nilpa1400_1ha_c8_1km.tif"))
fini <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/FINI_RF_nilpa1400_1ha_c8_0.5km.tif"))

shkl <- merge(shal,shma)
lath <- merge(mofa,moca)

specs <- list(shkl,lath,anni,eual,fini)
specs_names <- c("SHKL","LATH","ANNI","EUAL","FINI")

for (i in 1:5) {
  for (region in c("nil","pa")) {
    cc <- specs[[i]]
    
    if (specs_names[i] == "ANNI") {
      if (region == "nil") {
        slices <- rast("GIS/slices_nil_25ha.tif")
      }
      
      if (region == "pa") {
        slices <- rast("GIS/slices_pa_25ha.tif")
      }
    }
    
    if (specs_names[i] != "ANNI") {
      if (region == "nil") {
        slices <- rast("GIS/slices_nil_1ha.tif")
      }
      
      if (region == "pa") {
        slices <- rast("GIS/slices_pa_1ha.tif")
      }
    }
    
    cc <- crop(cc,slices)
    
    r <- c(cc,slices)
    
    r_df <- as.data.frame(r,xy = TRUE) %>%
      na.omit()
    
    r_df$ID <- as.factor(r_df$ID)
    
    ggplot(r_df,aes(x = ID,y = cum_currmap)) +
      geom_boxplot(outliers = FALSE) +
      ylab(label = "Change in CCF") +
      xlab(label = "Slices") +
      ggtitle(label = paste0(specs_names[i],"_",region))
    
    ggsave(paste0("Omniscape/Output/Heatmaps/",specs_names[i],"_",region,"_boxplot_slice.png"),
           dpi = 300,
           bg = "white")
  }
}

specs <- list(shal,shma,mofa,moca,anni,fini,eual)
specs_names <- c("SHAL","SHMA","MOFA","MOCA","ANNI","FINI","EUAL")

for (i in 1:7) {
  cc <- specs[[i]]
  
  lc_change <- rast("GIS/Land cover/grassland_woodland_change_1ha.tif")
  
  lc_change <- crop(lc_change,cc)
  
  r <- c(cc,lc_change)
  
  r_df <- as.data.frame(r,xy = TRUE) %>%
    na.omit()
  
  ggplot(r_df,aes(x = class,y = cum_currmap)) +
    geom_boxplot(outliers = FALSE) +
    ylab(label = "Change in CCF") +
    theme(axis.text.x = element_text(hjust = 1,vjust = 1,angle = 45),
          axis.title.x = element_blank())
  
  ggsave(paste0("Omniscape/Output/Heatmaps/",specs_names[i],"_boxplot_lc_change.png"),
         dpi = 300,
         bg = "white")
}

slices_pa <- vect("GIS/Shapefiles/Slices/pa.shp")
lc_change_pa_25ha <- rast("GIS/Land cover/grassland_woodland_change_25ha.tif")

slices_pa <- project(slices_pa,crs(lc_change_pa_25ha))
lc_change_pa_25ha <- crop(lc_change_pa_25ha,slices_pa)

classes <- levels(lc_change_pa_25ha)[[1]][,2]
slices <- 1:5

df <- matrix(nrow = 8,
             ncol = 5,
             dimnames = list(classes,slices))

for (i in 1:5){
  shp <- slices_pa[slices_pa$ID == i]
  lc_change_pa_25ha_i <- crop(lc_change_pa_25ha,shp,mask = TRUE)
  df_i <- freq(lc_change_pa_25ha_i)
  
  for (j in classes) {
    if (j %in% df_i$value) {
      df[j,i] <- df_i[df_i$value == j,"count"]
    }
  }
}

xlsx::write.xlsx(df,"GIS/Land cover/lc_change_slices_pa_25ha.xlsx")

r <- rast("Omniscape/Output/2017/SHAL_RF_pa1400_1ha_c8/0.5km/normalized_cum_currmap.tif")

# Convert raster to dataframe
r_df <- as.data.frame(r,xy = TRUE)

std <- sd(r_df$normalized_cum_currmap,na.rm = TRUE)

std_breaks <- c()
for (i in -3:3) {
  std_breaks <- c(std_breaks,1 + i * std)
}

r_df <- r_df %>%
  mutate(std_bin = NA)

for (i in 1:nrow(r_df)) {
  for (j in 1:(length(std_breaks)-1)) {
    if (r_df[i,"normalized_cum_currmap"] >= std_breaks[j] & r_df[i,"normalized_cum_currmap"] < std_breaks[j+1]) {
      r_df[i,"std_bin"] <- j + 1
    }
    
    if (r_df[i,"normalized_cum_currmap"] < min(std_breaks)) {
      r_df[i,"std_bin"] <- 1
    }
    
    if (r_df[i,"normalized_cum_currmap"] >= max(std_breaks)) {
      r_df[i,"std_bin"] <- 8
    }
  }
}

r_std_bin <- rast(r_df[,c("x","y","std_bin")],type = "xyz",crs = crs(r))

std_breaks <- c(min(r_df$normalized_cum_currmap),std_breaks,max(r_df$normalized_cum_currmap))

std_breaks <- std_breaks[std_breaks != 1]

plot(r,breaks = std_breaks,col = c("black",
                                   "darkblue",
                                   "blue",
                                   "lightblue",
                                   "yellow",
                                   "red",
                                   "white"))


r <- rast("Omniscape/Output/Change/Difference_cum_currmap/SHMA_RF_nil1400_1ha_c8_0.5km.tif")

# Convert raster to dataframe
r_df <- as.data.frame(r,xy = TRUE)

std <- sd(r_df$cum_currmap)

r_df <- r_df %>%
  mutate(std_stretch = cum_currmap/std)

r_std_stretch <- rast(r_df[,c("x","y","std_stretch")],type = "xyz",crs = crs(r))

plot(r_std_stretch)



temp_df <- as.data.frame(anni)
temp <- filter(r_df_i,class == "Grassland to woodland")

wilcox.test(temp_df$cum_currmap,conf.int = TRUE)
wilcox.test(temp$cum_currmap,conf.int = TRUE)


specs <- list(shkl,lath,anni,eual,fini)
specs_names <- c("SHKL","LATH","ANNI","EUAL","FINI")

for (region in c("nil","pa")) {

  r_df <- data.frame()
  
  for (i in 1:5) {
    cc <- specs[[i]]
    
    cc_df <- as.data.frame(cc,xy = TRUE)
    
    std <- sd(cc_df$cum_currmap)
    
    cc_df <- cc_df %>%
      mutate(std_stretch = cum_currmap/std)
    
    cc <- rast(cc_df[,c("x","y","std_stretch")],type = "xyz",crs = crs(cc))
    
    if (specs_names[i] == "ANNI") {
      if (region == "nil") {
        slices <- rast("GIS/slices_nil_25ha.tif")
      }
      
      if (region == "pa") {
        slices <- rast("GIS/slices_pa_25ha.tif")
      }
    }
    
    if (specs_names[i] != "ANNI") {
      if (region == "nil") {
        slices <- rast("GIS/slices_nil_1ha.tif")
      }
      
      if (region == "pa") {
        slices <- rast("GIS/slices_pa_1ha.tif")
      }
    }
    
    cc <- crop(cc,slices)
    
    r <- c(cc,slices)
    
    r_df_i <- as.data.frame(r) %>%
      mutate(spec = specs_names[i]) %>%
      na.omit()
    
    r_df_i$ID <- as.factor(r_df_i$ID)
    
    r_df<- rbind(r_df,r_df_i)
  }
  
  ggplot(r_df,aes(x = ID,y = std_stretch,fill = spec)) +
    geom_boxplot(outliers = FALSE) +
    ylab(label = "Change in CCF") +
    xlab(label = "Slices") +
    ggtitle(label = region)
  
  ggsave(paste0("Omniscape/Output/Heatmaps/combined_",region,"_boxplot_slice.png"),
         dpi = 300,
         bg = "white")
}

for (region in c("nil","pa")) {
  
  for (type in c("cc","pc")) {
    
    if (type == "pc") {
      shal <- rast(paste0(proj_path,"Omniscape/Output/2017/SHAL_RF_pa1400_1ha_c8/0.5km/cum_currmap.tif"))
      shma <- rast(paste0(proj_path,"Omniscape/Output/2017/SHMA_RF_nil1400_1ha_c8/0.5km/cum_currmap.tif"))
      mofa <- rast(paste0(proj_path,"Omniscape/Output/2017/MOFA_RF_pa1400_1ha_c8/0.5km/cum_currmap.tif"))
      moca <- rast(paste0(proj_path,"Omniscape/Output/2017/MOCA_RF_nil1400_1ha_c8/0.5km/cum_currmap.tif"))
      anni <- rast(paste0(proj_path,"Omniscape/Output/2017/ANNI_RF_nilpa1400_25ha_c0.25/3km/cum_currmap.tif"))
      eual <- rast(paste0(proj_path,"Omniscape/Output/2017/EUAL_RF_nilpa1400_1ha_c8/1km/cum_currmap.tif"))
      fini <- rast(paste0(proj_path,"Omniscape/Output/2017/FINI_RF_nilpa1400_1ha_c8/0.5km/cum_currmap.tif"))
    }
    
    if (type == "cc") {
      shal <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/SHAL_RF_pa1400_1ha_c8_0.5km.tif"))
      shma <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/SHMA_RF_nil1400_1ha_c8_0.5km.tif"))
      mofa <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/MOFA_RF_pa1400_1ha_c8_0.5km.tif"))
      moca <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/MOCA_RF_nil1400_1ha_c8_0.5km.tif"))
      anni <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/ANNI_RF_nilpa1400_25ha_c0.25_3km.tif"))
      eual <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/EUAL_RF_nilpa1400_1ha_c8_1km.tif"))
      fini <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/FINI_RF_nilpa1400_1ha_c8_0.5km.tif"))
    }
    
    if (region == "nil") {
      shp <- vect(paste0(proj_path,"GIS/Shapefiles/Nilgiri1400m/Nilgiri1400m.shp"))
      maps <- list(shma,moca,fini,eual,anni)
      species <- c("SHMA","MOCA","FINI ","EUAL","ANNI")
    }
    
    if (region == "pa") {
      shp <- vect(paste0(proj_path,"GIS/Shapefiles/PalaniAanamalai1400m/PalaniAanamalai1400m.shp"))
      maps <- list(shal,mofa,fini,eual,anni)
      species <- c("SHAL","MOFA","FINI ","EUAL","ANNI")
    }
    
    df <- c()
    
    for (i in seq_along(species)) {
      
      r <- maps[[i]]
      
      r <- project(r,"epsg:4326")
      
      r_df <- as.data.frame(r,xy = TRUE)
      
      colnames(r_df)[3] <- "value"
      
      std <- sd(r_df$value)
      
      if (type == "cc") {
        r_df <- r_df %>%
          mutate(std_stretch = value/std)
      }
      
      if (type == "pc") {
        r_df <- r_df %>%
          mutate(std_stretch = (value - 1)/std)
      }
      
      r <- rast(r_df[,c("x","y","std_stretch")],type = "xyz",crs = crs(r))
      
      r_clip <- crop(r,shp,mask = TRUE)
      
      r_df <- as.data.frame(r_clip,xy = TRUE)
      
      bin_size<-(max(r_df$x) - min(r_df$x))/5
      
      sum_val <- c()
      
      for (j in 1:5) {
        b <- filter(r_df,x >= min(r_df$x) + (j - 1) * bin_size,x < min(r_df$x) + j * bin_size)[,3]
        c <- mean(b)
        sum_val <- c(sum_val,c)
      }
      
      df <- rbind(df,sum_val)
    }
    
    row.names(df) <- species
    colnames(df) <- 1:5
    
    data <- reshape2::melt(df)
    
    colnames(data) <- c("Species","Slice","value")
    
    ggplot(data, aes(x = Slice, y = Species, fill = value))+
      geom_tile() +
      theme_minimal() + 
      scale_fill_gradient2(breaks = round(c(min(data$value),0,max(data$value)),2))
    
    ggsave(paste0(proj_path,"Omniscape/Output/Heatmaps/heatmap_",region,"_",type,"_std_stretch.png"),
           bg = "white",
           width = 230,
           height = 150,
           units = "mm",
           dpi = 500)
    
    write.csv(df,paste0(proj_path,"Omniscape/Output/Heatmaps/heatmap_",region,"_",type,"_std_stretch.csv"))
  }
}

shal <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/SHAL_RF_pa1400_1ha_c8_0.5km.tif"))
shma <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/SHMA_RF_nil1400_1ha_c8_0.5km.tif"))
mofa <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/MOFA_RF_pa1400_1ha_c8_0.5km.tif"))
moca <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/MOCA_RF_nil1400_1ha_c8_0.5km.tif"))
anni <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/ANNI_RF_nilpa1400_25ha_c0.25_3km.tif"))
eual <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/EUAL_RF_nilpa1400_1ha_c8_1km.tif"))
fini <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/FINI_RF_nilpa1400_1ha_c8_0.5km.tif"))

specs <- list(shal,shma,mofa,moca,anni,fini,eual)
specs_names <- c("SHAL","SHMA","MOFA","MOCA","ANNI","FINI","EUAL")

r_df <- data.frame()

for (i in 1:7) {
  cc <- specs[[i]]
  
  cc_df <- as.data.frame(cc,xy = TRUE)
  
  std <- sd(cc_df$cum_currmap)
  
  cc_df <- cc_df %>%
    mutate(std_stretch = cum_currmap/std)
  
  cc <- rast(cc_df[,c("x","y","std_stretch")],type = "xyz",crs = crs(cc))
  
  if (specs_names[i] == "ANNI") {
    lc_change <- rast("GIS/Land cover/grassland_woodland_change_25ha.tif")
  }
  
  if (specs_names[i] != "ANNI") {
    lc_change <- rast("GIS/Land cover/grassland_woodland_change_1ha.tif")
  }
  
  lc_change <- crop(lc_change,cc)
  
  r <- c(cc,lc_change)
  
  r_df_i <- as.data.frame(r) %>%
    mutate(spec = specs_names[i]) %>%
    na.omit()
  
  r_df <- rbind(r_df,r_df_i)
}

r_df$spec <- as.factor(r_df$spec)

r_df_spec <- filter(r_df,spec == "ANNI")[,c(1,2)]

kruskal.test(std_stretch ~ class,data = r_df_spec)

ggplot(r_df,aes(x = class,y = std_stretch,fill = spec)) +
  geom_boxplot(outliers = FALSE) +
  ylab(label = "Change in CCF") +
  theme(axis.text.x = element_text(hjust = 1,vjust = 1,angle = 45),
        axis.title.x = element_blank())

ggsave(paste0("Omniscape/Output/Heatmaps/combined_boxplot_lc_change_std_stretch.png"),
       dpi = 300,
       bg = "white")




for (spec in c("SHAL","SHMA","MOFA","MOCA","FINI","EUAL","ANNI")) {
  for (type in c("pc","cc")) {
    if (type == "pc") {
      files <- list.files(path = "Omniscape/Output/2017",
                          pattern = "normalized_cum_currmap.tif$",
                          recursive = TRUE,
                          full.names = TRUE)
      
      files_spec <- files[grepl(spec,files)]
      
      st <- rast(files_spec)
      names(st) <- lapply(files_spec,function(x) {
        
        x <- strsplit(x,"/")[[1]]
        c <- strsplit(x[4],"_")[[1]][length(strsplit(x[4],"_")[[1]])]
        d <- x[5]
        
        y <- paste(c,d,sep = "_")
        
        y
      })
    }
    
    if (type == "cc") {
      files <- list.files(path = "Omniscape/Output/Change/Difference_cum_currmap",
                          pattern = ".tif$",
                          recursive = TRUE,
                          full.names = TRUE)
      
      files_spec <- files[grepl(spec,files)]
      
      st <- rast(files_spec)
      names(st) <- lapply(files_spec,function(x) {
        
        x <- strsplit(x,"/")[[1]]
        c <- strsplit(x[5],"_")[[1]][length(strsplit(x[5],"_")[[1]]) - 1]
        d <- strsplit(x[5],"_")[[1]][length(strsplit(x[5],"_")[[1]])]
        d <- gsub(".tif","",d)
    
        y <- paste(c,d,sep = "_")
        
        y
      })
    }
    
    # Extract values, remove NA
    vals <- values(st)
    vals <- vals[complete.cases(vals), ]
    
    # Compute Spearman correlation
    corr_matrix <- cor(vals, method = "spearman")
    
    # Create correlation plot and save as PNG
    png(paste0(proj_path,"Omniscape/Output/Heatmaps/corrplot_",spec,".png"))
    corrplot(corr_matrix,
             is.corr = FALSE, 
             method = "square",
             type = "upper",
             diag = FALSE,
             addCoef.col = TRUE)
    dev.off()
  }
}

for (region in c("nil","pa")) {
  
  if (region == "nil") {
    shp <- vect(paste0(proj_path,"GIS/Shapefiles/Nilgiri1400m/Nilgiri1400m.shp"))
    species <- c("SHMA","MOCA","FINI","EUAL","ANNI")
  }
  
  if (region == "pa") {
    shp <- vect(paste0(proj_path,"GIS/Shapefiles/PalaniAanamalai1400m/PalaniAanamalai1400m.shp"))
    species <- c("SHAL","MOFA","FINI","EUAL","ANNI")
  }
  
  for (spec in species) {
    for (type in c("cc","pc")) {
      if (type == "pc") {
        files <- list.files(path = paste0(proj_path,"Omniscape/Output/2017"),
                            pattern = "cum_currmap.tif$",
                            recursive = TRUE,
                            full.names = TRUE)
        
        files_spec <- files[grepl(spec,files) & ! grepl("std_stretch",files) & ! grepl("normalized",files)]
        
        st <- rast(files_spec)
        names(st) <- lapply(files_spec,function(x) {
          
          x <- strsplit(x,"/")[[1]]
          c <- strsplit(x[4],"_")[[1]][length(strsplit(x[4],"_")[[1]])]
          d <- x[5]
          
          y <- paste(c,d,sep = "_")
          
          y
        })
      }
      
      if (type == "cc") {
        files <- list.files(path = paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap"),
                            pattern = ".tif$",
                            recursive = TRUE,
                            full.names = TRUE)
        
        files_spec <- files[grepl(spec,files) & ! grepl("std_stretch",files)]
        
        st <- rast(files_spec)
        names(st) <- lapply(files_spec,function(x) {
          
          x <- strsplit(x,"/")[[1]]
          c <- strsplit(x[5],"_")[[1]][length(strsplit(x[5],"_")[[1]]) - 1]
          d <- strsplit(x[5],"_")[[1]][length(strsplit(x[5],"_")[[1]])]
          
          y <- paste(c,d,sep = "_")
          
          y
        })
      }
      
      df <- c()
      row_names <- c()
      
      for (i in 1:nlyr(st)) {
        
        r <- st[[i]]
        
        std <- sd(values(r,na.rm = TRUE))
        avg <- mean(values(r,na.rm = TRUE))
        
        if (type == "cc") {
          r <- r/std
          type_name <- "Change in CCF"
        }
        
        if (type == "pc") {
          r <- (r - avg)/std
          type_name <- "CCF"
        }
        
        names(r) <- "std_stretch"
        
        writeRaster(r,sub(".tif","_std_stretch.tif",files_spec[i]),overwrite = TRUE)
        
        if ((spec == "ANNI" & ! grepl("c8",files_spec[i])) | (spec != "ANNI" & ! grepl("c0.25",files_spec[i]))) {
          
          r <- project(r,"epsg:4326")
          
          r_df <- as.data.frame(r,xy = TRUE)
          
          if (spec %in% c("FINI","EUAL","ANNI")) {
            r_clip <- crop(r,shp,mask = TRUE)
            r_df <- as.data.frame(r_clip,xy = TRUE)
          }
          
          bin_size <- (max(r_df$x) - min(r_df$x))/100
          
          sum_val <- c()
          longs <- c()
          
          for (j in 1:100) {
            b <- filter(r_df,x >= min(r_df$x) + (j - 1) * bin_size,x < min(r_df$x) + j * bin_size)[,"std_stretch"]
            c <- median(b,na.rm = TRUE)
            sum_val <- c(sum_val,c)
            longs <- c(longs,min(r_df$x) + (j - 1) * bin_size)
          }
          
          df <- rbind(df,sum_val)
          row_names <- c(row_names,names(r))
        }
      }
      
      row.names(df) <- row_names
      colnames(df) <- longs
      
      data <- reshape2::melt(df) %>% na.omit()
      
      colnames(data) <- c("Model","Longitude",type_name)
      
      ggplot(data,aes(x = Longitude,y = .data[[type_name]],fill = Model))+
        geom_smooth(aes(color = Model),method = "gam") +
        scale_y_continuous(
          limits = function(x) {
            max_val <- max(abs(x))
            c(-max_val, max_val)
          },
          breaks = function(x) {
            pretty(c(-max(abs(x)), max(abs(x))))
          }
        )
      
      ggsave(paste0(proj_path,"Omniscape/Output/Final plots/line_",spec,"_",region,"_",type,"_std_stretch.png"),
             bg = "white",
             width = 230,
             height = 150,
             units = "mm",
             dpi = 600)
    }
  }
}

shal <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/SHAL_RF_pa1400_1ha_c8_0.5km.tif"))
shma <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/SHMA_RF_nil1400_1ha_c8_0.5km.tif"))
mofa <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/MOFA_RF_pa1400_1ha_c8_0.5km.tif"))
moca <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/MOCA_RF_nil1400_1ha_c8_0.5km.tif"))
anni <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/ANNI_RF_nilpa1400_25ha_c0.25_3km.tif"))
eual <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/EUAL_RF_nilpa1400_1ha_c8_1km.tif"))
fini <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/FINI_RF_nilpa1400_1ha_c8_0.5km.tif"))

specs <- list(shal,shma,mofa,moca,anni,fini,eual)
specs_names <- c("SHAL","SHMA","MOFA","MOCA","ANNI","FINI","EUAL")

files <- list.files(path = paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap"),
                    pattern = ".tif$",
                    recursive = TRUE,
                    full.names = TRUE)

df <- c()
col_names <- c()

for (i in files) {
  cc <- rast(i)
  
  model <- sub(".tif","",strsplit(i,"/")[[1]][11])
  model <- paste(strsplit(model,"_")[[1]][1],strsplit(model,"_")[[1]][5],strsplit(model,"_")[[1]][6],sep = "_")
  
  col_names <- c(col_names,model)
  
  if (grepl("ANNI",model)) {
    lc_change <- rast("GIS/Land cover/grassland_woodland_change_25ha.tif")
    
    zero <- c("Stable woodland",
              "Stable grassland",
              "Human-dominated to woodland",
              "Woodland to human-dominated",
              "Historical non-habitat")
    
    negative <- c("Grassland to woodland",
                  "Grassland to human-dominated")
    
    positive <- "Grassland gain" 
  }
  
  if (! grepl("ANNI",model)) {
    lc_change <- rast("GIS/Land cover/grassland_woodland_change_1ha.tif")
    
    zero <- c("Stable woodland",
              "Stable grassland",
              "Grassland gain",
              "Grassland to human-dominated",
              "Historical non-habitat")
    
    negative <- "Woodland to human-dominated"
    
    positive <- c("Human-dominated to woodland",
                  "Grassland to woodland")
  }
  
  lc_change <- crop(lc_change,cc)
  
  r <- c(cc,lc_change)
  
  r_df <- as.data.frame(r) %>%
    na.omit()
  
  pvalue <- c()
  
  for (j in levels(lc_change)[[1]][,"class"]) {
    r_df_j <- filter(r_df,class == j)
    
    if (j %in% zero) {
      t <- wilcox.test(r_df_j$cum_currmap,alternative = "two.sided")
    }
    
    if (j %in% negative) {
      t <- wilcox.test(r_df_j$cum_currmap,alternative = "less")
    }
    
    if (j %in% positive) {
      t <- wilcox.test(r_df_j$cum_currmap,alternative = "greater")
    }
    
    pvalue <- c(pvalue,t$p.value)
  }
  
  df <- cbind(df,pvalue)
}

colnames(df) <- col_names
row.names(df) <- levels(lc_change)[[1]][,"class"]

df_p <- matrix(nrow = nrow(df),ncol = ncol(df),dimnames = dimnames(df))

for (i in row.names(df)) {
  for (j in colnames(df)) {
    if (df[i,j] <= 0.05) {
      df_p[i,j] <- TRUE
    }
    
    if (df[i,j] > 0.05) {
      df_p[i,j] <- FALSE
    }
  }
}

shal <- rast(paste0(proj_path,"Omniscape/Output/2017/SHAL_RF_pa1400_1ha_c8/0.5km/normalized_cum_currmap_std_stretch.tif"))
shma <- rast(paste0(proj_path,"Omniscape/Output/2017/SHMA_RF_nil1400_1ha_c8/0.5km/normalized_cum_currmap_std_stretch.tif"))
mofa <- rast(paste0(proj_path,"Omniscape/Output/2017/MOFA_RF_pa1400_1ha_c8/0.5km/normalized_cum_currmap_std_stretch.tif"))
moca <- rast(paste0(proj_path,"Omniscape/Output/2017/MOCA_RF_nil1400_1ha_c8/0.5km/normalized_cum_currmap_std_stretch.tif"))
anni <- rast(paste0(proj_path,"Omniscape/Output/2017/ANNI_RF_nilpa1400_25ha_c0.25/3km/normalized_cum_currmap_std_stretch.tif"))
eual <- rast(paste0(proj_path,"Omniscape/Output/2017/EUAL_RF_nilpa1400_1ha_c8/1km/normalized_cum_currmap_std_stretch.tif"))
fini <- rast(paste0(proj_path,"Omniscape/Output/2017/FINI_RF_nilpa1400_1ha_c8/0.5km/normalized_cum_currmap_std_stretch.tif"))

shkl <- merge(shma,shal)
lath <- merge(moca,mofa)

spec_names <- C("SHKL","LATH","EUAL","FINI","ANNI")

for (region in c("nil","pa")) {
  for (type in c("cc","pc")) {
      
    if (type == "pc") {
      shal <- rast(paste0(proj_path,"Omniscape/Output/2017/SHAL_RF_pa1400_1ha_c8/0.5km/normalized_cum_currmap_std_stretch.tif"))
      shma <- rast(paste0(proj_path,"Omniscape/Output/2017/SHMA_RF_nil1400_1ha_c8/0.5km/normalized_cum_currmap_std_stretch.tif"))
      mofa <- rast(paste0(proj_path,"Omniscape/Output/2017/MOFA_RF_pa1400_1ha_c8/0.5km/normalized_cum_currmap_std_stretch.tif"))
      moca <- rast(paste0(proj_path,"Omniscape/Output/2017/MOCA_RF_nil1400_1ha_c8/0.5km/normalized_cum_currmap_std_stretch.tif"))
      anni <- rast(paste0(proj_path,"Omniscape/Output/2017/ANNI_RF_nilpa1400_25ha_c0.25/3km/normalized_cum_currmap_std_stretch.tif"))
      eual <- rast(paste0(proj_path,"Omniscape/Output/2017/EUAL_RF_nilpa1400_1ha_c8/1km/normalized_cum_currmap_std_stretch.tif"))
      fini <- rast(paste0(proj_path,"Omniscape/Output/2017/FINI_RF_nilpa1400_1ha_c8/0.5km/normalized_cum_currmap_std_stretch.tif"))
    }
    
    if (type == "cc") {
      shal <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/SHAL_RF_pa1400_1ha_c8_0.5km/raster_std_stretch.tif"))
      shma <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/SHMA_RF_nil1400_1ha_c8_0.5km/raster_std_stretch.tif"))
      mofa <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/MOFA_RF_pa1400_1ha_c8_0.5km/raster_std_stretch.tif"))
      moca <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/MOCA_RF_nil1400_1ha_c8_0.5km/raster_std_stretch.tif"))
      anni <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/ANNI_RF_nilpa1400_25ha_c0.25_3km/raster_std_stretch.tif"))
      eual <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/EUAL_RF_nilpa1400_1ha_c8_1km/raster_std_stretch.tif"))
      fini <- rast(paste0(proj_path,"Omniscape/Output/Change/Difference_cum_currmap/FINI_RF_nilpa1400_1ha_c8_0.5km/raster_std_stretch.tif"))
    }
    
    if (region == "nil") {
      shp <- vect(paste0(proj_path,"GIS/Shapefiles/Nilgiri1400m/Nilgiri1400m.shp"))
      maps <- list(shma,moca,fini,eual,anni)
      species <- c("SHMA","MOCA","FINI ","EUAL","ANNI")
    }
    
    if (region == "pa") {
      shp <- vect(paste0(proj_path,"GIS/Shapefiles/PalaniAanamalai1400m/PalaniAanamalai1400m.shp"))
      maps <- list(shal,mofa,fini,eual,anni)
      species <- c("SHAL","MOFA","FINI ","EUAL","ANNI")
    }
    
    for (i in 1:5) {
      
      r <- maps[[i]]
      r <- project(r,"epsg:4326")
      
      r <- crop(r,shp,mask = TRUE)
      
      # Convert to dataframe
      r_df <- as.data.frame(r,xy = TRUE)
      
      # Define color scale
      colors <- scales::pal_viridis()(3)
      value_breaks <- quantile(r_df[,"std_stretch"], probs = seq(0,1,by = 0.01))
      
      # Plot difference in current flow raster with a diverging colour scale
      ggplot(r_df, aes(x, y, fill = std_stretch)) +
        geom_tile() +
        scale_fill_gradientn(
          name = t,
          colors = colors,
          values = scales::rescale(value_breaks),
          breaks = c(value_breaks[1],0,value_breaks[101]),
          labels = round(c(value_breaks[1],0,value_breaks[101]), 2)
        ) +
        theme_minimal() +
        theme(legend.position = "right",
              legend.title = element_blank()) +
        coord_fixed()
      
      ggsave(paste0(proj_path,"Presentation/Paper figures/",spec,"_",region,"_",type,".png"),
             width = 300,
             height = 300,
             units = "mm",
             dpi = 600)
    }
  }
}

cowplot::plot_grid(plotlist = plot_list,
                   nrow = 2,
                   ncol = 3)




ncf <- rast("Omniscape/Output/2017/MOCA_RF_nil1400_1ha_c8/0.5km/normalized_cum_currmap.tif")
fp <- rast("Omniscape/Output/2017/MOCA_RF_nil1400_1ha_c8/0.5km/flow_potential.tif")

plot(fp)

fp_min <- min(values(fp),na.rm = TRUE)
fp_max <- max(values(fp),na.rm = TRUE)
bin_size <- (fp_max - fp_min) / 3

r <- rast(ncf)

r[ncf[] >= 2 & fp[] < fp_min + bin_size] <- 1
r[ncf[] >= 2 & fp[] >= fp_min + bin_size & fp[] < fp_min + bin_size * 2 ] <- 2
r[ncf[] >= 2 & fp[] >= fp_min + bin_size * 2] <- 3

r[ncf[] >= 1.2 & ncf[] < 2 & fp[] < fp_min + bin_size] <- 4
r[ncf[] >= 1.2 & ncf[] < 2 & fp[] >= fp_min + bin_size & fp[] < fp_min + bin_size * 2 ] <- 5
r[ncf[] >= 1.2 & ncf[] < 2 & fp[] >= fp_min + bin_size * 2] <- 6

r[ncf[] >= 0.7 & ncf[] < 1.2 & fp[] < fp_min + bin_size] <- 7
r[ncf[] >= 0.7 & ncf[] < 1.2 & fp[] >= fp_min + bin_size & fp[] < fp_min + bin_size * 2 ] <- 8
r[ncf[] >= 0.7 & ncf[] < 1.2 & fp[] >= fp_min + bin_size * 2] <- 9

r[ncf[] < 0.7 & fp[] < fp_min + bin_size] <- 10
r[ncf[] < 0.7 & fp[] >= fp_min + bin_size & fp[] < fp_min + bin_size * 2 ] <- 11
r[ncf[] < 0.7 & fp[] >= fp_min + bin_size * 2] <- 12

plot(r,col = data.frame(value = 1:12,
                        colour = c("white","#93C572","#FFC300",
                                   "lightblue","#228B22","#FF5733",
                                   "blue","#454B1B","#C70039",
                                   "darkblue","#023020","#900C3F")))

for (region in c("nil","pa")) {
  
  shal <- rast(paste0(proj_path,"Omniscape/Output/2017/SHAL_RF_pa1400_1ha_c8/0.5km/cum_currmap.tif"))
  shma <- rast(paste0(proj_path,"Omniscape/Output/2017/SHMA_RF_nil1400_1ha_c8/0.5km/cum_currmap.tif"))
  mofa <- rast(paste0(proj_path,"Omniscape/Output/2017/MOFA_RF_pa1400_1ha_c8/0.5km/cum_currmap.tif"))
  moca <- rast(paste0(proj_path,"Omniscape/Output/2017/MOCA_RF_nil1400_1ha_c8/0.5km/cum_currmap.tif"))
  anni <- rast(paste0(proj_path,"Omniscape/Output/2017/ANNI_RF_nilpa1400_25ha_c0.25/3km/cum_currmap.tif"))
  eual <- rast(paste0(proj_path,"Omniscape/Output/2017/EUAL_RF_nilpa1400_1ha_c8/1km/cum_currmap.tif"))
  fini <- rast(paste0(proj_path,"Omniscape/Output/2017/FINI_RF_nilpa1400_1ha_c8/0.5km/cum_currmap.tif"))
  
  if (region == "nil") {
    shp <- vect(paste0(proj_path,"GIS/Shapefiles/Nilgiri1400m/Nilgiri1400m.shp"))
    maps <- list(anni,eual,moca,fini,shma)
    species <- c("ANNI","EUAL","MOCA","FINI","SHMA")
    cols <- c("#74c476",
              "#9ecae1",
              "#4292c6",
              "#fc9272",
              "#fb6a4a")
  }
  
  if (region == "pa") {
    shp <- vect(paste0(proj_path,"GIS/Shapefiles/PalaniAanamalai1400m/PalaniAanamalai1400m.shp"))
    maps <- list(anni,eual,mofa,fini,shal)
    species <- c("ANNI","EUAL","MOFA","FINI","SHAL")
    cols <- c("#74c476",
              "#9ecae1",
              "#084594",
              "#fc9272",
              "#ef3b2c")
  }
  
  df <- c()
  
  for (i in seq_along(species)) {
    
    r <- maps[[i]]
    r <- project(r,"epsg:4326")
    
    # std <- sd(values(r,na.rm = TRUE))
    # m <- mean(values(r,na.rm = TRUE))
    # 
    # r <- (r - m)/std
    # 
    # names(r) <- "std_stretch"
    
    type_name <- "CCF"
    
    r_clip <- crop(r,shp,mask = TRUE)
    
    r_df <- as.data.frame(r_clip,xy = TRUE)
    
    bin_size <- (max(r_df$x) - min(r_df$x))/100
    
    sum_val <- c()
    longs <- c()
    
    for (j in 1:100) {
      b <- filter(r_df,x >= min(r_df$x) + (j - 1) * bin_size,x < min(r_df$x) + j * bin_size)[,3]
      c <- median(b,na.rm = TRUE)
      sum_val <- c(sum_val,c)
      longs <- c(longs,min(r_df$x) + (j - 1) * bin_size)
    }
    
    df <- rbind(df,sum_val)
  }
  
  row.names(df) <- species
  colnames(df) <- longs
  
  data <- reshape2::melt(df) %>% na.omit()
  
  colnames(data) <- c("Species","Longitude",type_name)
  
  p <- ggplot(data,aes(x = Longitude,y = .data[[type_name]],fill = Species))+
    geom_smooth(aes(color = Species),method = "gam") +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    theme_classic() + 
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.line.x = element_blank(),
          # panel.grid.major.y = element_blank(), 
          # panel.grid.minor.y = element_blank(),
          # panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line(colour = "black"),
          legend.title = element_blank(),
          legend.key.spacing.y = unit(3,"mm"),
          text = element_text(size = 22,face = "bold")) +
    scale_y_continuous(
      limits = function(x) {
        max_val <- max(abs(x))
        c(-max_val, max_val)
      },
      breaks = function(x) {
        pretty(c(-max(abs(x)), max(abs(x))))
      }
    ) +
    scale_x_continuous(breaks = seq(min(data$Longitude) + bin_size * 20,max(data$Longitude),by = bin_size * 20))
  
  if (region == "pa") {
    p <- p + theme(axis.title.y = element_blank())
  }
  
  plot(p)
  
  ggsave(paste0(proj_path,"Omniscape/Output/Heatmaps/heatmap_",region,"_",type_name,"_std_stretch_line.png"),
         bg = "white",
         width = 280,
         height = 150,
         units = "mm",
         dpi = 600)
}