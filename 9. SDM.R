# Load packages
library(SDMtune)
library(zeallot)
library(tidyverse)
library(terra)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

# Create loop to run SDMs for all species
for (spec in c("SHAL","SHMA","MOFA","MOCA","ANNI")){
  ## Set species-specific region
  
  # Nilgiris
  if (spec %in% c("SHAL","MOFA")){
    region <- "pahw1400"
  }
  
  # Palani-Anamalai-Highwavies
  if (spec %in% c("SHMA","MOCA")){
    region <- "nil1400"
  }
  
  # Southern Western Ghats
  if (spec == "ANNI"){
    region <- "swg1400"
  }
  
  # Load vector with filtered variables
  load(paste0(proj_path,"SDM/Input/",region,"_1ha/Correlation/",region,"_fil.RData"))
  
  # Read raster stack of predictor variables
  predictors <- rast(paste0(proj_path,"SDM/Input/",region,"_1ha/",region,"_predictors_present.tif"))
  
  # Filter to get only non-correlated variables
  predictors <- predictors[[fil_vars]]
  
  # Load CSV file with occupancy data
  occ <- read.csv(paste0(proj_path,"occupancy data/Jobin/Filtered/",spec,".csv"))
  
  # Extract presence and absence locations
  p_coords <- filter(occ,Presence == 1)[,c(1,2)]
  a_coords <- filter(occ,Presence == 0)[,c(1,2)]
  
  # Combine all input data and extract predictor variables for all locations
  data <- prepareSWD(species = spec,
                     p = p_coords,
                     a = a_coords,
                     env = predictors,
                     categorical = c("landcov","clim_zone"))
  
  # Split data in training and testing datasets
  c(train, test) %<-% trainValTest(data,
                                   test = 0.2,
                                   seed = 25)
  
  # Create output folder in directory if does not exist
  if (!dir.exists(paste0(proj_path,"SDM/Output/",spec))){
    dir.create(paste0(proj_path,"SDM/Output/",spec),recursive = TRUE)
  }
  
  # Save training and testing datasets
  swd2csv(train, file_name = paste0(proj_path,"SDM/Output/",spec,"/",spec,"_",region,"_train.csv"))
  swd2csv(test, file_name = paste0(proj_path,"SDM/Output/",spec,"/",spec,"_",region,"_test.csv"))
  
  # Create folds
  folds <- randomFolds(train,
                       k = 10,
                       seed = 25)
  
  # Train preliminary model using training dataset and cross-validation
  set.seed(25)
  model <- train ("RF",
                  data = train,
                  folds = folds)
  
  # Set hyperparameter value ranges for tuning
  h <- list(mtry = floor(sqrt(nlyr(predictors))),
            ntree = 100:1000,
            nodesize = 1:20)
  
  # Train models with multiple hyperparameter values to get best model
  om <- optimizeModel(model,
                      hypers = h,
                      env = predictors,
                      metric = "tss",
                      test = test,
                      seed = 25)
  
  # View best model hyperparameter values
  om@results[1, ]
  
  # Combine all cross-validated models and retrain using best model hyperparameters
  set.seed(25)
  final_model <- combineCV(om@models[[1]])
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Output/",spec,"/Models"))){
    dir.create(paste0(proj_path,"SDM/Output/",spec,"/Models"),recursive = TRUE)
  }
  
  # Save model
  saveRDS(final_model,file = paste0(proj_path,"SDM/Output/",spec,"/Models/RF_",spec,"_",region,".rds"))
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Output/",spec,"/ROCs"))){
    dir.create(paste0(proj_path,"SDM/Output/",spec,"/ROCs"),recursive = TRUE)
  }
  
  # Evaluate model
  auc(final_model)
  tss(final_model)
  
  # Plot ROC curve and save plot
  plotROC(final_model, 
          test = test)
  ggsave(paste0(proj_path,"SDM/Output/",spec,"/ROCs/RF_",spec,"_",region,".png"),
         bg = "white")
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Output/",spec,"/Prediction"))){
    dir.create(paste0(proj_path,"SDM/Output/",spec,"/Prediction"),recursive = TRUE)
  }
  
  # Project prediction to entire region and save output raster
  map <- predict(final_model, 
                 data = predictors,
                 filename = paste0(proj_path,"SDM/Output/",spec,"/Prediction/RF_",spec,"_",region,".tif"),
                 overwrite = TRUE)
  
  # Plot output raster and save plot
  plotPred(map, 
           lt = "Habitat\nsuitability",
           hr = TRUE)
  ggsave(paste0(proj_path,"SDM/Output/",spec,"/Prediction/RF_",spec,"_",region,".png"),
         bg = "white")
  
  # Find optimal thresholds for model to split into binary values
  ths <- thresholds(final_model)
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Output/",spec,"/PA map"))){
    dir.create(paste0(proj_path,"SDM/Output/",spec,"/PA map"),recursive = TRUE)
  }
  
  # Create and plot binary presence-absence raster and save 
  plotPA(map, 
         th = ths[3, 2], 
         filename = paste0(proj_path,"SDM/Output/",spec,"/PA map/RF_",spec,"_",region,".tif"), 
         format = "GTiff",
         overwrite = TRUE,
         hr = TRUE)
  ggsave(paste0(proj_path,"SDM/Output/",spec,"/PA map/RF_",spec,"_",region,".png"),bg="white")
  
  # Compute variable importance for final model
  vi <- varImp(final_model)
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Output/",spec,"/Variable importance"))){
    dir.create(paste0(proj_path,"SDM/Output/",spec,"/Variable importance"),
               recursive = TRUE)
  }
  
  # Plot variable importance and save plot
  plotVarImp(vi)
  ggsave(paste0(proj_path,"SDM/Output/",spec,"/Variable importance/RF_",spec,"_",region,".png"),
         bg = "white")
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Output/",spec,"/Response plots/RF_",spec,"_",region))){
    dir.create(paste0(proj_path,"SDM/Output/",spec,"/Response plots/RF_",spec,"_",region),
               recursive = TRUE)
  }
  
  # Convert prediction raster to dataframe
  sdm_df <- as.data.frame(map,xy = TRUE,na.rm = TRUE)
  
  # Convert predictor variables to dataframe
  predictors_df <- as.data.frame(predictors,xy = TRUE,na.rm = TRUE)
  
  # Combine both dataframes and remove NAs
  df <- merge(x = sdm_df,
              y = predictors_df,
              by = c("x","y"),
              all.x = T) %>% na.omit()
  
  # Rename column
  colnames(df)[3] <- "suitability"
  
  # Plot predicted probability of presence vs. predictor variables 
  # for each variable and save plots
  
  for (variable in names(predictors)){
    if (variable %in% c("landcov","clim_zone")){
      ggplot(df, aes(.data[[variable]], .data[["suitability"]])) +
           geom_violin() +
           ylab("Probability of presence")
    }
    else {
      ggplot(df, aes(.data[[variable]], .data[["suitability"]])) +
           geom_smooth() +
           ylab("Probability of presence")
    }
    ggsave(paste0(proj_path,"SDM/Output/",spec,"/Response plots/RF_",spec,"_",region,"/",variable,".png"),
           bg = "white")
  }
  
  # Read past predictor variables raster stack
  predictors_past <- rast(paste0(proj_path,"SDM/Input/",region,"_1ha/",region,"_predictors_past.tif"))
  
  # Filter out unused variables
  predictors_past <- predictors_past[[fil_vars]]
    
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Output/",spec,"/Past prediction"))){
    dir.create(paste0(proj_path,"SDM/Output/",spec,"/Past prediction"),
               recursive = TRUE)
  }
  
  # Project prediction to past
  map_past <- predict(final_model,
                      data = predictors_past,
                      file = paste0(proj_path,"SDM/Output/",spec,"/Past prediction/RF_",spec,"_",region,".tif"),
                      overwrite = TRUE)
  
  # Plot prediction for past and save plot
  plotPred(map_past,
           lt = "Habitat\nsuitability",
           hr = TRUE)
  ggsave(paste0(proj_path,"SDM/Output/",spec,"/Past prediction/RF_",spec,"_",region,".png"),
         bg = "white")
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Output/",spec,"/Past PA map"))){
    dir.create(paste0(proj_path,"SDM/Output/",spec,"/Past PA map"),
               recursive = TRUE)
  }
  
  # Create and plot presence-absence raster for past scenario and save
  plotPA(map_past,
         th = ths[3, 2],
         filename = paste0(proj_path,"SDM/Output/",spec,"/Past PA map/RF_",spec,"_",region,".tif"),
         format = "GTiff",
         overwrite = TRUE,
         hr = TRUE)
  ggsave(paste0(proj_path,"SDM/Output/",spec,"/Past PA map/RF_",spec,"_",region,".png"),
         bg = "white")
}