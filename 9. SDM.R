# Load packages
library(tidyverse)
library(terra)
library(tuneRanger)
library(randomForest)
library(precrec)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

# Create list of all predictor variables across species
load(paste0("SDM/Input/nil1400_1ha/Correlation/nil1400_1ha_fil.RData"))
used_vars <- fil_vars

load(paste0("SDM/Input/pa1400_1ha/Correlation/pa1400_1ha_fil.RData"))
used_vars <- c(used_vars,fil_vars)

load(paste0("SDM/Input/nilpa1400_1ha/Correlation/nilpa1400_1ha_fil.RData"))
used_vars <- c(used_vars,fil_vars)

load(paste0("SDM/Input/nilpa1400_25ha/Correlation/nilpa1400_25ha_fil.RData"))
used_vars <- c(used_vars,fil_vars)

used_vars <- unique(unlist(used_vars))

# Set row and column names for model paramaters matrix
col <- c("Occurrence type",
         "Region",
         "Resolution",
         "Presences (train)",
         "Presences (test)",
         "Absences (train)",
         "Absences (test)",
         "AUC (ROC)",
         "AUC (PRC)",
         "mtry",
         "nodesize",
         used_vars)

spec_list <- c("SHAL","SHMA","MOFA","MOCA","ANNI","EUAL","FINI")

# Create empty matrix
model_param <- matrix(nrow = length(col),
                      ncol = length(spec_list),
                      dimnames = list(col,spec_list))

# Create loop to run SDMs for all species
for (spec in spec_list) {
  
  # Load species-specific predictor variables
  if (spec %in% c("SHAL","MOFA")){
    region <- "pa1400_1ha"
    model_param["Region",spec] <- "pa1400"
    model_param["Resolution",spec] <- "1ha"
  }
  
  if (spec %in% c("SHMA","MOCA")){
    region <- "nil1400_1ha"
    model_param["Region",spec] <- "nil1400"
    model_param["Resolution",spec] <- "1ha"
  }
  
  if (spec == "ANNI"){
    region <- "nilpa1400_25ha"
    model_param["Region",spec] <- "nilpa1400"
    model_param["Resolution",spec] <- "25ha"
  }
  
  if (spec %in% c("EUAL","FINI")) {
    region <- "nilpa1400_1ha"
    model_param["Region",spec] <- "nilpa1400"
    model_param["Resolution",spec] <- "1ha"
  }
  
  predictors <- rast(paste0(proj_path,"SDM/Input/",region,"/predictors_present.tif"))
  
  # Load filtered variable names
  load(paste0(proj_path,"SDM/Input/",region,"/Correlation/",region,"_fil.RData"))
  
  # Filter out correlated variables
  predictors <- predictors[[fil_vars]]
  
  #Load species occurrences
  occ <- read.csv(paste0(proj_path,"occupancy data/Filtered/",spec,".csv"))
  occ <- occ[,c("Longitude","Latitude","Presence")]
  
  # Convert to SpatVector of points
  occ_vect <- vect(occ,geom = c("Longitude","Latitude"),crs = crs(predictors))
  
  # Get data table
  df <- terra::extract(predictors,occ_vect,ID = FALSE,bind = TRUE)
  df <- as.data.frame(df,geom = "XY") %>% na.omit()
  
  # Set occurrence data type (Presence-Absence)
  model_param["Occurrence type",spec] <- "PA"
  
  # Generate pseudo-absences for Nilgiri Pipit using biomod2
  if (spec == "ANNI") {
    
    # Filter for presences only
    df_temp <- filter(df,Presence == 1)
    occ_vect <- occ_vect[occ_vect$Presence == 1]
    
    # Generate randomly sampled pseudo-absences with spatial exclusion
    BiomodData <- biomod2::BIOMOD_FormatingData(resp.name = spec,
                                                resp.var = occ_vect,
                                                expl.var = predictors,
                                                PA.nb.rep = 1,
                                                PA.nb.absences = nrow(df_temp)*10,
                                                PA.strategy = "disk",
                                                PA.dist.min = 1500,
                                                na.rm = TRUE,
                                                filter.raster = TRUE,
                                                seed.val = 25)
    
    # Convert absence values to 0
    BiomodData@data.species[is.na(BiomodData@data.species)] <- 0
    
    # Create new dataframe with pseudo absences
    df <- data.frame(Presence = BiomodData@data.species,
                     BiomodData@data.env.var,
                     BiomodData@coord)
    
    # Set occurrence data type (Presence-Background)
    model_param["Occurrence type",spec] <- "PB"
  }
  
  # Stratified sampling on presence-absence data
  set.seed(25)
  train_index <- caret::createDataPartition(df$Presence,p = 0.8,list = FALSE)
  
  # Convert response variable to factor
  df$Presence <- as.factor(df$Presence)
  
  # Split data into training and testing datasets
  train <- df[train_index,c("Presence",fil_vars)]
  test  <- df[-train_index,c("Presence",fil_vars)]
  
  # Create output folder in directory if does not exist
  if (!dir.exists(paste0(proj_path,"SDM/Output/Datasets"))){
    dir.create(paste0(proj_path,"SDM/Output/Datasets"),recursive = TRUE)
  }
  
  # Save values for number of presences and absences
  model_param["Presences (train)",spec] <- nrow(filter(train,Presence == 1))
  model_param["Absences (train)",spec] <- nrow(filter(train,Presence == 0))
  model_param["Presences (test)",spec] <- nrow(filter(test,Presence == 1))
  model_param["Absences (test)",spec] <- nrow(filter(test,Presence == 0))
  
  # Save training and testing datasets
  write.csv(df[train_index,], 
            file = paste0(proj_path,"SDM/Output/Datasets/",spec,"_",region,"_train.csv"),
            row.names = FALSE)
  write.csv(df[-train_index,], 
            file = paste0(proj_path,"SDM/Output/Datasets/",spec,"_",region,"_test.csv"),
            row.names = FALSE)
  
  # Create mlr task for tuning
  mlr.task <- mlr::makeClassifTask(data = train, target = "Presence")
  
  # Tune specific hyperparameters
  set.seed(25)
  res <- tuneRanger(mlr.task,tune.parameters = c("mtry","min.node.size"))
  
  # View mean of best 5 % of the results
  res
  
  # Set hyperparameter values
  mtry <- res$recommended.pars[["mtry"]]
  nodesize <- res$recommended.pars[["min.node.size"]]
  
  model_param["mtry",spec] <- mtry
  model_param["nodesize",spec] <- nodesize
  
  # Train random forest using true absences for forest species
  if (spec != "ANNI") {
    set.seed(25)
    rf <- randomForest(formula = Presence ~.,
                       data = train,
                       ntree = 1000,
                       mtry = mtry,
                       nodesize = nodesize,
                       importance = T)
  }
  
  # Train downsampled random forest using pseudo-absences for grassland species
  if (spec == "ANNI") {
    
    prNum <- as.numeric(table(train$Presence)["1"]) # number of presences
    bgNum <- as.numeric(table(train$Presence)["0"]) # number of backgrounds
    
    # Set sample size in each class; the same as presence number
    smpsize <- c("0" = prNum, "1" = prNum)
    
    # Train model
    set.seed(25)
    rf <- randomForest(formula = Presence ~.,
                       data = train,
                       ntree = 1000,
                       mtry = mtry,
                       sampsize = smpsize,
                       nodesize = mtry,
                       replace = TRUE)
  } 
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Output/Models"))){
    dir.create(paste0(proj_path,"SDM/Output/Models"),recursive = TRUE)
  }
  
  # Save model
  saveRDS(rf,file = paste0(proj_path,"SDM/Output/Models/",spec,"_RF_",region,".rds"))
  
  # Predict with RF and RF down-sampled
  rfpred <- predict(rf,test[,fil_vars],type = "prob")[,"1"]
  
  # Calculate area under the ROC and PR curves
  precrec_obj <- evalmod(scores = rfpred,labels = test[,"Presence"])
  precrec_obj
  
  # Get AUC values
  auc_df <- auc(precrec_obj)
  
  model_param["AUC (ROC)",spec] <- auc_df[auc_df$curvetypes == "ROC","aucs"]
  model_param["AUC (PRC)",spec] <- auc_df[auc_df$curvetypes == "PRC","aucs"]
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Output/Evaluation curves"))){
    dir.create(paste0(proj_path,"SDM/Output/Evaluation curves"),recursive = TRUE)
  }
  
  # Plot the ROC and PR curves
  autoplot(precrec_obj)
  ggsave(paste0(proj_path,"SDM/Output/Evaluation curves/",spec,"_RF_",region,".png"),
         bg = "white")
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Output/Variable importance"))){
    dir.create(paste0(proj_path,"SDM/Output/Variable importance"),recursive = TRUE)
  }
  
  # Compute variable importance by mean decrease in Gini index
  imp <- randomForest::importance(rf,type = 2)
  
  # Convert to percentage
  imp <- imp * 100 / sum(imp)
  
  # Save variable importance values
  for (i in used_vars) {
    if (i %in% row.names(imp)) {
      model_param[i,spec] <- imp[i,1]
    }
  }
  
  # Order variables by importance
  imp <- imp[order(imp[,1]),]
  
  # Plot variable importance
  png(filename = paste0(proj_path,"SDM/Output/Variable importance/",spec,"_RF_",region,".png"))
  dotchart(imp,
           xlim = c(0,100),
           xlab = "Percentage mean decrease in Gini index")
  dev.off()
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Output/Partial dependence plots"))){
    dir.create(paste0(proj_path,"SDM/Output/Partial dependence plots"),
               recursive = TRUE)
  }
  
  # Plot partial dependence plots in one window
  png(filename = paste0(proj_path,"SDM/Output/Partial dependence plots/",spec,"_RF_",region,".png"),
      height = 3000,
      width = 3000,
      res = 300)
  
  if (spec == "ANNI") {
    par(mfrow = c(3,3),cex.lab = 1.7)
  }
  
  if (spec != "ANNI") {
    par(mfrow = c(4,3),cex.lab = 1.7)
  }
  
  for (i in seq_along(fil_vars)){
    
    partialPlot(x = rf,
                pred.data = train,
                x.var = fil_vars[i],
                xlab = fil_vars[i],
                main = NULL,
                which.class = "1")
  }
  
  dev.off()
  
  # Convert present predictor variables dataset to dataframe
  env_present <- as.data.frame(predictors,xy = TRUE,na.rm = FALSE)
  
  # Predict RF on entire dataset for present scenario
  pred_present <- predict(rf,env_present[,fil_vars],type = "prob")[,"1"]
  
  # Create prediction raster
  present_df <- cbind(env_present[,c("x","y")],pred_present)
  
  present_rast <- rast(present_df,
                       type = "xyz", 
                       crs = crs(predictors), 
                       extent = ext(predictors))
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Output/Prediction"))){
    dir.create(paste0(proj_path,"SDM/Output/Prediction"),recursive = TRUE)
  }
  
  # Plot prediction raster
  png(filename = paste0(proj_path,"SDM/Output/Prediction/",spec,"_RF_",region,".png"))
  plot(present_rast,range = c(0,1))
  dev.off()
  
  # Save prediction raster
  writeRaster(present_rast,
              file = paste0(proj_path,"SDM/Output/Prediction/",spec,"_RF_",region,".tif"),
              overwrite = TRUE)
  
  # Read past predictor variables dataset 
  predictors_past <- rast(paste0(proj_path,"SDM/Input/",region,"/predictors_past.tif"))
  
  # Filter unused variables out
  predictors_past <- predictors_past[[fil_vars]]
  
  # Convert dataset to dataframe
  env_past <- as.data.frame(predictors_past,xy = TRUE,na.rm = FALSE)
  
  # Predict RF on entire dataset for past scenario
  pred_past <- predict(rf,env_past[,fil_vars],type = "prob")[,"1"]
  
  # Create past prediction raster
  past_df <- cbind(env_past[,c("x","y")],pred_past)
  
  past_rast <- rast(past_df,
                    type = "xyz",
                    crs = crs(predictors_past),
                    extent = ext(predictors_past))
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Output/Past prediction"))){
    dir.create(paste0(proj_path,"SDM/Output/Past prediction"),recursive = TRUE)
  }
  
  # Plot past prediction raster
  png(filename = paste0(proj_path,"SDM/Output/Past prediction/",spec,"_RF_",region,".png"))
  plot(past_rast,range = c(0,1))
  dev.off()
  
  # Save past prediction raster
  writeRaster(past_rast,
              file = paste0(proj_path,"SDM/Output/Past prediction/",spec,"_RF_",region,".tif"),
              overwrite = TRUE)
}

# Write model parameter matrix to CSV file
write.csv(model_param,file = paste0(proj_path,"SDM/Output/Models/Summary table.csv"))