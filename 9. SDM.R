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

# Create loop to run SDMs for all species
for (spec in c("SHAL","SHMA","MOFA","MOCA","ANNI")) {
  
  # Load species-specific predictor variables
  if (spec %in% c("SHAL","MOFA")){
    region <- "pahw1400_1ha"
  }
  
  if (spec %in% c("SHMA","MOCA")){
    region <- "nil1400_1ha"
  }
  
  if (spec == "ANNI"){
    region <- "swg1400_25ha"
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
  
  if (spec == "ANNI") {
    
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
    
    BiomodData@data.species[is.na(BiomodData@data.species)] <- 0
    
    df <- data.frame(Presence = BiomodData@data.species,
                     BiomodData@data.env.var,
                     BiomodData@coord)
  }
  
  # Stratified sampling on presence-absence data
  set.seed(25)
  train_index <- caret::createDataPartition(df$Presence,p = 0.8,list = FALSE)
  
  df$Presence <- as.factor(df$Presence)
  
  train <- df[train_index,c("Presence",fil_vars)]
  test  <- df[-train_index,c("Presence",fil_vars)]
  
  # Create output folder in directory if does not exist
  if (!dir.exists(paste0(proj_path,"SDM/Output/Datasets"))){
    dir.create(paste0(proj_path,"SDM/Output/Datasets"),recursive = TRUE)
  }
  
  # Save training and testing datasets
  write.csv(train, 
            file = paste0(proj_path,"SDM/Output/Datasets/",spec,"_",region,"_train.csv"),
            row.names = FALSE)
  write.csv(test, 
            file = paste0(proj_path,"SDM/Output/Datasets/",spec,"_",region,"_test.csv"),
            row.names = FALSE)
  
  # Creating mlr task
  mlr.task <- mlr::makeClassifTask(data = train, target = "Presence")
  
  # Tuning hyperparameters
  set.seed(25)
  res <- tuneRanger(mlr.task,tune.parameters = c("mtry","min.node.size"))
  
  # Mean of best 5 % of the results
  res
  
  if (spec != "ANNI") {
    set.seed(25)
    rf <- randomForest(formula = as.factor(train$Presence) ~.,
                       data = train,
                       ntree = 1000,
                       mtry = res$recommended.pars[["mtry"]],
                       nodesize = res$recommended.pars[["min.node.size"]])
  }
  
  if (spec == "ANNI") {
    
    prNum <- as.numeric(table(train$Presence)["1"]) # number of presences
    bgNum <- as.numeric(table(train$Presence)["0"]) # number of backgrounds
    
    # the sample size in each class; the same as presence number
    smpsize <- c("0" = prNum, "1" = prNum)
    
    set.seed(25)
    rf <- randomForest(formula = as.factor(train$Presence) ~.,
                       data = train,
                       ntree = 1000,
                       mtry = res$recommended.pars[["mtry"]],
                       sampsize = smpsize,
                       nodesize = res$recommended.pars[["min.node.size"]],
                       replace = TRUE)
  } 
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Output/Models"))){
    dir.create(paste0(proj_path,"SDM/Output/Models"),recursive = TRUE)
  }
  
  # Save model
  saveRDS(rf,file = paste0(proj_path,"SDM/Output/Models/",spec,"_RF_",region,".rds"))
  
  # predict with RF and RF down-sampled
  rfpred <- predict(rf,test[,fil_vars],type = "prob")[,"1"]
  
  plot(rf,main = "RF")
  
  # calculate area under the ROC and PR curves
  precrec_obj <- evalmod(scores = rfpred,labels = test[,"Presence"])
  precrec_obj
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Output/Evaluation curves"))){
    dir.create(paste0(proj_path,"SDM/Output/Evaluation curves"),recursive = TRUE)
  }
  
  # plot the ROC and PR curves
  autoplot(precrec_obj)
  ggsave(paste0(proj_path,"SDM/Output/Evaluation curves/",spec,"_RF_",region,".png"),
         bg = "white")
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Output/Variable importance"))){
    dir.create(paste0(proj_path,"SDM/Output/Variable importance"),recursive = TRUE)
  }
  
  png(filename = paste0(proj_path,"SDM/Output/Variable importance/",spec,"_RF_",region,".png"))
  varImpPlot(rf)
  dev.off()
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Output/Partial dependence plots/",spec,"_RF_",region))){
    dir.create(paste0(proj_path,"SDM/Output/Partial dependence plots/",spec,"_RF_",region),
               recursive = TRUE)
  }
  
  for (i in seq_along(fil_vars)){
    
    png(filename = paste0(proj_path,"SDM/Output/Partial dependence plots/",spec,"_RF_",region,"/",fil_vars[i],".png"))
    partialPlot(x = rf,
                pred.data = train,
                x.var = fil_vars[i],
                xlab = fil_vars[i],
                main = paste("Partial Dependence on", fil_vars[i]),
                which.class = "1")
    dev.off()
  }
  
  env_present <- as.data.frame(predictors,xy = TRUE,na.rm = FALSE)
  
  pred_present <- predict(rf,env_present[,fil_vars],type = "prob")[,"1"]
  
  present_df <- cbind(env_present[,c("x","y")],pred_present)
  
  present_rast <- rast(present_df,
                       type = "xyz", 
                       crs = crs(predictors), 
                       extent = ext(predictors))
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Output/Prediction"))){
    dir.create(paste0(proj_path,"SDM/Output/Prediction"),recursive = TRUE)
  }
  
  png(filename = paste0(proj_path,"SDM/Output/Prediction/",spec,"_RF_",region,".png"))
  plot(present_rast)
  dev.off()
  
  writeRaster(present_rast,
              file = paste0(proj_path,"SDM/Output/Prediction/",spec,"_RF_",region,".tif"),
              overwrite = TRUE)
  
  predictors_past <- rast(paste0(proj_path,"SDM/Input/",region,"/predictors_past.tif"))
  predictors_past <- predictors_past[[fil_vars]]
  
  env_past <- as.data.frame(predictors_past,xy = TRUE,na.rm = FALSE)
  
  pred_past <- predict(rf,env_past[,fil_vars],type = "prob")[,"1"]
  
  past_df <- cbind(env_past[,c("x","y")],pred_past)
  
  past_rast <- rast(past_df,type = "xyz",crs = crs(predictors_past),extent = ext(predictors_past))
  
  # Create output folder in directory if does not exist
  if (! dir.exists(paste0(proj_path,"SDM/Output/Past prediction"))){
    dir.create(paste0(proj_path,"SDM/Output/Past prediction"),recursive = TRUE)
  }
  
  png(filename = paste0(proj_path,"SDM/Output/Past prediction/",spec,"_RF_",region,".png"))
  plot(past_rast)
  dev.off()
  
  writeRaster(past_rast,
              file = paste0(proj_path,"SDM/Output/Past prediction/",spec,"_RF_",region,".tif"),
              overwrite = TRUE)
}