# Load packages
library(corrplot)
library(terra)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

## Repeat the following process for all raster stacks for present

# Set region of interest
#region <- "swg1400"
region <- "nil1400"
#region <- "pahw1400"

# Read raster stack
st <- rast(paste0(proj_path,"SDM/Input/",region,"_1ha/",region,"_predictors_present.tif"))

# Calculate correlation between all pairs of continuous variables
stack_corr <- layerCor(st,fun = "pearson",na.rm = TRUE)

# Extract correlation matrix
corr_matrix <- stack_corr$correlation

# Create output folder in directory if does not exist
if (! dir.exists(paste0(proj_path,"SDM/Input/",region,"_1ha/Correlation"))) {
  dir.create(paste0(proj_path,"SDM/Input/",region,"_1ha/Correlation"),recursive = TRUE)
}

# Create correlation plot and save as PNG
png(paste0(proj_path,"SDM/Input/",region,"_1ha/Correlation/",region,"_corrplot.png"))
corrplot(corr_matrix,
         is.corr = FALSE, 
         method = "square",
         type = "upper",
         diag = FALSE)
dev.off()

# Remove lower triangle of correlation matrix
corr_matrix[lower.tri(corr_matrix, diag = TRUE)] <- NA

# Convert matrix to dataframe
df <- reshape2::melt(as.matrix(corr_matrix), na.rm = TRUE)

# Order dataframe by correlation index
df <- df[order(-abs(df$value)), ]

# Remove row names
rownames(df) <- NULL

# Filter dataframe for correlation values above 0.7
df_th <- droplevels(df[abs(df$value) >= 0.7,])

# Write filtered dataframe to CSV file
write.csv(df,
          file = paste0(proj_path,"SDM/Input/",region,"_1ha/Correlation/",region,"_correlation.csv"))

# Extract variable names
all_vars <- names(st)

all_vars # view all variables
df_th # view correlated variables

# Remove variables with correlation > 0.7 
fil_vars <- all_vars[! all_vars %in% c("roughness","slope","canopyheight","elevation","prox_woodland")]

# Save list of filtered variable names
save(fil_vars,
     file = paste0(proj_path,"SDM/Input/",region,"_1ha/Correlation/",region,"_fil.RData"))
