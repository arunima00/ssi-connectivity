# Load packages
library(corrplot)
library(terra)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

## Repeat the following process for all regions

# Set region of interest
#region <- "nilpa1400_25ha"
#region <- "nilpa1400_1ha"
#region <- "nil1400_1ha"
region <- "pa1400_1ha"

# Read raster stack
st <- rast(paste0(proj_path,"SDM/Input/",region,"/predictors_present.tif"))

# Calculate correlation between all pairs of continuous variables
stack_corr <- layerCor(st,fun = "pearson",na.rm = TRUE)

# Extract correlation matrix
corr_matrix <- stack_corr$correlation

# Create output folder in directory if does not exist
if (! dir.exists(paste0(proj_path,"SDM/Input/",region,"/Correlation"))) {
  dir.create(paste0(proj_path,"SDM/Input/",region,"/Correlation"),recursive = TRUE)
}

# Create correlation plot and save as PNG
png(paste0(proj_path,"SDM/Input/",region,"/Correlation/",region,"_corrplot.png"))
corrplot(corr_matrix,
         is.corr = FALSE, 
         method = "square",
         type = "upper",
         diag = FALSE)
dev.off()

# Convert matrix to dataframe
corr_matrix[lower.tri(corr_matrix, diag = TRUE)] <- NA
df <- reshape2::melt(as.matrix(corr_matrix), na.rm = TRUE)

# Order dataframe by correlation index
df <- df[order(-abs(df$value)), ]

rownames(df) <- NULL

# Filter variables for correlation values above 0.7
df_th <- droplevels(df[abs(df$value) >= 0.7,])

# Write filtered dataframe to CSV file
write.csv(df,
          file = paste0(proj_path,"SDM/Input/",region,"/Correlation/",region,"_correlation.csv"))

# Extract variable names
all_vars <- names(st)

all_vars # view all variables
df_th # view correlated variables

# Remove variables with correlation > 0.7 
fil_vars <- all_vars[! all_vars %in% c("roughness","slope","dist_woodland")]

# Save list of filtered variable names
save(fil_vars,
     file = paste0(proj_path,"SDM/Input/",region,"/Correlation/",region,"_fil.RData"))