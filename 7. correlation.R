# Load packages
library(corrplot)
library(terra)

# Clear environment 
rm(list = ls())

# Set project path prefix
proj_path <- "C:/Users/aruni/arunima/IISERTpt/Connectivity/"

## Repeat the following process for all raster stacks for present

region <- "nil1400"
vars <- "gland"
sc <- "present"

# Read raster stack
st <- rast(paste0(proj_path,"SDM/Input/",region,"_1ha/",region,"_predictors_",sc,"_",vars,".tif"))

# Calculate correlation between all pairs of variables
stack_corr <- layerCor(st,fun = "pearson",na.rm = TRUE)

# Extract correlation matrix
corr_matrix <- stack_corr$correlation

# Create correlation plot and save as PNG
png(paste0(proj_path,"SDM/Input/",region,"_1ha/Correlation/",region,"_",vars,"_corrplot.png"))
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
          file = paste0(proj_path,"SDM/Input/",region,"_1ha/Correlation/",region,"_",vars,"_correlation.csv"))

# Extract variable names
all_vars <- names(st)

all_vars # view all variables
df_th # view correlated variables

# Remove variables with correlation > 0.7 
fil_vars <- all_vars[! all_vars %in% c("roughness","slope","elevation")]

# Save list of filtered variable names
save(fil_vars,
     file = paste0(proj_path,"SDM/Input/",region,"_1ha/Correlation/",region,"_",vars,"_fil.RData"))
