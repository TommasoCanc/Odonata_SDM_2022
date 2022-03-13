########################
# Correlation analysis #
########################

# Load libraries
library(raster) # <- GIS
library(HH) # <- VARIANCE INFLATION FACTOR

#########################
# Set working directory #
#########################

main.path <- "YOUR_PATH"
setwd(main.path)

# Load predictor variables -----------------------------------------------------
# Elevation
elev <- raster("./elevation_5_null.asc")
# Water bodies
wb <- raster("./Waterbodies_5_null.asc")

# Envierem and bioclimatec variables present
enviFiles <- list.files(path = "./Present/envirem_null/", 
                        pattern = ".tif$", all.files = TRUE, full.names = TRUE)
bioclimFiles <- list.files(path = "./Present/bioVar_null/", 
                           pattern = ".tif$", all.files = TRUE, 
                           full.names = TRUE)

# Stack ------------------------------------------------------------------------
enviStack <- stack(enviFiles)
bioclimStack <- stack(bioclimFiles)

rm(bioclimFiles, enviFiles)

# Check extension, resolution and number of cells
extent(elev) == extent(bioclimStack) 
extent(elev) == extent(enviStack) 
extent(elev) == extent(wb)

xres(elev) == xres(bioclimStack) 
xres(elev) == xres(enviStack) 
xres(elev) == xres(wb) 

elev@ncols*elev@nrows == bioclimStack[[1]]@ncols*bioclimStack[[1]]@nrows 
elev@ncols*elev@nrows == enviStack[[1]]@ncols*enviStack[[1]]@nrows
elev@ncols*elev@nrows == wb@ncols*wb@nrows

# Create a total stack of all variables ----------------------------------------
variablesStack <- stack(elev, wb, bioclimStack, enviStack)
names(variablesStack)

rm(elev, wb, bioclimStack, enviStack); gc()

# Convert stack to dataframe
variable.df <- as.data.frame(variablesStack)

# Remove NA from dataframe
variable.df <- na.omit(variable.df)

# Correlation matrix
variable.cor <- cor(variable.df)

# Convert the correlation matrix to distance matrix
variable.dist <- as.dist(abs(variable.cor))

# Distance dendrogram
variable.cluster <- hclust(1-variable.dist) 
plot(variable.cluster)
abline(h=0.5, col="red") # <- 0.5

# Subset selected variables ----------------------------------------------------
variables.subset <- c("embergerQ", "Waterbodies_5_null", "elevation_5_null",
                      "bio1", "bio7", "bio8", "bio9", "bio10","bio15", "bio17")
variable.df2 <- variable.df[ ,variables.subset]

# Check linear combination between predictor variables with 
# VIF (Variance Inflation Factor)
vif.res <- vif(variable.df2); sort(vif.res)

# Remove bio1
variable.df2$bio1 <- NULL
vif.res <- vif(variable.df2); sort(vif.res)

# Remove bio17
variable.df2$bio17 <- NULL
vif.res <- vif(variable.df2); sort(vif.res)

# Remove bio9
variable.df2$bio9 <- NULL
vif.res <- vif(variable.df2); sort(vif.res)

# Variables selected
variables.subset <- names(vif.res)
variables.subset
