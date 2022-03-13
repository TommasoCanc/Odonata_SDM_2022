#####################
# Model calibration #
#####################

# we are going to use training_table to calibrate the models.

# Load libraries
library(mgcv) # <- GAM library
library(plotmo) # <- Plot response curve of GAM
library(dismo) # <- MAXENT
library(gbm) # <- Boosted regression trees
library(osfr) # <- Connect to OSF

#####################
# Connection to OSF #
#####################

main.path <- "YOUR_PATH"
setwd(main.path)

# Connect to Open Science Framework
cr_project <- osf_retrieve_node("4rjuc")
osf_files <- osf_ls_files(cr_project, type = "folder")

# Download Odonata checklist
file.number <- which(osf_ls_files(osf_files[1, ], pattern = "Function")[ ,1] == "Function.zip")
osf_download(osf_ls_files(osf_files[1, ], pattern = "Function")[file.number, ], 
             path = main.path)
rm(file.number)

# Unzip Function folder
# Unzip file
unzip("./Function.zip", exdir = main.path); file.remove("./Function.zip")

# Load the function to weight background B. Blas
source("./Function/funcionesSDM_taller2.R")

# Create folder to save data
dir.create("./Models")
dir.create("./Models/GAM")
dir.create("./Models/GAM/Response_cureve")
dir.create("./Models/GAM/Map_training")
dir.create("./Models/GAM/Map_raster")

dir.create("./Models/Maxent")
dir.create("./Models/Maxent/Response_cureve")
dir.create("./Models/Maxent/Map_training")
dir.create("./Models/Maxent/Map_raster")

dir.create("./Models/BRT")
dir.create("./Models/BRT/Response_cureve")
dir.create("./Models/BRT/Map_training")
dir.create("./Models/BRT/Map_raster")

dir.create("./Models/Ensemble")
dir.create("./Models/Ensemble/Map_training")
dir.create("./Models/Ensemble/Map_raster")

# Load variables ---------------------------------------------------------------
# Elevation and waterbodies
elev <- raster("./elevation_5_null.asc")
wb <- raster("./Waterbodies_5_null.asc")

# Envirem and bioclimatic variables
enviFiles <- list.files(path = "./Present/envirem_null/", 
                        pattern = ".tif$", all.files = TRUE, full.names = TRUE)
bioclimFiles <- list.files(path = "./Present/bioVar_null/", 
                           pattern = ".tif$", all.files = TRUE, full.names = TRUE)

# Stack 
enviStack <- stack(enviFiles)
bioclimStack <- stack(bioclimFiles)

# Create a total stack containing all variables
variablesStack <- stack(elev, wb, bioclimStack, enviStack)
names(variablesStack)

# Remove unnecessary files
rm(elev, wb, bioclimStack, enviStack, bioclimFiles, enviFiles)

# Select only the variable for this study
variables.names <- c("embergerQ", "Waterbodies_5_null", "elevation_5_null", 
                     "bio7", "bio8", "bio10", "bio15")
variable.seleccionadas <- stack(variablesStack[[variables.names]])
names(variable.seleccionadas)

# Load Odonata training files --------------------------------------------------
odonFiles <- list.files(path = "./Presence_Background/Training/", 
                        pattern = ".csv$", all.files = TRUE, full.names = TRUE)

# Number of training presence
num.presence <- data.frame()

for(i in 1:length(odonFiles)){
sp <- read.csv(odonFiles[i]) 
num.presence.1 <- data.frame(canonicalName = unique(sp$canonicalName),
                              n.presence = nrow(sp[sp$presence == 1,]))
num.presence <- rbind(num.presence, num.presence.1)                    
}

# We select only the species with => 30.
sp30 <- num.presence$canonicalName[num.presence$n.presence >= 30]

for(i in 1:length(sp30)){
odonFiles30 <- odonFiles[ grepl(sp30[i], odonFiles) ]
sp <- read.csv(odonFiles30) 

# Clip Variables to species area
mcpBuffer <- list.files(path = "./MCP/MCP_Buffer/", 
                        pattern = ".asc$", all.files = TRUE, full.names = TRUE)

mcpRaster <- raster(mcpBuffer[grep(unique(sp$canonicalName), mcpBuffer)])

# Crop and mask
mcpRaster.sub <- crop(variable.seleccionadas, extent(mcpRaster))
mcpRaster.sub <- mask(mcpRaster.sub, mcpRaster); rm(mcpRaster)

#-----#
# GAM #
#-----#

# Weighted background
weight <- WeightPresenceBackground(sp[ , "presence"])

# Formula for GAM
formula.gam <- as.formula(paste("presence ~ s(", paste(names(sp[5:11]), 
                          collapse=") + s("), ")", collapse=""))
# GAM model calibration
m.gam.backgroundw <- gam(formula.gam, family = binomial(link=logit), 
                         data = sp, weights = weight)

# Save the response curves
png(paste0("./Models/GAM/Response_cureve/", unique(sp$canonicalName), "_gam.png"), 
    width = 1200, height = 1200, pointsize = 30)
plotmo(m.gam.backgroundw, type = "response", caption = unique(sp$canonicalName))
dev.off()

# Geographical prediction
m.gam.backgroundw.mapa <- predict(mcpRaster.sub, m.gam.backgroundw, type="response")

# Save geographical prediction as image
png(paste0("./Models/GAM/Map_training/", unique(sp$canonicalName), "_gam.png"), 
    width = 1200, height = 1200, pointsize = 30)
plot(m.gam.backgroundw.mapa, main=paste(unique(sp$canonicalName), "(background weighted)"))   
points(sp[sp$presence == 1,]$x, sp[sp$presence == 1,]$y, cex = .1)
dev.off()

# Save geographic prediction as a raster
writeRaster(m.gam.backgroundw.mapa, filename = paste0("./Models/GAM/Map_raster/",
            unique(sp$canonicalName), "_gam.tif"), format = "ascii", 
            overwrite = FALSE)

# Remove objects
rm(weight, formula.gam, m.gam.backgroundw, m.gam.backgroundw.mapa)

#--------#
# MAXENT #
#--------#

# x: dataframe with variables
# p: occurrence data

variables.maxent <- sp[,5:11] # <- Variables
presence.background.maxent <- sp[,4] # <- Occurrences

# Create a folder where we can save the Maxent results
dir.create(paste0("./Models/Maxent/Response_cureve/", unique(sp$canonicalName)))

# Calibration Maxent results
maxEnt <- maxent(variables.maxent, presence.background.maxent,
                 path = paste0("./Models/Maxent/Response_cureve/", 
                               unique(sp$canonicalName), "/"))

# Geographical prediction
maxEnt.map <- predict(maxEnt, mcpRaster.sub) 

# Save geographical prediction as image
png(paste0("./Models/Maxent/Map_training/", unique(sp$canonicalName), "_maxent.png"), 
    width = 1200, height = 1200, pointsize = 30)
plot(maxEnt.map, main = paste(unique(sp$canonicalName), "(maxent)"))
points(sp[sp$presence == 1,]$x, sp[sp$presence == 1,]$y, cex = .1)
dev.off()

# Save geographic prediction as a raster
writeRaster(maxEnt.map, filename = paste0("./Models/Maxent/Map_raster/", 
            unique(sp$canonicalName), "_maxent.tif"), 
            format = "ascii", overwrite = FALSE)

# Remove unnecessary objects
rm(variables.maxent, presence.background.maxent, maxEnt, maxEnt.map)

#--------------------------#
# Boosted regression trees #
#--------------------------#

#data:	input data.frame
#gbm.x: index or names of predictor variables in data
#gbm.y: index or name of response variable in data

# Calibrate BRT model
sp.tc5.lr01 <- gbm.step(data = sp, gbm.x = 5:11, gbm.y = 4,
                        family = "bernoulli", tree.complexity = 5,
                        learning.rate = 0.001, bag.fraction = 0.5)


# Save the response curves
png(paste0("./Models/BRT/Response_cureve/", unique(sp$canonicalName), "_brt.png"), 
    width = 1200, height = 1200, pointsize = 30)
gbm.plot.fits(sp.tc5.lr01, mask.presence = TRUE)
dev.off()

# Geographical prediction
brt.map <- predict(mcpRaster.sub, sp.tc5.lr01,
                   n.trees = sp.tc5.lr01$gbm.call$best.trees, type = "response")

# Save geographical prediction as image
png(paste0("./Models/BRT/Map_training/", unique(sp$canonicalName), "_BRT.png"), 
    width = 1200, height = 1200, pointsize = 30)
plot(brt.map, main = paste(unique(sp$canonicalName), "(BRT)"))
points(sp[sp$presence == 1,]$x, sp[sp$presence == 1,]$y, cex = .1)
dev.off()

# Save geographic prediction as a raster
writeRaster(brt.map, filename=paste0("./Models/BRT/Map_raster/", 
            unique(sp$canonicalName), "_brt.tif"), 
            format = "ascii", overwrite = FALSE)

# Remove unnecessary objects
rm(sp.tc5.lr01, brt.map)

#----------#
# Ensemble #
#----------#

# Load training models
gam.model <- list.files(path="./Models/GAM/Map_raster/", pattern = ".asc", 
                        full.names = TRUE)
max.model <- list.files(path="./Models/Maxent/Map_raster/", pattern = ".asc", 
                       full.names = TRUE)
brt.model <- list.files(path="./Models/BRT/Map_raster/", pattern = ".asc", 
                       full.names = TRUE)

models <- stack(gam.model[i], brt.model[i], max.model[i])

# We are going to create ensemble model with the mean of GAM, MAXENT and BRT
ensemble <- calc(models, mean, filename = paste0("./Models/Ensemble/Map_raster/", 
                 stringr::str_remove(names(models)[1], "_gam"), ".asc"), 
                 overwrite = FALSE)

# Save geographical prediction as image
png(paste0("./Models/Ensemble/Map_training/", unique(sp$canonicalName), "_BRT.png"), 
    width = 1200, height = 1200, pointsize = 30)
plot(ensemble, main = paste(unique(sp$canonicalName), "(Ensemble)"))
points(sp[sp$presence == 1,]$x, sp[sp$presence == 1,]$y, cex = .1)
dev.off()

# Remove unnecessary objects
rm(gam.model, max.model, brt.model, models, ensemble)
}

# Clean workspace
rm(list = ls())
