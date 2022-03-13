######################
# FUTURE PROJECTIONS #
######################

# WE HAVE TO USE THE TABLE WITH ALL PRESENCE AND BACKGROUND POINTS

# Load libraries
library(mgcv) # <- GAM library
library(plotmo) # <- To plot response curve of GAM
library(dismo) # MAXENT
library(gbm) # Boosted regression trees
library(viridis) # <-  Plot Color
library(ggplot2) # <- Plot

# Main path
main.path <- "YOUR_PATH"
setwd(main.path)

# Create directory to save the files
dir.create("./Models/GAM/All_presence")
dir.create("./Models/GAM/All_presence/Map_plot")
dir.create("./Models/GAM/All_presence/Map_raster")
dir.create("./Models/GAM/All_presence/Map_raster/Present")
dir.create("./Models/GAM/All_presence/Map_raster/BCC_2050")
dir.create("./Models/GAM/All_presence/Map_raster/BCC_2070")
dir.create("./Models/GAM/All_presence/Map_raster/MIROC_2050")
dir.create("./Models/GAM/All_presence/Map_raster/MIROC_2070")
dir.create("./Models/GAM/All_presence/Map_raster/NOR_2050")
dir.create("./Models/GAM/All_presence/Map_raster/NOR_2070")

dir.create("./Models/Maxent/All_presence")
dir.create("./Models/Maxent/All_presence/Map_plot")
dir.create("./Models/Maxent/All_presence/Map_raster")
dir.create("./Models/Maxent/All_presence/Map_raster/Present")
dir.create("./Models/Maxent/All_presence/Map_raster/BCC_2050")
dir.create("./Models/Maxent/All_presence/Map_raster/BCC_2070")
dir.create("./Models/Maxent/All_presence/Map_raster/MIROC_2050")
dir.create("./Models/Maxent/All_presence/Map_raster/MIROC_2070")
dir.create("./Models/Maxent/All_presence/Map_raster/NOR_2050")
dir.create("./Models/Maxent/All_presence/Map_raster/NOR_2070")

dir.create("./Models/BRT/All_presence")
dir.create("./Models/BRT/All_presence/Map_plot")
dir.create("./Models/BRT/All_presence/Map_raster")
dir.create("./Models/BRT/All_presence/Map_raster/Present")
dir.create("./Models/BRT/All_presence/Map_raster/BCC_2050")
dir.create("./Models/BRT/All_presence/Map_raster/BCC_2070")
dir.create("./Models/BRT/All_presence/Map_raster/MIROC_2050")
dir.create("./Models/BRT/All_presence/Map_raster/MIROC_2070")
dir.create("./Models/BRT/All_presence/Map_raster/NOR_2050")
dir.create("./Models/BRT/All_presence/Map_raster/NOR_2070")

dir.create("./Models/Ensemble/All_presence")
dir.create("./Models/Ensemble/All_presence/Map_plot")
dir.create("./Models/Ensemble/All_presence/Map_raster")
dir.create("./Models/Ensemble/All_presence/Map_raster/Present")
dir.create("./Models/Ensemble/All_presence/Map_raster/BCC_2050")
dir.create("./Models/Ensemble/All_presence/Map_raster/BCC_2070")
dir.create("./Models/Ensemble/All_presence/Map_raster/MIROC_2050")
dir.create("./Models/Ensemble/All_presence/Map_raster/MIROC_2070")
dir.create("./Models/Ensemble/All_presence/Map_raster/NOR_2050")
dir.create("./Models/Ensemble/All_presence/Map_raster/NOR_2070")



#---------#
# PRESENT #
#---------#

# Load variables ---------------------------------------------------------------
# Elevation and waterbodies
elev <- raster("./elevation_5_null.asc")
wb <- raster("./Waterbodies_5_null.asc")

# Envirem and Bioclim variables
enviFiles <- list.files(path = "./Present/envirem_null/", 
                        pattern = ".tif$", all.files = TRUE, full.names = TRUE)
bioclimFiles <- list.files(path = "./Present/bioVar_null/", 
                           pattern = ".tif$", all.files = TRUE, full.names = TRUE)
# Stack
enviStack <- stack(enviFiles)
bioclimStack <- stack(bioclimFiles)

rm(bioclimFiles, enviFiles)

#----------------#
# MIROC-ESM-CHEM #
#----------------#

# 2050 -------------------------------------------------------------------------
# Envirem and Bioclim variables
enviFiles_MIROC_50 <- list.files(path = "./MIROC_2050/envirem_null/", 
                          pattern = ".tif$", all.files = TRUE, full.names = TRUE)
bioclimFiles_MIROC_50 <- list.files(path = "./MIROC_2050/bioVar_null/", 
                             pattern = ".tif$", all.files = TRUE, full.names = TRUE)
# Stack
enviStack_MIROC_50 <- stack(enviFiles_MIROC_50)
bioclimStack_MIROC_50 <- stack(bioclimFiles_MIROC_50)

rm(bioclimFiles_MIROC_50, enviFiles_MIROC_50)

# 2070 -------------------------------------------------------------------------
# Envirem and Bioclim variables
enviFiles_MIROC_70 <- list.files(path = "./MIROC_2070/envirem_null/", 
                      pattern = ".tif$", all.files = TRUE, full.names = TRUE)
bioclimFiles_MIROC_70 <- list.files(path = "./MIROC_2070/bioVar_null/",
                      pattern = ".tif$", all.files = TRUE, full.names = TRUE)
# Stack 
enviStack_MIROC_70 <- stack(enviFiles_MIROC_70)
bioclimStack_MIROC_70 <- stack(bioclimFiles_MIROC_70)

rm(bioclimFiles_MIROC_70, enviFiles_MIROC_70)

#-----------#
# NorESM1-M #
#-----------#

# 2050 -------------------------------------------------------------------------
# Envirem and Bioclim variables
enviFiles_NOR_50 <- list.files(path = "./NOR_2050/envirem_null/", 
                        pattern = ".tif$", all.files = TRUE, full.names = TRUE)
bioclimFiles_NOR_50 <- list.files(path = "./NOR_2050/bioVar_null/", 
                        pattern = ".tif$", all.files = TRUE, full.names = TRUE)
# Stack
enviStack_NOR_50 <- stack(enviFiles_NOR_50)
bioclimStack_NOR_50 <- stack(bioclimFiles_NOR_50)

rm(bioclimFiles_NOR_50, enviFiles_NOR_50)

# 2070 -------------------------------------------------------------------------
# Envirem and Bioclim variables
enviFiles_NOR_70 <- list.files(path = "./NOR_2070/envirem_null/", 
                        pattern = ".tif$", all.files = TRUE, full.names = TRUE)
bioclimFiles_NOR_70 <- list.files(path = "./NOR_2070/bioVar_null/",
                        pattern = ".tif$", all.files = TRUE, full.names = TRUE)
# Stack
enviStack_NOR_70 <- stack(enviFiles_NOR_70)
bioclimStack_NOR_70 <- stack(bioclimFiles_NOR_70)

rm(bioclimFiles_NOR_70, enviFiles_NOR_70)

#------------#
# BCC-CSM1-1 #
#------------#

# 2050 -------------------------------------------------------------------------
# Envirem and Bioclim variables
enviFiles_BCC_50 <- list.files(path = "./BCC_2050/envirem_null/", 
                        pattern = ".tif$", all.files = TRUE, full.names = TRUE)
bioclimFiles_BCC_50 <- list.files(path = "./BCC_2050/bioVar_null/", 
                        pattern = ".tif$", all.files = TRUE, full.names = TRUE)
# Stack
enviStack_BCC_50 <- stack(enviFiles_BCC_50)
bioclimStack_BCC_50 <- stack(bioclimFiles_BCC_50)

rm(bioclimFiles_BCC_50, enviFiles_BCC_50)

# 2070 -------------------------------------------------------------------------
# Envirem and Bioclim variables 
enviFiles_BCC_70 <- list.files(path = "./BCC_2070/envirem_null/", 
                       pattern = ".tif$", all.files = TRUE, full.names = TRUE)
bioclimFiles_BCC_70 <- list.files(path = "./BCC_2070/bioVar_null/",
                       pattern = ".tif$", all.files = TRUE, full.names = TRUE)
# Stack
enviStack_BCC_70 <- stack(enviFiles_BCC_70)
bioclimStack_BCC_70 <- stack(bioclimFiles_BCC_70)

rm(bioclimFiles_BCC_70, enviFiles_BCC_70)

# PRESENT ----------------------------------------------------------------------
# Create a total stack of present variables 
variablesStack <- stack(elev, wb, bioclimStack, enviStack)
names(variablesStack)

# FUTURES ----------------------------------------------------------------------
# Create a total stack of - MIROC-ESM-CHEM  2050 and 2070
variablesStack_MIROC_50 <- stack(elev, wb, bioclimStack_MIROC_50, enviStack_MIROC_50)
variablesStack_MIROC_70 <- stack(elev, wb, bioclimStack_MIROC_70, enviStack_MIROC_70)

# Create a total stack of - NorESM1-M  2050 and 2070
variablesStack_NOR_50 <- stack(elev, wb, bioclimStack_NOR_50, enviStack_NOR_50)
variablesStack_NOR_70 <- stack(elev, wb, bioclimStack_NOR_70, enviStack_NOR_70)

# Create a total stack of - BCC-CSM1-1  2050 and 2070
variablesStack_BCC_50 <- stack(elev, wb, bioclimStack_BCC_50, enviStack_BCC_50)
variablesStack_BCC_70 <- stack(elev, wb, bioclimStack_BCC_70, enviStack_BCC_70)


# Remove single stacks
rm(elev, wb, bioclimStack, enviStack, 
   bioclimStack_MIROC_50, enviStack_MIROC_50,bioclimStack_MIROC_70, enviStack_MIROC_70,
   bioclimStack_NOR_50, enviStack_NOR_50,bioclimStack_NOR_70, enviStack_NOR_70,
   bioclimStack_BCC_50, enviStack_BCC_50,bioclimStack_BCC_70, enviStack_BCC_70)

# Seleionamos solo las variables que nos interesan
variables.names <- c("embergerQ", "Waterbodies_5_null", "elevation_5_null", 
                     "bio7", "bio8", "bio10", "bio15")

# PRESENT ----------------------------------------------------------------------
variable.select <- stack(variablesStack[[variables.names]])
names(variable.select)

# FUTURES ----------------------------------------------------------------------
# MIROC-ESM-CHEM 2050 and 2070
variable.select_MIROC_50 <- stack(variablesStack_MIROC_50[[variables.names]])
variable.select_MIROC_70 <- stack(variablesStack_MIROC_70[[variables.names]])

# NorESM1-M 2050 and 2070
variable.select_NOR_50 <- stack(variablesStack_NOR_50[[variables.names]])
variable.select_NOR_70 <- stack(variablesStack_NOR_70[[variables.names]])

# BCC-CSM1-1 2050 and 2070
variable.select_BCC_50 <- stack(variablesStack_BCC_50[[variables.names]])
variable.select_BCC_70 <- stack(variablesStack_BCC_70[[variables.names]])

# Remove stack that we don't need 
rm(variablesStack, variables.names,
   variablesStack_MIROC_50, variablesStack_MIROC_70,
   variablesStack_NOR_50, variablesStack_NOR_70,
   variablesStack_BCC_50, variablesStack_BCC_70)

# Check quick
par(mfrow=c(3,3), mar=c(1,1,1,1), oma=c(3,3,4,3))
plot(variable.select[[4]], main = "Pesent")
plot(variable.select_MIROC_50[[4]], main = "MIROC 2050")
plot(variable.select_MIROC_70[[4]], main = "MIROC 2070")
plot(variable.select_NOR_50[[4]], main = "NOR 2050")
plot(variable.select_NOR_70[[4]], main = "NOR 2070")
plot(variable.select_BCC_50[[4]], main = "BCC 2050")
plot(variable.select_BCC_70[[4]], main = "BCC 2070")
dev.off()

# Load function to Weight background B. Blas
source("./Function/funcionesSDM_taller2.R")

# Load odonata files list species ----------------------------------------------
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

# Select .csv with total occurrences with training set =>30
# Load table with all Odonata presence and background points -------------------
odonFiles <- list.files(path = "./Presence_Background/Odonata_presence_background_variables/", 
                        pattern = ".csv$", all.files = TRUE, full.names = TRUE)

odonFiles.model <- data.frame()
for(n in 1:length(sp30)){
  odonFiles.model.1 <- odonFiles[grepl(sp30[n], odonFiles)]
  odonFiles.model <- rbind(odonFiles.model, odonFiles.model.1)
  }

# Load elevation
elev <- raster("./elevation_5_null.asc")
elev.df <- as.data.frame(elev, xy = TRUE)
elev.df <- elev.df[!is.na(elev.df$elevation_5_null),]
elev.df$elevation_5_null <- 0

# We can start the loop here!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
for(i in 1:length(sp30)){
sp <- read.csv(odonFiles.model[i,]) 

# Clip variables to species area
mcpBuffer <- list.files(path = "./MCP/MCP_Buffer/", 
                        pattern = ".asc$", all.files = TRUE, full.names = TRUE)

mcpRaster <- raster(mcpBuffer[grep(unique(sp$canonicalName), mcpBuffer)])

# PRESENT ----------------------------------------------------------------------
# Crop and mask
mcpRaster.sub <- crop(variable.select, extent(mcpRaster))
mcpRaster.sub <- mask(mcpRaster.sub, mcpRaster)

# FUTURES ----------------------------------------------------------------------
# Crop and mask MIROC-ESM-CHEM 2050 and 2070
mcpRaster.sub_MIROC_50 <- crop(variable.select_MIROC_50, extent(mcpRaster))
mcpRaster.sub_MIROC_50 <- mask(mcpRaster.sub_MIROC_50, mcpRaster)
mcpRaster.sub_MIROC_70 <- crop(variable.select_MIROC_70, extent(mcpRaster))
mcpRaster.sub_MIROC_70 <- mask(mcpRaster.sub_MIROC_70, mcpRaster)

# Crop and mask NorESM1-M 2050 and 2070
mcpRaster.sub_NOR_50 <- crop(variable.select_NOR_50, extent(mcpRaster))
mcpRaster.sub_NOR_50 <- mask(mcpRaster.sub_NOR_50, mcpRaster)
mcpRaster.sub_NOR_70 <- crop(variable.select_NOR_70, extent(mcpRaster))
mcpRaster.sub_NOR_70 <- mask(mcpRaster.sub_NOR_70, mcpRaster)

# Crop and mask BCC-CSM1-1 2050 and 2070
mcpRaster.sub_BCC_50 <- crop(variable.select_BCC_50, extent(mcpRaster))
mcpRaster.sub_BCC_50 <- mask(mcpRaster.sub_BCC_50, mcpRaster)
mcpRaster.sub_BCC_70 <- crop(variable.select_BCC_70, extent(mcpRaster))
mcpRaster.sub_BCC_70 <- mask(mcpRaster.sub_BCC_70, mcpRaster)

#-----#
# GAM #
#-----#

# Weighted background
weight <- WeightPresenceBackground(sp[ , "presence"])

# Formula for gam
formula.gam <- as.formula(paste("presence ~ s(", paste(names(sp[5:11]), 
                          collapse=") + s("), ")", collapse=""))

# Gam model calibration
m.gam.backgroundw <- gam(formula.gam, family = binomial(link=logit), 
                         data = sp, weights = weight)

# Geographical prediction present
m.gam.backgroundw.mapa <- predict(mcpRaster.sub, m.gam.backgroundw, type="response")

# Geographical prediction future MIROC-ESM-CHEM 2050 and 2070
m.gam.backgroundw.mapa_MIROC_50 <- predict(mcpRaster.sub_MIROC_50, 
                                           m.gam.backgroundw, type="response")
m.gam.backgroundw.mapa_MIROC_70 <- predict(mcpRaster.sub_MIROC_70, 
                                           m.gam.backgroundw, type="response")

# Geographical prediction future 2050 NorESM1-M 2050 and 2070
m.gam.backgroundw.mapa_NOR_50 <- predict(mcpRaster.sub_NOR_50, 
                                         m.gam.backgroundw, type="response")
m.gam.backgroundw.mapa_NOR_70 <- predict(mcpRaster.sub_NOR_70, 
                                         m.gam.backgroundw, type="response")

# Geographical prediction future 2050 BCC-CSM1-1 2050 and 2070
m.gam.backgroundw.mapa_BCC_50 <- predict(mcpRaster.sub_BCC_50, 
                                         m.gam.backgroundw, type="response")
m.gam.backgroundw.mapa_BCC_70 <- predict(mcpRaster.sub_BCC_70, 
                                         m.gam.backgroundw, type="response")

# Save the geographical prediction as image
png(paste0("./Models/GAM/All_presence/Map_plot/", 
           unique(sp$canonicalName), "_gam.png"), 
    width=2400, height=2400, pointsize=30)
# Plot present
m.gam.backgroundw.mapa.df <- as.data.frame(m.gam.backgroundw.mapa, xy = TRUE)
m.gam.backgroundw.mapa.df <- m.gam.backgroundw.mapa.df[
  !is.na(m.gam.backgroundw.mapa.df$layer),]

plot.1 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "Present")) +
  geom_raster(data=elev.df,aes(x = x, y = y)) +
  geom_raster(data = m.gam.backgroundw.mapa.df,aes(x = x, y = y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position = "bottom", legend.box = "vertical") +
  geom_point(aes(x = sp[sp$presence == 1,]$x,
                 y = sp[sp$presence == 1,]$y), size = .4)+
  labs(fill = "Presence probability") 

rm(m.gam.backgroundw.mapa.df)

#Plot MIROC-ESM-CHEM 2050
m.gam.backgroundw.mapa_MIROC_50.df <- as.data.frame(m.gam.backgroundw.mapa_MIROC_50, xy=TRUE)
m.gam.backgroundw.mapa_MIROC_50.df <- m.gam.backgroundw.mapa_MIROC_50.df[
  !is.na(m.gam.backgroundw.mapa_MIROC_50.df$layer),]

plot.2 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "MIROC-ESM-CHEM 2050")) +
  geom_raster(data = elev.df,aes(x = x, y = y)) +
  geom_raster(data = m.gam.backgroundw.mapa_MIROC_50.df,aes(x = x, y = y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position = "none")

rm(m.gam.backgroundw.mapa_MIROC_50.df)

#Plot MIROC-ESM-CHEM 2070
m.gam.backgroundw.mapa_MIROC_70.df <- as.data.frame(m.gam.backgroundw.mapa_MIROC_70, xy=TRUE)
m.gam.backgroundw.mapa_MIROC_70.df <- m.gam.backgroundw.mapa_MIROC_70.df[
  !is.na(m.gam.backgroundw.mapa_MIROC_70.df$layer),]

plot.3 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "MIROC-ESM-CHEM 2070")) +
  geom_raster(data = elev.df,aes(x = x, y = y)) +
  geom_raster(data = m.gam.backgroundw.mapa_MIROC_70.df,aes(x = x, y = y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position = "none")

rm(m.gam.backgroundw.mapa_MIROC_70.df)

#Plot NorESM1-M 2050
m.gam.backgroundw.mapa_NOR_50.df <- as.data.frame(m.gam.backgroundw.mapa_NOR_50, xy=TRUE)
m.gam.backgroundw.mapa_NOR_50.df <- m.gam.backgroundw.mapa_NOR_50.df[
  !is.na(m.gam.backgroundw.mapa_NOR_50.df$layer),]

plot.4 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "NorESM1-M 2050")) +
  geom_raster(data = elev.df,aes(x = x, y = y)) +
  geom_raster(data = m.gam.backgroundw.mapa_NOR_50.df,aes(x = x, y = y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position = "none")

rm(m.gam.backgroundw.mapa_NOR_50.df)

#Plot NorESM1-M 2070
m.gam.backgroundw.mapa_NOR_70.df <- as.data.frame(m.gam.backgroundw.mapa_NOR_70, xy=TRUE)
m.gam.backgroundw.mapa_NOR_70.df <- m.gam.backgroundw.mapa_NOR_70.df[
  !is.na(m.gam.backgroundw.mapa_NOR_70.df$layer),]

plot.5 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "NorESM1-M 2070")) +
  geom_raster(data = elev.df,aes(x = x, y = y)) +
  geom_raster(data = m.gam.backgroundw.mapa_NOR_70.df,aes(x = x,y = y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position = "none") 

rm(m.gam.backgroundw.mapa_NOR_70.df)

#Plot BCC-CSM1-1 2050
m.gam.backgroundw.mapa_BCC_50.df <- as.data.frame(m.gam.backgroundw.mapa_BCC_50, xy=TRUE)
m.gam.backgroundw.mapa_BCC_50.df <- m.gam.backgroundw.mapa_BCC_50.df[
  !is.na(m.gam.backgroundw.mapa_BCC_50.df$layer),]

plot.6 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "BCC-CSM1-1 2050")) +
  geom_raster(data = elev.df,aes(x = x, y = y)) +
  geom_raster(data = m.gam.backgroundw.mapa_BCC_50.df,aes(x = x,y=y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position = "none")

rm(m.gam.backgroundw.mapa_BCC_50.df)

#Plot BCC-CSM1-1 2070
m.gam.backgroundw.mapa_BCC_70.df <- as.data.frame(m.gam.backgroundw.mapa_BCC_70, xy=TRUE)
m.gam.backgroundw.mapa_BCC_70.df <- m.gam.backgroundw.mapa_BCC_70.df[
  !is.na(m.gam.backgroundw.mapa_BCC_70.df$layer),]

plot.7 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "BCC-CSM1-1 2070")) +
  geom_raster(data = elev.df,aes(x = x, y = y)) +
  geom_raster(data = m.gam.backgroundw.mapa_BCC_70.df,aes(x = x, y = y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position = "none")

rm(m.gam.backgroundw.mapa_BCC_70.df)

# Plot grid
gridExtra::grid.arrange(plot.1,plot.2,plot.3,plot.4,plot.5,plot.6,plot.7,
                        widths = c(1, 1, 1, 1),
                        layout_matrix = rbind(c(NA,1,1,NA), c(2,2,3,3), 
                                              c(4,4,5,5), c(6,6,7,7)))
dev.off()

# PRESENT ----------------------------------------------------------------------
# Save the geographic prediction as a raster present
writeRaster(m.gam.backgroundw.mapa, 
            filename = paste0("./Models/GAM/All_presence/Map_raster/Present/",
            unique(sp$canonicalName), "_gam.tif"), 
            format = "ascii", overwrite = TRUE)

# FUTURES ----------------------------------------------------------------------
# Save the geographic prediction as a raster future 2050 and 2070 MIROC-ESM-CHEM
writeRaster(m.gam.backgroundw.mapa_MIROC_50, 
            filename = paste0("./Models/GAM/All_presence/Map_raster/MIROC_2050/",
            unique(sp$canonicalName), "_gam_MIROC_2050.tif"), 
            format = "ascii", overwrite = TRUE)
writeRaster(m.gam.backgroundw.mapa_MIROC_70, 
            filename = paste0("./Models/GAM/All_presence/Map_raster/MIROC_2070/",
            unique(sp$canonicalName), "_gam_MIROC_2070.tif"), 
            format = "ascii", overwrite = TRUE)

# Save the geographic prediction as a raster future 2050 and 2070 NorESM1-M
writeRaster(m.gam.backgroundw.mapa_NOR_50, 
            filename = paste0("./Models/GAM/All_presence/Map_raster/NOR_2050/",
            unique(sp$canonicalName), "_gam_NOR_2050.tif"), 
            format = "ascii", overwrite = TRUE)
writeRaster(m.gam.backgroundw.mapa_NOR_70, 
            filename = paste0("./Models/GAM/All_presence/Map_raster/NOR_2070/",
            unique(sp$canonicalName), "_gam_NOR_2070.tif"), 
            format = "ascii", overwrite = TRUE)

# Save the geographic prediction as a raster future 2050 and 2070 BCC-CSM1-1
writeRaster(m.gam.backgroundw.mapa_BCC_50, 
            filename = paste0("./Models/GAM/All_presence/Map_raster/BCC_2050/",
            unique(sp$canonicalName), "_gam_BCC_2050.tif"), 
            format = "ascii", overwrite = TRUE)
writeRaster(m.gam.backgroundw.mapa_BCC_70, 
            filename = paste0("./Models/GAM/All_presence/Map_raster/BCC_2070/",
            unique(sp$canonicalName), "_gam_BCC_2070.tif"), 
            format = "ascii", overwrite = TRUE)

rm(m.gam.backgroundw.mapa,
   m.gam.backgroundw.mapa_MIROC_50, m.gam.backgroundw.mapa_MIROC_70,
   m.gam.backgroundw.mapa_NOR_50, m.gam.backgroundw.mapa_NOR_70,
   m.gam.backgroundw.mapa_BCC_50, m.gam.backgroundw.mapa_BCC_70,
   plot.1, plot.2, plot.3, plot.4, plot.5, plot.6, plot.7)

#--------#
# MAXENT #
#--------#
variables.maxent <- sp[,5:11] # <- Variables
presence.background.maxent <- sp[,4] # <- Occurrences

# Calibrate maxent results
maxEnt <- maxent(variables.maxent, presence.background.maxent)

# Geographical prediction present
maxEnt.map <- predict(maxEnt, mcpRaster.sub, type="response") 

# Geographical prediction future 2050 and 2070 MIROC-ESM-CHEM
maxEnt.map_MIROC_50 <- predict(maxEnt, mcpRaster.sub_MIROC_50, type="response") 
maxEnt.map_MIROC_70 <- predict(maxEnt, mcpRaster.sub_MIROC_70, type="response") 

# Geographical prediction future 2050 and 2070 NorESM1-M
maxEnt.map_NOR_50 <- predict(maxEnt, mcpRaster.sub_NOR_50, type="response") 
maxEnt.map_NOR_70 <- predict(maxEnt, mcpRaster.sub_NOR_70, type="response") 

# Geographical prediction future 2050 and 2070 BCC-CSM1-1
maxEnt.map_BCC_50 <- predict(maxEnt, mcpRaster.sub_BCC_50, type="response") 
maxEnt.map_BCC_70 <- predict(maxEnt, mcpRaster.sub_BCC_70, type="response") 

png(paste0("./Models/Maxent/All_presence/Map_plot/",
           unique(sp$canonicalName), "_maxent.png"), 
    width=2400, height=2400, pointsize=30)
# Plot present
maxEnt.map.df <- as.data.frame(maxEnt.map, xy = TRUE)
maxEnt.map.df <- maxEnt.map.df[!is.na(maxEnt.map.df$layer),]

plot.1 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "Present")) +
  geom_raster(data = elev.df,aes(x = x, y = y)) +
  geom_raster(data = maxEnt.map.df,aes(x = x, y = y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position = "bottom", legend.box = "vertical") +
  geom_point(aes(x = sp[sp$presence == 1,]$x,
                 y = sp[sp$presence == 1,]$y), size = .4)+
  labs(fill = "Presence probability") 

rm(maxEnt.map.df)

#Plot MIROC-ESM-CHEM 2050
maxEnt.map_MIROC_50.df <- as.data.frame(maxEnt.map_MIROC_50, xy = TRUE)
maxEnt.map_MIROC_50.df <- maxEnt.map_MIROC_50.df[!is.na(maxEnt.map_MIROC_50.df$layer),]

plot.2 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "MIROC-ESM-CHEM 2050")) +
  geom_raster(data = elev.df,aes(x = x, y = y)) +
  geom_raster(data = maxEnt.map_MIROC_50.df,aes(x = x, y = y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position = "none")

rm(maxEnt.map_MIROC_50.df)

#Plot MIROC-ESM-CHEM 2070
maxEnt.map_MIROC_70.df <- as.data.frame(maxEnt.map_MIROC_70, xy = TRUE)
maxEnt.map_MIROC_70.df <- maxEnt.map_MIROC_70.df[!is.na(maxEnt.map_MIROC_70.df$layer),]

plot.3 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "MIROC-ESM-CHEM 2070")) +
  geom_raster(data = elev.df,aes(x = x, y = y)) +
  geom_raster(data = maxEnt.map_MIROC_70.df,aes(x = x, y = y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position = "none")

rm(maxEnt.map_MIROC_70.df)

#Plot NorESM1-M 2050
maxEnt.map_NOR_50.df <- as.data.frame(maxEnt.map_NOR_50, xy = TRUE)
maxEnt.map_NOR_50.df <- maxEnt.map_NOR_50.df[!is.na(maxEnt.map_NOR_50.df$layer),]

plot.4 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "NorESM1-M 2050")) +
  geom_raster(data = elev.df,aes(x = x, y = y)) +
  geom_raster(data = maxEnt.map_NOR_50.df,aes(x = x, y = y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position = "none")

rm(maxEnt.map_NOR_50.df)

#Plot NorESM1-M 2070
maxEnt.map_NOR_70.df <- as.data.frame(maxEnt.map_NOR_70, xy = TRUE)
maxEnt.map_NOR_70.df <- maxEnt.map_NOR_70.df[!is.na(maxEnt.map_NOR_70.df$layer),]

plot.5 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "NorESM1-M 2070")) +
  geom_raster(data = elev.df,aes(x = x, y = y)) +
  geom_raster(data = maxEnt.map_NOR_70.df,aes(x = x, y = y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position = "none") 

rm(maxEnt.map_NOR_70.df)

#Plot BCC-CSM1-1 2050
maxEnt.map_BCC_50.df <- as.data.frame(maxEnt.map_BCC_50, xy = TRUE)
maxEnt.map_BCC_50.df <- maxEnt.map_BCC_50.df[!is.na(maxEnt.map_BCC_50.df$layer),]

plot.6 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "BCC-CSM1-1 2050")) +
  geom_raster(data = elev.df,aes(x = x, y = y)) +
  geom_raster(data = maxEnt.map_BCC_50.df,aes(x = x, y = y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position = "none")

rm(maxEnt.map_BCC_50.df)

#Plot BCC-CSM1-1 2070
maxEnt.map_BCC_70.df <- as.data.frame(maxEnt.map_BCC_70, xy = TRUE)
maxEnt.map_BCC_70.df <- maxEnt.map_BCC_70.df[!is.na(maxEnt.map_BCC_70.df$layer),]

plot.7 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "BCC-CSM1-1 2070")) +
  geom_raster(data=elev.df,aes(x=x, y=y)) +
  geom_raster(data=maxEnt.map_BCC_70.df,aes(x=x,y=y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position="none")

rm(maxEnt.map_BCC_70.df)

# Plot grid
gridExtra::grid.arrange(plot.1,plot.2,plot.3,plot.4,plot.5,plot.6,plot.7,
                        widths = c(1, 1, 1, 1),
                        layout_matrix = rbind(c(NA,1,1,NA), c(2,2,3,3), 
                                              c(4,4,5,5), c(6,6,7,7)))
dev.off()

# PRESENT ----------------------------------------------------------------------
# Save the geographic prediction as a raster present
writeRaster(maxEnt.map, 
            filename = paste0("./Models/Maxent/All_presence/Map_raster/Present/",
            unique(sp$canonicalName), "_maxent.tif"), 
            format = "ascii", overwrite = TRUE)

# FUTURES ----------------------------------------------------------------------
# Save the geographic prediction as a raster future 2050 and 2070 MIROC-ESM-CHEM
writeRaster(maxEnt.map_MIROC_50, 
            filename = paste0("./Models/Maxent/All_presence/Map_raster/MIROC_2050/",
            unique(sp$canonicalName), "_maxent_MIROC_2050.tif"), 
            format = "ascii", overwrite = TRUE)
writeRaster(maxEnt.map_MIROC_70, 
            filename = paste0("./Models/Maxent/All_presence/Map_raster/MIROC_2070/",
            unique(sp$canonicalName), "_maxent_MIROC_2070.tif"), 
            format = "ascii", overwrite = TRUE)

# Save the geographic prediction as a raster future 2050 and 2070 NorESM1-M
writeRaster(maxEnt.map_NOR_50, 
            filename = paste0("./Models/Maxent/All_presence/Map_raster/NOR_2050/",
            unique(sp$canonicalName), "_maxent_NOR_2050.tif"), 
            format = "ascii", overwrite = TRUE)
writeRaster(maxEnt.map_NOR_70, 
            filename = paste0("./Models/Maxent/All_presence/Map_raster/NOR_2070/",
            unique(sp$canonicalName), "_maxent_NOR_2070.tif"), 
            format = "ascii", overwrite = TRUE)

# Save the geographic prediction as a raster future 2050 and 2070 BCC-CSM1-1
writeRaster(maxEnt.map_BCC_50, 
            filename = paste0("./Models/Maxent/All_presence/Map_raster/BCC_2050/",
            unique(sp$canonicalName), "_maxent_BCC_2050.tif"), 
            format = "ascii", overwrite = TRUE)

# Save the geographic prediction as a raster future 2070 BCC-CSM1-1
writeRaster(maxEnt.map_BCC_70, 
            filename = paste0("./Models/Maxent/All_presence/Map_raster/BCC_2070/",
            unique(sp$canonicalName), "_maxent_BCC_2070.tif"), 
            format = "ascii", overwrite = TRUE)

rm(maxEnt.map,
   maxEnt.map_MIROC_50, maxEnt.map_MIROC_70,
   maxEnt.map_NOR_50, maxEnt.map_NOR_70,
   maxEnt.map_BCC_50, maxEnt.map_BCC_70,
   plot.1, plot.2, plot.3, plot.4, plot.5, plot.6, plot.7)


#--------------------------#
# Boosted regression trees #
#--------------------------#
# Calibrate BRT model
sp.tc5.lr01 <- gbm.step(data=sp, gbm.x = 5:11, gbm.y = 4,
                        family = "bernoulli", tree.complexity = 5,
                        learning.rate = 0.001, bag.fraction = 0.5)

# Geographical prediction present
brt.map <- predict(mcpRaster.sub, sp.tc5.lr01,
                   n.trees=sp.tc5.lr01$gbm.call$best.trees, type = "response")

# Geographical prediction future 2050 and 2070 MIROC-ESM-CHEM
brt.map_MIROC_50 <- predict(mcpRaster.sub_MIROC_50, sp.tc5.lr01,
                   n.trees=sp.tc5.lr01$gbm.call$best.trees, type = "response")
brt.map_MIROC_70 <- predict(mcpRaster.sub_MIROC_70, sp.tc5.lr01,
                   n.trees=sp.tc5.lr01$gbm.call$best.trees, type = "response")

# Geographical prediction future 2050 and 2070 NorESM1-M
brt.map_NOR_50 <- predict(mcpRaster.sub_NOR_50, sp.tc5.lr01,
                  n.trees=sp.tc5.lr01$gbm.call$best.trees, type = "response")
brt.map_NOR_70 <- predict(mcpRaster.sub_NOR_70, sp.tc5.lr01,
                  n.trees=sp.tc5.lr01$gbm.call$best.trees, type = "response")

# Geographical prediction future 2050 and 2070 BCC-CSM1-1
brt.map_BCC_50 <- predict(mcpRaster.sub_BCC_50, sp.tc5.lr01,
                  n.trees=sp.tc5.lr01$gbm.call$best.trees, type = "response")
brt.map_BCC_70 <- predict(mcpRaster.sub_BCC_70, sp.tc5.lr01,
                  n.trees=sp.tc5.lr01$gbm.call$best.trees, type = "response")

png(paste0("./Models/BRT/All_presence/Map_plot/",
           unique(sp$canonicalName), "_brt.png"), 
    width = 2400, height = 2400, pointsize = 30)

# Plot present
brt.map.df <- as.data.frame(brt.map, xy = TRUE)
brt.map.df <- brt.map.df[!is.na(brt.map.df$layer),]

plot.1 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "Present")) +
  geom_raster(data = elev.df,aes(x = x, y = y)) +
  geom_raster(data = brt.map.df,aes(x = x, y = y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position = "bottom", legend.box = "vertical") +
  geom_point(aes(x = sp[sp$presence == 1,]$x,
                 y = sp[sp$presence == 1,]$y), size = .4)+
  labs(fill = "Presence probability") 

rm(brt.map.df)

#Plot MIROC-ESM-CHEM 2050
brt.map_MIROC_50.df <- as.data.frame(brt.map_MIROC_50, xy = TRUE)
brt.map_MIROC_50.df <- brt.map_MIROC_50.df[!is.na(brt.map_MIROC_50.df$layer),]

plot.2 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "MIROC-ESM-CHEM 2050")) +
  geom_raster(data = elev.df,aes(x = x, y = y)) +
  geom_raster(data = brt.map_MIROC_50.df,aes(x = x, y = y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position = "none")

rm(brt.map_MIROC_50_50.df)

#Plot MIROC-ESM-CHEM 2070
brt.map_MIROC_70.df <- as.data.frame(brt.map_MIROC_70, xy=TRUE)
brt.map_MIROC_70.df <- brt.map_MIROC_70.df[!is.na(brt.map_MIROC_70.df$layer),]

plot.3 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "MIROC-ESM-CHEM 2070")) +
  geom_raster(data = elev.df,aes(x = x, y = y)) +
  geom_raster(data = brt.map_MIROC_70.df,aes(x = x, y = y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position = "none")

rm(brt.map_MIROC_70.df)

#Plot NorESM1-M 2050
brt.map_NOR_50.df <- as.data.frame(brt.map_NOR_50, xy=TRUE)
brt.map_NOR_50.df <- brt.map_NOR_50.df[!is.na(brt.map_NOR_50.df$layer),]

plot.4 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "NorESM1-M 2050")) +
  geom_raster(data = elev.df,aes(x = x, y = y)) +
  geom_raster(data = brt.map_NOR_50.df,aes(x = x, y = y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position = "none")

rm(brt.map_NOR_50.df)

#Plot NorESM1-M 2070
brt.map_NOR_70.df <- as.data.frame(brt.map_NOR_70, xy=TRUE)
brt.map_NOR_70.df <- brt.map_NOR_70.df[!is.na(brt.map_NOR_70.df$layer),]

plot.5 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "NorESM1-M 2070")) +
  geom_raster(data = elev.df,aes(x = x, y = y)) +
  geom_raster(data = brt.map_NOR_70.df,aes(x = x, y = y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position = "none") 

rm(mbrt.map_NOR_70.df)

#Plot BCC-CSM1-1 2050
brt.map_BCC_50.df <- as.data.frame(brt.map_BCC_50, xy=TRUE)
brt.map_BCC_50.df <- brt.map_BCC_50.df[!is.na(brt.map_BCC_50.df$layer),]

plot.6 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "BCC-CSM1-1 2050")) +
  geom_raster(data = elev.df,aes(x = x, y = y)) +
  geom_raster(data = brt.map_BCC_50.df,aes(x = x, y = y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position = "none")

rm(brt.map_BCC_50.df)

#Plot BCC-CSM1-1 2070
brt.map_BCC_70.df <- as.data.frame(brt.map_BCC_70, xy=TRUE)
brt.map_BCC_70.df <- brt.map_BCC_70.df[!is.na(brt.map_BCC_70.df$layer),]

plot.7 <- ggplot() +
  ggtitle(paste(unique(sp$canonicalName), "BCC-CSM1-1 2070")) +
  geom_raster(data = elev.df,aes(x = x, y = y)) +
  geom_raster(data = brt.map_BCC_70.df,aes(x = x, y = y, fill = layer))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position = "none")

rm(brt.map_BCC_70.df)

# Plot grid
gridExtra::grid.arrange(plot.1,plot.2,plot.3,plot.4,plot.5,plot.6,plot.7,
                        widths = c(1, 1, 1, 1),
                        layout_matrix = rbind(c(NA,1,1,NA), c(2,2,3,3), 
                                              c(4,4,5,5), c(6,6,7,7)))
dev.off()

# PRESENT ----------------------------------------------------------------------
# Save the geographic prediction as a raster present
writeRaster(brt.map, 
            filename = paste0("./Models/BRT/All_presence/Map_raster/Present/", 
            unique(sp$canonicalName), "_brt.tif"), 
            format = "ascii", overwrite = TRUE)

# FUTURES ----------------------------------------------------------------------
# Save the geographic prediction as a raster future 2050 and 2070 MIROC-ESM-CHEM
writeRaster(brt.map_MIROC_50, 
            filename = paste0("./Models/BRT/All_presence/Map_raster/MIROC_2050/",
            unique(sp$canonicalName), "_brt_MIROC_2050.tif"), 
            format = "ascii", overwrite=TRUE)
writeRaster(brt.map_MIROC_70, 
            filename = paste0("./Models/BRT/All_presence/Map_raster/MIROC_2070/",
            unique(sp$canonicalName), "_brt_MIROC_2070.tif"), 
            format = "ascii", overwrite = TRUE)

# Save the geographic prediction as a raster future 2050 and 2070 NorESM1-M
writeRaster(brt.map_NOR_50, 
            filename = paste0("./Models/BRT/All_presence/Map_raster/NOR_2050/",
            unique(sp$canonicalName), "_brt_NOR_2050.tif"), 
            format = "ascii", overwrite = TRUE)
writeRaster(brt.map_NOR_70, 
            filename = paste0("./Models/BRT/All_presence/Map_raster/NOR_2070/",
            unique(sp$canonicalName), "_brt_NOR_2070.tif"), 
            format = "ascii", overwrite = TRUE)

# Save the geographic prediction as a raster future 2050 and 2070 BCC-CSM1-1
writeRaster(brt.map_BCC_50, 
            filename = paste0("./Models/BRT/All_presence/Map_raster/BCC_2050/",
            unique(sp$canonicalName), "_brt_BCC_2050.tif"), 
            format = "ascii", overwrite = TRUE)
writeRaster(brt.map_BCC_70, 
            filename = paste0("./Models/BRT/All_presence/Map_raster/BCC_2070/",
            unique(sp$canonicalName), "_brt_BCC_2070.tif"), 
            format = "ascii", overwrite = TRUE)

rm(brt.map,
   brt.map_MIROC_50, brt.map_MIROC_70,
   brt.map_NOR_50, brt.map_NOR_70,
   brt.map_BCC_50, brt.map_BCC_70,
   plot.1, plot.2, plot.3, plot.4, plot.5, plot.6, plot.7)


#----------#
# Ensemble #
#----------#

# PRESENT ----------------------------------------------------------------------
# Load total models
gam.models <- list.files(path = "./Models/GAM/All_presence/Map_raster/Present/",
                          pattern='.asc', full.names = TRUE)
max.models <- list.files(path = "./Models/Maxent/All_presence/Map_raster/Present/",
                          pattern='.asc', full.names=TRUE)
brt.models <- list.files(path = "./Models/BRT/All_presence/Map_raster/Present/",
                          pattern='.asc', full.names=TRUE)

models <- stack(gam.models[i], brt.models[i], max.models[i])

# Ensemble
ensemble <- calc(models, mean, 
         filename = paste0("./Models/Ensemble/All_presence/Map_raster/Present/", 
         stringr::str_remove(names(models)[1], "_gam"), ".asc"), 
         overwrite = TRUE)

# FUTURES ----------------------------------------------------------------------
# Load total models 2050 and 2070 MIROC-ESM-CHEM
gam.models <- list.files(path = "./Models/GAM/All_presence/Map_raster/MIROC_2050/",
                          pattern = ".asc", full.names = TRUE)
max.models <- list.files(path = "./Models/Maxent/All_presence/Map_raster/MIROC_2050/",
                          pattern = ".asc", full.names = TRUE)
brt.models <- list.files(path = "./Models/BRT/All_presence/Map_raster/MIROC_2050/",
                          pattern = ".asc", full.names = TRUE)

models <- stack(gam.models[i], brt.models[i], max.models[i])

# Ensemble
ensemble <- calc(models, mean, 
     filename = paste0("./Models/Ensemble/All_presence/Map_raster/MIROC_2050/", 
     stringr::str_remove(names(models)[1], "_gam"), ".asc"), 
     overwrite = TRUE)


gam.models <- list.files(path = "./Models/GAM/All_presence/Map_raster/MIROC_2070/", 
                          pattern = ".asc", full.names = TRUE)
max.models <- list.files(path = "./Models/Maxent/All_presence/Map_raster/MIROC_2070/",
                          pattern = ".asc", full.names = TRUE)
brt.models <- list.files(path = "./Models/BRT/All_presence/Map_raster/MIROC_2070/",
                          pattern = ".asc", full.names = TRUE)

models <- stack(gam.models[i], brt.models[i], max.models[i])

# Ensemble
ensemble <- calc(models, mean, 
      filename = paste0("./Models/Ensemble/All_presence/Map_raster/MIROC_2070/", 
      stringr::str_remove(names(models)[1], "_gam"), ".asc"), 
      overwrite = TRUE)

# Load total models 2050 and 2070 NorESM1-M
gam.models <- list.files(path = "./Models/GAM/All_presence/Map_raster/NOR_2050/",
                          pattern = ".asc", full.names = TRUE)
max.models <- list.files(path = "./Models/Maxent/All_presence/Map_raster/NOR_2050/",
                          pattern = ".asc", full.names = TRUE)
brt.models <- list.files(path = "./Models/BRT/All_presence/Map_raster/NOR_2050/",
                          pattern='.asc', full.names = TRUE)

models <- stack(gam.models[i], brt.models[i], max.models[i])

# Ensemble
ensemble <- calc(models, mean, 
       filename = paste0("./Models/Ensemble/All_presence/Map_raster/NOR_2050/", 
       stringr::str_remove(names(modelos)[1], "_gam"), ".asc"), 
       overwrite = TRUE)

gam.models <- list.files(path = "./Models/GAM/All_presence/Map_raster/NOR_2070/",
                          pattern = ".asc", full.names = TRUE)
max.models <- list.files(path = "./Models/Maxent/All_presence/Map_raster/NOR_2070/",
                          pattern = ".asc", full.names = TRUE)
brt.models <- list.files(path = "./Models/BRT/All_presence/Map_raster/NOR_2070/",
                          pattern = ".asc", full.names = TRUE)

models <- stack(gam.models[i], brt.models[i], max.models[i])

# Ensemble
ensemble <- calc(models, mean,
      filename = paste0("./Models/Ensemble/All_presence/Map_raster/NOR_2070/", 
      stringr::str_remove(names(modelos)[1], "_gam"), ".asc"), 
      overwrite = TRUE)


# Load total models 2050 and 2070 BCC-CSM1-1
gam.models <- list.files(path = "./Models/GAM/All_presence/Map_raster/BCC_2050/",
                          pattern = ".asc", full.names = TRUE)
max.models <- list.files(path = "./Models/Maxent/All_presence/Map_raster/BCC_2050/",
                          pattern = ".asc", full.names = TRUE)
brt.models <- list.files(path = "./Models/BRT/All_presence/Map_raster/BCC_2050/",
                          pattern = ".asc", full.names = TRUE)

models <- stack(gam.models[i], brt.models[i], max.models[i])

# Ensemble
ensemble <- calc(models, mean, 
        filename = paste0("./Models/Ensemble/All_presence/Map_raster/BCC_2050/", 
        stringr::str_remove(names(models)[1], "_gam"), ".asc"), 
        overwrite = TRUE)

gam.models <- list.files(path = "./Models/GAM/All_presence/Map_raster/BCC_2070/",
                          pattern = ".asc", full.names = TRUE)
max.models <- list.files(path = "./Models/Maxent/All_presence/Map_raster/BCC_2070/",
                          pattern = ".asc", full.names = TRUE)
brt.models <- list.files(path = "./Models/BRT/All_presence/Map_raster/BCC_2070/",
                          pattern = ".asc", full.names = TRUE)

models <- stack(gam.models[i], brt.models[i], max.models[i])

# Ensemble
ensemble <- calc(modelos, mean, 
       filename = paste0("./Models/Ensemble/All_presence/Map_raster/BCC_2070/", 
       stringr::str_remove(names(models)[1], "_gam"), ".asc"), 
       overwrite = TRUE)
}

# Clean workspace
rm(list = ls())
