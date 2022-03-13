###############################
# THRESHOLD AnD MAP Gain-Loss #
###############################

# Load libraries
library(raster) # <- GIS
library(ggplot2) # <- Plot
library(gridExtra) # <- Plot
library(grid) # <- Plot
library(osfr) # <- Connect to OSF

# Main path
main.path <- "YOUR_PATH"
setwd(main.path)

# Connect to Open Science Framework
cr_project <- osf_retrieve_node("4rjuc")
osf_files <- osf_ls_files(cr_project, type = "folder")

# Download main variables
file.number <- which(osf_ls_files(osf_files[1, ], pattern = "01_AUC_>7.csv")[ ,1] == "01_AUC_>7.csv")
osf_download(osf_ls_files(osf_files[1, ], pattern = "01_AUC_>7.csv")[file.number, ], path = main.path)

# Create folder to save the data
dir.create("./Models/Gain-Loss")

# Load Environmental variables ------------------------------------------------- 
#---------#
# PRESENT #
#---------#

# Elevation
elev <- raster("./elevation_5_null.asc")

# SELEction VAriables
variable.seleccionadas <- elev

# Load .asc and csv Threshold -----------------------------------------------------------
# Present
asc.ths <- list.files(path = "./Models/Threshold/Present/raster/",
                      pattern = ".asc", full.names = TRUE)

# Future - 2050-2070 
asc.ths_MIROC_2050 <- list.files(path = "./Models/Threshold/MIROC_2050/raster/",
                                 pattern = ".asc", full.names = TRUE)
asc.ths_MIROC_2070 <- list.files(path = "./Models/Threshold/MIROC_2070/raster/",
                                 pattern = ".asc", full.names = TRUE)

asc.ths_NOR_2050 <- list.files(path = "./Models/Threshold/NOR_2050/raster/",
                               pattern = ".asc", full.names = TRUE)
asc.ths_NOR_2070 <- list.files(path = "./Models/Threshold/NOR_2070/raster/",
                               pattern = ".asc", full.names = TRUE)

asc.ths_BCC_2050 <- list.files(path = "./Models/Threshold/BCC_2050/raster/",
                               pattern = ".asc", full.names = TRUE)
asc.ths_BCC_2070 <- list.files(path = "./Models/Threshold/BCC_2070/raster/",
                               pattern = ".asc", full.names = TRUE)

# LOAD PRESENCE OCCURRENCES
# Load Odonata files list species ----------------------------------------------
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

sp30 <- num.presence$canonicalName[num.presence$n.presence >= 30]

# Load table with all Odonata presence and background points -------------------
odonFiles <- list.files(path = "./Presence_Background/Odonata_presence_background_variables/", 
                        pattern = ".csv$", all.files = TRUE, full.names = TRUE)

odonFiles.model <- data.frame()
for(n in 1:length(sp30)){
odonFiles.model.1 <- odonFiles[grepl(sp30[n], odonFiles)]
odonFiles.model <- rbind(odonFiles.model, odonFiles.model.1)
}


# LOAD models ------------------------------------------------------------------
# Present
gam.pres <- list.files(path = "./Models/GAM/All_presence/Map_raster/Present/",
                       pattern = ".asc", full.names = TRUE)
maxent.pres <- list.files(path = "./Models/Maxent/All_presence/Map_raster/Present/",
                          pattern = ".asc", full.names = TRUE)
brt.pres <- list.files(path = "./Models/BRT/All_presence/Map_raster/Present/",
                       pattern = ".asc", full.names = TRUE)
ensemble.pres <- list.files(path = "./Models/Ensemble/All_presence/Map_raster/Present/",
                            pattern = ".asc", full.names = TRUE)

presentFiles <- c(gam.pres, maxent.pres, brt.pres, ensemble.pres)
rm(gam.pres, maxent.pres, brt.pres, ensemble.pres)


# Load Max AUC
maxAuc <- read.csv("./01_AUC_>7.csv")


###########################################
for(i in 1:107){
  
# Select Threshold with best evaluation
# Extract model with highest AUC -----------------------------------------------
spPresent <- gsub("_", " ", maxAuc$model[i])
  
if(grepl("ensamblado", spPresent) == FALSE){
spPresent <- paste0(stringr::word(spPresent, 1),",",
             stringr::word(spPresent, 2), "_", stringr::word(spPresent, 3))
spPresent <- gsub(",", " ", spPresent)
} else {
spPresent <- paste0(stringr::word(spPresent, 1),",",
             stringr::word(spPresent, 2), "_",stringr::word(spPresent, 3), ".asc")
spPresent <- gsub(",", "_", spPresent)
}
spPresent <- presentFiles[grep(spPresent, presentFiles)]
spStack <- stack(spPresent)
  
# Present Threshold
pres.ths <- asc.ths[grepl(maxAuc$model[i], asc.ths)]
  
# MIROC Threshold
cen.ths_MIROC_2050 <- asc.ths_MIROC_2050[grepl(maxAuc$model[i], asc.ths_MIROC_2050)]
cen.ths_MIROC_2070 <- asc.ths_MIROC_2070[grepl(maxAuc$model[i], asc.ths_MIROC_2070)]
  
# NOR Threshold
cen.ths_NOR_2050 <- asc.ths_NOR_2050[grepl(maxAuc$model[i], asc.ths_NOR_2050)]
cen.ths_NOR_2070 <- asc.ths_NOR_2070[grepl(maxAuc$model[i], asc.ths_NOR_2070)]
  
# BCC
cen.ths_BCC_2050 <- asc.ths_BCC_2050[grepl(maxAuc$model[i], asc.ths_BCC_2050)]
cen.ths_BCC_2070 <- asc.ths_BCC_2070[grepl(maxAuc$model[i], asc.ths_BCC_2070)]
  
# Stack best model threshold
ras.pres <- stack(pres.ths)
ras.MIROC_2050 <- stack(cen.ths_MIROC_2050)
ras.MIROC_2070 <- stack(cen.ths_MIROC_2070)
ras.NOR_2050 <- stack(cen.ths_NOR_2050)
ras.NOR_2070 <- stack(cen.ths_NOR_2070)
ras.BCC_2050 <- stack(cen.ths_BCC_2050)
ras.BCC_2070 <- stack(cen.ths_BCC_2070)
  
rm(pres.ths,
   cen.ths_MIROC_2050, cen.ths_MIROC_2070,
   cen.ths_NOR_2050, cen.ths_NOR_2070,
   cen.ths_BCC_2050, cen.ths_BCC_2070)
  
# Gain-Loss Maps  

gl.MIROC_2050 <- ras.MIROC_2050 - ras.pres
gl.MIROC_2070 <- ras.MIROC_2070 - ras.pres
gl.NOR_2050 <- ras.NOR_2050 - ras.pres
gl.NOR_2070 <- ras.NOR_2070 - ras.pres
gl.BCC_2050 <- ras.BCC_2050 - ras.pres
gl.BCC_2070 <- ras.BCC_2070 - ras.pres
  
# Convert 0 to NA
ras.pres[ras.pres == 0] <- NA
ras.MIROC_2050[ras.MIROC_2050 == 0] <- NA
ras.MIROC_2070[ras.MIROC_2070 == 0] <- NA
ras.NOR_2050[ras.NOR_2050 == 0] <- NA
ras.NOR_2070[ras.NOR_2070 == 0] <- NA
ras.BCC_2050[ras.BCC_2050 == 0] <- NA
ras.BCC_2070[ras.BCC_2070 == 0] <- NA
  
# Centroid Present
centroid.pres <- colMeans(xyFromCell(ras.pres, which(ras.pres[] == 1)))
  
# Centroid Futures
centroid.MIROC_2050 <- colMeans(xyFromCell(ras.MIROC_2050, 
                                           which(ras.MIROC_2050[] == 1)))
centroid.MIROC_2070 <- colMeans(xyFromCell(ras.MIROC_2070, 
                                           which(ras.MIROC_2070[] == 1)))
  
centroid.NOR_2050 <- colMeans(xyFromCell(ras.NOR_2050, 
                                         which(ras.NOR_2050[] == 1)))
centroid.NOR_2070 <- colMeans(xyFromCell(ras.NOR_2070, 
                                         which(ras.NOR_2070[] == 1)))
  
centroid.BCC_2050 <- colMeans(xyFromCell(ras.BCC_2050, 
                                         which(ras.BCC_2050[] == 1)))
centroid.BCC_2070 <- colMeans(xyFromCell(ras.BCC_2070, 
                                         which(ras.BCC_2070[] == 1)))
  
# Create one dataframe with centroid information
centroid.MIROC <- as.data.frame(rbind(centroid.pres, 
                                      centroid.MIROC_2050, centroid.MIROC_2070))
centroid.MIROC$period <- c("Present", "2050", "2070")
centroid.NOR <- as.data.frame(rbind(centroid.pres, 
                                    centroid.NOR_2050, centroid.NOR_2070))
centroid.NOR$period <- c("Present", "2050", "2070")
centroid.BCC <- as.data.frame(rbind(centroid.pres, 
                                    centroid.BCC_2050, centroid.BCC_2070))
centroid.BCC$period <- c("Present", "2050", "2070")
  
rm(centroid.pres, 
   centroid.MIROC_2050, centroid.MIROC_2070,
   centroid.NOR_2050, centroid.NOR_2070,
   centroid.BCC_2050, centroid.BCC_2070)  
  
# Convert elev to dataframe to use in ggplot2
elev.df <- as.data.frame(elev, xy=TRUE)
elev.df <- elev.df[!is.na(elev.df$elevation_5_null),]
elev.df$elevation_5_null <- 0
  
spStack.df <- as.data.frame(spStack, xy=TRUE)
spStack.df <- spStack.df[!is.na(spStack.df[,3]),]
  
# PRESENT MODEL with DISTRIBUTION POINTS ---------------------------------------
sp <- read.csv(odonFiles.model[i,]) 
  
presentDistribution <- ggplot() +
  theme(plot.title = element_text(size=12, face="bold")) +
  ggtitle(paste("Present \nBest model:", maxAuc$modelName[i] ,
                "\nAUC:", maxAuc$auc[i],
                "\nBoyce:", maxAuc$boyce[i])) +
  geom_raster(data=elev.df,aes(x=x, y=y)) +
  geom_raster(data=spStack.df,aes(x=x,y=y, fill = spStack.df[,3]))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())+
  scale_fill_viridis_c(option = "viridis")+
  theme(legend.position="bottom", legend.box = "horizontal") +
  geom_point(aes(x = sp[sp$presence == 1,]$x,
                 y = sp[sp$presence == 1,]$y), 
             colour ="black", fill="cornsilk",size = .3, shape=21, alpha = 0.7)+
  geom_point(aes(x = sp$x[1],
                 y = sp$y[1], colour = ""), size = .3, shape=21, alpha = .7) +
  scale_colour_manual(values = c("black")) +
  labs(fill = "Presence probability") + labs(colour = "Occurences")
  
# MIROC
gl.MIROC_2050.df <- as.data.frame(gl.MIROC_2050, xy=TRUE)
gl.MIROC_2050.df <- gl.MIROC_2050.df[!is.na(gl.MIROC_2050.df[,3]),]
gl.MIROC_2050.df <- gl.MIROC_2050.df[gl.MIROC_2050.df[,3] != 0,]
  
MIROC_2050.plot <- ggplot() +
  ggtitle(paste("2050")) +
  theme(plot.title = element_text(size=10, face="bold")) +
  geom_raster(data=elev.df,aes(x=x,y=y), fill = c("#414141"))+
  geom_raster(data=gl.MIROC_2050.df, aes(x=x,y=y, fill = as.factor(layer))) +
  scale_fill_manual(values = c("firebrick1", "springgreen2")) +
  labs(fill = "Gain - Loss") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  geom_point(data = centroid.MIROC[1:2,], aes(x = x,
                                              y = y, color = period)) +
  geom_segment(data = centroid.MIROC[1:2,], aes(x = x[1], xend = x[2], 
                                y = y[1], yend = y[2]), colour = "darkorchid1",
               arrow = arrow(length = unit(0.04, "inches")))+
  scale_colour_manual(values = c("gold1", "darkorange")) +
  labs(color="Period") +
  theme(legend.position="right", legend.key.size = unit(0.4, "cm"))
  
gl.MIROC_2070.df <- as.data.frame(gl.MIROC_2070, xy=TRUE)
gl.MIROC_2070.df <- gl.MIROC_2070.df[!is.na(gl.MIROC_2070.df[,3]),]
gl.MIROC_2070.df <- gl.MIROC_2070.df[gl.MIROC_2070.df[,3] != 0,]
  
MIROC_2070.plot <- ggplot() +
  ggtitle(paste("2070")) +
  theme(plot.title = element_text(size=10, face="bold")) +
  geom_raster(data=elev.df,aes(x=x,y=y), fill = c("#414141"))+
  geom_raster(data=gl.MIROC_2070.df,aes(x=x,y=y, fill = as.factor(layer))) +
  scale_fill_manual(values = c("firebrick1", "springgreen2")) +
  labs(fill = "Gain - Loss") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  geom_point(data = centroid.MIROC[c(1,3),], aes(x = x,
                                                 y = y, colour = period)) +
  geom_segment(data = centroid.MIROC[c(1,3),], aes(x = x[1], xend = x[2], 
                                 y = y[1], yend = y[2]), colour = "darkorchid1",
               arrow = arrow(length = unit(0.04, "inches")))+
  scale_colour_manual(values = c("gold1", "darkorange")) +
  labs(color="Period") +
  theme(legend.position="right", legend.key.size = unit(0.4, "cm"))
  
# NOR
gl.NOR_2050.df <- as.data.frame(gl.NOR_2050, xy=TRUE)
gl.NOR_2050.df <- gl.NOR_2050.df[!is.na(gl.NOR_2050.df[,3]),]
gl.NOR_2050.df <- gl.NOR_2050.df[gl.NOR_2050.df[,3] != 0,]
  
NOR_2050.plot <- ggplot() +
  ggtitle(paste("2050")) +
  theme(plot.title = element_text(size=10, face="bold")) +
  geom_raster(data=elev.df,aes(x=x,y=y), fill = c("#414141"))+
  geom_raster(data=gl.NOR_2050.df,aes(x=x,y=y, fill = as.factor(layer)))+
  scale_fill_manual(values = c("firebrick1", "springgreen2")) +
  labs(fill = "Gain - Loss") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  geom_point(data = centroid.NOR[1:2,], aes(x = x,
                                            y = y, color = period)) +
  geom_segment(data = centroid.NOR[1:2,], aes(x = x[1], xend = x[2], 
                                  y = y[1], yend = y[2]),colour = "darkorchid1",
               arrow = arrow(length = unit(0.04, "inches")))+
  scale_colour_manual(values = c("gold1", "darkorange")) +
  labs(color="Period") +
  theme(legend.position="right", legend.key.size = unit(0.4, "cm"))
  
gl.NOR_2070.df <- as.data.frame(gl.NOR_2070, xy=TRUE)
gl.NOR_2070.df <- gl.NOR_2070.df[!is.na(gl.NOR_2070.df[,3]),]
gl.NOR_2070.df <- gl.NOR_2070.df[gl.NOR_2070.df[,3] != 0,]
  
NOR_2070.plot <- ggplot() +
  ggtitle(paste("2070")) +
  theme(plot.title = element_text(size=10, face="bold")) +
  geom_raster(data=elev.df,aes(x=x,y=y), fill = c("#414141"))+
  geom_raster(data=gl.NOR_2070.df,aes(x=x,y=y, fill = as.factor(layer)))+
  scale_fill_manual(values = c("firebrick1", "springgreen2")) +
  labs(fill = "Gain - Loss") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  geom_point(data = centroid.NOR[c(1,3),], aes(x = x,
                                               y = y, colour = period)) +
  geom_segment(data = centroid.NOR[c(1,3),], aes(x = x[1], xend = x[2], 
                                 y = y[1], yend = y[2]),colour = "darkorchid1",
               arrow = arrow(length = unit(0.04, "inches")))+
  scale_colour_manual(values = c("gold1", "darkorange")) +
  labs(color="Period") +
  theme(legend.position="right", legend.key.size = unit(0.4, "cm"))
  
# BCC
gl.BCC_2050.df <- as.data.frame(gl.BCC_2050, xy=TRUE)
gl.BCC_2050.df <- gl.BCC_2050.df[!is.na(gl.BCC_2050.df[,3]),]
gl.BCC_2050.df <- gl.BCC_2050.df[gl.BCC_2050.df[,3] != 0,]
  
BCC_2050.plot <- ggplot() +
  ggtitle(paste("2050")) +
  theme(plot.title = element_text(size=10, face="bold")) +
  geom_raster(data=elev.df,aes(x=x,y=y), fill = c("#414141"))+
  geom_raster(data=gl.BCC_2050.df,aes(x=x,y=y, fill = as.factor(layer)))+
  scale_fill_manual(values = c("firebrick1", "springgreen2")) +
  labs(fill = "Gain - Loss") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  geom_point(data = centroid.BCC[1:2,], aes(x = x,
                                            y = y, color = period)) +
  geom_segment(data = centroid.BCC[1:2,], aes(x = x[1], xend = x[2], 
                                  y = y[1], yend = y[2]),colour = "darkorchid1",
               arrow = arrow(length = unit(0.04, "inches")))+
  scale_colour_manual(values = c("gold1", "darkorange")) +
  labs(color="Period") +
  theme(legend.position="right", legend.key.size = unit(0.4, "cm"))
  
gl.BCC_2070.df <- as.data.frame(gl.BCC_2070, xy=TRUE)
gl.BCC_2070.df <- gl.BCC_2070.df[!is.na(gl.BCC_2070.df[,3]),]
gl.BCC_2070.df <- gl.BCC_2070.df[gl.BCC_2070.df[,3] != 0,]
  
BCC_2070.plot <- ggplot() +
  ggtitle(paste("2070")) +
  theme(plot.title = element_text(size=10, face="bold")) +
  geom_raster(data=elev.df,aes(x=x,y=y), fill = c("#414141"))+
  geom_raster(data=gl.BCC_2070.df,aes(x=x,y=y, fill = as.factor(layer)))+
  scale_fill_manual(values = c("firebrick1", "springgreen2")) +
  labs(fill = "Gain - Loss") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  geom_point(data = centroid.BCC[c(1,3),], aes(x = x,
                                               y = y, colour = period)) +
  geom_segment(data = centroid.BCC[c(1,3),], aes(x = x[1], xend = x[2], 
                                 y = y[1], yend = y[2]),colour = "darkorchid1",
               arrow = arrow(length = unit(0.04, "inches")))+
  scale_colour_manual(values = c("gold1", "darkorange")) +
  labs(color="Period") +
  theme(legend.position="right", legend.key.size = unit(0.4, "cm"))
  
  
# Table annotations ------------------------------------------------------------
  
# Average Elevation ------------------------------------------------------------
# PRESENT
# Crop and mask
mcpRaster.pres <- crop(variable.seleccionadas, extent(ras.pres)) 
mcpRaster.pres <- mask(mcpRaster.pres, ras.pres)
  
# FUTURE
# Crop and mask
mcpRaster.MIROC_2050 <- crop(variable.seleccionadas, extent(ras.MIROC_2050)) 
mcpRaster.MIROC_2070 <- crop(variable.seleccionadas, extent(ras.MIROC_2070)) 
mcpRaster.MIROC_2050 <- mask(mcpRaster.MIROC_2050, ras.MIROC_2050)
mcpRaster.MIROC_2070 <- mask(mcpRaster.MIROC_2070, ras.MIROC_2070)
  
# Crop and mask
mcpRaster.NOR_2050 <- crop(variable.seleccionadas, extent(ras.NOR_2050)) 
mcpRaster.NOR_2070 <- crop(variable.seleccionadas, extent(ras.NOR_2070)) 
mcpRaster.NOR_2050 <- mask(mcpRaster.NOR_2050, ras.NOR_2050)
mcpRaster.NOR_2070 <- mask(mcpRaster.NOR_2070, ras.NOR_2070)
  
# Crop and mask
mcpRaster.BCC_2050 <- crop(variable.seleccionadas, extent(ras.BCC_2050)) 
mcpRaster.BCC_2070 <- crop(variable.seleccionadas, extent(ras.BCC_2070)) 
mcpRaster.BCC_2050 <- mask(mcpRaster.BCC_2050, ras.BCC_2050)
mcpRaster.BCC_2070 <- mask(mcpRaster.BCC_2070, ras.BCC_2070)
  
# Elevation mean
mean.pres <- cellStats(mcpRaster.pres, stat='mean')
mean.MIROC_2050 <- cellStats(mcpRaster.MIROC_2050, stat='mean')
mean.MIROC_2070 <- cellStats(mcpRaster.MIROC_2070, stat='mean')
mean.NOR_2050 <- cellStats(mcpRaster.NOR_2050, stat='mean')
mean.NOR_2070 <- cellStats(mcpRaster.NOR_2070, stat='mean')
mean.BCC_2050 <- cellStats(mcpRaster.BCC_2050, stat='mean')
mean.BCC_2070 <- cellStats(mcpRaster.BCC_2070, stat='mean')
  
# Raster cell sum for value = 1
sum.pres <- length(which(ras.pres[] == 1))
sum.MIROC_2050 <- length(which(ras.MIROC_2050[] == 1))
sum.MIROC_2070 <- length(which(ras.MIROC_2070[] == 1))
sum.NOR_2050 <- length(which(ras.NOR_2050[] == 1))
sum.NOR_2070 <- length(which(ras.NOR_2070[] == 1))
sum.BCC_2050 <- length(which(ras.BCC_2050[] == 1))
sum.BCC_2070 <- length(which(ras.BCC_2070[] == 1))
  
table.MIROC <- centroid.MIROC
table.MIROC$avgAltitude <- round(c(mean.pres, mean.MIROC_2050, mean.MIROC_2070), digits=0)
table.MIROC$cellNumber <- c(sum.pres, sum.MIROC_2050, sum.MIROC_2070)
table.MIROC$cellArea <- round(table.MIROC$cellNumber * xres(ras.pres)*111.19, digits=0)
table.MIROC$cellDifArea <- NA
table.MIROC$cellDifArea[2] <- table.MIROC$cellArea[2] - table.MIROC$cellArea[1]
table.MIROC$cellDifArea[3] <- table.MIROC$cellArea[3] - table.MIROC$cellArea[1]
colnames(table.MIROC)[1:2] <- c("Lon", "Lat")

table.NOR <- centroid.NOR
table.NOR$avgAltitude <- round(c(mean.pres, mean.NOR_2050, mean.NOR_2070), digits=0)
table.NOR$cellNumber <- c(sum.pres, sum.NOR_2050, sum.NOR_2070)
table.NOR$cellArea <- round(table.NOR$cellNumber * xres(ras.pres)*111.19, digits=0)
table.NOR$cellDifArea <- NA
table.NOR$cellDifArea[2] <- table.NOR$cellArea[2] - table.NOR$cellArea[1]
table.NOR$cellDifArea[3] <- table.NOR$cellArea[3] - table.NOR$cellArea[1]
colnames(table.NOR)[1:2] <- c("Lon", "Lat")

table.BCC <- centroid.BCC
table.BCC$avgAltitude <- round(c(mean.pres, mean.BCC_2050, mean.BCC_2070), digits=0)
table.BCC$cellNumber <- c(sum.pres, sum.BCC_2050, sum.BCC_2070)
table.BCC$cellArea <- round(table.BCC$cellNumber * xres(ras.pres)*111.19, digits =0)
table.BCC$cellDifArea <- NA
table.BCC$cellDifArea[2] <- table.BCC$cellArea[2] - table.BCC$cellArea[1]
table.BCC$cellDifArea[3] <- table.BCC$cellArea[3] - table.BCC$cellArea[1]
colnames(table.BCC)[1:2] <- c("Lon", "Lat")

rm(mean.pres, mean.MIROC_2050, mean.MIROC_2070, 
   mean.NOR_2050, mean.NOR_2070,
   mean.BCC_2050, mean.BCC_2070, 
   sum.pres, sum.MIROC_2050, sum.MIROC_2070, 
   sum.NOR_2050, sum.NOR_2070,
   sum.BCC_2050, sum.BCC_2070)

# Average and Distances
# MIROC
p0 <- c(table.MIROC$Lon[1], table.MIROC$Lat[1])
p2050 <- c(table.MIROC$Lon[2], table.MIROC$Lat[2])
p2070 <- c(table.MIROC$Lon[3], table.MIROC$Lat[3])

table.MIROC$direction <- NA ; table.MIROC$dist_km <- NA
table.MIROC$direction[2] <- round(geosphere::bearing(p0, p2050, a=6378137, f=1/298.257223563), digits=2)
table.MIROC$direction[3] <- round(geosphere::bearing(p0, p2070, a=6378137, f=1/298.257223563), digits=2)
table.MIROC$dist_km[2] <- round(geosphere::distCosine(p0, p2050)/1000, digits=0) # Vector of distances in the same unit as r (default is meters)
table.MIROC$dist_km[3] <- round(geosphere::distCosine(p0, p2070)/1000, digits=0)

rm(p0,p2050,p2070)

table.MIROC.grob <- tableGrob(table.MIROC, rows=NULL, theme=ttheme_default(base_size = 7))

# NOR
p0 <- c(table.NOR$Lon[1], table.NOR$Lat[1])
p2050 <- c(table.NOR$Lon[2], table.NOR$Lat[2])
p2070 <- c(table.NOR$Lon[3], table.NOR$Lat[3])

table.NOR$direction <- NA ; table.NOR$dist_km <- NA
table.NOR$direction[2] <- round(geosphere::bearing(p0, p2050, a=6378137, f=1/298.257223563), digits=2)
table.NOR$direction[3] <- round(geosphere::bearing(p0, p2070, a=6378137, f=1/298.257223563), digits=2)
table.NOR$dist_km[2] <- round(geosphere::distCosine(p0, p2050)/1000, digits=0)
table.NOR$dist_km[3] <- round(geosphere::distCosine(p0, p2070)/1000, digits=0)

rm(p0,p2050,p2070)

table.NOR.grob <- tableGrob(table.NOR, rows=NULL, theme=ttheme_default(base_size = 7))

# BCC
p0 <- c(table.BCC$Lon[1], table.BCC$Lat[1])
p2050 <- c(table.BCC$Lon[2], table.BCC$Lat[2])
p2070 <- c(table.BCC$Lon[3], table.BCC$Lat[3])

table.BCC$direction <- NA ; table.BCC$dist_km <- NA
table.BCC$direction[2] <- round(geosphere::bearing(p0, p2050, a=6378137, f=1/298.257223563), digits=2)
table.BCC$direction[3] <- round(geosphere::bearing(p0, p2070, a=6378137, f=1/298.257223563), digits=2)
table.BCC$dist_km[2] <- round(geosphere::distCosine(p0, p2050)/1000, digits=0)
table.BCC$dist_km[3] <- round(geosphere::distCosine(p0, p2070)/1000, digits=0)

rm(p0,p2050,p2070)

table.BCC.grob <- tableGrob(table.BCC, rows=NULL, theme=ttheme_default(base_size = 7))

# Violin Plot ------------------------------------------------------------------
mcpDf.pres <- na.omit(as.data.frame(mcpRaster.pres))
mcpDf.pres$period <- "PRESENT"

mcpDf.MIROC_2050 <- na.omit(as.data.frame(mcpRaster.MIROC_2050))
mcpDf.MIROC_2050$period <- "MIROC_2050"
mcpDf.MIROC_2070 <- na.omit(as.data.frame(mcpRaster.MIROC_2070))
mcpDf.MIROC_2070$period <- "MIROC_2070"
  
mcpDf.NOR_2050 <- na.omit(as.data.frame(mcpRaster.NOR_2050))
mcpDf.NOR_2050$period <- "NOR_2050"
mcpDf.NOR_2070 <- na.omit(as.data.frame(mcpRaster.NOR_2070))
mcpDf.NOR_2070$period <- "NOR_2070"
  
mcpDf.BCC_2050 <- na.omit(as.data.frame(mcpRaster.BCC_2050))
mcpDf.BCC_2050$period <- "BCC_2050"
mcpDf.BCC_2070 <- na.omit(as.data.frame(mcpRaster.BCC_2070))
mcpDf.BCC_2070$period <- "BCC_2070"
  
mcpDf.MIROC <- rbind(mcpDf.pres, mcpDf.MIROC_2050, mcpDf.MIROC_2070) 
mcpDf.NOR <- rbind(mcpDf.pres, mcpDf.NOR_2050, mcpDf.NOR_2070) 
mcpDf.BCC <- rbind(mcpDf.pres, mcpDf.BCC_2050, mcpDf.BCC_2070) 
  
rm(mcpDf.pres, mcpDf.MIROC_2050, mcpDf.MIROC_2070 ,
   mcpDf.NOR_2050, mcpDf.NOR_2070,
   mcpDf.BCC_2050, mcpDf.BCC_2070)
  
# Plot 
mcpDf.MIROC$period <- factor(mcpDf.MIROC$period, 
                             levels = c("PRESENT", "MIROC_2050", "MIROC_2070"))
violin.MIROC <- ggplot(data = mcpDf.MIROC, 
                       aes(x= layer , y = period, col=period, fill = period)) + 
  geom_violin(trim=FALSE, position = position_dodge(0.9)) +
  geom_boxplot(width=0.1, position = position_dodge(0.9)) + 
  scale_color_manual(values=c("#edae49", "#566be9","#e98b56")) +
  scale_fill_manual(values = alpha(c("#edae49","#566be9","#e98b56"), .2)) +
  theme_bw() +
  xlab("Elevation") + ylab("Period") +
  theme(legend.position="none") +
  coord_flip()
  
mcpDf.NOR$period <- factor(mcpDf.NOR$period, 
                           levels = c("PRESENT", "NOR_2050", "NOR_2070"))
violin.NOR <- ggplot(data = mcpDf.NOR, 
                     aes(x= layer , y = period, col=period, fill = period)) + 
  geom_violin(trim=FALSE, position = position_dodge(0.9)) +
  geom_boxplot(width=0.1, position = position_dodge(0.9)) + 
  scale_color_manual(values=c("#edae49", "#566be9","#e98b56")) +
  scale_fill_manual(values = alpha(c("#edae49","#566be9","#e98b56"), .2)) +
  theme_bw() +
  xlab("Elevation") + ylab("Period") +
  theme(legend.position="none") +
  coord_flip()
  
mcpDf.BCC$period <- factor(mcpDf.BCC$period, levels = c("PRESENT", "BCC_2050", "BCC_2070"))
violin.BCC <- ggplot(data = mcpDf.BCC, aes(x= layer , y = period, col=period, fill = period)) + 
  geom_violin(trim=FALSE, position = position_dodge(0.9)) +
  geom_boxplot(width=0.1, position = position_dodge(0.9)) + 
  scale_color_manual(values=c("#edae49", "#566be9","#e98b56")) +
  scale_fill_manual(values = alpha(c("#edae49","#566be9","#e98b56"), .2)) +
  theme_bw() +
  xlab("Elevation") + ylab("Period") +
  theme(legend.position="none") +
  coord_flip()
  
# Extract species name for the table
species <- unique(sp$canonicalName)
  
pdf(paste("./Models/Gain-Loss/", species, "summary_Gain_Loss.pdf"), 
    width = 16.5, height = 11.7)
# MIROC
gridExtra::grid.arrange(presentDistribution, 
                        MIROC_2050.plot, MIROC_2070.plot,
                        table.MIROC.grob, violin.MIROC,
  top = grid::textGrob(paste(species, "- MIROC-ESM-CHEM"), x = 0.01, hjust = 0,
  gp=gpar(fontsize=15,fontface = "bold")),
                        widths = c(1, 1, 1 ,1),
                        layout_matrix = rbind(c(1,1,2,2),
                                              c(1,1,3,3),
                                              c(5,5,6,6)))
# NOR
gridExtra::grid.arrange(presentDistribution, 
                        NOR_2050.plot, NOR_2070.plot,
                        table.NOR.grob, violin.NOR,
          top = grid::textGrob(paste(species, "- NorESM1-M"), x = 0, hjust = 0,
         gp=gpar(fontsize=15,fontface = "bold")),
                        widths = c(1, 1, 1 ,1),
                        layout_matrix = rbind(c(1,1,2,2),
                                              c(1,1,3,3),
                                              c(5,5,6,6)))
  
# BCC
gridExtra::grid.arrange(presentDistribution, 
                        BCC_2050.plot, BCC_2070.plot,
                        table.BCC.grob, violin.BCC,
          top = grid::textGrob(paste(species, "- BCC-CSM1-1"), x = 0, hjust = 0,
          gp=gpar(fontsize=15,fontface = "bold")),
                         widths = c(1, 1, 1 ,1),
                         layout_matrix = rbind(c(1,1,2,2),
                                               c(1,1,3,3),
                                               c(5,5,6,6)))
 dev.off()
}

# Clean wworkspace

rm(list = ls())
