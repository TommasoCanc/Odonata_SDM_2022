##################
# Table for PGLS #
##################

# Load libraries
library(raster)
library(geosphere)

#---------#
# PRESENT #
#---------#

# Elevation
elev <- raster("LOAD ELEVATION MAPS")

# LOAD asc and csv Threshold -----------------------------------------------------------
# Present
asc.ths <- list.files(path="LOAD THRESHOLD PRESENT FILES",pattern='.asc', full.names=TRUE)

# Future - 2050-2070 
asc.ths_MIROC_2050 <- unique(list.files(path="LOAD THRESHOLD MIROC 2050 FILES",pattern='.asc', full.names=TRUE))
asc.ths_MIROC_2070 <- unique(list.files(path="LOAD THRESHOLD MIROC 2070 FILES",pattern='.asc', full.names=TRUE))

asc.ths_NOR_2050 <- unique(list.files(path="LOAD THRESHOLD NOR 2050 FILES",pattern='.asc', full.names=TRUE))
asc.ths_NOR_2070 <- unique(list.files(path="LOAD THRESHOLD NOR 2070 FILES",pattern='.asc', full.names=TRUE))

asc.ths_BCC_2050 <- unique(list.files(path="LOAD THRESHOLD BCC 2050 FILES",pattern='.asc', full.names=TRUE))
asc.ths_BCC_2070 <- unique(list.files(path="LOAD THRESHOLD BCC 2070 FILES",pattern='.asc', full.names=TRUE))

# LOAD PRESENCE OCCURRENCES
# Load odonata files list species ----------------------------------------------
odonFiles <- list.files(path = "/Odonata_training/", 
                        pattern = ".csv$", all.files = TRUE, full.names = TRUE)

# Number of training presence
num.presencia <- data.frame()

for(i in 1:length(odonFiles)){
  sp <- read.csv(odonFiles[i]) 
  num.presencia.1 <- data.frame(canonicalName = unique(sp$canonicalName),
                                n.presencia = nrow(sp[sp$presencia == 1,]))
  num.presencia <- rbind(num.presencia, num.presencia.1)                    
}

# We select only the species with => 30. We have to opt for this solution
# because if not the model is very poor... We have a lot of species, so in this
# case we don't want buils a fine model
sp20 <- num.presencia$canonicalName[num.presencia$n.presencia >= 30]

# Load Max AUC
maxAuc <- read.csv("./01_AUC_>7.csv")


###########################################
# Prepare total tables where we can store the data
table.present <- data.frame()
table.miroc50 <- data.frame()
table.miroc70 <- data.frame()
table.nor50 <- data.frame()
table.nor70 <- data.frame()
table.bcc50 <- data.frame()
table.bcc70 <- data.frame()

#i=1
for(i in 1:107){
  
  print(paste("-----", i, "-----"))
  
# Present Threshold
pres.ths <- asc.ths[grepl(maxAuc$modelo[i], asc.ths)]
  
# MIROC Threshold
cen.ths_MIROC_2050 <- asc.ths_MIROC_2050[grepl(maxAuc$modelo[i], asc.ths_MIROC_2050)]
cen.ths_MIROC_2070 <- asc.ths_MIROC_2070[grepl(maxAuc$modelo[i], asc.ths_MIROC_2070)]
  
# NOR
cen.ths_NOR_2050 <- asc.ths_NOR_2050[grepl(maxAuc$modelo[i], asc.ths_NOR_2050)]
cen.ths_NOR_2070 <- asc.ths_NOR_2070[grepl(maxAuc$modelo[i], asc.ths_NOR_2070)]
  
# BCC
cen.ths_BCC_2050 <- asc.ths_BCC_2050[grepl(maxAuc$modelo[i], asc.ths_BCC_2050)]
cen.ths_BCC_2070 <- asc.ths_BCC_2070[grepl(maxAuc$modelo[i], asc.ths_BCC_2070)]
  
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
centroid.MIROC_2050 <- colMeans(xyFromCell(ras.MIROC_2050, which(ras.MIROC_2050[] == 1)))
centroid.MIROC_2070 <- colMeans(xyFromCell(ras.MIROC_2070, which(ras.MIROC_2070[] == 1)))
  
centroid.NOR_2050 <- colMeans(xyFromCell(ras.NOR_2050, which(ras.NOR_2050[] == 1)))
centroid.NOR_2070 <- colMeans(xyFromCell(ras.NOR_2070, which(ras.NOR_2070[] == 1)))
  
centroid.BCC_2050 <- colMeans(xyFromCell(ras.BCC_2050, which(ras.BCC_2050[] == 1)))
centroid.BCC_2070 <- colMeans(xyFromCell(ras.BCC_2070, which(ras.BCC_2070[] == 1)))
  
# Create one dataframe with centroid information
centroid.MIROC <- as.data.frame(rbind(centroid.pres, centroid.MIROC_2050, centroid.MIROC_2070))
centroid.MIROC$period <- c("Present", "2050", "2070")
centroid.NOR <- as.data.frame(rbind(centroid.pres, centroid.NOR_2050, centroid.NOR_2070))
centroid.NOR$period <- c("Present", "2050", "2070")
centroid.BCC <- as.data.frame(rbind(centroid.pres, centroid.BCC_2050, centroid.BCC_2070))
centroid.BCC$period <- c("Present", "2050", "2070")
  
rm(centroid.pres, 
   centroid.MIROC_2050, centroid.MIROC_2070,
   centroid.NOR_2050, centroid.NOR_2070,
   centroid.BCC_2050, centroid.BCC_2070)  
  
# Table annotations ------------------------------------------------------------
# Average Elevation ------------------------------------------------------------
# PRESENT
# Crop
mcpRaster.pres <- crop(elev, extent(ras.pres)) 
# Mask
mcpRaster.pres <- mask(mcpRaster.pres, ras.pres)
  
# FUTURE
# Crop
mcpRaster.MIROC_2050 <- crop(elev, extent(ras.MIROC_2050))
mcpRaster.MIROC_2070 <- crop(elev, extent(ras.MIROC_2070)) 
# Mask
mcpRaster.MIROC_2050 <- mask(mcpRaster.MIROC_2050, ras.MIROC_2050) 
mcpRaster.MIROC_2070 <- mask(mcpRaster.MIROC_2070, ras.MIROC_2070)
  
# Crop
mcpRaster.NOR_2050 <- crop(elev, extent(ras.NOR_2050)) 
mcpRaster.NOR_2070 <- crop(elev, extent(ras.NOR_2070)) 
# Mask
mcpRaster.NOR_2050 <- mask(mcpRaster.NOR_2050, ras.NOR_2050) 
mcpRaster.NOR_2070 <- mask(mcpRaster.NOR_2070, ras.NOR_2070)
  
# Crop
mcpRaster.BCC_2050 <- crop(elev, extent(ras.BCC_2050)) 
mcpRaster.BCC_2070 <- crop(elev, extent(ras.BCC_2070)) 
# Mask
mcpRaster.BCC_2050 <- mask(mcpRaster.BCC_2050, ras.BCC_2050) 
mcpRaster.BCC_2070 <- mask(mcpRaster.BCC_2070, ras.BCC_2070)
  
# Elevation mean
mean.pres <- cellStats(mcpRaster.pres, stat = "mean")
mean.MIROC_2050 <- cellStats(mcpRaster.MIROC_2050, stat = "mean")
mean.MIROC_2070 <- cellStats(mcpRaster.MIROC_2070, stat = "mean")
mean.NOR_2050 <- cellStats(mcpRaster.NOR_2050, stat = "mean")
mean.NOR_2070 <- cellStats(mcpRaster.NOR_2070, stat = "mean")
mean.BCC_2050 <- cellStats(mcpRaster.BCC_2050, stat = "mean")
mean.BCC_2070 <- cellStats(mcpRaster.BCC_2070, stat = "mean")
  
# Raster cell sum for value = 1
sum.pres <- length(which(ras.pres[] == 1))
sum.MIROC_2050 <- length(which(ras.MIROC_2050[] == 1))
sum.MIROC_2070 <- length(which(ras.MIROC_2070[] == 1))
sum.NOR_2050 <- length(which(ras.NOR_2050[] == 1))
sum.NOR_2070 <- length(which(ras.NOR_2070[] == 1))
sum.BCC_2050 <- length(which(ras.BCC_2050[] == 1))
sum.BCC_2070 <- length(which(ras.BCC_2070[] == 1))
  
# Table preparation
# Resolution of raster <- 1cell ~ 10km
# xres(ras.pres)*111.19

# MIROC
table.MIROC <- centroid.MIROC
table.MIROC$LatDif <- NA 
table.MIROC$LatDif[2] <- table.MIROC$y[2] - table.MIROC$y[1]
table.MIROC$LatDif[3] <- table.MIROC$y[3] - table.MIROC$y[1]
table.MIROC$avgAltitude <- round(c(mean.pres, mean.MIROC_2050, mean.MIROC_2070), digits=0)
table.MIROC$DifavgAltitude <- NA
table.MIROC$DifavgAltitude[2] <- round(mean.MIROC_2050 - mean.pres, digits=0)
table.MIROC$DifavgAltitude[3] <- round(mean.MIROC_2070 - mean.pres, digits=0)
table.MIROC$cellNumber <- c(sum.pres, sum.MIROC_2050, sum.MIROC_2070)
table.MIROC$rel_n_cell <- NA
table.MIROC$rel_n_cell[2] <- table.MIROC$cellNumber[2] / table.MIROC$cellNumber[1]
table.MIROC$rel_n_cell[3] <- table.MIROC$cellNumber[3] / table.MIROC$cellNumber[1]
table.MIROC$dif_cetroid <- NA
table.MIROC$dif_cetroid[2] <- distGeo(c(table.MIROC$x[1], table.MIROC$y[1]), c(table.MIROC$x[2], table.MIROC$y[2]))
table.MIROC$dif_cetroid[3] <- distGeo(c(table.MIROC$x[1], table.MIROC$y[1]), c(table.MIROC$x[3], table.MIROC$y[3]))
# table.MIROC$cellArea <- round(table.MIROC$cellNumber * xres(ras.pres)*111.19, digits=0)
# table.MIROC$cellDifArea <- NA
# table.MIROC$cellDifArea[2] <- table.MIROC$cellArea[2] - table.MIROC$cellArea[1]
# table.MIROC$cellDifArea[3] <- table.MIROC$cellArea[3] - table.MIROC$cellArea[1]
table.MIROC$species <- sp20[i]
colnames(table.MIROC)[1:2] <- c("Lon", "Lat")

# NOR
table.NOR <- centroid.NOR
table.NOR$LatDif <- NA 
table.NOR$LatDif[2] <- table.NOR$y[2] - table.NOR$y[1]
table.NOR$LatDif[3] <- table.NOR$y[3] - table.NOR$y[1]
table.NOR$avgAltitude <- round(c(mean.pres, mean.NOR_2050, mean.NOR_2070), digits=0)
table.NOR$DifavgAltitude <- NA
table.NOR$DifavgAltitude[2] <- round(mean.NOR_2050 - mean.pres, digits=0)
table.NOR$DifavgAltitude[3] <- round(mean.NOR_2070 - mean.pres, digits=0)
table.NOR$cellNumber <- c(sum.pres, sum.NOR_2050, sum.NOR_2070)
table.NOR$rel_n_cell <- NA
table.NOR$rel_n_cell[2] <- table.NOR$cellNumber[2] / table.NOR$cellNumber[1]
table.NOR$rel_n_cell[3] <- table.NOR$cellNumber[3] / table.NOR$cellNumber[1]
table.NOR$dif_cetroid <- NA
table.NOR$dif_cetroid[2] <- distGeo(c(table.NOR$x[1], table.NOR$y[1]), c(table.NOR$x[2], table.NOR$y[2]))
table.NOR$dif_cetroid[3] <- distGeo(c(table.NOR$x[1], table.NOR$y[1]), c(table.NOR$x[3], table.NOR$y[3]))
# table.NOR$cellArea <- round(table.NOR$cellNumber * xres(ras.pres)*111.19, digits=0)
# table.NOR$cellDifArea <- NA
# table.NOR$cellDifArea[2] <- table.NOR$cellArea[2] - table.NOR$cellArea[1]
# table.NOR$cellDifArea[3] <- table.NOR$cellArea[3] - table.NOR$cellArea[1]
table.NOR$species <- sp20[i]
colnames(table.NOR)[1:2] <- c("Lon", "Lat")

# BCC  
table.BCC <- centroid.BCC
table.BCC$LatDif <- NA 
table.BCC$LatDif[2] <- table.BCC$y[2] - table.BCC$y[1]
table.BCC$LatDif[3] <- table.BCC$y[3] - table.BCC$y[1]
table.BCC$avgAltitude <- round(c(mean.pres, mean.BCC_2050, mean.BCC_2070), digits=0)
table.BCC$DifavgAltitude <- NA
table.BCC$DifavgAltitude[2] <- round(mean.BCC_2050 - mean.pres, digits=0)
table.BCC$DifavgAltitude[3] <- round(mean.BCC_2070 - mean.pres, digits=0)
table.BCC$cellNumber <- c(sum.pres, sum.BCC_2050, sum.BCC_2070)
table.BCC$rel_n_cell <- NA
table.BCC$rel_n_cell[2] <- table.BCC$cellNumber[2] / table.BCC$cellNumber[1]
table.BCC$rel_n_cell[3] <- table.BCC$cellNumber[3] / table.BCC$cellNumber[1]
table.BCC$dif_cetroid <- NA
table.BCC$dif_cetroid[2] <- distGeo(c(table.BCC$x[1], table.BCC$y[1]), c(table.BCC$x[2], table.BCC$y[2]))
table.BCC$dif_cetroid[3] <- distGeo(c(table.BCC$x[1], table.BCC$y[1]), c(table.BCC$x[3], table.BCC$y[3]))
# table.BCC$cellArea <- round(table.BCC$cellNumber * xres(ras.pres)*111.19, digits =0)
# table.BCC$cellDifArea <- NA
# table.BCC$cellDifArea[2] <- table.BCC$cellArea[2] - table.BCC$cellArea[1]
# table.BCC$cellDifArea[3] <- table.BCC$cellArea[3] - table.BCC$cellArea[1]
table.BCC$species <- sp20[i]
colnames(table.BCC)[1:2] <- c("Lon", "Lat")  

# rbind tables
table.present <- rbind(table.present, table.MIROC[1,])
table.miroc50 <- rbind(table.miroc50, table.MIROC[2,])
table.miroc70 <- rbind(table.miroc70, table.MIROC[3,])
table.nor50 <- rbind(table.nor50, table.NOR[2,])
table.nor70 <- rbind(table.nor70, table.NOR[3,])
table.bcc50 <- rbind(table.bcc50, table.BCC[2,])
table.bcc70 <- rbind(table.bcc70, table.BCC[3,])

rm(table.MIROC, table.NOR, table.BCC, 
   mean.pres, mean.MIROC_2050, mean.MIROC_2070, 
   mean.NOR_2050, mean.NOR_2070,
   mean.BCC_2050, mean.BCC_2070, 
   sum.pres, sum.MIROC_2050, sum.MIROC_2070, 
   sum.NOR_2050, sum.NOR_2070,
   sum.BCC_2050, sum.BCC_2070)
}

table.present$scenario <- "present"; head(table.present)
table.miroc50$scenario <- "miroc2050"; head(table.miroc50)
table.miroc70$scenario <- "miroc2070"; head(table.miroc70)
table.nor50$scenario <- "nor2050"; head(table.nor50)
table.nor70$scenario <- "nor2070"; head(table.nor70)
table.bcc50$scenario <- "bcc2050"; head(table.bcc50)
table.bcc70$scenario <- "bcc2070"; head(table.bcc70)

total.table <- rbind(table.present,
                     table.miroc50, table.miroc70,
                     table.nor50, table.nor70,
                     table.bcc50, table.bcc70)

write.csv(total.table, "./PGLS_2022_03_01.csv", row.names = FALSE)
