###################
# Raster Richness #
###################

# Load libraries
library(raster) # <- GIS
library(ggplot2) # <-  Plot

# Main path
main.path <- "YOUR_PATH"

# Create folder to save results
dir.create("./Models/Richness")
dir.create("./Models/Richness/Present")
dir.create("./Models/Richness/MIROC_2050")
dir.create("./Models/Richness/MIROC_2070")
dir.create("./Models/Richness/NOR_2050")
dir.create("./Models/Richness/NOR_2070")
dir.create("./Models/Richness/BCC_2050")
dir.create("./Models/Richness/BCC_2070")

# Elevation
elev <- raster("./elevation_5_null.asc")

# All values of elevation have to be 0 
elev[elev] = 0
plot(elev)

# LOAD .asc and csv Threshold -----------------------------------------------------------
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

# Load Max AUC
maxAuc <- read.csv("./01_AUC_>7.csv")

# LOOP
for(i in 1:107){
# Extract model with highest AUC -----------------------------------------------
spPresent <- gsub("_", " ", maxAuc$model[i])

if(grepl("ensamblado", spPresent) == F){
  spPresent <- paste0(stringr::word(spPresent, 1),",",
                 stringr::word(spPresent, 2), "_", stringr::word(spPresent, 3))
  spPresent <- gsub(",", " ", spPresent)
} else {
  spPresent <- paste0(stringr::word(spPresent, 1),",",
          stringr::word(spPresent, 2), "_",stringr::word(spPresent, 3), ".asc")
  spPresent <- gsub(",", "_", spPresent)
}

# Present Threshold
pres.ths <- asc.ths[grepl(maxAuc$model[i], asc.ths)]

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

# To calculate the richness each raster need the same extension. To do 
# that we merge the threshold-raster with elevation.

# Present
pres.mrg <- merge(ras.pres, elev)
names(pres.mrg) <- names(ras.pres)
writeRaster(pres.mrg, filename = paste0("./Models/Richness/Present/", 
            names(ras.pres), ".asc"), 
            format = "ascii", overwrite = FALSE)

# MIROC
ras.MIROC_2050.mrg <- merge(ras.MIROC_2050, elev)
names(ras.MIROC_2050.mrg) <- names(ras.MIROC_2050)
writeRaster(ras.MIROC_2050.mrg, 
            filename = paste0("./Models/Richness/MIROC_2050/", 
            names(ras.MIROC_2050), ".asc"), 
            format = "ascii", overwrite = FALSE)

ras.MIROC_2070.mrg <- merge(ras.MIROC_2070, elev)
names(ras.MIROC_2070.mrg) <- names(ras.MIROC_2070)
writeRaster(ras.MIROC_2070.mrg, 
            filename = paste0("./Models/Richness/MIROC_2070/", 
            names(ras.MIROC_2070), ".asc"), 
            format = "ascii", overwrite = FALSE)

# NOR
ras.NOR_2050.mrg <- merge(ras.NOR_2050, elev)
names(ras.NOR_2050.mrg) <- names(ras.NOR_2050)
writeRaster(ras.NOR_2050.mrg, 
            filename = paste0("./Models/Richness/NOR_2050/", 
            names(ras.NOR_2050), ".asc"), 
            format = "ascii", overwrite = FALSE)

ras.NOR_2070.mrg <- merge(ras.NOR_2070, elev)
names(ras.NOR_2070.mrg) <- names(ras.NOR_2070)
writeRaster(ras.NOR_2070.mrg, 
            filename = paste0("./Models/Richness/NOR_2070/", 
            names(ras.NOR_2070), ".asc"), 
            format = "ascii", overwrite = FALSE)

# BCC
ras.BCC_2050.mrg <- merge(ras.BCC_2050, elev)
names(ras.BCC_2050.mrg) <- names(ras.BCC_2050)
writeRaster(ras.BCC_2050.mrg, 
            filename = paste0("./Models/Richness/BCC_2050/", 
            names(ras.BCC_2050), ".asc"), 
            format = "ascii", overwrite = FALSE)

ras.BCC_2070.mrg <- merge(ras.BCC_2070, elev)
names(ras.BCC_2070.mrg) <- names(ras.BCC_2070)
writeRaster(ras.BCC_2070.mrg, 
            filename = paste0("./Models/Richness/BCC_2070/", 
            names(ras.BCC_2070), ".asc"), 
            format = "ascii", overwrite = FALSE)
}
