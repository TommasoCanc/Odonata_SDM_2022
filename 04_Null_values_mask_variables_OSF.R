##############################
# NULL Values Mask Variables #
##############################

# Load libraries
library(raster) # <- GIS

#########################
# Set working directory #
#########################

main.path <- "YOUR_PATH"
setwd(main.path)


# Load topographyc and climate variables----------------------------------------
# Elevation and water bodies
elev <- raster("./elevation_5_crop.tif")
wb <- raster("./Waterbodies_5m_crop.tif")

#---------#
# PRESENT #
#---------#
# Envirem and bioclimatic variables
enviFiles <- list.files(path = "./Present/envirem_crop/", 
                        pattern = ".tif$", all.files = TRUE, full.names = TRUE)
bioclimFiles <- list.files(path = "./Present/bioVar_crop/", 
                         pattern=".tif$", all.files = TRUE, full.names = TRUE)

#----------------#
# MIROC-ESM-CHEM #
#----------------#
# Envirem and bioclimatic variables 2050
enviFiles50 <- list.files(path = "./MIROC_2050/envirem_crop/", 
                        pattern = ".tif$", all.files = TRUE, full.names = TRUE)
bioclimFiles50 <- list.files(path = "./MIROC_2050/bioVar_crop/",
                           pattern = ".tif$", all.files = TRUE, 
                           full.names = TRUE)

# Envirem and bioclimatic variables 2070
enviFiles70 <- list.files(path = "./MIROC_2070/envirem_crop/", 
                          pattern = ".tif$", all.files = TRUE, full.names = TRUE)
bioclimFiles70 <- list.files(path = "./MIROC_2070/bioVar_crop/", 
                             pattern = ".tif$", all.files = TRUE, 
                             full.names = TRUE)

# Stack ------------------------------------------------------------------------
enviStack <- stack(enviFiles)
bioclimStack <- stack(bioclimFiles)

enviStack50 <- stack(enviFiles50)
bioclimStack50 <- stack(bioclimFiles50)

enviStack70 <- stack(enviFiles70)
bioclimStack70 <- stack(bioclimFiles70)

# Remove unnecessary files
rm(enviFiles, bioclimFiles, enviFiles50, bioclimFiles50, enviFiles70, bioclimFiles70)

# Check extention
extent(elev.5) == extent(enviStack)
extent(elev.5) == extent(bioclimStack)
extent(elev.5) == extent(wb.5)
extent(elev.5) == extent(enviStack50)
extent(elev.5) == extent(bioclimStack50)
extent(elev.5) == extent(enviStack70)
extent(elev.5) == extent(bioclimStack70)

# Check resolution
xres(elev.5) == xres(enviStack)
xres(elev.5) == xres(bioclimStack)
xres(elev.5) == xres(wb.5)
xres(elev.5) == xres(enviStack50)
xres(elev.5) == xres(bioclimStack50)
xres(elev.5) == xres(enviStack70)
xres(elev.5) == xres(bioclimStack70)

# Preparation of null cells map to mask our variables
null.values <- enviStack[[1]]*bioclimStack[[1]]*elev.5*wb.5*enviStack50[[1]]*
               bioclimStack50[[1]]*enviStack70[[1]]*bioclimStack70[[1]] 

plot(null.values) 

# Mask the variable with null cell raster --------------------------------------

# Elevation and water bidies ---------------------------------------------------
elev.null <- mask(elev.5, null.values); plot(elev.null)
wb.null <- mask(wb.5, null.values); plot(wb.null)

# Save elevation and water body null mask
writeRaster(elev.null, filename="./elevation_5_null.tif", 
            format = "ascii", overwrite = FALSE)
writeRaster(wb.null, filename="./Waterbodies_5_null.tif", 
            format = "ascii", overwrite = FALSE)

# Present ----------------------------------------------------------------------
envi.null <- mask(enviStack, null.values); plot(envi.null[[1]])
bioclim.null <- mask(bioclimStack, null.values); plot(bioclim.null[[1]])

# Create folder to save the raster
dir.create("./Present/envirem_null")
dir.create("./Present/bioVar_null")

# Save Envirem and biovariables Present
for(i in 1:length(names(envi.null))){
  writeRaster(envi.null[[i]], filename=paste0("./Present/envirem_null", 
              names(envi.null)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:length(names(bioclim.null))){
  writeRaster(bioclim.null[[i]], filename=paste0("./Present/bioVar_null/", 
              names(bioclim.null)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)


# MIROC ------------------------------------------------------------------------
envi.null.50 <- mask(enviStack50, null.values); plot(envi.null.50[[1]])
bioclim.null.50 <- mask(bioclimStack50, null.values); plot(bioclim.null.50[[1]])

envi.null.70 <- mask(enviStack70, null.values); plot(envi.null.70[[1]])
bioclim.null.70 <- mask(bioclimStack70, null.values); plot(bioclim.null.70[[1]])

# Create folder to save the raster
dir.create("./MIROC_2050/envirem_null")
dir.create("./MIROC_2050/bioVar_null")
dir.create("./MIROC_2070/envirem_null")
dir.create("./MIROC_2070/bioVar_null")

# Save Envirem and biovariables 2050
for(i in 1:length(names(envi.null))){
  writeRaster(envi.null.50[[i]], filename = paste0("./MIROC_2050/envirem_null/", 
              names(envi.null)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:length(names(bioclim.null))){
  writeRaster(bioclim.null.50[[i]], filename=paste0("./MIROC_2050/bioVar_null/", 
              names(bioclim.null)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

# Save Envirem and biovariables 2070
for(i in 1:length(names(envi.null))){
  writeRaster(envi.null.70[[i]], filename=paste0("./MIROC_2070/envirem_null/", 
              names(envi.null)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:length(names(bioclim.null))){
  writeRaster(bioclim.null.70[[i]], filename=paste0("./MIROC_2070/bioVar_null/", 
              names(bioclim.null)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)


#-----------#
# NorESM1-M #
#-----------# 
# Envirem and bioclimatic variables 2050
enviFiles50 <- list.files(path = "./NOR_2050/envirem_crop/", 
                          pattern = ".tif$", all.files = TRUE, 
                          full.names = TRUE)
bioclimFiles50 <- list.files(path = "./NOR_2050/bioVar_crop/",
                             pattern = ".tif$", all.files = TRUE, 
                             full.names = TRUE)

# Envirem and bioclimatic variables 2070
enviFiles70 <- list.files(path = "./NOR_2070/envirem_crop/", 
                          pattern = ".tif$", all.files = TRUE, 
                          full.names = TRUE)
bioclimFiles70 <- list.files(path = "./NOR_2070/bioVar_crop/", 
                             pattern = ".tif$", all.files = TRUE, 
                             full.names = TRUE)

# Stack ------------------------------------------------------------------------
enviStack50 <- stack(enviFiles50)
bioclimStack50 <- stack(bioclimFiles50)
enviStack70 <- stack(enviFiles70)
bioclimStack70 <- stack(bioclimFiles70)

rm(enviFiles50, bioclimFiles50, enviFiles70, bioclimFiles70)

# Check extention
extent(elev.5) == extent(enviStack50)
extent(elev.5) == extent(bioclimStack50)
extent(elev.5) == extent(enviStack70)
extent(elev.5) == extent(bioclimStack70)
# Check resolution
xres(elev.5) == xres(enviStack50)
xres(elev.5) == xres(bioclimStack50)
xres(elev.5) == xres(enviStack70)
xres(elev.5) == xres(bioclimStack70)

# Mask the variable with null cell raster --------------------------------------
envi.null.50 <- mask(enviStack50, null.values); plot(envi.null.50[[1]])
bioclim.null.50 <- mask(bioclimStack50, null.values); plot(bioclim.null.50[[1]])

envi.null.70 <- mask(enviStack70, null.values); plot(envi.null.70[[1]])
bioclim.null.70 <- mask(bioclimStack70, null.values); plot(bioclim.null.70[[1]])

# Create folder to save the raster
dir.create("./NOR_2050/envirem_null")
dir.create("./NOR_2050/bioVar_null")
dir.create("./NOR_2070/envirem_null")
dir.create("./NOR_2070/bioVar_null")

# Save Envirem and biovariables 2050
for(i in 1:length(names(envi.null.50))){
  writeRaster(envi.null.50[[i]], filename = paste0("./NOR_2050/envirem_null/", 
              names(envi.null.50)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)
for(i in 1:length(names(bioclim.null.50))){
  writeRaster(bioclim.null.50[[i]], filename = paste0("./NOR_2070/bioVar_null/", 
              names(bioclim.null.50)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

# Save Envirem and biovariables 2070
for(i in 1:length(names(envi.null.70))){
  writeRaster(envi.null.70[[i]], filename = paste0("./NOR_2070/envirem_null/", 
              names(envi.null.70)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)
for(i in 1:length(names(bioclim.null.70))){
  writeRaster(bioclim.null.70[[i]], filename = paste0("./NOR_2070/bioVar_null/", 
              names(bioclim.null.70)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)


#------------#
# BCC-CSM1-1 #
#------------# 
# Envirem and bioclimatic variables 2050
enviFiles50 <- list.files(path = "./BCC_2050/envirem_crop/", 
                          pattern = ".tif$", all.files = TRUE, 
                          full.names = TRUE)
bioclimFiles50 <- list.files(path = "./BCC_2050/bioVar_crop/",
                             pattern = ".tif$", all.files = TRUE, 
                             full.names = TRUE)

# Envirem and bioclimatic variables 2070
enviFiles70 <- list.files(path = "./BCC_2070/envirem_crop/", 
                          pattern = ".tif$", all.files = TRUE, 
                          full.names = TRUE)
bioclimFiles70 <- list.files(path = "./BCC_2070/bioVar_crop/", 
                             pattern = ".tif$", all.files = TRUE, 
                             full.names = TRUE)

# Stack ------------------------------------------------------------------------
enviStack50 <- stack(enviFiles50)
bioclimStack50 <- stack(bioclimFiles50)
enviStack70 <- stack(enviFiles70)
bioclimStack70 <- stack(bioclimFiles70)

rm(enviFiles50, bioclimFiles50, enviFiles70, bioclimFiles70)

# Check extention
extent(elev.5) == extent(enviStack50)
extent(elev.5) == extent(bioclimStack50)
extent(elev.5) == extent(enviStack70)
extent(elev.5) == extent(bioclimStack70)
# Check resolution
xres(elev.5) == xres(enviStack50)
xres(elev.5) == xres(bioclimStack50)
xres(elev.5) == xres(enviStack70)
xres(elev.5) == xres(bioclimStack70)

# Mask the variable with null cell raster --------------------------------------
envi.null.50 <- mask(enviStack50, null.values); plot(envi.null.50[[1]])
bioclim.null.50 <- mask(bioclimStack50, null.values); plot(bioclim.null.50[[1]])

envi.null.70 <- mask(enviStack70, null.values); plot(envi.null.70[[1]])
bioclim.null.70 <- mask(bioclimStack70, null.values); plot(bioclim.null.70[[1]])

# Create folder to save the raster
dir.create("./BCC_2050/envirem_null")
dir.create("./BCC_2050/bioVar_null")
dir.create("./BCC_2070/envirem_null")
dir.create("./BCC_2070/bioVar_null")

# Save Envirem and biovariables 2050
for(i in 1:length(names(envi.null.50))){
  writeRaster(envi.null.50[[i]], filename = paste0("./BCC_2050/envirem_null/", 
              names(envi.null.50)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)
for(i in 1:length(names(bioclim.null.50))){
  writeRaster(bioclim.null.50[[i]], filename = paste0("./BCC_2050/bioVar_null/", 
              names(bioclim.null.50)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

# Save Envirem and biovariables 2070
for(i in 1:length(names(envi.null.70))){
  writeRaster(envi.null.70[[i]], filename = paste0("./BCC_2070/envirem_null/", 
              names(envi.null.70)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)
for(i in 1:length(names(bioclim.null.70))){
  writeRaster(bioclim.null.70[[i]], filename = paste0("./BCC_2070/bioVar_null/", names(bioclim.null.70)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)
