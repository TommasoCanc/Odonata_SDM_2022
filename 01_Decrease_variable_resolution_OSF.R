#################################
# Decrease resolution variables #
#################################

# Load libraries
library(sf) # <- GIS
library(rgdal) # <- GIS
library(raster) # <- GIS
library(osfr) # <- Connect to OSF

#####################
# Connection to OSF #
#####################

main.path <- "/Users/tommasocancellario/Desktop/prova/"
# Create folder where you can download the files. 
# For more information: ?dir.create
dir.create(main.path)
setwd(main.path)

# Connect to Open Science Framework
cr_project <- osf_retrieve_node("4rjuc")
osf_files <- osf_ls_files(cr_project, type = "folder")

#---------------------#
# WorldClim variables # 
#---------------------#

# WorldClim web page: https://www.worldclim.org/data/v1.4/cmip5_5m.html

# Download main variables
file.number <- which(osf_ls_files(osf_files[1, ])[ ,1] == "Present.zip")
osf_download(osf_ls_files(osf_files[1, ])[file.number, ], path = main.path)
rm(file.number)

# Unzip file
unzip("./Present.zip", exdir=main.path); file.remove("./Present.zip")

# Climate Variable PRESENT -----------------------------------------------------
t_min <- list.files(path = "./Present/tMin/", pattern = '.tif$', 
                    all.files = TRUE, full.names = TRUE)

t_max <- list.files(path = "./Present/tMax/", pattern = '.tif$', 
                    all.files = TRUE, full.names = TRUE)

prec <- list.files(path = "./Present/Prec/", pattern = '.tif$', 
                   all.files = TRUE, full.names = TRUE)

# Download and load Elevation
file.number <- which(osf_ls_files(osf_files[1, ])[ ,1] == "wc2.1_5m_elev.tif")
osf_download(osf_ls_files(osf_files[1, ])[file.number, ], path = main.path) 
rm(file.number)

elev.5 <- raster("./wc2.1_5m_elev.tif")
xres(elev.5)*111.19 # <- Check resolution in kilometers

# Stack climate variables
t_min.stack <- stack(t_min)
t_max.stack <- stack(t_max)
prec.stack <- stack(prec)

par(mfrow=c(1,3), mar=c(1,1,1,1), oma=c(3,3,4,3))
plot(t_min.stack[[1]])
plot(t_max.stack[[1]])
plot(prec.stack[[1]])
dev.off()

# Remove unnecessary files
rm(t_min, t_max, prec)

# Check variable extention
extent(t_min.stack) == extent(elev.5)
extent(t_max.stack) == extent(elev.5)
extent(prec.stack) == extent(elev.5)

# Check variable resolution
xres(t_min.stack) == xres(elev.5)
xres(t_max.stack) == xres(elev.5)
xres(prec.stack) == xres(elev.5)


# Crop climate variables to EU extent ------------------------------------------
# Download Europe shapefile
file.number <- which(osf_ls_files(osf_files[1, ])[ ,1] == "World_Countries.zip")
osf_download(osf_ls_files(osf_files[1, ])[file.number, ], 
             path = main.path); rm(file.number)

# Unzip files
unzip("./World_Countries.zip", exdir=main.path) 
file.remove("./World_Countries.zip")

# Load shp and extract study regions
world <- st_read("./World_Countries/World_Countries.shp")
countries <- c("Albania", "Austria", "Belarus", "Belgium", 
               "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", 
               "Czech Republic", "Denmark", "Estonia", "Finland", "France",
               "Germany", "United Kingdom", "Greece", "Hungary", "Ireland", 
               "Italy", "Latvia", "Lithuania", "Luxembourg", "Macedonia", 
               "Malta", "Moldova", "Montenegro", "Netherlands", "Norway", 
               "Poland", "Portugal", "Romania", "Serbia", "Slovakia", 
               "Slovenia","Spain", "Sweden", "Switzerland", "Turkey", "Ukraine")

eu <- world[world$COUNTRY %in% countries, ]
plot(eu); rm(world, countries)

# Crop the variable with EU extension
elev.5.sub <- crop(elev.5, extent(eu))
# Mask
elev.5.sub <- mask(elev.5.sub, eu)
plot(elev.5.sub)
gc(); rm(elev.5)

# T Min ------------------------------------------------------------------------
# Crop the variable with EU extension
t_min.sub <- crop(t_min.stack, extent(eu))
# Mask
t_min.sub <- mask(t_min.sub, eu)
plot(t_min.sub[[1]])
gc(); rm(t_min.stack)

# T Max ------------------------------------------------------------------------
# Crop the variable with EU extension
t_max.sub <- crop(t_max.stack, extent(eu))
# Mask
t_max.sub <- mask(t_max.sub, eu)
plot(t_max.sub[[1]])
gc(); rm(t_max.stack)

# Prec -------------------------------------------------------------------------
# Crop the variable with EU extension
prec.sub <- crop(prec.stack, extent(eu))
# Mask
prec.sub <- mask(prec.sub, eu)
plot(prec.sub[[1]])
gc(); rm(prec.stack)

# Check extention
extent(t_min.sub) == extent(elev.5.sub)
extent(t_max.sub) == extent(elev.5.sub)
extent(prec.sub) == extent(elev.5.sub)

# Check resolution
xres(t_min.sub) == xres(elev.5.sub)
xres(t_max.sub) == xres(elev.5.sub)
xres(prec.sub) == xres(elev.5.sub)

# Bio-variables Creation
bioVar <- dismo::biovars(prec = prec.sub, tmin = t_min.sub, tmax = t_max.sub)
plot(bioVar[[1]])
extent(bioVar) == extent(elev.5.sub)
xres(bioVar) == xres(elev.5.sub)

# Create folder to save the raster
dir.create("./Present/tMin_crop")
dir.create("./Present/tMax_crop")
dir.create("./Present/Prec_crop")
dir.create("./Present/bioVar_crop")

# Save rasters
for(i in 1:12){
  writeRaster(t_min.sub[[i]], filename = paste0(getwd(), "/Present/tMin_crop/", 
              names(t_min.sub)[i], ".tif"), format = "GTiff", overwrite = FALSE)
}; rm(i)

for(i in 1:12){
  writeRaster(t_max.sub[[i]], filename = paste0(getwd(), "/Present/tMax_crop/", 
              names(t_max.sub)[i], ".tif"), format = "GTiff", overwrite = FALSE)
}; rm(i)

for(i in 1:12){
  writeRaster(prec.sub[[i]], filename = paste0(getwd(), "/Present/Prec_crop/", 
              names(prec.sub)[i], ".tif"), format = "GTiff", overwrite = FALSE)
}; rm(i)

for(i in 1:19){
  writeRaster(bioVar[[i]], filename = paste0(getwd(), "/Present/bioVar_crop/", 
              names(bioVar)[i], ".tif"), format = "GTiff", overwrite = FALSE)
}; rm(i)

writeRaster(elev.5.sub, filename = paste0(getwd(), "/elevation_5_crop.tif"), 
            format="ascii", overwrite=TRUE)

# Clean workspace
rm(list=setdiff(ls(), c("osf_files", "cr_project", "main.path", 
                        "eu", "elev.5")))

##################
# MIROC-ESM-CHEM #
##################

#------------#
# MIROC 2050 #
#------------#
# Download climate variables MIROC 2050
file.number <- which(osf_ls_files(osf_files[1, ])[ ,1] == "MIROC_2050.zip")
osf_download(osf_ls_files(osf_files[1, ])[file.number, ], 
             path = main.path); rm(file.number)

# Unzip files
unzip("./MIROC_2050.zip", exdir=main.path); file.remove("./MIROC_2050.zip")

# Climate variable FUTURE 2050 -------------------------------------------------
t_min <- list.files(path = "./MIROC_2050/mi45tn50/", 
                    pattern = '.tif$', all.files = TRUE, full.names = TRUE)

t_max <- list.files(path = "./MIROC_2050/mi45tx50/", 
                    pattern = '.tif$', all.files = TRUE, full.names = TRUE)

prec <- list.files(path = "./MIROC_2050/mi45pr50/", 
                   pattern = '.tif$', all.files = TRUE, full.names = TRUE)

# Stack
t_min.stack <- stack(t_min)
t_max.stack <- stack(t_max)
prec.stack <- stack(prec)

par(mfrow=c(1,3), mar=c(1,1,1,1), oma=c(3,3,4,3))
plot(t_min.stack[[1]])
plot(t_max.stack[[1]])
plot(prec.stack[[1]])
dev.off()

# Remove unnecessary files
rm(t_min, t_max, prec)

# Check extention
extent(t_min.stack) == extent(elev.5)
extent(t_max.stack) == extent(elev.5)
extent(prec.stack) == extent(elev.5)

# Check resolution
xres(t_min.stack) == xres(elev.5)
xres(t_max.stack) == xres(elev.5)
xres(prec.stack) == xres(elev.5)


# Crop Variables to EU extent --------------------------------------------------
# T Min ------------------------------------------------------------------------
# Crop the variable with EU extension
t_min.sub <- crop(t_min.stack, extent(eu))
# Mask
t_min.sub <- mask(t_min.sub, eu)
plot(t_min.sub[[1]])
gc(); rm(t_min.stack)

# T Max ------------------------------------------------------------------------
# Crop the variable with EU extension
t_max.sub <- crop(t_max.stack, extent(eu))
# Mask
t_max.sub <- mask(t_max.sub, eu)
plot(t_max.sub[[1]])
gc(); rm(t_max.stack)

# Prec -------------------------------------------------------------------------
# Crop the variable with EU extension
prec.sub <- crop(prec.stack, extent(eu))
# Mask
prec.sub <- mask(prec.sub, eu)
plot(prec.sub[[1]])
gc(); rm(prec.stack)

# Check extention
elev.5.sub <- raster("./elevation_5_crop.tif")
extent(t_min.sub) == extent(elev.5.sub)
extent(t_max.sub) == extent(elev.5.sub)
extent(prec.sub) == extent(elev.5.sub)

# Check resolution
xres(t_min.sub) == xres(elev.5.sub)
xres(t_max.sub) == xres(elev.5.sub)
xres(prec.sub) == xres(elev.5.sub)

# Bio-variables Creation - we have to divide the temperature by 10
t_min.sub <- t_min.sub/10
t_max.sub <- t_max.sub/10

par(mfrow=c(1,2), mar=c(1,1,1,1), oma=c(3,3,4,3))
plot(t_min.sub[[1]])
plot(t_max.sub[[1]])
dev.off()

# Bio-variables Creation
bioVar <- dismo::biovars(prec = prec.sub, tmin = t_min.sub, tmax = t_max.sub)
plot(bioVar[[1]])
extent(bioVar) == extent(elev.5.sub)
xres(bioVar) == xres(elev.5.sub)

# Create folder to save the raster
dir.create("./MIROC_2050/tMin_crop")
dir.create("./MIROC_2050/tMax_crop")
dir.create("./MIROC_2050/Prec_crop")
dir.create("./MIROC_2050/bioVar_crop")

# Save rasters
for(i in 1:12){
  writeRaster(t_min.sub[[i]], filename = paste0(getwd(), 
              "/MIROC_2050/tMin_crop/", names(t_min.sub)[i], ".tif"), 
              format = "GTiff", overwriteFALSE= FALSE)}; rm(i)

for(i in 1:12){
  writeRaster(t_max.sub[[i]], filename = paste0(getwd(), 
              "/MIROC_2050/tMax_crop/", names(t_max.sub)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:12){
  writeRaster(prec.sub[[i]], filename = paste0(getwd(), 
              "/MIROC_2050/Prec_crop/", names(prec.sub)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:19){
  writeRaster(bioVar[[i]], filename = paste0(getwd(), 
              "/MIROC_2050/bioVar_crop/", names(bioVar)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

# Clean workspace
rm(list=setdiff(ls(), c("osf_files", "cr_project", "main.path", "eu", 
                        "elev.5", "elev.5.sub")))

#------------#
# MIROC 2070 #
#------------#
# Download climate variables MIROC 2070
file.number <- which(osf_ls_files(osf_files[1, ])[ ,1] == "MIROC_2070.zip")
osf_download(osf_ls_files(osf_files[1, ])[file.number, ], 
             path = main.path); rm(file.number)

# Unzip files
unzip("./MIROC_2070.zip", exdir=main.path); file.remove("./MIROC_2070.zip")

# Climate variable FUTURE 2070 -------------------------------------------------
t_min <- list.files(path = "./MIROC_2070/mi45tn70/", 
                    pattern = '.tif$', all.files = TRUE, full.names = TRUE)

t_max <- list.files(path = "./MIROC_2070/mi45tx70/", 
                    pattern = '.tif$', all.files = TRUE, full.names = TRUE)

prec <- list.files(path = "./MIROC_2070/mi45pr70/", 
                   pattern = '.tif$', all.files = TRUE, full.names = TRUE)

t_min.stack <- stack(t_min)
t_max.stack <- stack(t_max)
prec.stack <- stack(prec)

par(mfrow=c(1,3), mar=c(1,1,1,1), oma=c(3,3,4,3))
plot(t_min.stack[[1]])
plot(t_max.stack[[1]])
plot(prec.stack[[1]])
dev.off()

# Remove unnecessary files
rm(t_min, t_max, prec)

# Check extention
extent(t_min.stack) == extent(elev.5)
extent(t_max.stack) == extent(elev.5)
extent(prec.stack) == extent(elev.5)

# Check resolution
xres(t_min.stack) == xres(elev.5)
xres(t_max.stack) == xres(elev.5)
xres(prec.stack) == xres(elev.5)

# Crop Variables to EU extent --------------------------------------------------
# T Min ------------------------------------------------------------------------
# Crop the variable with EU extension
t_min.sub <- crop(t_min.stack, extent(eu))
# Mask
t_min.sub <- mask(t_min.sub, eu)
plot(t_min.sub[[1]])
gc(); rm(t_min.stack)

# T Max ------------------------------------------------------------------------
# Crop the variable with EU extension
t_max.sub <- crop(t_max.stack, extent(eu))
# Mask
t_max.sub <- mask(t_max.sub, eu)
plot(t_max.sub[[1]])
gc(); rm(t_max.stack)

# Prec -------------------------------------------------------------------------
# Crop the variable with EU extension
prec.sub <- crop(prec.stack, extent(eu))
# Mask
prec.sub <- mask(prec.sub, eu)
plot(prec.sub[[1]])
gc(); rm(prec.stack)

# Check extention
extent(t_min.sub) == extent(elev.5.sub)
extent(t_max.sub) == extent(elev.5.sub)
extent(prec.sub) == extent(elev.5.sub)

# Check resolution
xres(t_min.sub) == xres(elev.5.sub)
xres(t_max.sub) == xres(elev.5.sub)
xres(prec.sub) == xres(elev.5.sub)

# Bio-variables Creation - we need to divide the temperature by 10
t_min.sub <- t_min.sub/10
t_max.sub <- t_max.sub/10
plot(t_min.sub[[1]])
plot(t_max.sub[[1]])

# Bio-variables Creation
bioVar <- dismo::biovars(prec = prec.sub, tmin = t_min.sub, tmax = t_max.sub)
plot(bioVar[[1]])
extent(bioVar) == extent(elev.5.sub)
xres(bioVar) == xres(elev.5.sub)

# Create folder to save the raster
dir.create("./MIROC_2070/tMin_crop")
dir.create("./MIROC_2070/tMax_crop")
dir.create("./MIROC_2070/Prec_crop")
dir.create("./MIROC_2070/bioVar_crop")

# Save rasters
for(i in 1:12){
  writeRaster(t_min.sub[[i]], filename = paste0(getwd(), 
              "/MIROC_2070/tMin_crop/", names(t_min.sub)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:12){
  writeRaster(t_max.sub[[i]], filename = paste0(getwd(),
              "/MIROC_2070/tMax_crop/", names(t_max.sub)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:12){
  writeRaster(prec.sub[[i]], filename=paste0(getwd(), 
              "/MIROC_2070/Prec_crop/", names(prec.sub)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:19){
  writeRaster(bioVar[[i]], filename=paste0(getwd(), 
              "/MIROC_2070/bioVar_crop/", names(bioVar)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

# Clean workspace
rm(list=setdiff(ls(), c("osf_files", "cr_project", "main.path", "eu", 
                        "elev.5", "elev.5.sub")))


#############
# NorESM1-M #
#############

#----------#
# NOR 2050 #
#----------#

# Download climate variables NOR 2050
file.number <- which(osf_ls_files(osf_files[1, ])[ ,1] == "NOR_2050.zip")
osf_download(osf_ls_files(osf_files[1, ])[file.number, ], 
             path = main.path); rm(file.number)

# Unzip files
unzip("./NOR_2050.zip", exdir=main.path); file.remove("./NOR_2050.zip")

# Climate variable FUTURE 2050 -------------------------------------------------
t_min <- list.files(path = "./NOR_2050/no45tn50/", 
                    pattern ='.tif$', all.files = TRUE, full.names = TRUE)

t_max <- list.files(path = "./NOR_2050/no45tx50/", 
                    pattern = '.tif$', all.files = TRUE, full.names = TRUE)

prec <- list.files(path = "./NOR_2050/no45pr50/", 
                   pattern = '.tif$', all.files = TRUE, full.names = TRUE)

t_min.stack <- stack(t_min)
t_max.stack <- stack(t_max)
prec.stack <- stack(prec)


par(mfrow=c(1,3), mar=c(1,1,1,1), oma=c(3,3,4,3))
plot(t_min.stack[[1]])
plot(t_max.stack[[1]])
plot(prec.stack[[1]])
dev.off()

rm(t_min, t_max, prec)

# Check extention
extent(t_min.stack) == extent(elev.5)
extent(t_max.stack) == extent(elev.5)
extent(prec.stack) == extent(elev.5)

# Check resolution
xres(t_min.stack) == xres(elev.5)
xres(t_max.stack) == xres(elev.5)
xres(prec.stack) == xres(elev.5)

# Crop Variables to EU extent --------------------------------------------------
# T Min ------------------------------------------------------------------------
# Crop the variable with EU extension
t_min.sub <- crop(t_min.stack, extent(eu))
# Mask
t_min.sub <- mask(t_min.sub, eu)
plot(t_min.sub[[1]])
gc(); rm(t_min.stack)

# T Max ------------------------------------------------------------------------
# Crop the variable with EU extension
t_max.sub <- crop(t_max.stack, extent(eu))
# Mask
t_max.sub <- mask(t_max.sub, eu)
plot(t_max.sub[[1]])
gc(); rm(t_max.stack)

# Prec -------------------------------------------------------------------------
# Crop the variable with EU extension
prec.sub <- crop(prec.stack, extent(eu))
# Mask
prec.sub <- mask(prec.sub, eu)
plot(prec.sub[[1]])
gc(); rm(prec.stack)

# Check extention
extent(t_min.sub) == extent(elev.5.sub)
extent(t_max.sub) == extent(elev.5.sub)
extent(prec.sub) == extent(elev.5.sub)

# Check resolution
xres(t_min.sub) == xres(elev.5.sub)
xres(t_max.sub) == xres(elev.5.sub)
xres(prec.sub) == xres(elev.5.sub)

# Biovariables Creation - we have to divide the temperature by 10
t_min.sub <- t_min.sub/10
t_max.sub <- t_max.sub/10

par(mfrow=c(1,2), mar=c(1,1,1,1), oma=c(3,3,4,3))
plot(t_min.sub[[1]])
plot(t_max.sub[[1]])
dev.off()

# Bio-variables Creation
bioVar <- dismo::biovars(prec = prec.sub, tmin = t_min.sub, tmax = t_max.sub)
plot(bioVar[[1]])
extent(bioVar) == extent(elev.5.sub)
xres(bioVar) == xres(elev.5.sub)

# Create folder to save the raster
dir.create("./NOR_2050/tMin_crop")
dir.create("./NOR_2050/tMax_crop")
dir.create("./NOR_2050/Prec_crop")
dir.create("./NOR_2050/bioVar_crop")

# Save rasters
for(i in 1:12){
  writeRaster(t_min.sub[[i]], filename = paste0(getwd(), 
              "/NOR_2050/tMin_crop/", names(t_min.sub)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:12){
  writeRaster(t_max.sub[[i]], filename = paste0(getwd(),
              "/NOR_2050/tMax_crop/", names(t_max.sub)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:12){
  writeRaster(prec.sub[[i]], filename = paste0(getwd(), 
              "/NOR_2050/Prec_crop/", names(prec.sub)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:19){
  writeRaster(bioVar[[i]], filename = paste0(getwd(), 
              "/NOR_2050/bioVar_crop/", names(bioVar)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

# Clean workspace
rm(list=setdiff(ls(), c("osf_files", "cr_project", "main.path", "eu", 
                        "elev.5", "elev.5.sub")))

#----------#
# NOR 2070 #
#----------#

# Download climate variables NOR 2070
file.number <- which(osf_ls_files(osf_files[1, ])[ ,1] == "NOR_2070.zip")
osf_download(osf_ls_files(osf_files[1, ])[file.number, ], 
             path = main.path); rm(file.number)

# Unzip files
unzip("./NOR_2070.zip", exdir=main.path); file.remove("./NOR_2070.zip")

# Climate variable FUTURE 2070 -------------------------------------------------
t_min <- list.files(path = "./NOR_2070/no45tn70/", 
                    pattern = '.tif$', all.files = TRUE, full.names = TRUE)

t_max <- list.files(path = "./NOR_2070/no45tx70/", 
                    pattern = '.tif$', all.files =TRUE, full.names = TRUE)

prec <- list.files(path = "./NOR_2070/no45pr70/", 
                   pattern = '.tif$', all.files = TRUE, full.names = TRUE)

t_min.stack <- stack(t_min)
t_max.stack <- stack(t_max)
prec.stack <- stack(prec)

par(mfrow=c(1,3), mar=c(1,1,1,1), oma=c(3,3,4,3))
plot(t_min.stack[[1]])
plot(t_max.stack[[1]])
plot(prec.stack[[1]])
dev.off()

rm(t_min, t_max, prec)

# Check extention
extent(t_min.stack) == extent(elev.5)
extent(t_max.stack) == extent(elev.5)
extent(prec.stack) == extent(elev.5)

# Check resolution
xres(t_min.stack) == xres(elev.5)
xres(t_max.stack) == xres(elev.5)
xres(prec.stack) == xres(elev.5)

# Crop Variables to EU extent --------------------------------------------------
# T Min ------------------------------------------------------------------------
# Crop the variable with EU extension
t_min.sub <- crop(t_min.stack, extent(eu))
# Mask
t_min.sub <- mask(t_min.sub, eu)
plot(t_min.sub[[1]])
gc(); rm(t_min.stack)

# T Max ------------------------------------------------------------------------
# Crop the variable with EU extension
t_max.sub <- crop(t_max.stack, extent(eu))
# Mask
t_max.sub <- mask(t_max.sub, eu)
plot(t_max.sub[[1]])
gc(); rm(t_max.stack)

# Prec -------------------------------------------------------------------------
# Crop the variable with EU extension
prec.sub <- crop(prec.stack, extent(eu))
# Mask
prec.sub <- mask(prec.sub, eu)
plot(prec.sub[[1]])
gc(); rm(prec.stack)


# Check extention
extent(t_min.sub) == extent(elev.5.sub)
extent(t_max.sub) == extent(elev.5.sub)
extent(prec.sub) == extent(elev.5.sub)

# Check resolution
xres(t_min.sub) == xres(elev.5.sub)
xres(t_max.sub) == xres(elev.5.sub)
xres(prec.sub) == xres(elev.5.sub)

# Biovariables Creation - we have to divide the temperature by 10
t_min.sub <- t_min.sub/10
t_max.sub <- t_max.sub/10

par(mfrow=c(1,2), mar=c(1,1,1,1), oma=c(3,3,4,3))
plot(t_min.sub[[1]])
plot(t_max.sub[[1]])
dev.off()

# Bio-variables Creation
bioVar <- dismo::biovars(prec = prec.sub, tmin = t_min.sub, tmax = t_max.sub)
plot(bioVar[[1]])
extent(bioVar) == extent(elev.5.sub)
xres(bioVar) == xres(elev.5.sub)

# Create folder to save the raster
dir.create("./NOR_2070/tMin_crop")
dir.create("./NOR_2070/tMax_crop")
dir.create("./NOR_2070/Prec_crop")
dir.create("./NOR_2070/bioVar_crop")

# Save rasters
for(i in 1:12){
  writeRaster(t_min.sub[[i]], filename = paste0(getwd(),
              "/NOR_2070/tMin_crop/", names(t_min.sub)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:12){
  writeRaster(t_max.sub[[i]], filename = paste0(getwd(),
              "/NOR_2070/tMax_crop/", names(t_max.sub)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:12){
  writeRaster(prec.sub[[i]], filename = paste0(getwd(),
              "/NOR_2070/Prec_crop/", names(prec.sub)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:19){
  writeRaster(bioVar[[i]], filename = paste0(getwd(),
              "/NOR_2070/bioVar_crop/", names(bioVar)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)


##############
# BCC-CSM1-1 #
##############

#----------#
# BCC 2050 #
#----------#
# Download climate variables MIROC 2050
file.number <- which(osf_ls_files(osf_files[1, ])[ ,1] == "BCC_2050.zip")
osf_download(osf_ls_files(osf_files[1, ])[file.number, ], 
             path = main.path); rm(file.number)

# Unzip files
unzip("./BCC_2050.zip", exdir=main.path); file.remove("./BCC_2050.zip")


# Climate variable FUTURE 2050 -------------------------------------------------

t_min <- list.files(path = "./BCC_2050/bc45tn50/", 
                    pattern = '.tif$', all.files = TRUE, full.names = TRUE)

t_max <- list.files(path = "./BCC_2050/bc45tx50/", 
                    pattern = '.tif$', all.files = TRUE, full.names = TRUE)

prec <- list.files(path = "./BCC_2050/bc45pr50/", 
                   pattern = '.tif$', all.files = TRUE, full.names = TRUE)

t_min.stack <- stack(t_min)
t_max.stack <- stack(t_max)
prec.stack <- stack(prec)

par(mfrow=c(1,3), mar=c(1,1,1,1), oma=c(3,3,4,3))
plot(t_min.stack[[1]])
plot(t_max.stack[[1]])
plot(prec.stack[[1]])
dev.off()

rm(t_min, t_max, prec)

# Check extention
extent(t_min.stack) == extent(elev.5)
extent(t_max.stack) == extent(elev.5)
extent(prec.stack) == extent(elev.5)

# Check resolution
xres(t_min.stack) == xres(elev.5)
xres(t_max.stack) == xres(elev.5)
xres(prec.stack) == xres(elev.5)

# Crop Variables to EU extent --------------------------------------------------
# T Min ------------------------------------------------------------------------
# Crop the variable with EU extension
t_min.sub <- crop(t_min.stack, extent(eu))
# Mask
t_min.sub <- mask(t_min.sub, eu)
plot(t_min.sub[[1]])
gc(); rm(t_min.stack)

# T Max ------------------------------------------------------------------------
# Crop the variable with EU extension
t_max.sub <- crop(t_max.stack, extent(eu))
# Mask
t_max.sub <- mask(t_max.sub, eu)
plot(t_max.sub[[1]])
gc(); rm(t_max.stack)

# Prec -------------------------------------------------------------------------
# Crop the variable with EU extension
prec.sub <- crop(prec.stack, extent(eu))
# Mask
prec.sub <- mask(prec.sub, eu)
plot(prec.sub[[1]])
gc(); rm(prec.stack)

# Check extention
extent(t_min.sub) == extent(elev.5.sub)
extent(t_max.sub) == extent(elev.5.sub)
extent(prec.sub) == extent(elev.5.sub)

# Check resolution
xres(t_min.sub) == xres(elev.5.sub)
xres(t_max.sub) == xres(elev.5.sub)
xres(prec.sub) == xres(elev.5.sub)

# Biovariables Creation - we have to divide the temperature by 10
t_min.sub <- t_min.sub/10
t_max.sub <- t_max.sub/10

par(mfrow=c(1,2), mar=c(1,1,1,1), oma=c(3,3,4,3))
plot(t_min.sub[[1]])
plot(t_max.sub[[1]])
dev.off()

# Bio-variables Creation
bioVar <- dismo::biovars(prec = prec.sub, tmin = t_min.sub, tmax = t_max.sub)
plot(bioVar[[1]])
extent(bioVar) == extent(elev.5.sub)
xres(bioVar) == xres(elev.5.sub)

# Create folder to save the raster
dir.create("./BCC_2050/tMin_crop")
dir.create("./BCC_2050/tMax_crop")
dir.create("./BCC_2050/Prec_crop")
dir.create("./BCC_2050/bioVar_crop")

# Save rasters
for(i in 1:12){
  writeRaster(t_min.sub[[i]], filename = paste0(getwd(),
              "/BCC_2050/tMin_crop/", names(t_min.sub)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:12){
  writeRaster(t_max.sub[[i]], filename = paste0(getwd(), 
              "/BCC_2050/tMax_crop/", names(t_max.sub)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:12){
  writeRaster(prec.sub[[i]], filename = paste0(getwd(),
              "/BCC_2050/Prec_crop/", names(prec.sub)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:19){
  writeRaster(bioVar[[i]], filename = paste0(getwd(), 
              "/BCC_2050/bioVar_crop/", names(bioVar)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)


#----------#
# BCC 2070 #
#----------#
# Download climate variables BCC 2070
file.number <- which(osf_ls_files(osf_files[1, ])[ ,1] == "BCC_2070.zip")
osf_download(osf_ls_files(osf_files[1, ])[file.number, ], 
             path = main.path); rm(file.number)

# Unzip files
unzip("./BCC_2070.zip", exdir=main.path); file.remove("./BCC_2070.zip")


# Climate variable FUTURE 2070 ---------------------------------------------------------
t_min <- list.files(path = "./BCC_2070/bc45tn70/", 
                    pattern = '.tif$', all.files = TRUE, full.names = TRUE)

t_max <- list.files(path = "./BCC_2070/bc45tx70/", 
                    pattern = '.tif$', all.files = TRUE, full.names = TRUE)

prec <- list.files(path = "./BCC_2070/bc45pr70/", 
                   pattern = '.tif$', all.files = TRUE, full.names = TRUE)

t_min.stack <- stack(t_min)
t_max.stack <- stack(t_max)
prec.stack <- stack(prec)

par(mfrow=c(1,3), mar=c(1,1,1,1), oma=c(3,3,4,3))
plot(t_min.stack[[1]])
plot(t_max.stack[[1]])
plot(prec.stack[[1]])
dev.off()

rm(t_min, t_max, prec)

# Check extention
extent(t_min.stack) == extent(elev.5)
extent(t_max.stack) == extent(elev.5)
extent(prec.stack) == extent(elev.5)

# Check resolution
xres(t_min.stack) == xres(elev.5)
xres(t_max.stack) == xres(elev.5)
xres(prec.stack) == xres(elev.5)

# Crop Variables to EU extent --------------------------------------------------
# T Min ------------------------------------------------------------------------
# Crop the variable with EU extension
t_min.sub <- crop(t_min.stack, extent(eu))
# Mask
t_min.sub <- mask(t_min.sub, eu)
plot(t_min.sub[[1]])
gc(); rm(t_min.stack)

# T Max ------------------------------------------------------------------------
# Crop the variable with EU extension
t_max.sub <- crop(t_max.stack, extent(eu))
# Mask
t_max.sub <- mask(t_max.sub, eu)
plot(t_max.sub[[1]])
gc(); rm(t_max.stack)

# Prec -------------------------------------------------------------------------
# Crop the variable with EU extension
prec.sub <- crop(prec.stack, extent(eu))
# Mask
prec.sub <- mask(prec.sub, eu)
plot(prec.sub[[1]])
gc(); rm(prec.stack)

# Check extention
extent(t_min.sub) == extent(elev.5.sub)
extent(t_max.sub) == extent(elev.5.sub)
extent(prec.sub) == extent(elev.5.sub)

# Check resolution
xres(t_min.sub) == xres(elev.5.sub)
xres(t_max.sub) == xres(elev.5.sub)
xres(prec.sub) == xres(elev.5.sub)

# Biovariables Creation - we have to divide the temperature by 10
t_min.sub <- t_min.sub/10
t_max.sub <- t_max.sub/10

par(mfrow=c(1,2), mar=c(1,1,1,1), oma=c(3,3,4,3))
plot(t_min.sub[[1]])
plot(t_max.sub[[1]])
dev.off()

# Bio-variables Creation
bioVar <- dismo::biovars(prec = prec.sub, tmin = t_min.sub, tmax = t_max.sub)
plot(bioVar[[1]])
extent(bioVar) == extent(elev.5.sub)
xres(bioVar) == xres(elev.5.sub)

# Create folder to save the raster
dir.create("./BCC_2070/tMin_crop")
dir.create("./BCC_2070/tMax_crop")
dir.create("./BCC_2070/Prec_crop")
dir.create("./BCC_2070/bioVar_crop")

# Save rasters
for(i in 1:12){
  writeRaster(t_min.sub[[i]], filename = paste0(getwd(),
              "/BCC_2070/tMin_crop/", names(t_min.sub)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:12){
  writeRaster(t_max.sub[[i]], filename = paste0(getwd(), 
              "/BCC_2070/tMax_crop/", names(t_max.sub)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:12){
  writeRaster(prec.sub[[i]], filename = paste0(getwd(),
              "/BCC_2070/Prec_crop/", names(prec.sub)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

for(i in 1:19){
  writeRaster(bioVar[[i]], filename = paste0(getwd(), 
              "/BCC_2070/bioVar_crop/", names(bioVar)[i], ".tif"), 
              format = "GTiff", overwrite = FALSE)}; rm(i)

# Clean workspace
rm(list = ls())
