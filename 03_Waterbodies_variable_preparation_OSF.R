################
# Water bodies #
################

# Load libraries
library(sf) # <- GIS
library(rgdal) # <- GIS
library(raster) # <- GIS
library(osfr) # <- Connect to OSF

#####################
# Connection to OSF #
#####################

main.path <- "YOUR_PATH"
setwd(main.path)

# Connect to Open Science Framework
cr_project <- osf_retrieve_node("4rjuc")
osf_files <- osf_ls_files(cr_project, type = "folder")

# Download main variables
file.number <- which(osf_ls_files(osf_files[1, ])[ ,1] == "glc_shv10_11.Tif")
osf_download(osf_ls_files(osf_files[1, ])[file.number, ], path = main.path)
rm(file.number)

# Load water bodies------- -----------------------------------------------------
wb <-  raster("./glc_shv10_11.Tif") 
elev <- raster("./wc2.1_5m_elev.tif")

# Match extension and resolution with Elevation
wbRes <- resample(x = wb, y = elev, method = "bilinear")

# Check extention
extent(wbRes) == extent(elev)
# Check resolution
xres(wbRes) == xres(elev)

# Crop water bodies ------------------------------------------------------------
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
rm(world, countries)

# Crop the variable with EU extension
wb.sub <- crop(wbRes, extent(eu))
# Mask
wb.sub <- mask(wb.sub, eu)
plot(wb.sub)
gc()

# Save raster
writeRaster(wb.sub, filename=paste0("./Waterbodies_5m_crop.tif"), 
              format="GTiff", overwrite=F)

# Clean workspace
rm(list=ls())
