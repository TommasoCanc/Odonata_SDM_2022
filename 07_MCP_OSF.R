###############
# Odonata MCP #
###############

# We have a huge amount of Odonata species with different distribution areas, 
# so we have to think about a methodology to produce background points for all 
# species in a single loop.
# A solution could be to use the Minimum Convex Polygon of each species and add a 
# buffer. To take account of each species' potential dispersion, the buffer is 
# weighted with the species' flight period. Done this, we add the background 
# points twice the number of occurrences for each species. We can not fix the 
# same number of background points for all the species because the MCP is very 
# different.

# Load packages
library(readxl) # <- Reax xlsx
library(sf) # <- GIS
library(rgeos) # <- GIS
library(raster) # <- GIS
library(adehabitatHR)# <- Home range estimation

# Create folder to save MCP
dir.create("./MCP")
dir.create("./MCP/Odonata_15_Presence")
dir.create("./MCP/MCP_shape")

# Load odonata files without duplicates
odon <- list.files(path = "./Odonata_NO_duplicated/", 
                   pattern=".csv$", all.files = TRUE, full.names = TRUE)

# Loop to create MCP and save only the species with >= 15 occurrences ----------
for(i in 1:length(odon)){
  sp <- read.csv(odon[i])
  if(nrow(sp) >= 5){ 
    
    # Extract The Minimum Convex Polygon (MCP) at 99%
    sp.poly <- sp[,c("canonicalName","y","x")]
    coordinates(sp.poly) = ~ x + y
    sp.99 <- mcp(sp.poly, percent = 99)
    
    # Create dataframe with species coordinate inside the MCP
    pts_in <- sp.poly[complete.cases(over(sp.poly, sp.99)), ]
    pts_in.df <- as.data.frame(pts_in)
    
    # Save only the species with presence >15
    if(nrow(pts_in.df) >= 15){
      write.csv(pts_in.df, paste0("./MCP/Odonata_15_Presence/", 
                                  unique(sp$canonicalName), ".csv"),
               row.names = FALSE)
      
      # Save shp of MCP.99
      rgdal::writeOGR(sp.99, 
                      layer = unique(sp$canonicalName),
                      paste0("./MCP/MCP_shape/", unique(sp$canonicalName)), 
                      driver="ESRI Shapefile")
    }
  }
}; rm(list = ls())


# Add buffer to the MCP. In this area we will add the background points. -------

# Create folder to save files
dir.create("./MCP/MCP_Buffer")

# Load elevation raster
elev <- raster("./elevation_5_null.asc")

# Buffer is weighted with the flight period of the species.
mcp.dir <- list.dirs(path = "./MCP/MCP_shape/", full.names = TRUE)

# Load table with fly period
flight.p <- read_xlsx("./European_Odonata_checklist.xlsx", sheet = "Traits")
flight.p <- as.data.frame(flight.p[,c(4,12)])

# Check solution to use meters for the buffer 
# https://stackoverflow.com/questions/54754277/what-unit-is-the-dist-argument-in-st-buffer-set-to-by-default

for(i in 2:length(mcp.dir)){
  odonShp <- st_read(dsn = mcp.dir[i])
  st_crs(odonShp) = 4326
  flight <- flight.p$`FlightSeason (month)`[flight.p$Species == odonShp$id]
  
  # Transform to crs=7801
  odonShp <- st_transform(odonShp, crs = 7801)
  # Here we have the distance in meters
  odonBuffer <- st_buffer(odonShp, dist = 100000*(flight/10))
  odonBuffer <- st_transform(odonBuffer, crs = 4326)
  
  bufferRaster <- mask(crop(elev ,odonBuffer), odonBuffer)
  
  # Save .shp of buffer MCP.99
  writeRaster(bufferRaster, 
              filename = paste0("./MCP/MCP_Buffer/", odonShp$id, ".tif"), 
              format = "ascii", overwrite = TRUE)
}

# Clean workspace
rm(list = ls())
