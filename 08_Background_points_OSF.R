#####################
# Background points #
#####################

# Create folder to save data
dir.create("./Presence_Background")
dir.create("./Presence_Background/Odonata_Presence")
dir.create("./Presence_Background/Odonata_presencia_background")

# Load Odonata files with points within MCP ------------------------------------
odon <- list.files(path = "./MCP/Odonata_15_Presence/", 
                   pattern=".csv$", all.files = TRUE, full.names = TRUE)

for(i in 1:length(odon)){
presence <- read.csv(odon[i])
# We are going to add a column of presence. The presence is marked as 1.
presence$presence <- 1

# Save .csv in Odonata_presencia_2
write.csv(presence, paste0("./Presence_Background/Odonata_Presence/", 
                           unique(presence$canonicalName), ".csv"), 
          row.names = FALSE)
}; rm(list = ls())


# Generate background points ---------------------------------------------------

# Load Odonata file the the presence column
presenceFiles <- list.files(path = "./Presence_Background/Odonata_Presence/", 
                        pattern = ".csv$", all.files = TRUE, full.names = TRUE)

# Load raster with buffered MCP
bufferFiles <- list.files(path = "./MCP/MCP_Buffer/", 
                   pattern = ".asc$", all.files = TRUE, full.names = TRUE)

# Check if we have the same order in buffered-MCP and odonata list.
length(presenceFiles) == length(bufferFiles) # <- YES

for(i in 1:length(presenceFiles)){

# Load the presence
presence <- read.csv(presenceFiles[i])

n.Background <- nrow(presence)*2 # <- Generate background points twice the presence
bufferRaster <- raster(bufferFiles[i])
background <- dismo::randomPoints(mask = bufferRaster, n = n.Background)

# Convert to dataframe
background <- data.frame(background)

# Total dataframe with presence and background
background.df <- data.frame(canonicalName = unique(presence$canonicalName),
                            y = background$y,
                            x = background$x,
                            presence = 0)

presence.backgraound <- rbind(presence, background.df)

# Save dataframe with presence (1) and backgrund (0)
write.csv(presence.backgraound, 
          paste0("./Presence_Background/Odonata_presencia_background/", 
                 unique(presence$canonicalName), ".csv"), row.names = FALSE)

}; rm(list = ls())
