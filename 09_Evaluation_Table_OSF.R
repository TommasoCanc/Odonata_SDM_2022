############################################
# PRESENCE-BACKGROUND AND EVALUATION TABLE #
############################################

# Load libraries
library(readxl) # <- Read .xlsx 
library(rgeos) # <- GIS
library(raster) # <-GIS

# Load variables chosen with the lower VIF 
# embergerQ, Waterbodies_5_null, elevation_5_null, bio7, bio8, bio10, bio15

# Elevation and waterbodies
elev <- raster("./elevation_5_null.asc")
wb <- raster("./Waterbodies_5_null.asc")

# Envirem and Bioclimatic variables
enviFiles <- list.files(path = "./Present/envirem_null/", 
                        pattern = ".tif$", all.files = TRUE, full.names=T)

bioclimFiles <- list.files(path = "./Present/bioVar_null/", 
                           pattern='.tif$', all.files=TRUE, full.names=T)
# Stack variables
enviStack <- stack(enviFiles)
bioclimStack <- stack(bioclimFiles)

# Create a total stack containing all the variables
variablesStack <- stack(elev, wb, bioclimStack, enviStack)
names(variablesStack)

# Remove unnecessary files
rm(bioclimFiles, enviFiles, elev, bioclimStack, enviStack)

# Subset variables useful for the analysis
variables.names <- c("embergerQ", "Waterbodies_5_null", "elevation_5_null", 
                     "bio7", "bio8", "bio10", "bio15")
variable.seleccionadas <- stack(variablesStack[[variables.names]])
names(variable.seleccionadas)

# Extract variable values for the Odonata occurrences --------------------------

# Create folder to save data
dir.create("./Presence_Background/Odonata_presence_background_variables")
dir.create("./Presence_Background/Training")
dir.create("./Presence_Background/Evaluation")

# Load file paths
odonFiles <- list.files(path = "./Presence_Background/Odonata_presencia_background/", 
                        pattern = ".csv$", all.files = TRUE, full.names = TRUE)

for(i in 1:length(odonFiles)){
sp <- read.csv(odonFiles[i])
sp.spt <- sp
coordinates(sp.spt) = ~ x + y

# Extract variable data
pres.back.variables <- data.frame(extract(variable.seleccionadas, sp.spt))

# Join the presence-background data with the variables
pres.back.variables <- cbind(sp, pres.back.variables)

write.csv(pres.back.variables, 
          paste0("./Presence_Background/Odonata_presence_background_variables/", 
          unique(pres.back.variables$canonicalName), ".csv"), 
          row.names = FALSE)
}; rm(list = ls())

# Creation of train and evaluation tables --------------------------------------

# Load odonata files with records of presence (1) and background (0)
odonFiles <- list.files(path = "./Presence_Background/Odonata_presence_background_variables/", 
                        pattern = ".csv$", all.files = TRUE, full.names = TRUE)

# To evaluate our models we need to choose a percent of records of presence and 
# background in order to create a training and evaluation datasets.
# Percent of evaluation points. 25% of presence and 25% of background
percent <- 25

for(i in 1:length(odonFiles)){

# Load .csv
sp <- read.csv(odonFiles[i])
presence.sp <- sp[sp$presence == 1,]

# Establish the values for the evaluation dataset
number.presence <- nrow(presence.sp)
number.absence <- number.presence
number.presence.evaluation <- round((percent*number.presence)/100)
number.absence.evaluation <- number.presence.evaluation

set.seed(1234)
# Random selection of presence... and background records
sample.presence <- sample(number.presence, number.presence.evaluation)
#... and background records
sample.absence <- sample(number.absence, number.absence.evaluation) + number.presence

# We create a table that we will use to evaluate the models.
presence.background.evaluation <- sp[c(sample.presence, sample.absence), 
                                     c("canonicalName","x","y","presence")]

# Save the evaluation table in the folder Odonata_evaluacion_2
write.csv(presence.background.evaluation, 
          paste0("./Presence_Background/Evaluation/", 
          unique(presence.background.evaluation$canonicalName), "_evaluation.csv"), 
          row.names = FALSE)

# To create the training table we have to remove the evaluation record from the 
# table containing all record.
# For this reason we obtain a training table = total_table - (25% presence)
# We have to perform the final models with all the presences

# Training tables
presence.background.training <- sp[-sample.presence, ]

write.csv(presence.background.training, 
          paste0("./Presence_Background/Training/", 
          unique(presence.background.training$canonicalName), "_training.csv"), 
          row.names = FALSE)
}
