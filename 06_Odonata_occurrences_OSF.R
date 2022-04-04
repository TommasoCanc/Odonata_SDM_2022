################
# Odonata data #
################

# Preparation of odonata occurrences

# Load packages
library(readxl) # <- Read .xlsx
library(rgbif) # <- API Gbif 
library(data.table) # <- Readbbig table
library(rgeos) # <- GIS
library(raster)# <- GIS
library(adehabitatHR) # <- Home range estimation
library(osfr) # <- Connect to OSF

#####################
# Connection to OSF #
#####################

main.path <- "YOUR_PATH"
setwd(main.path)

# Connect to Open Science Framework
cr_project <- osf_retrieve_node("4rjuc")
osf_files <- osf_ls_files(cr_project, type = "folder")

# Download Odonata checklist
file.number <- which(osf_ls_files(osf_files[1, ], pattern = "xlsx")[ ,1] == 
                       "European_Odonata_checklist.xlsx")
osf_download(osf_ls_files(osf_files[1, ], pattern = "xlsx")[file.number, ], 
             path = main.path)
rm(file.number)

# Load European Odonata checklist
odoCkList <- read_excel("./European_Odonata_checklist.xlsx", sheet = "Taxonomy")

# List of species and subspecies
species <- unique(odoCkList$Species)
subspecies <- unique(odoCkList$Subspecies)
subspecies <- subspecies[!is.na(subspecies)]

# To check the raw data downloaded from GBIF please check the URL ... 

#---------------#
# Cleaning data #
#---------------#

# Download Odonata occurrence points
file.number <- which(osf_ls_files(osf_files[1, ], pattern = "csv")[ ,1] == 
                       "European_Odonata_Taxonomy.csv")
osf_download(osf_ls_files(osf_files[1, ], pattern = "csv")[file.number, ], 
             path = main.path)
rm(file.number)

# Load European_Odonata_Taxonomy.csv
odonata <- fread("./European_Odonata_Taxonomy.csv")
head(odonata)


# Remove FOSSIL records --------------------------------------------------------

# Check if the dataframe includes fossil records
unique(odonata$basisOfRecord)
barplot(table(odonata$basisOfRecord), las = 2)

# Remove fossil record
odonata <- odonata[odonata$basisOfRecord != "FOSSIL_SPECIMEN", ]
# Check if FOSSIL_SPECIMEN persists
unique(odonata$basisOfRecord)

# Remove records out of our study area and records -----------------------------

# Load elevation as reference variable
elev <-  raster("./elevation_5_null.asc")

odonata.variables <- extract(x = elev, 
                     y = odonata[ , c("decimalLongitude","decimalLatitude")])

#Convert to dataframe
odonata.variables <- data.frame(odonata.variables)

#joint with the occurrences
odonta.presencia <- data.frame(odonata, odonata.variables)
rm(odonata.variables)

#Remove records with null values
odonta.presencia <- odonta.presencia[!(is.na(odonta.presencia$odonata.variables)), ]

# Remove old records -----------------------------------------------------------

# Histogram with year distribution
hist(odonta.presencia$year)

#How many cell have NA values in the column year?
sum(is.na(odonta.presencia$year))

year.na <- as.data.frame(
  table(odonta.presencia$canonicalName[is.na(odonta.presencia$year)]))
n.record <- table(odonta.presencia$canonicalName)

# For each species which is the % of NA-year?
year.na.record <- merge(x = year.na, y = n.record, by = "Var1", all = TRUE)
year.na.record$dif <- year.na.record$Freq.y - year.na.record$Freq.x
year.na.record$ratio <- round(
  (year.na.record$Freq.x * 100) / year.na.record$Freq.y, digits = 1)

# We can remove the species without year
odonta.presencia <-odonta.presencia[!is.na(odonta.presencia$year), ]
# Quickly check
sum(is.na(odonta.presencia$year)); rm(year.na.record, year.na)

# How many record have the year >= 1970?
year.1970 <- as.data.frame(
  table(odonta.presencia$canonicalName[odonta.presencia$year <= 1970]))
n.record <- table(odonta.presencia$canonicalName)

year.1970.record <- merge(x = year.1970, y = n.record, by = "Var1", all=T)
year.1970.record$dif <- year.1970.record$Freq.y - year.1970.record$Freq.x
year.1970.record$ratio <- round(
  (year.1970.record$Freq.x * 100) / year.1970.record$Freq.y, digits = 1)

# We filter the records >=1970
odonta.presencia<-odonta.presencia[odonta.presencia$year >= 1970, ]
# Quickly check
min(odonta.presencia$year); rm(year.1970, year.1970.record)

# Check spatial resolution of records ------------------------------------------

# Unique values of meters uncertainty
unique(odonta.presencia$coordinateUncertaintyInMeters)

# Data distribution
barplot(table(odonta.presencia$coordinateUncertaintyInMeters), las = 2) 

# How many cells have NA data in the column 'coordinateUncertaintyInMeters'?
sum(is.na(odonta.presencia$coordinateUncertaintyInMeters))
# Where is these records?
prec.na <- as.data.frame(
  table(odonta.presencia$canonicalName[is.na(odonta.presencia$coordinateUncertaintyInMeters)]))
n.record.prec <- table(odonta.presencia$canonicalName)

prec.na.record <- merge(x = prec.na, y = n.record.prec, by = "Var1", all = TRUE)
prec.na.record$dif <- prec.na.record$Freq.y - prec.na.record$Freq.x
prec.na.record$ratio <- round(
  (prec.na.record$Freq.x * 100) / prec.na.record$Freq.y, digits = 1)

# Remove data without coordinate precision 
odonta.presencia <- odonta.presencia[!is.na(odonta.presencia$coordinateUncertaintyInMeters), ]
# Quickly check
sum(is.na(odonta.presencia$coordinateUncertaintyInMeters))

# Raster variables have a resolution of 5 min (~ 10km). 
# Remove record with spatial uncertainty greater 
# than raster resolution.
cinco.na <- as.data.frame(
  table(odonta.presencia$canonicalName[odonta.presencia$coordinateUncertaintyInMeters >= 10000])) 
n.record.prec <- table(odonta.presencia$canonicalName)

cinco.na.record <- merge(x = cinco.na, y = n.record.prec, by = "Var1", all=T)
cinco.na.record$dif <- cinco.na.record$Freq.y - cinco.na.record$Freq.x
cinco.na.record$ratio <- round(
  (cinco.na.record$Freq.x * 100) / cinco.na.record$Freq.y, digits = 1)

# Remove data with >= 10 Km
odonta.presencia <- odonta.presencia[odonta.presencia$coordinateUncertaintyInMeters <= 10000, ] 
## Quickly check
max(odonta.presencia$coordinateUncertaintyInMeters)

# Merge species and subspecies -------------------------------------------------
sp.word <- data.frame(canonicalName = character(),
                      n_words = integer())

for (i in unique(odonta.presencia$canonicalName)){
  n.word <- data.frame(canonicalName = i,
                n_words = ngram::wordcount(i, sep = " ", count.function = sum))
  sp.word <- rbind(sp.word, n.word)
  }; rm(i)

subspecies <- sp.word[sp.word$n_words == 3,]
rm(sp.word, n.word)

# Assign  species to subspecies
odonta.presencia$canonicalName[odonta.presencia$canonicalName == "Lestes virens vestalis"] <- "Lestes virens"
odonta.presencia$canonicalName[odonta.presencia$canonicalName == "Calopteryx virgo festiva"] <- "Calopteryx virgo"
odonta.presencia$canonicalName[odonta.presencia$canonicalName == "Calopteryx virgo meridionalis"] <- "Calopteryx virgo"
odonta.presencia$canonicalName[odonta.presencia$canonicalName == "Orthetrum coerulescens coerulescens"] <- "Orthetrum coerulescens"
odonta.presencia$canonicalName[odonta.presencia$canonicalName == "Sympetrum vulgatum ibericum"] <- "Sympetrum vulgatum"
odonta.presencia$canonicalName[odonta.presencia$canonicalName == "Aeshna subarctica elisabethae"] <- "Aeshna subarctica"
odonta.presencia$canonicalName[odonta.presencia$canonicalName == "Onychogomphus forcipatus albotibialis"] <- "Onychogomphus forcipatus"
odonta.presencia$canonicalName[odonta.presencia$canonicalName == "Onychogomphus forcipatus unguiculatus"] <- "Onychogomphus forcipatus"
odonta.presencia$canonicalName[odonta.presencia$canonicalName == "Platycnemis pennipes nitidula"] <- "Platycnemis pennipes"
odonta.presencia$canonicalName[odonta.presencia$canonicalName == "Ischnura elegans elegans"] <- "Ischnura elegans"
odonta.presencia$canonicalName[odonta.presencia$canonicalName == "Lestes virens virens"] <- "Lestes virens"
odonta.presencia$canonicalName[odonta.presencia$canonicalName == "Onychogomphus forcipatus forcipatus"] <- "Onychogomphus forcipatus"
odonta.presencia$canonicalName[odonta.presencia$canonicalName == "Cordulegaster helladica helladica"] <- "Cordulegaster helladica"
odonta.presencia$canonicalName[odonta.presencia$canonicalName == "Calopteryx virgo virgo"] <- "Calopteryx virgo"
odonta.presencia$canonicalName[odonta.presencia$canonicalName == "Sympetrum vulgatum vulgatum"] <- "Sympetrum vulgatum"
odonta.presencia$canonicalName[odonta.presencia$canonicalName == "Orthetrum brunneum brunneum"] <- "Orthetrum brunneum"
odonta.presencia$canonicalName[odonta.presencia$canonicalName == "Orthetrum coerulescens anceps"] <- "Orthetrum coerulescens"

# Save a .csv file for each species
# Create a folder to store the data
dir.create("./Odonata_species")

sp.odo <- unique(odonta.presencia.1$canonicalName)
for(i in 1:length(sp.odo)) {
occ.sp <- odonta.presencia[odonta.presencia$canonicalName == sp.odo[i],]
fwrite(occ.sp, paste0("./Odonata_species/" , sp.odo[i], ".csv"))
}

rm(list=setdiff(ls(), c("osf_files", "cr_project", "main.path", "elev")))

#-----------------------------------------#
# NOW WE HAVE TO THINK SPECIES BY SPECIES #
#-----------------------------------------#

# Remove spatial correlation ---------------------------------------------------

# Create folder to store de data
dir.create("./Odonata_NO_spatial_correlation")

# Load files paths
odon <- list.files(path = "./Odonata_species", 
           pattern=".csv$", all.files = TRUE, full.names = TRUE)

# This treatment is done to reduce the spatial correlation of the sample.
# This reduces sampling biases, and avoids inflating the assessment values.

# Check variable spatial resolution
xres(elev); yres(elev) 
res.deg <- xres(elev)

# Number of null cell that we want to use to reduce spatial correlation
empty.cell <- 1
# Minimum distance
#distancia.minima <- res.deg * empty.cell <- 1
minimum.dist <- res.deg * empty.cell
#In km ~ 10 km
minimum.dist * 111.19

for(i in 1:length(odon)){
# Load .csv for each Odonata species
sp.spatial.cor <- read.csv(odon[i])
sp.spatial.cor.1 <- as.data.frame(
  sp.spatial.cor[,c("decimalLongitude","decimalLatitude")])
colnames(sp.spatial.cor.1) <- c("x","y")
# Reduce spatial autocorrelation
xy.thin <- SDMworkshop::reduceSpatialCorrelation(
  xy = sp.spatial.cor.1,
  variables = elev,
  minimum.distance = minimum.dist 
)

# Merge coordinate and species name
sp <- cbind(unique(sp.spatial.cor$canonicalName), xy.thin)

#Save .csv
write.csv(sp, paste0("./Odonata_NO_spatial_correlation/", 
                     unique(sp.spatial.cor$canonicalName), ".csv"), 
          row.names = FALSE)
}; rm(i, sp.spatial.cor, sp.spatial.cor.1, xy.thin, sp, empty.cell, 
      minimum.dist, res.deg)

# Remove duplicate in coordinates ----------------------------------------------

# Create folder to store de data
dir.create("./Odonata_NO_duplicated")

# Load files paths
odon <- list.files(path = "./Odonata_NO_spatial_correlation/", 
                   pattern = ".csv$", all.files = TRUE, full.names = TRUE)

i=1
for(i in 1:length(odon)){
sp <- read.csv(odon[i])
colnames(sp)[1] <- "canonicalName"
duplic <- duplicated(sp[ , c("y", "x")])

# Remove duplicate
sp <- sp[!duplic, ]
write.csv(sp, paste0("./Odonata_NO_duplicated/" , 
                     unique(sp$canonicalName), ".csv"), row.names = FALSE)
}; 

# Clean workspace
rm(list=ls())
