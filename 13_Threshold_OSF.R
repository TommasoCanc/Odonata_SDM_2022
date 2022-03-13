#############
# THRESHOLD #
#############

##########################################################
# Threshold with optimization techniques
##########################################################

# Load libraries
library(raster) # <- GIS
library(ggplot2) # <- Plot
library(gridExtra) # <- Plot
library(grid) # <- Plot

# Main path
main.path <- "YOUR_PATH"
setwd(main.path)

# Create folder to save data
dir.create("./Models/Threshold")
dir.create("./Models/Threshold/Present")
dir.create("./Models/Threshold/Present/raster")
dir.create("./Models/Threshold/Present/tables")

dir.create("./Models/Threshold/MIROC_2050")
dir.create("./Models/Threshold/MIROC_2050/raster")
dir.create("./Models/Threshold/MIROC_2050/tables")
dir.create("./Models/Threshold/MIROC_2070")
dir.create("./Models/Threshold/MIROC_2070/raster")
dir.create("./Models/Threshold/MIROC_2070/tables")

dir.create("./Models/Threshold/NOR_2050")
dir.create("./Models/Threshold/NOR_2050/raster")
dir.create("./Models/Threshold/NOR_2050/tables")
dir.create("./Models/Threshold/NOR_2070")
dir.create("./Models/Threshold/NOR_2070/raster")
dir.create("./Models/Threshold/NOR_2070/tables")

dir.create("./Models/Threshold/BCC_2050")
dir.create("./Models/Threshold/BCC_2050/raster")
dir.create("./Models/Threshold/BCC_2050/tables")
dir.create("./Models/Threshold/BCC_2070")
dir.create("./Models/Threshold/BCC_2070/raster")
dir.create("./Models/Threshold/BCC_2070/tables")


# Load elevation
elev <- raster("./elevation_5_null.asc")

# PRESENT ----------------------------------------------------------------------
# Load models with all presences
gam.models <- list.files(path = "./Models/GAM/All_presence/Map_raster/Present/",
                          pattern = ".asc", full.names = TRUE)
max.models <- list.files(path = "./Models/Maxent/All_presence/Map_raster/Present/", 
                          pattern = ".asc", full.names = TRUE)
brt.models <- list.files(path = "./Models/BRT/All_presence/Map_raster/Present/",
                          pattern = ".asc", full.names = TRUE)
ens.models <- list.files(path = "./Models/Ensemble/All_presence/Map_raster/Present/",
                          pattern = ".asc", full.names = TRUE)

# FUTURES ----------------------------------------------------------------------
# Load total models 2050 and 2070 MIROC-ESM-CHEM
gam.models_MIROC_2050 <- list.files(path = "./Models/GAM/All_presence/Map_raster/MIROC_2050/",
                                     pattern = ".asc", full.names = TRUE)
max.models_MIROC_2050 <- list.files(path = "./Models/Maxent/All_presence/Map_raster/MIROC_2050/",
                                     pattern = ".asc", full.names = TRUE)
brt.models_MIROC_2050 <- list.files(path = "./Models/BRT/All_presence/Map_raster/MIROC_2050/",
                                     pattern = ".asc", full.names = TRUE)
ens.models_MIROC_2050 <- list.files(path = "./Models/Ensemble/All_presence/Map_raster/MIROC_2050/",
                                     pattern = ".asc", full.names = TRUE)

gam.models_MIROC_2070 <- list.files(path = "./Models/GAM/All_presence/Map_raster/MIROC_2070/",
                                     pattern = ".asc", full.names = TRUE)
max.models_MIROC_2070 <- list.files(path = "./Models/Maxent/All_presence/Map_raster/MIROC_2070/",
                                     pattern = ".asc", full.names = TRUE)
brt.models_MIROC_2070 <- list.files(path = "./Models/BRT/All_presence/Map_raster/MIROC_2070/",
                                     pattern = ".asc", full.names = TRUE)
ens.models_MIROC_2070 <- list.files(path = "./Models/Ensemble/All_presence/Map_raster/MIROC_2070/",
                                     pattern = ".asc", full.names = TRUE)

# Load total models 2050 and 2070 NorESM1-M
gam.models_NOR_2050 <- list.files(path = "./Models/GAM/All_presence/Map_raster/NOR_2050/",
                                   pattern = ".asc", full.names = TRUE)
max.models_NOR_2050 <- list.files(path = "./Models/Maxent/All_presence/Map_raster/NOR_2050/",
                                   pattern = ".asc", full.names = TRUE)
brt.models_NOR_2050 <- list.files(path = "./Models/BRT/All_presence/Map_raster/NOR_2050/",
                                   pattern = ".asc", full.names = TRUE)
ens.models_NOR_2050 <- list.files(path = "./Models/Ensemble/All_presence/Map_raster/NOR_2050/",
                                   pattern = ".asc", full.names = TRUE)

gam.models_NOR_2070 <- list.files(path = "./Models/GAM/All_presence/Map_raster/NOR_2070/",
                                   pattern = ".asc", full.names = TRUE)
max.models_NOR_2070 <- list.files(path = "./Models/Maxent/All_presence/Map_raster/NOR_2070/",
                                   pattern = ".asc", full.names = TRUE)
brt.models_NOR_2070 <- list.files(path = "./Models/BRT/All_presence/Map_raster/NOR_2070/",
                                   pattern = ".asc", full.names = TRUE)
ens.models_NOR_2070 <- list.files(path = "./Models/Ensemble/All_presence/Map_raster/NOR_2070/",
                                   pattern = ".asc", full.names = TRUE)

# Load total models 2050 and 2070 BCC-CSM1-1
gam.models_BCC_2050 <- list.files(path = "./Models/GAM/All_presence/Map_raster/BCC_2050/",
                                   pattern = ".asc", full.names = TRUE)
max.models_BCC_2050 <- list.files(path = "./Models/Maxent/All_presence/Map_raster/BCC_2050/",
                                   pattern = ".asc", full.names = TRUE)
brt.models_BCC_2050 <- list.files(path = "./Models/BRT/All_presence/Map_raster/BCC_2050/",
                                   pattern = ".asc", full.names = TRUE)
ens.models_BCC_2050 <- list.files(path = "./Models/Ensemble/All_presence/Map_raster/BCC_2050/",
                                   pattern = ".asc", full.names = TRUE)

gam.models_BCC_2070 <- list.files(path = "./Models/GAM/All_presence/Map_raster/BCC_2070/",
                                   pattern = ".asc", full.names = TRUE)
max.models_BCC_2070 <- list.files(path = "./Models/Maxent/All_presence/Map_raster/BCC_2070/",
                                   pattern = ".asc", full.names = TRUE)
brt.models_BCC_2070 <- list.files(path = "./Models/BRT/All_presence/Map_raster/BCC_2070/",
                                   pattern = ".asc", full.names = TRUE)
ens.models_BCC_2070 <- list.files(path = "./Models/Ensemble/All_presence/Map_raster/BCC_2070/",
                                   pattern = ".asc", full.names = TRUE)

for(i in 1:length(gam.modelos)) {
  
# PRESENT ----------------------------------------------------------------------  
models <- stack(gam.models[i], brt.models[i], max.models[i], ens.models[i])
  
# FUTURES ----------------------------------------------------------------------
# Load total models 2050 and 2070 MIROC-ESM-CHEM
models_MIROC_2050 <- stack(gam.models_MIROC_2050[i], brt.models_MIROC_2050[i], 
                           max.models_MIROC_2050[i], ens.models_MIROC_2050[i])

models_MIROC_2070 <- stack(gam.models_MIROC_2070[i], brt.models_MIROC_2070[i], 
                           max.models_MIROC_2070[i], ens.models_MIROC_2070[i])
  
# Load total models 2050 and 2070 NorESM1-M
models_NOR_2050 <- stack(gam.models_NOR_2050[i], brt.models_NOR_2050[i], 
                         max.models_NOR_2050[i], ens.models_NOR_2050[i])
  
models_NOR_2070 <- stack(gam.models_NOR_2070[i], brt.models_NOR_2070[i], 
                         max.models_NOR_2070[i], ens.models_NOR_2070[i])
  
# Load total models 2050 and 2070 BCC-CSM1-1
models_BCC_2050 <- stack(gam.models_BCC_2050[i], brt.models_BCC_2050[i], 
                         max.models_BCC_2050[i], ens.models_BCC_2050[i])
  
models_BCC_2070 <- stack(gam.models_BCC_2070[i], brt.models_BCC_2070[i], 
                         max.models_BCC_2070[i], ens.models_BCC_2070[i])
  
# Load evaluation table with presence and background points
presence.absence.evaluationFiles <- list.files(path = "./Presence_Background/Evaluation/",
                                               pattern = ".csv", full.names = TRUE)
  
species <- stringr::str_remove(names(models)[1], "_gam")
species <- gsub("_", " ",species)

presence.absence.evaluationFiles30 <- presence.absence.evaluationFiles[grepl(species, presence.absence.evaluationFiles)]
presence.absence.evaluation <- read.csv(presence.absence.evaluationFiles30)
  

# PRESENT ----------------------------------------------------------------------
# Extract the values of the evaluation points on total models
val.evaluation.points <- data.frame(
  extract(models, presence.absence.evaluation[, c("x","y")]))
  
# FUTURE -----------------------------------------------------------------------
# Extract the values of the evaluation points on total models MIROC-ESM-CHEM
val.evaluation.points_MIROC_2050 <- data.frame(
  extract(models_MIROC_2050, presence.absence.evaluation[, c("x","y")]))
val.evaluation.points_MIROC_2070 <- data.frame(
  extract(models_MIROC_2070, presence.absence.evaluation[, c("x","y")]))
  
# Extract the values of the evaluation points on total models NorESM1-M
val.evaluation.points_NOR_2050 <- data.frame(
  extract(models_NOR_2050, presence.absence.evaluation[, c("x","y")]))
val.evaluation.points_NOR_2070 <- data.frame(
  extract(models_NOR_2070, presence.absence.evaluation[, c("x","y")]))
  
# Extract the values of the evaluation points on total models BCC-CSM1-1
val.evaluation.points_BCC_2050 <- data.frame(
  extract(models_BCC_2050, presence.absence.evaluation[, c("x","y")]))
val.evaluation.points_BCC_2070 <- data.frame(
  extract(models_BCC_2070, presence.absence.evaluation[, c("x","y")]))

# Merge with the column presence-background
val.evaluation.points$presence <- presence.absence.evaluation$presence
  
val.evaluation.points_MIROC_2050$presence <- presence.absence.evaluation$presence
val.evaluation.points_MIROC_2070$presence <- presence.absence.evaluation$presence
  
val.evaluation.points_NOR_2050$presence <- presence.absence.evaluation$presence
val.evaluation.points_NOR_2070$presence <- presence.absence.evaluation$presence
  
val.evaluation.points_BCC_2050$presence <- presence.absence.evaluation$presence
val.evaluation.points_BCC_2070$presence <- presence.absence.evaluation$presence
  
# Threshold with optimization techniques ---------------------------------------
  
# Main table to save the results
table_present <- data.frame()
for(model in names(models)){
val.presence <- val.evaluation.points[ , c(model,"presence")]
# Extract present and background points
presence.evaluation <- val.presence[val.presence$presence == 1 , model]
absence.evaluation <- val.presence[val.presence$presence == 0 , model]
# Evaluate
eval.background <- dismo::evaluate(p = presence.evaluation, 
                                   a = absence.evaluation)
# Threshold TPR_TNR
thre <- eval.background@t[which.max(eval.background@TPR + eval.background@TNR)]

model.thre <- models[[model]] > thre
writeRaster(model.thre, 
            filename = paste0("./Models/Threshold/Present/raster/", model, ".tif"), 
            format = "ascii", overwrite = FALSE)

table.1 <- data.frame(model = modelo,
                      auc = evaluacion.background@auc)
    
table_present <- rbind(table_present, table.1)
write.csv(table_present, paste0("./Models/Threshold/Present/tables/",
          stringr::str_remove(names(modelos)[1], "_gam") ,".csv"),
          row.names = FALSE)
  }; rm(modelo)
  
# FUTURE -----------------------------------------------------------------------
  
#----------------#
# MIROC-ESM-CHEM #
#----------------#
# Main table to save the results 2050 
table_MIROC_2050 <- data.frame()
for(model in names(models_MIROC_2050)){
val.presence <- val.evaluation.points_MIROC_2050[ , c(model,"presence")]
# Extract present and background points
presence.evaluation <- val.presence[val.presence$presence == 1 , model]
absence.evaluation <- val.presence[val.presence$presence == 0 , model]
# Evaluate
eval.background <- dismo::evaluate(p = presence.evaluation, 
                                   a = absence.evaluation)
# Threshold TPR_TNR
thre <- eval.background@t[which.max(eval.background@TPR + eval.background@TNR)]
    
model.thre <- models_MIROC_2050[[model]] > thre
writeRaster(model.thre, 
            filename = paste0("./Models/Threshold/MIROC_2050/raster/", model, ".tif"), 
            format = "ascii", overwrite = FALSE)

table.1 <- data.frame(model = model, auc = eval.background@auc)
table_MIROC_2050 <- rbind(table_MIROC_2050, table.1)

write.csv(table_MIROC_2050, paste0("./Models/Threshold/MIROC_2050/tables/",
          stringr::str_remove(names(modelos)[1], "_gam") ,".csv"),
          row.names = FALSE)
  }
  
# Main table to save the results 2070 
table_MIROC_2070 <- data.frame()
for(model in names(models_MIROC_2070)){
val.presence <- val.evaluation.points_MIROC_2070[ , c(model,"presence")]
# Extract present and background points
presence.evaluation <- val.presence[val.presence$presence == 1 , model]
absence.evaluation <- val.presence[val.presence$presence == 0 , model]
# Evaluate
eval.background <- dismo::evaluate(p = presence.evaluation, 
                                   a = absence.evaluation)
# Threshold TPR_TNR
thre <- eval.background@t[which.max(eval.background@TPR + eval.background@TNR)]

model.thre <- models_MIROC_2070[[model]] > thre
writeRaster(model.thre, 
            filename = paste0("./Models/Threshold/MIROC_2070/raster/", model, ".tif"), 
            format = "ascii", overwrite = FALSE)

table.1 <- data.frame(model = model, auc = eval.background@auc)
table_MIROC_2070 <- rbind(table_MIROC_2070, table.1)

write.csv(table_MIROC_2070, paste0("./Models/Threshold/MIROC_2070/tables/",
          stringr::str_remove(names(modelos)[1], "_gam") ,".csv"),
          row.names = FALSE)
}
  
#-----------#
# NorESM1-M #
#-----------#
# Main table to save the results 2050 
table_NOR_2050 <- data.frame()
for(model in names(models_NOR_2050)){
val.presence <- val.evaluation.points_NOR_2050[ , c(model,"presence")]
# Extract present and background points
presence.evaluation <- val.presence[val.presence$presence == 1 , model]
absence.evaluation <- val.presence[val.presence$presence == 0 , model]
# Evaluate
eval.background <- dismo::evaluate(p = presence.evaluation, 
                                 a = absence.evaluation)
# Threshold TPR_TNR
thre <- eval.background@t[which.max(eval.background@TPR + eval.background@TNR)]

model.thre <- models_NOR_2050[[model]] > thre
writeRaster(model.thre, filename=paste0("./Models/Threshold/NOR_2050/raster/", model, ".tif"), 
                format = "ascii", overwrite = TRUE)

table.1 <- data.frame(model = model, auc = eval.background@auc)
    
table_NOR_2050 <- rbind(table_NOR_2050, table.1)

write.csv(table_NOR_2050, paste0("./Models/Threshold/NOR_2050/tables/",
          stringr::str_remove(names(modelos)[1], "_gam") ,".csv"),
          row.names = FALSE)
  }
  
# Main table to save the results 2070
table_NOR_2070 <- data.frame()
for(model in names(models_NOR_2070)){
val.presence <- val.evaluation.points_NOR_2070[ , c(model,"presence")]
# Extract present and background points
presence.evaluation <- val.presence[val.presence$presence == 1 , model]
absence.evaluation <- val.presence[val.presence$presence == 0 , model]
# Evaluate
eval.background <- dismo::evaluate(p = presence.evaluation, 
                                   a = absence.evaluation)
# Threshold TPR_TNR
thre <- eval.background@t[which.max(eval.background@TPR + eval.background@TNR)]

model.thre <- models_NOR_2070[[model]] > thre
writeRaster(model.thre, filename=paste0("./Models/Threshold/NOR_2070/raster/",modelo, ".tif"), 
                format = "ascii", overwrite = TRUE)

table.1 <- data.frame(model = model, auc = eval.background@auc)
    
table_NOR_2070 <- rbind(table_NOR_2070, table.1)

write.csv(table_NOR_2070, paste0("./Models/Threshold/NOR_2070/tables/",
          stringr::str_remove(names(modelos)[1], "_gam") ,".csv"),
          row.names = FALSE)
  }
  
#------------#
# BCC-CSM1-1 #
#------------#
# Main table to save the results 2050 
table_BCC_2050 <- data.frame()
for(model in names(models_BCC_2050)){
val.presence <- val.evaluation.points_BCC_2050[ , c(model,"presence")]
# Extract present and background points
presence.evaluation <- val.presence[val.presence$presence == 1 , model]
absence.evaluation <- val.presence[val.presence$presence == 0 , model]
# Evaluate
eval.background <- dismo::evaluate(p = presence.evaluation, 
                                   a = absence.evaluation)
# Threshold TPR_TNR
thre <- eval.background@t[which.max(eval.background@TPR + eval.background@TNR)]
    
model.thre <- models_BCC_2050[[model]] > thre
writeRaster(model.thre, 
            filename=paste0("./Models/Threshold/BCC_2050/raster/", model, ".tif"), 
            format = "ascii", overwrite = TRUE)

table.1 <- data.frame(model = model, auc = eval.background@auc)
    
table_BCC_2050 <- rbind(table_BCC_2050, table.1)
write.csv(table_BCC_2050, paste0("./Models/Threshold/BCC_2050/tables/",
          stringr::str_remove(names(models)[1], "_gam") ,".csv"),
          row.names = FALSE)
  }
  
# Main table to save the results 2070
table_BCC_2070 <- data.frame()
for(model in names(models_BCC_2070)){
val.presence <- val.evaluation.points_BCC_2070[ , c(model,"presence")]
# Extract present and background points
presence.evaluation <- val.presence[val.presence$presence == 1 , model]
absence.evaluation <- val.presence[val.presence$presence == 0 , model]
# Evaluate
eval.background <- dismo::evaluate(p = presence.evaluation, 
                                   a = absence.evaluation)
# Threshold TPR_TNR
thre <- eval.background@t[which.max(eval.background@TPR + eval.background@TNR)]
    
model.thre <- models_BCC_2070[[model]] > thre
writeRaster(model.thre, 
            filename=paste0("./Models/Threshold/BCC_2070/raster/", model, ".tif"), 
            format = "ascii", overwrite = TRUE)

table.1 <- data.frame(model = model, auc = eval.background@auc)
    
table_BCC_2070 <- rbind(table_BCC_2070, table.1)

write.csv(table_BCC_2070, 
          paste0("./Models/Threshold/BCC_2070/tables/",
          stringr::str_remove(names(models)[1], "_gam") ,".csv"),
          row.names = FALSE)
  }
  
}
