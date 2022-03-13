#####################
# MODELS EVALUATION #
#####################

# Main path
main.path <- "YOUR_PATH"
setwd(main.path)

# Create folder to save the data
dir.create("./Models/Evaluation_results_plot")
dir.create("./Models/Evaluation_results_table")

# Load the rasters of all training models
# Load training models
gam.modelos <- list.files(path="./Models/GAM/Map_raster/", pattern = ".asc", 
                          full.names = TRUE)
max.modelos <- list.files(path="./Models/Maxent/Map_raster/", pattern = ".asc", 
                          full.names = TRUE)
brt.modelos <- list.files(path="./Models/BRT/Map_raster/", pattern = ".asc", 
                          full.names = TRUE)
ens.modelos <- list.files(path="./Models/Ensemble/Map_raster/",pattern = ".asc", 
                          full.names = TRUE)

for(i in 1:length(gam.modelos)){
models <- stack(gam.modelos[i], brt.modelos[i], max.modelos[i], ens.modelos[i])

# Load the evaluation tables to evaluate the models
# Recuerda que tenemos presencias y ausencias de evaluación!!!!! Ahora lo necesitamos!!!
presence.absence.evaluationFiles <- list.files(path = "./Presence_Background/Evaluation/",
                                            pattern = ".csv", full.names = TRUE)

# Extract the species name in this way we are sure to pick the correct 
# evaluation table
species <- gsub("_", " ",stringr::str_remove(names(modelos)[1], "_gam"))

presence.absence.evaluationFiles <- presence.absence.evaluationFiles[
  grepl(species, presence.absence.evaluationFiles)]
presence.absence.evaluation <- read.csv(presence.absence.evaluationFiles)

# Extract the evaluation point on the map produced with the model training
val.evaluation.points <- as.data.frame(
  extract(models, presence.absence.evaluation[, c("x","y")]))

# Merge the presence - background column with the extracted model points
val.evaluation.points$presence <- presence.absence.evaluation$presence

# Create empty dataframe where we can save the evaluation results
evaluation.results <- data.frame(stringsAsFactors = FALSE)
# Row counter
row = 0

# Create pdf to save the plots
pdf(paste0("./Models/Evaluation_results_plot/", 
    stringr::str_remove(names(models)[1], "_gam"), ".pdf"), 
    width = 15, height = 8, pointsize = 30)

# For each model and each we create an evaluation
for (model in names(models)){
  
# Subset presence and background occurrences
val.presence <- val.evaluation.points[val.evaluation.points$presence == 1, model]
val.absence <- val.evaluation.points[val.evaluation.points$presence == 0, model]
  
# Model evaluation with dismo::evaluate and boyce index
evaluation <- dismo::evaluate(p = val.presence, a = val.absence)
  
# fit: A vector or Raster-Layer containing the predicted suitability values
# obs: A vector containing the predicted suitability values or xy-coordinates (if "fit" is a Raster-Layer) of the validation points (presence records)
evaluation.boyce <- ecospat::ecospat.boyce(models[[model]], 
                                           val.presence, PEplot = F)
row = row + 1
# Fill the table with the results
evaluation.results[row, "modelo"] <- model
evaluation.results[row, "auc"] <- evaluation@auc
evaluation.results[row, "cor"] <- evaluation@cor
evaluation.results[row, "boyce"] <- evaluation.boyce$Spearman.cor
  
# Plots
par(mfrow=c(1,3), mar=c(2,2,4,2), oma=c(3,3,5,3))
density(evaluation)
boxplot(evaluation, col=c("blue", "red"))
plot(evaluation, "ROC")
mtext(model, outer = TRUE, cex=1.3)
  }; rm(i)
dev.off()

# Reorder the table with the evaluation results
evaluation.results <- evaluation.results[order(evaluation.results$auc, 
                                               decreasing = TRUE), ]
# Save the table with evaluation results
write.csv(evaluation.results,paste0("./Models/Evaluation_results_table/", 
          stringr::str_remove(names(models)[1], "_gam"), ".csv"), 
          row.names = FALSE)
}

# Clean workspace
rm(list = ls())
