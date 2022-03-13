###########################
# Traits and phylogenesis #
###########################

# Load libraries
library(ape) # <- Genetic
library(splits) # <- Genetic
library(caper) # <- Genetic
library(phytools) # <- Genetic
library(MCMCglmm) # <- Multi-response Generalized Linear Mixed Models
library(xlsx) # <- Open .xlsx files
library(viridis) # <- Manage color
library(osfr) # <- Connect to OSF
library(scico)

# #####################
# # Connection to OSF #
# #####################
# 
main.path <- "YOUR_PATH"
# Create folder where you can download the files.
# For more information: ?dir.create
setwd(main.path)

# Connect to Open Science Framework
cr_project <- osf_retrieve_node("4p63t")
osf_files <- osf_ls_files(cr_project, type = "folder")

# Download outcomes of table phylogenetic analyses
file.number <- which(osf_ls_files(osf_files[1, ], pattern = "csv")[ ,1] == 
                        "PGLS_2022_03_01.csv")
osf_download(osf_ls_files(osf_files[1, ], pattern = "csv")[file.number, ], 
             path = main.path)
rm(file.number)

# Functions
'%ni%' <- Negate('%in%')


# Load phylogenetic tree
fict.phylo <- read.nexus("./tree_treeannotator.nex")
#inspect the resulting object:
str(fict.phylo)
fict.phylo$tip.label

# Load the traits table
traits <- read.xlsx(file="./Odonata_traits_SDM.xlsx", sheetIndex = 1)
head(traits)

# Load table with model results (e.g. diff. area)
table.model <- read.csv("./PGLS_2022_03_01.csv")
head(table.model)

# Create folder to save results
dir.create("./Ancestor_recostriction")



##############
# MIROC 2050 #
##############

# filter scenarios. Merge traits and variables
miroc2050 <- table.model[table.model$scenario == "miroc2050", ]
colnames(miroc2050)[10] <- "Species"
miroc2050.mg <- merge(traits, miroc2050, by = "Species", all.x = TRUE); rm(miroc2050)

# Check outliers
dotchart(miroc2050.mg$rel_n_cell, labels = miroc2050.mg$Species, cex = 0.6, main = "Relative area change")
dotchart(miroc2050.mg$DifavgAltitude, labels = miroc2050.mg$Species, cex = 0.6, main = "Altitude difference")
dotchart(miroc2050.mg$dif_cetroid, labels = miroc2050.mg$Species, cex = 0.6, main = "Centroid difference")

outliers_rel_n_cell <- c("Caliaeschna microstigma")
outliers_dif_cetroid <- c("Trithemis arteriosa")

miroc2050.noOut_rel_n_cell <- miroc2050.mg[miroc2050.mg$Species %ni% outliers_rel_n_cell, ]
dotchart(miroc2050.noOut_rel_n_cell$rel_n_cell, labels = miroc2050.noOut_rel_n_cell$Species, cex = 0.6, main = "Relative area change")

miroc2050.noOut_dif_cetroid <- miroc2050.mg[miroc2050.mg$Species %ni% outliers_dif_cetroid, ]
dotchart(miroc2050.noOut_dif_cetroid$dif_cetroid, labels = miroc2050.noOut_dif_cetroid$Species, cex = 0.6, main = "Centroid difference")

# Drop tip ---------------------------------------------------------------------
tree_rel_n_cell <- drop.tip(fict.phylo, c("Caliaeschna_microstigma"))
tree_dif_cetroid <- drop.tip(fict.phylo, c("Trithemis_arteriosa"))

#-----------------------------------#
# Ancestor character reconstruction #
#-----------------------------------#

# Relative area change MIROC 2050 -----
rel_n_cel_miroc2050 <- miroc2050.noOut_rel_n_cell$rel_n_cel
sp <- gsub(" ", "_", miroc2050.noOut_rel_n_cell$Species)
names(rel_n_cel_miroc2050) <- sp
rel_n_cel_miroc2050_1 <- rel_n_cel_miroc2050[tree_rel_n_cell$tip.label] # Riordino secondo lálbero filogenetico

obj <- contMap(tree_rel_n_cell, rel_n_cel_miroc2050_1, fsize = .3, scale = 1, lwd = 1.5, 
               tip.labels = TRUE, outline = TRUE, plot = TRUE)
object <- setMap(obj,scico(length(obj$cols), palette = "roma"))
plot(object)

pdf("./Relative_area_change_miroc2050.pdf")
plot(object)
title("Relative area change MIROC 2050")
dev.off()

# Per il plot del paper farlo con il body size

body_miroc2050 <- miroc2050.noOut_rel_n_cell$TotMax_mm
sp <- gsub(" ", "_", miroc2050.noOut_rel_n_cell$Species)
names(body_miroc2050) <- sp
body_miroc2050_1 <- body_miroc2050[tree_rel_n_cell$tip.label] # Riordino secondo lálbero filogenetico

obj <- contMap(tree_rel_n_cell, body_miroc2050_1, fsize = .3, scale = 1, lwd = 1.5, 
               tip.labels = TRUE, outline = TRUE, plot = TRUE)
object <- setMap(obj,scico(length(obj$cols), palette = "roma"))
plot(object)

pdf("./body_size_fig_paper.pdf")
plot(object)
title("Body size")
dev.off()


# Altitude difference MIROC 2050 

dif_alt_miroc2050 <- miroc2050.mg$DifavgAltitude
sp <- gsub(" ", "_", miroc2050.mg$Species)
names(dif_alt_miroc2050) <- sp
dif_alt_miroc2050_1 <-dif_alt_miroc2050[fict.phylo$tip.label] 

pdf("./Final_Paper/Scripts_PGLS/Ancestor_recostriction/Altitude_difference_miroc2050.pdf")
contMap(fict.phylo, dif_alt_miroc2050_1, fsize = .3, scale = 1, lwd = 1.5, 
        tip.labels = TRUE, outline = TRUE, plot = TRUE)
title("Altitude difference MIROC 2050")
dev.off()

# Centroid difference MIROC 2050 ----- 
dif_cetroid_miroc2050 <- miroc2050.noOut_dif_cetroid$dif_cetroid
sp <- gsub(" ", "_", miroc2050.noOut_dif_cetroid$Species)
names(dif_cetroid_miroc2050) <- sp
dif_cetroid_miroc2050_1 <- dif_cetroid_miroc2050[tree_dif_cetroid$tip.label] 

# Convert km to m
obj <- contMap(tree_dif_cetroid, dif_cetroid_miroc2050_1/1000, fsize = .3, scale = 1, lwd = 1.5, 
               tip.labels = TRUE, outline = TRUE, plot = TRUE)
object <- setMap(obj,scico(length(obj$cols), palette = "roma"))
plot(object)

pdf("./Final_Paper/Scripts_PGLS/Ancestor_recostriction/Centroid_difference_miroc2050_fig_paper.pdf")
plot(object)
title("Centroid difference MIROC 2050")
dev.off()


#---------------------#
# Phylogenetic signal #
#---------------------#

# Relative area change MIROC 2050 -----
phylosig(tree_rel_n_cell, miroc2050.noOut_rel_n_cell$rel_n_cel, "K", test = TRUE)
phylosig(tree_rel_n_cell, miroc2050.noOut_rel_n_cell$rel_n_cel, "lambda", test = TRUE)

# Altitude difference MIROC 2050 -----
phylosig(fict.phylo, miroc2050.mg$DifavgAltitude, "K", test = TRUE)
phylosig(fict.phylo, miroc2050.mg$DifavgAltitude, "lambda", test = TRUE)

# Centroid difference MIROC 2050 ----- 
phylosig(tree_dif_cetroid, miroc2050.noOut_dif_cetroid$dif_cetroid, "K", test = TRUE)
phylosig(tree_dif_cetroid, miroc2050.noOut_dif_cetroid$dif_cetroid, "lambda", test = TRUE)


#------#
# PGLS #
#------#

# Relative area change MIROC 2050 -----
str(miroc2050.noOut_rel_n_cell)
psych::pairs.panels(miroc2050.noOut_rel_n_cell[,c("TotMax_mm", "AbMax_mm", "HwMax_mm", "Habitat", "FlightSeason_month")])

miroc2050.noOut_rel_n_cell$Habitat <- as.factor(miroc2050.noOut_rel_n_cell$Habitat)
miroc2050.noOut_rel_n_cell$Species <- gsub(" ", "_", miroc2050.noOut_rel_n_cell$Species)
test.data  <-  comparative.data(phy = tree_rel_n_cell, data = miroc2050.noOut_rel_n_cell, names.col = Species, vcv = TRUE, vcv.dim = 3)

odo_model <- pgls(rel_n_cell ~ TotMax_mm + FlightSeason_month + Habitat, 
                data = test.data, kappa = "ML", lambda = "ML", delta = "ML")

pdf("./Relative_area_change_MIROC2050.pdf")
par(mfrow = c(2, 2))
hist(odo_model$phyres)
qqnorm(odo_model$phyres)
qqline(odo_model$phyres)
plot(x = fitted(odo_model), y = odo_model$phyres, pch = 19)
dev.off()
miroc2050.odo_model <- summary(odo_model)
miroc2050.odo_model <- as.data.frame(miroc2050.odo_model$coefficients)
miroc2050.odo_model$response_variable <- c("Relative area change")
miroc2050.odo_model$scenario <- "MIROC"
miroc2050.odo_model$period <- "2050"
write.csv(miroc2050.odo_model, "./Relative_area_change_MIROC2050.csv", row.names = F)

# Altitude difference MIROC 2050 -----
miroc2050.mg$Habitat <- as.factor(miroc2050.mg$Habitat)
miroc2050.mg$Species <- gsub(" ", "_", miroc2050.mg$Species)
test.data  <-  comparative.data(phy = fict.phylo, data = miroc2050.mg, names.col = Species, vcv = TRUE, vcv.dim = 3)

odo_model <- pgls(DifavgAltitude ~ TotMax_mm + FlightSeason_month + Habitat, 
                       data = test.data, kappa = "ML", lambda = "ML", delta = "ML")

pdf("./Altitude_difference_MIROC2050.pdf")
par(mfrow = c(2, 2))
hist(odo_model$phyres)
qqnorm(odo_model$phyres)
qqline(odo_model$phyres)
plot(x = fitted(odo_model), y = odo_model$phyres, pch = 19)
dev.off()
miroc2050.odo_model <- summary(odo_model)
miroc2050.odo_model <- as.data.frame(miroc2050.odo_model$coefficients)
miroc2050.odo_model$response_variable <- c("Altitude difference")
miroc2050.odo_model$scenario <- "MIROC"
miroc2050.odo_model$period <- "2050"
write.csv(miroc2050.odo_model, "./Altitude_difference_MIROC2050.csv", row.names = F)

# Centroid difference MIROC 2050 ----- 
miroc2050.noOut_dif_cetroid$Habitat <- as.factor(miroc2050.noOut_dif_cetroid$Habitat)
miroc2050.noOut_dif_cetroid$Species <- gsub(" ", "_", miroc2050.noOut_dif_cetroid$Species)
test.data  <-  comparative.data(phy = tree_dif_cetroid, data = miroc2050.noOut_dif_cetroid, names.col = Species, vcv = TRUE, vcv.dim = 3)

odo_model <- pgls(dif_cetroid ~ TotMax_mm + FlightSeason_month + Habitat, 
                  data = test.data, kappa = "ML", lambda = "ML", delta = "ML")

pdf("./Centroid_difference_MIROC2050.pdf")
par(mfrow = c(2, 2))
hist(odo_model$phyres)
qqnorm(odo_model$phyres)
qqline(odo_model$phyres)
plot(x = fitted(odo_model), y = odo_model$phyres, pch = 19)
dev.off()
miroc2050.odo_model <- summary(odo_model)
miroc2050.odo_model <- as.data.frame(miroc2050.odo_model$coefficients)
miroc2050.odo_model$response_variable <- c("Centroid difference")
miroc2050.odo_model$scenario <- "MIROC"
miroc2050.odo_model$period <- "2050"
write.csv(miroc2050.odo_model, "./Centroid_difference_MIROC2050.csv", row.names = F)

rm(miroc2050_k_rel_n_cel, miroc2050_lam_dif_alt, miroc2050_lam_rel_n_cel,
   miroc2050.mg, miroc2050.noOut_dif_cetroid, miroc2050.noOut_rel_n_cell,
   miroc2050.odo_model, miroc2050.rel_n_cell, dif_alt_miroc2050, dif_alt_miroc2050_1,
   dif_cetroid_miroc2050, dif_cetroid_miroc2050_1, rel_n_cel_miroc2050, rel_n_cel_miroc2050_1,
   tree_miroc_2050, tree_dif_cetroid, tree_rel_n_cell, odo_model, odo_rel_n_cell,
   outliers_dif_cetroid, outliers_rel_n_cell)

##############
# MIROC 2070 #
##############

# filter scenarios. Merge traits and variables
miroc2070 <- table.model[table.model$scenario == "miroc2070", ]
colnames(miroc2070)[10] <- "Species"
miroc2070.mg <- merge(traits, miroc2070, by = "Species", all.x = TRUE); rm(miroc2070)

# Check outliers
dotchart(miroc2070.mg$rel_n_cell, labels = miroc2070.mg$Species, cex = 0.6, main = "Relative area change")
dotchart(miroc2070.mg$DifavgAltitude, labels = miroc2070.mg$Species, cex = 0.6, main = "Altitude difference")
dotchart(miroc2070.mg$dif_cetroid, labels = miroc2070.mg$Species, cex = 0.6, main = "Centroid difference")

outliers_rel_n_cell <- c("Caliaeschna microstigma")
outliers_dif_cetroid <- c("Trithemis arteriosa")

miroc2070.noOut_rel_n_cell <- miroc2070.mg[miroc2070.mg$Species %ni% outliers_rel_n_cell, ]
dotchart(miroc2070.noOut_rel_n_cell$rel_n_cell, labels = miroc2070.noOut_rel_n_cell$Species, cex = 0.6, main = "Relative area change")

miroc2070.noOut_dif_cetroid <- miroc2070.mg[miroc2070.mg$Species %ni% outliers_dif_cetroid, ]
dotchart(miroc2070.noOut_dif_cetroid$dif_cetroid, labels = miroc2070.noOut_dif_cetroid$Species, cex = 0.6, main = "Centroid difference")

# Drop tip ---------------------------------------------------------------------
tree_rel_n_cell <- drop.tip(fict.phylo, "Caliaeschna_microstigma")
tree_dif_cetroid <- drop.tip(fict.phylo, "Trithemis_arteriosa")

#-----------------------------------#
# Ancestor character reconstruction #
#-----------------------------------#

# Relative area change MIROC 2070 -----
rel_n_cel_miroc2070 <- miroc2070.noOut_rel_n_cell$rel_n_cel
sp <- gsub(" ", "_", miroc2070.noOut_rel_n_cell$Species)
names(rel_n_cel_miroc2070) <- sp
rel_n_cel_miroc2070_1 <- rel_n_cel_miroc2070[tree_rel_n_cell$tip.label] # Riordino secondo lálbero filogenetico

pdf("./Relative_area_change_miroc2070.pdf")
contMap(tree_rel_n_cell, rel_n_cel_miroc2070_1, fsize = .3, scale = 1, lwd = 1.5, 
        tip.labels = TRUE, outline = TRUE, plot = TRUE)
title("Relative area change MIROC 2070")
dev.off()

# Altitude difference MIROC 2050 ----- Non abbiamo eliminato outlier per questo utilizziamo in df miroc2050.mg e l'albero fict.phylo
dif_alt_miroc2070 <- miroc2070.mg$DifavgAltitude
sp <- gsub(" ", "_", miroc2070.mg$Species)
names(dif_alt_miroc2070) <- sp
dif_alt_miroc2070_1 <-dif_alt_miroc2070[fict.phylo$tip.label] # Riordino secondo lálbero filogenetico

pdf("./Altitude_difference_miroc2070.pdf")
contMap(fict.phylo, dif_alt_miroc2070_1, fsize = .3, scale = 1, lwd = 1.5, 
        tip.labels = TRUE, outline = TRUE, plot = TRUE)
title("Altitude difference MIROC 2070")
dev.off()

# Centroid difference MIROC 2050 ----- 
dif_cetroid_miroc2070 <- miroc2070.noOut_dif_cetroid$dif_cetroid
sp <- gsub(" ", "_", miroc2070.noOut_dif_cetroid$Species)
names(dif_cetroid_miroc2070) <- sp
dif_cetroid_miroc2070_1 <- dif_cetroid_miroc2070[tree_dif_cetroid$tip.label] # Riordino secondo lálbero filogenetico

pdf("./Centroid_difference_miroc2070.pdf")
contMap(tree_dif_cetroid, dif_cetroid_miroc2070_1, fsize = .3, scale = 1, lwd = 1.5, 
        tip.labels = TRUE, outline = TRUE, plot = TRUE)
title("Centroid difference MIROC 2070")
dev.off()


#---------------------#
# Phylogenetic signal #
#---------------------#

# Relative area change MIROC 2050 -----
phylosig(tree_rel_n_cell, miroc2070.noOut_rel_n_cell$rel_n_cel, "K", test = TRUE)
phylosig(tree_rel_n_cell, miroc2070.noOut_rel_n_cell$rel_n_cel, "lambda", test = TRUE)

# Altitude difference MIROC 2050 -----
phylosig(fict.phylo, miroc2070.mg$DifavgAltitude, "K", test = TRUE)
phylosig(fict.phylo, miroc2070.mg$DifavgAltitude, "lambda", test = TRUE)

# Centroid difference MIROC 2050 ----- 
phylosig(tree_dif_cetroid, miroc2070.noOut_dif_cetroid$dif_cetroid, "K", test = TRUE)
phylosig(tree_dif_cetroid, miroc2070.noOut_dif_cetroid$dif_cetroid, "lambda", test = TRUE)


#------#
# PGLS #
#------#

# Relative area change MIROC 2070 -----
# str(miroc2070.noOut_rel_n_cell)
# psych::pairs.panels(miroc2070.noOut_rel_n_cell[,c("TotMax_mm", "AbMax_mm", "HwMax_mm", "Habitat", "FlightSeason_month")])

miroc2070.noOut_rel_n_cell$Habitat <- as.factor(miroc2070.noOut_rel_n_cell$Habitat)
miroc2070.noOut_rel_n_cell$Species <- gsub(" ", "_", miroc2070.noOut_rel_n_cell$Species)
test.data  <-  comparative.data(phy = tree_rel_n_cell, data = miroc2070.noOut_rel_n_cell, names.col = Species, vcv = TRUE, vcv.dim = 3)

odo_model <- pgls(rel_n_cell ~ TotMax_mm + FlightSeason_month + Habitat, 
                  data = test.data, kappa = "ML", lambda = "ML", delta = "ML")

pdf("./Relative_area_change_MIROC2070.pdf")
par(mfrow = c(2, 2))
hist(odo_model$phyres)
qqnorm(odo_model$phyres)
qqline(odo_model$phyres)
plot(x = fitted(odo_model), y = odo_model$phyres, pch = 19)
dev.off()
miroc2070.odo_model <- summary(odo_model)
miroc2070.odo_model <- as.data.frame(miroc2070.odo_model$coefficients)
miroc2070.odo_model$response_variable <- c("Relative area change")
miroc2070.odo_model$scenario <- "MIROC"
miroc2070.odo_model$period <- "2070"
write.csv(miroc2070.odo_model, "./Relative_area_change_MIROC2070.csv", row.names = F)

# Altitude difference MIROC 2070 -----
miroc2070.mg$Habitat <- as.factor(miroc2070.mg$Habitat)
miroc2070.mg$Species <- gsub(" ", "_", miroc2070.mg$Species)
test.data  <-  comparative.data(phy = fict.phylo, data = miroc2070.mg, names.col = Species, vcv = TRUE, vcv.dim = 3)

odo_model <- pgls(DifavgAltitude ~ TotMax_mm + FlightSeason_month + Habitat, 
                  data = test.data, kappa = "ML", lambda = "ML", delta = "ML")

pdf("./Altitude_difference_MIROC2070.pdf")
par(mfrow = c(2, 2))
hist(odo_model$phyres)
qqnorm(odo_model$phyres)
qqline(odo_model$phyres)
plot(x = fitted(odo_model), y = odo_model$phyres, pch = 19)
dev.off()
miroc2070.odo_model <- summary(odo_model)
miroc2070.odo_model <- as.data.frame(miroc2070.odo_model$coefficients)
miroc2070.odo_model$response_variable <- c("Altitude difference")
miroc2070.odo_model$scenario <- "MIROC"
miroc2070.odo_model$period <- "2070"
write.csv(miroc2070.odo_model, "./Altitude_difference_MIROC2070.csv", row.names = F)

# Centroid difference MIROC 2070 ----- 
miroc2070.noOut_dif_cetroid$Habitat <- as.factor(miroc2070.noOut_dif_cetroid$Habitat)
miroc2070.noOut_dif_cetroid$Species <- gsub(" ", "_", miroc2070.noOut_dif_cetroid$Species)
test.data  <-  comparative.data(phy = tree_dif_cetroid, data = miroc2070.noOut_dif_cetroid, names.col = Species, vcv = TRUE, vcv.dim = 3)

odo_model <- pgls(dif_cetroid ~ TotMax_mm + FlightSeason_month + Habitat, 
                  data = test.data, kappa = "ML", lambda = "ML", delta = "ML")

pdf("./Centroid_difference_MIROC2070.pdf")
par(mfrow = c(2, 2))
hist(odo_model$phyres)
qqnorm(odo_model$phyres)
qqline(odo_model$phyres)
plot(x = fitted(odo_model), y = odo_model$phyres, pch = 19)
dev.off()
miroc2070.odo_model <- summary(odo_model)
miroc2070.odo_model <- as.data.frame(miroc2070.odo_model$coefficients)
miroc2070.odo_model$response_variable <- c("Centroid difference")
miroc2070.odo_model$scenario <- "MIROC"
miroc2070.odo_model$period <- "2070"
write.csv(miroc2070.odo_model, "./Centroid_difference_MIROC2070.csv", row.names = F)

rm(miroc2070_k_rel_n_cel, miroc2070_lam_dif_alt, miroc2070_lam_rel_n_cel,
   miroc2070.mg, miroc2070.noOut_dif_cetroid, miroc2070.noOut_rel_n_cell,
   miroc2070.odo_model, miroc2070.rel_n_cell, dif_alt_miroc2070, dif_alt_miroc2070_1,
   dif_cetroid_miroc2070, dif_cetroid_miroc2070_1, rel_n_cel_miroc2070, rel_n_cel_miroc2070_1,
   tree_miroc_2070, tree_dif_cetroid, tree_rel_n_cell, odo_model, odo_rel_n_cell,
   outliers_dif_cetroid, outliers_rel_n_cell)

############
# NOR 2050 #
############

# filter scenarios. Merge traits and variables
nor2050 <- table.model[table.model$scenario == "nor2050", ]
colnames(nor2050)[10] <- "Species"
nor2050.mg <- merge(traits, nor2050, by = "Species", all.x = TRUE); rm(nor2050)

# Check outliers
dotchart(nor2050.mg$rel_n_cell, labels = nor2050.mg$Species, cex = 0.6, main = "Relative area change")
dotchart(nor2050.mg$DifavgAltitude, labels = nor2050.mg$Species, cex = 0.6, main = "Altitude difference")
dotchart(nor2050.mg$dif_cetroid, labels = nor2050.mg$Species, cex = 0.6, main = "Centroid difference")

outliers_rel_n_cell <- c("Trithemis arteriosa")
outliers_dif_cetroid <- c("Trithemis arteriosa")

nor2050.noOut_rel_n_cell <- nor2050.mg[nor2050.mg$Species %ni% outliers_rel_n_cell, ]
dotchart(nor2050.noOut_rel_n_cell$rel_n_cell, labels = nor2050.noOut_rel_n_cell$Species, cex = 0.6, main = "Relative area change")

nor2050.noOut_dif_cetroid <- nor2050.mg[nor2050.mg$Species %ni% outliers_dif_cetroid, ]
dotchart(nor2050.noOut_dif_cetroid$dif_cetroid, labels = nor2050.noOut_dif_cetroid$Species, cex = 0.6, main = "Centroid difference")

# Drop tip ---------------------------------------------------------------------
tree_rel_n_cell <- drop.tip(fict.phylo, "Trithemis_arteriosa")
tree_dif_cetroid <- drop.tip(fict.phylo, "Trithemis_arteriosa")

#-----------------------------------#
# Ancestor character reconstruction #
#-----------------------------------#

# Relative area change NOR 2050 -----
rel_n_cel_nor2050 <- nor2050.noOut_rel_n_cell$rel_n_cel
sp <- gsub(" ", "_", nor2050.noOut_rel_n_cell$Species)
names(rel_n_cel_nor2050) <- sp
rel_n_cel_nor2050_1 <- rel_n_cel_nor2050[tree_rel_n_cell$tip.label] # Riordino secondo lálbero filogenetico

pdf("./Relative_area_change_nor2050.pdf")
contMap(tree_rel_n_cell, rel_n_cel_nor2050_1, fsize = .3, scale = 1, lwd = 1.5, 
        tip.labels = TRUE, outline = TRUE, plot = TRUE)
title("Relative area change NOR 2050")
dev.off()

# Altitude difference NOR 2050 ----- 
dif_alt_nor2050 <- nor2050.mg$DifavgAltitude
sp <- gsub(" ", "_", nor2050.mg$Species)
names(dif_alt_nor2050) <- sp
dif_alt_nor2050_1 <-dif_alt_nor2050[fict.phylo$tip.label] # Riordino secondo lálbero filogenetico

pdf("./Altitude_difference_nor2050.pdf")
contMap(fict.phylo, dif_alt_nor2050_1, fsize = .3, scale = 1, lwd = 1.5, 
        tip.labels = TRUE, outline = TRUE, plot = TRUE)
title("Altitude difference NOR 2050")
dev.off()

# Centroid difference NOR 2050 ----- 
dif_cetroid_nor2050 <- nor2050.noOut_dif_cetroid$dif_cetroid
sp <- gsub(" ", "_", nor2050.noOut_dif_cetroid$Species)
names(dif_cetroid_nor2050) <- sp
dif_cetroid_nor2050_1 <- dif_cetroid_nor2050[tree_dif_cetroid$tip.label] # Riordino secondo lálbero filogenetico

pdf("./Centroid_difference_nor2050.pdf")
contMap(tree_dif_cetroid, dif_cetroid_nor2050_1, fsize = .3, scale = 1, lwd = 1.5, 
        tip.labels = TRUE, outline = TRUE, plot = TRUE)
title("Centroid difference NOR 2050")
dev.off()


#---------------------#
# Phylogenetic signal #
#---------------------#

# Relative area change MIROC 2050 -----
phylosig(tree_rel_n_cell, nor2050.noOut_rel_n_cell$rel_n_cel, "K", test = TRUE)
phylosig(tree_rel_n_cell, nor2050.noOut_rel_n_cell$rel_n_cel, "lambda", test = TRUE)

# Altitude difference MIROC 2050 -----
phylosig(fict.phylo, nor2050.mg$DifavgAltitude, "K", test = TRUE)
phylosig(fict.phylo, nor2050.mg$DifavgAltitude, "lambda", test = TRUE)

# Centroid difference MIROC 2050 ----- 
phylosig(tree_dif_cetroid, nor2050.noOut_dif_cetroid$dif_cetroid, "K", test = TRUE)
phylosig(tree_dif_cetroid, nor2050.noOut_dif_cetroid$dif_cetroid, "lambda", test = TRUE)


#------#
# PGLS #
#------#

# Relative area change NOR 2050 -----
# str(miroc2070.noOut_rel_n_cell)
# psych::pairs.panels(miroc2070.noOut_rel_n_cell[,c("TotMax_mm", "AbMax_mm", "HwMax_mm", "Habitat", "FlightSeason_month")])

nor2050.noOut_rel_n_cell$Habitat <- as.factor(nor2050.noOut_rel_n_cell$Habitat)
nor2050.noOut_rel_n_cell$Species <- gsub(" ", "_", nor2050.noOut_rel_n_cell$Species)
test.data  <-  comparative.data(phy = tree_rel_n_cell, data = nor2050.noOut_rel_n_cell, names.col = Species, vcv = TRUE, vcv.dim = 3)

odo_model <- pgls(rel_n_cell ~ TotMax_mm + FlightSeason_month + Habitat, 
                  data = test.data, kappa = "ML", lambda = "ML", delta = "ML")

pdf("./Relative_area_change_NOR2050.pdf")
par(mfrow = c(2, 2))
hist(odo_model$phyres)
qqnorm(odo_model$phyres)
qqline(odo_model$phyres)
plot(x = fitted(odo_model), y = odo_model$phyres, pch = 19)
dev.off()
nor2050.odo_model <- summary(odo_model)
nor2050.odo_model <- as.data.frame(nor2050.odo_model$coefficients)
nor2050.odo_model$response_variable <- c("Relative area change")
nor2050.odo_model$scenario <- "NOR"
nor2050.odo_model$period <- "2050"
write.csv(nor2050.odo_model, "./Relative_area_change_NOR2050.csv", row.names = F)

# Altitude difference NOR 2050 -----
nor2050.mg$Habitat <- as.factor(nor2050.mg$Habitat)
nor2050.mg$Species <- gsub(" ", "_", nor2050.mg$Species)
test.data  <-  comparative.data(phy = fict.phylo, data = nor2050.mg, names.col = Species, vcv = TRUE, vcv.dim = 3)

odo_model <- pgls(DifavgAltitude ~ TotMax_mm + FlightSeason_month + Habitat, 
                  data = test.data, kappa = "ML", lambda = "ML", delta = "ML")

pdf("./Altitude_difference_NOR2050.pdf")
par(mfrow = c(2, 2))
hist(odo_model$phyres)
qqnorm(odo_model$phyres)
qqline(odo_model$phyres)
plot(x = fitted(odo_model), y = odo_model$phyres, pch = 19)
dev.off()
nor2050.odo_model <- summary(odo_model)
nor2050.odo_model <- as.data.frame(nor2050.odo_model$coefficients)
nor2050.odo_model$response_variable <- c("Altitude difference")
nor2050.odo_model$scenario <- "NOR"
nor2050.odo_model$period <- "2050"
write.csv(nor2050.odo_model, "./Altitude_difference_NOR2050.csv", row.names = F)

# Centroid difference NOR 2050 ----- 
nor2050.noOut_dif_cetroid$Habitat <- as.factor(nor2050.noOut_dif_cetroid$Habitat)
nor2050.noOut_dif_cetroid$Species <- gsub(" ", "_", nor2050.noOut_dif_cetroid$Species)
test.data  <-  comparative.data(phy = tree_dif_cetroid, data = nor2050.noOut_dif_cetroid, names.col = Species, vcv = TRUE, vcv.dim = 3)

odo_model <- pgls(dif_cetroid ~ TotMax_mm + FlightSeason_month + Habitat, 
                  data = test.data, kappa = "ML", lambda = "ML", delta = "ML")

pdf("./Centroid_difference_NOR2050.pdf")
par(mfrow = c(2, 2))
hist(odo_model$phyres)
qqnorm(odo_model$phyres)
qqline(odo_model$phyres)
plot(x = fitted(odo_model), y = odo_model$phyres, pch = 19)
dev.off()
nor2050.odo_model <- summary(odo_model)
nor2050.odo_model <- as.data.frame(nor2050.odo_model$coefficients)
nor2050.odo_model$response_variable <- c("Centroid difference")
nor2050.odo_model$scenario <- "NOR"
nor2050.odo_model$period <- "2050"
write.csv(nor2050.odo_model, "./Centroid_difference_NOR2050.csv", row.names = F)

rm(nor2050_k_rel_n_cel, nor2050_lam_dif_alt, nor2050_lam_rel_n_cel,
   nor2050.mg, nor2050.noOut_dif_cetroid, nor2050.noOut_rel_n_cell,
   nor2050.odo_model, nor2050.rel_n_cell, dif_alt_nor2050, dif_alt_nor2050_1,
   dif_cetroid_nor2050, dif_cetroid_nor2050_1, rel_n_cel_nor2050, rel_n_cel_nor2050_1,
   tree_miroc_2070, tree_dif_cetroid, tree_rel_n_cell, odo_model, odo_rel_n_cell,
   outliers_dif_cetroid, outliers_rel_n_cell)


############
# NOR 2070 #
############

# filter scenarios. Merge traits and variables
nor2070 <- table.model[table.model$scenario == "nor2070", ]
colnames(nor2070)[10] <- "Species"
nor2070.mg <- merge(traits, nor2070, by = "Species", all.x = TRUE); rm(nor2070)

# Check outliers
dotchart(nor2070.mg$rel_n_cell, labels = nor2070.mg$Species, cex = 0.6, main = "Relative area change")
dotchart(nor2070.mg$DifavgAltitude, labels = nor2070.mg$Species, cex = 0.6, main = "Altitude difference")
dotchart(nor2070.mg$dif_cetroid, labels = nor2070.mg$Species, cex = 0.6, main = "Centroid difference")

outliers_rel_n_cell <- c("Macromia splendens", "Trithemis arteriosa", "Coenagrion ornatum")
outliers_dif_cetroid <- c("Trithemis arteriosa")

nor2070.noOut_rel_n_cell <- nor2070.mg[nor2070.mg$Species %ni% outliers_rel_n_cell, ]
dotchart(nor2070.noOut_rel_n_cell$rel_n_cell, labels = nor2070.noOut_rel_n_cell$Species, cex = 0.6, main = "Relative area change")

nor2070.noOut_dif_cetroid <- nor2070.mg[nor2070.mg$Species %ni% outliers_dif_cetroid, ]
dotchart(nor2070.noOut_dif_cetroid$dif_cetroid, labels = nor2070.noOut_dif_cetroid$Species, cex = 0.6, main = "Centroid difference")

# Drop tip ---------------------------------------------------------------------
tree_rel_n_cell <- drop.tip(fict.phylo, c("Macromia_splendens", "Trithemis_arteriosa", "Coenagrion_ornatum"))
tree_dif_cetroid <- drop.tip(fict.phylo, "Trithemis_arteriosa")

#-----------------------------------#
# Ancestor character reconstruction #
#-----------------------------------#

# Relative area change NOR 2070 -----
rel_n_cel_nor2070 <- nor2070.noOut_rel_n_cell$rel_n_cel
sp <- gsub(" ", "_", nor2070.noOut_rel_n_cell$Species)
names(rel_n_cel_nor2070) <- sp
rel_n_cel_nor2070_1 <- rel_n_cel_nor2070[tree_rel_n_cell$tip.label] # Riordino secondo lálbero filogenetico

pdf("./Relative_area_change_nor2070.pdf")
contMap(tree_rel_n_cell, rel_n_cel_nor2070_1, fsize = .3, scale = 1, lwd = 1.5, 
        tip.labels = TRUE, outline = TRUE, plot = TRUE)
title("Relative area change NOR 2070")
dev.off()

# Altitude difference NOR 2050 ----- 
dif_alt_nor2070 <- nor2070.mg$DifavgAltitude
sp <- gsub(" ", "_", nor2070.mg$Species)
names(dif_alt_nor2070) <- sp
dif_alt_nor2070_1 <-dif_alt_nor2070[fict.phylo$tip.label] # Riordino secondo lálbero filogenetico

pdf("./Altitude_difference_nor2070.pdf")
contMap(fict.phylo, dif_alt_nor2070_1, fsize = .3, scale = 1, lwd = 1.5, 
        tip.labels = TRUE, outline = TRUE, plot = TRUE)
title("Altitude difference NOR 2070")
dev.off()

# Centroid difference NOR 2050 ----- 
dif_cetroid_nor2070 <- nor2070.noOut_dif_cetroid$dif_cetroid
sp <- gsub(" ", "_", nor2070.noOut_dif_cetroid$Species)
names(dif_cetroid_nor2070) <- sp
dif_cetroid_nor2070_1 <- dif_cetroid_nor2070[tree_dif_cetroid$tip.label] # Riordino secondo lálbero filogenetico

pdf("./Centroid_difference_nor2070.pdf")
contMap(tree_dif_cetroid, dif_cetroid_nor2070_1, fsize = .3, scale = 1, lwd = 1.5, 
        tip.labels = TRUE, outline = TRUE, plot = TRUE)
title("Centroid difference NOR 2070")
dev.off()


#---------------------#
# Phylogenetic signal #
#---------------------#

# Relative area change MIROC 2050 -----
phylosig(tree_rel_n_cell, nor2070.noOut_rel_n_cell$rel_n_cel, "K", test = TRUE)
phylosig(tree_rel_n_cell, nor2070.noOut_rel_n_cell$rel_n_cel, "lambda", test = TRUE)

# Altitude difference MIROC 2050 -----
phylosig(fict.phylo, nor2070.mg$DifavgAltitude, "K", test = TRUE)
phylosig(fict.phylo, nor2070.mg$DifavgAltitude, "lambda", test = TRUE)

# Centroid difference MIROC 2050 ----- 
phylosig(tree_dif_cetroid, nor2070.noOut_dif_cetroid$dif_cetroid, "K", test = TRUE)
phylosig(tree_dif_cetroid, nor2070.noOut_dif_cetroid$dif_cetroid, "lambda", test = TRUE)


#------#
# PGLS #
#------#

# Relative area change NOR 2070 -----
# str(miroc2070.noOut_rel_n_cell)
# psych::pairs.panels(miroc2070.noOut_rel_n_cell[,c("TotMax_mm", "AbMax_mm", "HwMax_mm", "Habitat", "FlightSeason_month")])

nor2070.noOut_rel_n_cell$Habitat <- as.factor(nor2070.noOut_rel_n_cell$Habitat)
nor2070.noOut_rel_n_cell$Species <- gsub(" ", "_", nor2070.noOut_rel_n_cell$Species)
test.data  <-  comparative.data(phy = tree_rel_n_cell, data = nor2070.noOut_rel_n_cell, names.col = Species, vcv = TRUE, vcv.dim = 3)

odo_model <- pgls(rel_n_cell ~ TotMax_mm + FlightSeason_month + Habitat, 
                  data = test.data, kappa = "ML", lambda = "ML", delta = "ML")

pdf("./Relative_area_change_NOR2070.pdf")
par(mfrow = c(2, 2))
hist(odo_model$phyres)
qqnorm(odo_model$phyres)
qqline(odo_model$phyres)
plot(x = fitted(odo_model), y = odo_model$phyres, pch = 19)
dev.off()
nor2070.odo_model <- summary(odo_model)
nor2070.odo_model <- as.data.frame(nor2070.odo_model$coefficients)
nor2070.odo_model$response_variable <- c("Relative area change")
nor2070.odo_model$scenario <- "NOR"
nor2070.odo_model$period <- "2070"
write.csv(nor2070.odo_model, "./Relative_area_change_NOR2070.csv", row.names = F)

# Altitude difference NOR 2070 -----
nor2070.mg$Habitat <- as.factor(nor2070.mg$Habitat)
nor2070.mg$Species <- gsub(" ", "_", nor2070.mg$Species)
test.data  <-  comparative.data(phy = fict.phylo, data = nor2070.mg, names.col = Species, vcv = TRUE, vcv.dim = 3)

odo_model <- pgls(DifavgAltitude ~ TotMax_mm + FlightSeason_month + Habitat, 
                  data = test.data, kappa = "ML", lambda = "ML", delta = "ML")

pdf("./Altitude_difference_NOR2070.pdf")
par(mfrow = c(2, 2))
hist(odo_model$phyres)
qqnorm(odo_model$phyres)
qqline(odo_model$phyres)
plot(x = fitted(odo_model), y = odo_model$phyres, pch = 19)
dev.off()
nor2070.odo_model <- summary(odo_model)
nor2070.odo_model <- as.data.frame(nor2070.odo_model$coefficients)
nor2070.odo_model$response_variable <- c("Altitude difference")
nor2070.odo_model$scenario <- "NOR"
nor2070.odo_model$period <- "2070"
write.csv(nor2070.odo_model, "./Altitude_difference_NOR2070.csv", row.names = F)

# Centroid difference NOR 2050 ----- 
nor2070.noOut_dif_cetroid$Habitat <- as.factor(nor2070.noOut_dif_cetroid$Habitat)
nor2070.noOut_dif_cetroid$Species <- gsub(" ", "_", nor2070.noOut_dif_cetroid$Species)
test.data  <-  comparative.data(phy = tree_dif_cetroid, data = nor2070.noOut_dif_cetroid, names.col = Species, vcv = TRUE, vcv.dim = 3)

odo_model <- pgls(dif_cetroid ~ TotMax_mm + FlightSeason_month + Habitat, 
                  data = test.data, kappa = "ML", lambda = "ML", delta = "ML")

pdf("./Centroid_difference_NOR2070.pdf")
par(mfrow = c(2, 2))
hist(odo_model$phyres)
qqnorm(odo_model$phyres)
qqline(odo_model$phyres)
plot(x = fitted(odo_model), y = odo_model$phyres, pch = 19)
dev.off()
nor2070.odo_model <- summary(odo_model)
nor2070.odo_model <- as.data.frame(nor2070.odo_model$coefficients)
nor2070.odo_model$response_variable <- c("Centroid difference")
nor2070.odo_model$scenario <- "NOR"
nor2070.odo_model$period <- "2070"
write.csv(nor2070.odo_model, "./Centroid_difference_NOR2070.csv", row.names = F)

rm(nor2070_k_rel_n_cel, nor2070_lam_dif_alt, nor2070_lam_rel_n_cel,
   nor2070.mg, nor2070.noOut_dif_cetroid, nor2070.noOut_rel_n_cell,
   nor2070.odo_model, nor2070.rel_n_cell, dif_alt_nor2070, dif_alt_nor2070_1,
   dif_cetroid_nor2070, dif_cetroid_nor2070_1, rel_n_cel_nor2070, rel_n_cel_nor2070_1,
   tree_miroc_2070, tree_dif_cetroid, tree_rel_n_cell, odo_model, odo_rel_n_cell,
   outliers_dif_cetroid, outliers_rel_n_cell)


############
# BCC 2050 #
############

# filter scenarios. Merge traits and variables
bcc2050 <- table.model[table.model$scenario == "bcc2050", ]
colnames(bcc2050)[10] <- "Species"
bcc2050.mg <- merge(traits, bcc2050, by = "Species", all.x = TRUE); rm(bcc2050)

# Check outliers
dotchart(bcc2050.mg$rel_n_cell, labels = bcc2050.mg$Species, cex = 0.6, main = "Relative area change")
dotchart(bcc2050.mg$DifavgAltitude, labels = bcc2050.mg$Species, cex = 0.6, main = "Altitude difference")
dotchart(bcc2050.mg$dif_cetroid, labels = bcc2050.mg$Species, cex = 0.6, main = "Centroid difference")

outliers_rel_n_cell <- c("Trithemis arteriosa", "Coenagrion ornatum")
outliers_dif_cetroid <- c("Trithemis arteriosa")

bcc2050.noOut_rel_n_cell <- bcc2050.mg[bcc2050.mg$Species %ni% outliers_rel_n_cell, ]
dotchart(bcc2050.noOut_rel_n_cell$rel_n_cell, labels = bcc2050.noOut_rel_n_cell$Species, cex = 0.6, main = "Relative area change")

bcc2050.noOut_dif_cetroid <- bcc2050.mg[bcc2050.mg$Species %ni% outliers_dif_cetroid, ]
dotchart(bcc2050.noOut_dif_cetroid$dif_cetroid, labels = bcc2050.noOut_dif_cetroid$Species, cex = 0.6, main = "Centroid difference")

# Drop tip ---------------------------------------------------------------------
tree_rel_n_cell <- drop.tip(fict.phylo, c("Trithemis_arteriosa", "Coenagrion_ornatum"))
tree_dif_cetroid <- drop.tip(fict.phylo, "Trithemis_arteriosa")

#-----------------------------------#
# Ancestor character reconstruction #
#-----------------------------------#

# Relative area change BCC 2050 -----
rel_n_cel_bcc2050 <- bcc2050.noOut_rel_n_cell$rel_n_cel
sp <- gsub(" ", "_", bcc2050.noOut_rel_n_cell$Species)
names(rel_n_cel_bcc2050) <- sp
rel_n_cel_bcc2050_1 <- rel_n_cel_bcc2050[tree_rel_n_cell$tip.label] # Riordino secondo lálbero filogenetico

pdf("./Relative_area_change_bcc2050.pdf")
contMap(tree_rel_n_cell, rel_n_cel_bcc2050_1, fsize = .3, scale = 1, lwd = 1.5, 
        tip.labels = TRUE, outline = TRUE, plot = TRUE)
title("Relative area change BCC 2050")
dev.off()

# Altitude difference BCC 2050 ----- 
dif_alt_bcc2050 <- bcc2050.mg$DifavgAltitude
sp <- gsub(" ", "_", bcc2050.mg$Species)
names(dif_alt_bcc2050) <- sp
dif_alt_bcc2050_1 <-dif_alt_bcc2050[fict.phylo$tip.label] # Riordino secondo lálbero filogenetico

pdf("./Altitude_difference_bcc2050.pdf")
contMap(fict.phylo, dif_alt_bcc2050_1, fsize = .3, scale = 1, lwd = 1.5, 
        tip.labels = TRUE, outline = TRUE, plot = TRUE)
title("Altitude difference BCC 2050")
dev.off()

# Centroid difference BCC 2050 ----- 
dif_cetroid_bcc2050 <- bcc2050.noOut_dif_cetroid$dif_cetroid
sp <- gsub(" ", "_", bcc2050.noOut_dif_cetroid$Species)
names(dif_cetroid_bcc2050) <- sp
dif_cetroid_bcc2050_1 <- dif_cetroid_bcc2050[tree_dif_cetroid$tip.label] # Riordino secondo lálbero filogenetico

pdf("./Centroid_difference_bcc2050.pdf")
contMap(tree_dif_cetroid, dif_cetroid_bcc2050_1, fsize = .3, scale = 1, lwd = 1.5, 
        tip.labels = TRUE, outline = TRUE, plot = TRUE)
title("Centroid difference BCC 2050")
dev.off()


#---------------------#
# Phylogenetic signal #
#---------------------#

# Relative area change BCC 2050 -----
phylosig(tree_rel_n_cell, bcc2050.noOut_rel_n_cell$rel_n_cel, "K", test = TRUE)
phylosig(tree_rel_n_cell, bcc2050.noOut_rel_n_cell$rel_n_cel, "lambda", test = TRUE)

# Altitude difference BCC 2050 -----
phylosig(fict.phylo, bcc2050.mg$DifavgAltitude, "K", test = TRUE)
phylosig(fict.phylo, bcc2050.mg$DifavgAltitude, "lambda", test = TRUE)

# Centroid difference BCC 2050 ----- 
phylosig(tree_dif_cetroid, bcc2050.noOut_dif_cetroid$dif_cetroid, "K", test = TRUE)
phylosig(tree_dif_cetroid, bcc2050.noOut_dif_cetroid$dif_cetroid, "lambda", test = TRUE)


#------#
# PGLS #
#------#

# Relative area change BCC 2050 -----
# str(miroc2070.noOut_rel_n_cell)
# psych::pairs.panels(miroc2070.noOut_rel_n_cell[,c("TotMax_mm", "AbMax_mm", "HwMax_mm", "Habitat", "FlightSeason_month")])

bcc2050.noOut_rel_n_cell$Habitat <- as.factor(bcc2050.noOut_rel_n_cell$Habitat)
bcc2050.noOut_rel_n_cell$Species <- gsub(" ", "_", bcc2050.noOut_rel_n_cell$Species)
test.data  <-  comparative.data(phy = tree_rel_n_cell, data = bcc2050.noOut_rel_n_cell, names.col = Species, vcv = TRUE, vcv.dim = 3)

odo_model <- pgls(rel_n_cell ~ TotMax_mm + FlightSeason_month + Habitat, 
                  data = test.data, kappa = "ML", lambda = "ML", delta = "ML")

pdf("./Relative_area_change_BCC2050.pdf")
par(mfrow = c(2, 2))
hist(odo_model$phyres)
qqnorm(odo_model$phyres)
qqline(odo_model$phyres)
plot(x = fitted(odo_model), y = odo_model$phyres, pch = 19)
dev.off()
bcc2050.odo_model <- summary(odo_model)
bcc2050.odo_model <- as.data.frame(bcc2050.odo_model$coefficients)
bcc2050.odo_model$response_variable <- c("Relative area change")
bcc2050.odo_model$scenario <- "BCC"
bcc2050.odo_model$period <- "2050"
write.csv(bcc2050.odo_model, "./Relative_area_change_BCC2050.csv", row.names = F)

# Altitude difference BCC 2050 -----
bcc2050.mg$Habitat <- as.factor(bcc2050.mg$Habitat)
bcc2050.mg$Species <- gsub(" ", "_", bcc2050.mg$Species)
test.data  <-  comparative.data(phy = fict.phylo, data = bcc2050.mg, names.col = Species, vcv = TRUE, vcv.dim = 3)

odo_model <- pgls(DifavgAltitude ~ TotMax_mm + FlightSeason_month + Habitat, 
                  data = test.data, kappa = "ML", lambda = "ML", delta = "ML")

pdf("./Altitude_difference_BCC2050.pdf")
par(mfrow = c(2, 2))
hist(odo_model$phyres)
qqnorm(odo_model$phyres)
qqline(odo_model$phyres)
plot(x = fitted(odo_model), y = odo_model$phyres, pch = 19)
dev.off()
bcc2050.odo_model <- summary(odo_model)
bcc2050.odo_model <- as.data.frame(bcc2050.odo_model$coefficients)
bcc2050.odo_model$response_variable <- c("Altitude difference")
bcc2050.odo_model$scenario <- "BCC"
bcc2050.odo_model$period <- "2050"
write.csv(bcc2050.odo_model, "./Altitude_difference_BCC2050.csv", row.names = F)

# Centroid difference BCC 2050 ----- 
bcc2050.noOut_dif_cetroid$Habitat <- as.factor(bcc2050.noOut_dif_cetroid$Habitat)
bcc2050.noOut_dif_cetroid$Species <- gsub(" ", "_", bcc2050.noOut_dif_cetroid$Species)
test.data  <-  comparative.data(phy = tree_dif_cetroid, data = bcc2050.noOut_dif_cetroid, names.col = Species, vcv = TRUE, vcv.dim = 3)

odo_model <- pgls(dif_cetroid ~ TotMax_mm + FlightSeason_month + Habitat, 
                  data = test.data, kappa = "ML", lambda = "ML", delta = "ML")

pdf("./Centroid_difference_BCC2050.pdf")
par(mfrow = c(2, 2))
hist(odo_model$phyres)
qqnorm(odo_model$phyres)
qqline(odo_model$phyres)
plot(x = fitted(odo_model), y = odo_model$phyres, pch = 19)
dev.off()
bcc2050.odo_model <- summary(odo_model)
bcc2050.odo_model <- as.data.frame(bcc2050.odo_model$coefficients)
bcc2050.odo_model$response_variable <- c("Centroid difference")
bcc2050.odo_model$scenario <- "BCC"
bcc2050.odo_model$period <- "2050"
write.csv(bcc2050.odo_model, "./Centroid_difference_BCC2050.csv", row.names = F)

rm(bcc2050_k_rel_n_cel, bcc2050_lam_dif_alt, bcc2050_lam_rel_n_cel,
   bcc2050.mg, bcc2050.noOut_dif_cetroid, bcc2050.noOut_rel_n_cell,
   bcc2050.odo_model, bcc2050.rel_n_cell, dif_alt_bcc2050, dif_alt_bcc2050_1,
   dif_cetroid_bcc2050, dif_cetroid_bcc2050_1, rel_n_cel_bcc2050, rel_n_cel_bcc2050_1,
   tree_miroc_2070, tree_dif_cetroid, tree_rel_n_cell, odo_model, odo_rel_n_cell,
   outliers_dif_cetroid, outliers_rel_n_cell)

############
# BCC 2070 #
############

# filter scenarios. Merge traits and variables
bcc2070 <- table.model[table.model$scenario == "bcc2070", ]
colnames(bcc2070)[10] <- "Species"
bcc2070.mg <- merge(traits, bcc2070, by = "Species", all.x = TRUE); rm(bcc2070)

# Check outliers
dotchart(bcc2070.mg$rel_n_cell, labels = bcc2070.mg$Species, cex = 0.6, main = "Relative area change")
dotchart(bcc2070.mg$DifavgAltitude, labels = bcc2070.mg$Species, cex = 0.6, main = "Altitude difference")
dotchart(bcc2070.mg$dif_cetroid, labels = bcc2070.mg$Species, cex = 0.6, main = "Centroid difference")

outliers_rel_n_cell <- c("Trithemis arteriosa", "Sympecma paedisca")
outliers_dif_cetroid <- c("Trithemis arteriosa", "Orthetrum chrysostigma")

bcc2070.noOut_rel_n_cell <- bcc2070.mg[bcc2070.mg$Species %ni% outliers_rel_n_cell, ]
dotchart(bcc2070.noOut_rel_n_cell$rel_n_cell, labels = bcc2070.noOut_rel_n_cell$Species, cex = 0.6, main = "Relative area change")

bcc2070.noOut_dif_cetroid <- bcc2070.mg[bcc2070.mg$Species %ni% outliers_dif_cetroid, ]
dotchart(bcc2070.noOut_dif_cetroid$dif_cetroid, labels = bcc2070.noOut_dif_cetroid$Species, cex = 0.6, main = "Centroid difference")

# Drop tip ---------------------------------------------------------------------
tree_rel_n_cell <- drop.tip(fict.phylo, c("Trithemis_arteriosa", "Sympecma_paedisca"))
tree_dif_cetroid <- drop.tip(fict.phylo, c("Trithemis_arteriosa", "Orthetrum_chrysostigma"))

#-----------------------------------#
# Ancestor character reconstruction #
#-----------------------------------#

# Relative area change BCC 2070 -----
rel_n_cel_bcc2070 <- bcc2070.noOut_rel_n_cell$rel_n_cel
sp <- gsub(" ", "_", bcc2070.noOut_rel_n_cell$Species)
names(rel_n_cel_bcc2070) <- sp
rel_n_cel_bcc2070_1 <- rel_n_cel_bcc2070[tree_rel_n_cell$tip.label] # Riordino secondo lálbero filogenetico

pdf("./Relative_area_change_bcc2070.pdf")
contMap(tree_rel_n_cell, rel_n_cel_bcc2070_1, fsize = .3, scale = 1, lwd = 1.5, 
        tip.labels = TRUE, outline = TRUE, plot = TRUE)
title("Relative area change BCC 2070")
dev.off()

# Altitude difference BCC 2070 ----- 
dif_alt_bcc2070 <- bcc2070.mg$DifavgAltitude
sp <- gsub(" ", "_", bcc2070.mg$Species)
names(dif_alt_bcc2070) <- sp
dif_alt_bcc2070_1 <-dif_alt_bcc2070[fict.phylo$tip.label] # Riordino secondo lálbero filogenetico

pdf("./Altitude_difference_bcc2070.pdf")
contMap(fict.phylo, dif_alt_bcc2070_1, fsize = .3, scale = 1, lwd = 1.5, 
        tip.labels = TRUE, outline = TRUE, plot = TRUE)
title("Altitude difference BCC 2070")
dev.off()

# Centroid difference BCC 2070 ----- 
dif_cetroid_bcc2070 <- bcc2070.noOut_dif_cetroid$dif_cetroid
sp <- gsub(" ", "_", bcc2070.noOut_dif_cetroid$Species)
names(dif_cetroid_bcc2070) <- sp
dif_cetroid_bcc2070_1 <- dif_cetroid_bcc2070[tree_dif_cetroid$tip.label] # Riordino secondo lálbero filogenetico

pdf("./Centroid_difference_bcc2070.pdf")
contMap(tree_dif_cetroid, dif_cetroid_bcc2070_1, fsize = .3, scale = 1, lwd = 1.5, 
        tip.labels = TRUE, outline = TRUE, plot = TRUE)
title("Centroid difference BCC 2070")
dev.off()


#---------------------#
# Phylogenetic signal #
#---------------------#

# Relative area change BCC 2070 -----
phylosig(tree_rel_n_cell, bcc2070.noOut_rel_n_cell$rel_n_cel, "K", test = TRUE)
phylosig(tree_rel_n_cell, bcc2070.noOut_rel_n_cell$rel_n_cel, "lambda", test = TRUE)

# Altitude difference BCC 2070 -----
phylosig(fict.phylo, bcc2070.mg$DifavgAltitude, "K", test = TRUE)
phylosig(fict.phylo, bcc2070.mg$DifavgAltitude, "lambda", test = TRUE)

# Centroid difference BCC 2070 ----- 
phylosig(tree_dif_cetroid, bcc2070.noOut_dif_cetroid$dif_cetroid, "K", test = TRUE)
phylosig(tree_dif_cetroid, bcc2070.noOut_dif_cetroid$dif_cetroid, "lambda", test = TRUE)


#------#
# PGLS #
#------#

# Relative area change BCC 2070 -----
# str(miroc2070.noOut_rel_n_cell)
# psych::pairs.panels(miroc2070.noOut_rel_n_cell[,c("TotMax_mm", "AbMax_mm", "HwMax_mm", "Habitat", "FlightSeason_month")])

bcc2070.noOut_rel_n_cell$Habitat <- as.factor(bcc2070.noOut_rel_n_cell$Habitat)
bcc2070.noOut_rel_n_cell$Species <- gsub(" ", "_", bcc2070.noOut_rel_n_cell$Species)
test.data  <-  comparative.data(phy = tree_rel_n_cell, data = bcc2070.noOut_rel_n_cell, names.col = Species, vcv = TRUE, vcv.dim = 3)

odo_model <- pgls(rel_n_cell ~ TotMax_mm + FlightSeason_month + Habitat, 
                  data = test.data, kappa = "ML", lambda = "ML", delta = "ML")

pdf("./Relative_area_change_BCC2070.pdf")
par(mfrow = c(2, 2))
hist(odo_model$phyres)
qqnorm(odo_model$phyres)
qqline(odo_model$phyres)
plot(x = fitted(odo_model), y = odo_model$phyres, pch = 19)
dev.off()
bcc2070.odo_model <- summary(odo_model)
bcc2070.odo_model <- as.data.frame(bcc2070.odo_model$coefficients)
bcc2070.odo_model$response_variable <- c("Relative area change")
bcc2070.odo_model$scenario <- "BCC"
bcc2070.odo_model$period <- "2070"
write.csv(bcc2070.odo_model, "./Relative_area_change_BCC2070.csv", row.names = F)

# Altitude difference BCC 2070 -----
bcc2070.mg$Habitat <- as.factor(bcc2070.mg$Habitat)
bcc2070.mg$Species <- gsub(" ", "_", bcc2070.mg$Species)
test.data  <-  comparative.data(phy = fict.phylo, data = bcc2070.mg, names.col = Species, vcv = TRUE, vcv.dim = 3)

odo_model <- pgls(DifavgAltitude ~ TotMax_mm + FlightSeason_month + Habitat, 
                  data = test.data, kappa = "ML", lambda = "ML", delta = "ML")

pdf("./Altitude_difference_BCC2070.pdf")
par(mfrow = c(2, 2))
hist(odo_model$phyres)
qqnorm(odo_model$phyres)
qqline(odo_model$phyres)
plot(x = fitted(odo_model), y = odo_model$phyres, pch = 19)
dev.off()
bcc2070.odo_model <- summary(odo_model)
bcc2070.odo_model <- as.data.frame(bcc2070.odo_model$coefficients)
bcc2070.odo_model$response_variable <- c("Altitude difference")
bcc2070.odo_model$scenario <- "BCC"
bcc2070.odo_model$period <- "2070"
write.csv(bcc2070.odo_model, "./Altitude_difference_BCC2070.csv", row.names = F)

# Centroid difference BCC 2070 ----- 
bcc2070.noOut_dif_cetroid$Habitat <- as.factor(bcc2070.noOut_dif_cetroid$Habitat)
bcc2070.noOut_dif_cetroid$Species <- gsub(" ", "_", bcc2070.noOut_dif_cetroid$Species)
test.data  <-  comparative.data(phy = tree_dif_cetroid, data = bcc2070.noOut_dif_cetroid, names.col = Species, vcv = TRUE, vcv.dim = 3)

odo_model <- pgls(dif_cetroid ~ TotMax_mm + FlightSeason_month + Habitat, 
                  data = test.data, kappa = "ML", lambda = "ML", delta = "ML")

pdf("./Centroid_difference_BCC2070.pdf")
par(mfrow = c(2, 2))
hist(odo_model$phyres)
qqnorm(odo_model$phyres)
qqline(odo_model$phyres)
plot(x = fitted(odo_model), y = odo_model$phyres, pch = 19)
dev.off()
bcc2070.odo_model <- summary(odo_model)
bcc2070.odo_model <- as.data.frame(bcc2070.odo_model$coefficients)
bcc2070.odo_model$response_variable <- c("Centroid difference")
bcc2070.odo_model$scenario <- "BCC"
bcc2070.odo_model$period <- "2070"
write.csv(bcc2070.odo_model, "./Centroid_difference_BCC2070.csv", row.names = F)

rm(bcc2070_k_rel_n_cel, bcc2070_lam_dif_alt, bcc2070_lam_rel_n_cel,
   bcc2070.mg, bcc2070.noOut_dif_cetroid, bcc2070.noOut_rel_n_cell,
   bcc2070.odo_model, bcc2070.rel_n_cell, dif_alt_bcc2070, dif_alt_bcc2070_1,
   dif_cetroid_bcc2070, dif_cetroid_bcc2070_1, rel_n_cel_bcc2070, rel_n_cel_bcc2070_1,
   tree_miroc_2070, tree_dif_cetroid, tree_rel_n_cell, odo_model, odo_rel_n_cell,
   outliers_dif_cetroid, outliers_rel_n_cell)
