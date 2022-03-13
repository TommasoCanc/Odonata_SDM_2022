##########################
# Alpha - Beta - PD - FD #
##########################

# Load libraries
library(caper) # <- Genetic staffs
library(raster) # <- GIS
library(osfr) # <- Connect to OSF
  
# Main path 
main.path <- "YOUR_PATH"

# Connect to Open Science Framework
cr_project <- osf_retrieve_node("4rjuc")
osf_files <- osf_ls_files(cr_project, type = "folder")

file.number <- which(
  osf_ls_files(osf_files[1,],
               pattern = "tree_treeannotator.nex")[, 1] == "tree_treeannotator.nex"
)
osf_download(osf_ls_files(osf_files[1,],
                          pattern = "tree_treeannotator.nex")[file.number,],
             path = main.path)
rm(file.number)

# Create folder to save results
dir.create("./Models/ALPHA")
dir.create("./Models/ALPHA/Taxonomic")
dir.create("./Models/ALPHA/Functional")
dir.create("./Models/ALPHA/Phylogenetic")
dir.create("./Models/BETA")
dir.create("./Models/BETA/Taxonomic")
dir.create("./Models/BETA/Functional")
dir.create("./Models/BETA/Phylogenetic")

# Load function
source("./Function/alphaRaster_V1.R")
source("./Function/betaRaster_V2.R")
"%inot%" <- Negate("%in%")

# Load elev
elev <- raster("./elevation_5_null.asc")
  
# Load present variables
presentFiles <- list.files("./Models/Richness/Present/", 
                           pattern = ".asc", full.names = TRUE)
# MIROC 2050 - 2070
mirocFiles_2050 <- list.files("./Models/Richness/MIROC_2050//",
                              pattern = ".asc", full.names = TRUE)
mirocFiles_2070 <- list.files("./Models/Richness/MIROC_2070/",
                              pattern = ".asc", full.names = TRUE)
# NOR 2050 - 2070
norFiles_2050 <- list.files("./Models/Richness/NOR_2050/",
                            pattern = ".asc", full.names = TRUE)
norFiles_2070 <- list.files("./Models/Richness/NOR_2070/",
                            pattern = ".asc", full.names = TRUE)
# BCC 2050 - 2070
bccFiles_2050 <- list.files("./Models/Richness/BCC_2050/",
                            pattern = ".asc", full.names = TRUE)
bccFiles_2070 <- list.files("./Models/Richness/BCC_2070/",
                            pattern = ".asc", full.names = TRUE)

# Stack file
present <- brick(stack(presentFiles))
miroc_2050 <- brick(stack(mirocFiles_2050))
miroc_2070 <- brick(stack(mirocFiles_2070))
nor_2050 <- brick(stack(norFiles_2050))
nor_2070 <- brick(stack(norFiles_2070))
bcc_2050 <- brick(stack(bccFiles_2050))
bcc_2070 <- brick(stack(bccFiles_2070))

rm(presentFiles,
   mirocFiles_2050, mirocFiles_2070,
   norFiles_2050, norFiles_2070,
   bccFiles_2050, bccFiles_2070)


#---------------------#
# Taxonomic diversity #
#---------------------#

# Be careful, 
# pipeline: a.tree --> b.tree --> save 
# OR
# pipeline: beta.tree --> c.tree --> save 

# Alpha diversity
a.tree <- alphaRaster(x = present, y = miroc_2050)  
a.tree <- alphaRaster(x = present, y = miroc_2070)  

a.tree <- alphaRaster(x = present, y = nor_2050)  
a.tree <- alphaRaster(x = present, y = nor_2070)  

a.tree <- alphaRaster(x = present, y = bcc_2050)  
a.tree <- alphaRaster(x = present, y = bcc_2070)  

b.tree <- mask(x = a.tree$aplpha.raster, mask = elev)

# Save raster
for(i in 1:3){
  writeRaster(b.tree[[i]], 
              paste0("./Models/ALPHA/Taxonomic/", 
              names(b.tree)[i],"_present_bcc_2070.asc"),
              format = "GTiff", overwrite = FALSE, bylayer = TRUE)}


# Beta diversity
beta.tree <- betaRaster(x = ordered_stack.present, y = ordered_stack.miroc_2050,
                        Ncore = 15)
beta.tree <- betaRaster(x = ordered_stack.present, y = ordered_stack.miroc_2070,
                        Ncore = 15)

beta.tree <- betaRaster(x = ordered_stack.present, y = ordered_stack.nor_2050,
                        Ncore = 15)
beta.tree <- betaRaster(x = ordered_stack.present, y = ordered_stack.nor_2070,
                        Ncore = 15)

beta.tree <- betaRaster(x = ordered_stack.present, y = ordered_stack.bcc_2050,
                        Ncore = 15)
beta.tree <- betaRaster(x = ordered_stack.present, y = ordered_stack.bcc_2070,
                        Ncore = 15)

c.tree <- mask(x = beta.tree$beta.raster, mask = elev)

# Save Raster 
for(i in 1:3){
  writeRaster(c.tree[[i]], 
              paste0("./Models/BETA/Taxonomic/", 
              names(c.tree)[i],"_present_bcc_2070.asc",
              format = "GTiff", overwrite = F, bylayer=T))}


#-----------------------#
# Phylogeneic diversity #
#-----------------------#

# Load BEAST tree
tree <- read.nexus("./tree_treeannotator.nex")
  
# Before calculate PD we have to check if the species' raster are in phylogenetic tree
unique(labels(present) %in% (tree$tip.label))
length(labels(present)) == length((tree$tip.label)) 
unique(labels(miroc_2050) %in% (tree$tip.label))
unique(labels(miroc_2070) %in% tree$tip.label)
unique(labels(nor_2050) %in% tree$tip.label)
unique(labels(nor_2070) %in% tree$tip.label)
unique(labels(bcc_2050) %in% tree$tip.label)
unique(labels(bcc_2070) %in% tree$tip.label)
  
# order as tree
is_tip <- tree$edge[,2] <= length(tree$tip.label)
ordered_tips <- tree$edge[is_tip, 2]
ordered_names <- tree$tip.label[ordered_tips]
  
ordered_stack.present <- present[[ordered_names]]
ordered_stack.miroc_2050 <- miroc_2050[[ordered_names]]
ordered_stack.miroc_2070 <- miroc_2070[[ordered_names]]
ordered_stack.nor_2050 <- nor_2050[[ordered_names]]
ordered_stack.nor_2070 <- nor_2070[[ordered_names]]
ordered_stack.bcc_2050 <- bcc_2050[[ordered_names]]
ordered_stack.bcc_2070 <- bcc_2070[[ordered_names]]
  
# Alpha diversity phylogenetic tree
a.tree <- alphaRaster(x = ordered_stack.present, y = ordered_stack.miroc_2050, 
                      atree = tree)  
a.tree <- alphaRaster(x = ordered_stack.present, y = ordered_stack.miroc_2070, 
                      atree = tree)  

a.tree <- alphaRaster(x = ordered_stack.present, y = ordered_stack.nor_2050, 
                      atree = tree)  
a.tree <- alphaRaster(x = ordered_stack.present, y = ordered_stack.nor_2070, 
                      atree = tree)  

a.tree <- alphaRaster(x = ordered_stack.present, y = ordered_stack.bcc_2050, 
                      atree = tree)  
a.tree <- alphaRaster(x = ordered_stack.present, y = ordered_stack.bcc_2070, 
                      atree = tree)  
  
b.tree <- mask(x = a.tree$aplpha.raster, mask = elev)
plot(b.tree)

# Save raster
for(i in 1:3){
  writeRaster(b.tree[[i]], paste0("./Models/ALPHA/Phylogenetic/",
              names(b.tree)[i],"_present_bcc_2070.asc"),
              format = "GTiff", overwrite = FALSE, bylayer = TRUE)}

# Beta diversity
beta.tree <- betaRaster(x = ordered_stack.present, y = ordered_stack.miroc_2050,
                        betree = tree, Ncore = 15)
beta.tree <- betaRaster(x = ordered_stack.present, y = ordered_stack.miroc_2070,
                        betree = tree, Ncore = 15)

beta.tree <- betaRaster(x = ordered_stack.present, y = ordered_stack.nor_2050,
                        betree = tree, Ncore = 15)
beta.tree <- betaRaster(x = ordered_stack.present, y = ordered_stack.nor_2070,
                        betree = tree, Ncore = 15)

beta.tree <- betaRaster(x = ordered_stack.present, y = ordered_stack.bcc_2050,
                        betree = tree, Ncore = 15)
beta.tree <- betaRaster(x = ordered_stack.present, y = ordered_stack.bcc_2070,
                        betree = tree, Ncore = 15)
  
c.tree <- mask(x = beta.tree$beta.raster, mask = elev)
plot(c.tree)

# Save Raster 
for(i in 1:3){
    writeRaster(c.tree[[i]], paste0("./Models/BETA/Phylogenetic/", 
                names(c.tree)[i],"_present_bcc_2070.asc",
                format = "GTiff", overwrite = FALSE, bylayer = TRUE))}


#----------------------#
# Functional diversity #
#----------------------#

file.number <- which(
  osf_ls_files(osf_files[1,],
               pattern = "functional_tree.nex")[, 1] == "functional_tree.nex"
)
osf_download(osf_ls_files(osf_files[1,],
                          pattern = "functional_tree.nex")[file.number,],
             path = main.path)
rm(file.number)


# Load functional tree
tree <- read.nexus("./functional_tree.nex")

# Before calculate PD we have to check if the species' raster are in functional tree
unique(labels(present) %in% (tree$tip.label))
unique(labels(miroc_2050) %in% (tree$tip.label))
unique(labels(miroc_2070) %in% tree$tip.label)
unique(labels(nor_2050) %in% tree$tip.label)
unique(labels(nor_2070) %in% tree$tip.label)
unique(labels(bcc_2050) %in% tree$tip.label)
unique(labels(bcc_2070) %in% tree$tip.label)

# Order as tree
is_tip <- tree$edge[,2] <= length(tree$tip.label)
ordered_tips <- tree$edge[is_tip, 2]
ordered_names <- tree$tip.label[ordered_tips]

ordered_stack.present <- present[[ordered_names]]
ordered_stack.miroc_2050 <- miroc_2050[[ordered_names]]
ordered_stack.miroc_2070 <- miroc_2070[[ordered_names]]
ordered_stack.nor_2050 <- nor_2050[[ordered_names]]
ordered_stack.nor_2070 <- nor_2070[[ordered_names]]
ordered_stack.bcc_2050 <- bcc_2050[[ordered_names]]
ordered_stack.bcc_2070 <- bcc_2070[[ordered_names]]


#Functional alpha diversity
a.tree <- alphaRaster(x = ordered_stack.present, y = ordered_stack.miroc_2050, 
                      atree = tree)  
a.tree <- alphaRaster(x = ordered_stack.present, y = ordered_stack.miroc_2070, 
                      atree = tree)  
a.tree <- alphaRaster(x = ordered_stack.present, y = ordered_stack.nor_2050, 
                      atree = tree)  
a.tree <- alphaRaster(x = ordered_stack.present, y = ordered_stack.nor_2070, 
                      atree = tree)  

a.tree <- alphaRaster(x = ordered_stack.present, y = ordered_stack.bcc_2050, 
                      atree = tree)  
a.tree <- alphaRaster(x = ordered_stack.present, y = ordered_stack.bcc_2070, 
                      atree = tree)  

b.tree <- mask(x = a.tree$aplpha.raster, mask = elev)


# Save raster
for(i in 1:3){
  writeRaster(b.tree[[i]], paste0("./Models/ALPHA/Functional/",
              names(b.tree)[i],"_present_bcc_2070.asc"),
              format="GTiff", overwrite = FALSE, bylayer = TRUE)}

# Beta diversity
beta.tree <- betaRaster(x = ordered_stack.present, y = ordered_stack.miroc_2050,
                        betree = tree, Ncore = 15)
beta.tree <- betaRaster(x = ordered_stack.present, y = ordered_stack.miroc_2070,
                        betree = tree, Ncore = 15)

beta.tree <- betaRaster(x = ordered_stack.present, y = ordered_stack.nor_2050,
                        betree = tree, Ncore = 15)
beta.tree <- betaRaster(x = ordered_stack.present, y = ordered_stack.nor_2070,
                        betree = tree, Ncore = 15)

beta.tree <- betaRaster(x = ordered_stack.present, y = ordered_stack.bcc_2050,
                        betree = tree, Ncore = 15)
beta.tree <- betaRaster(x = ordered_stack.present, y = ordered_stack.bcc_2070,
                        betree = tree, Ncore = 15)

c.tree <- mask(x = beta.tree$beta.raster, mask = elev)
plot(c.tree)

# save rasters
for(i in 1:3){
  writeRaster(c.tree[[i]], paste0("./Models/BETA/Functional/", 
                           names(c.tree)[i],"_present_bcc_2070.asc",
                           format = "GTiff", overwrite = FALSE, bylayer = TRUE))}

# Clean workspace
rm(list = ls())
