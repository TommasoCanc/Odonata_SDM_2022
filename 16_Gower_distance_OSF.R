##################
# Gower distance #
##################

# Load libraries
library(xlsx) # <- Open .xlsx
library(gawdis) # Gower distance

# Main path
main.path <- "YOUR_PATH"

# Load file with traits
traits <- read.xlsx("/Volumes/GoogleDrive/Il mio Drive/Odonata_SDM/Odonata_traits_SDM.xlsx", sheetIndex = 1) 

traits.fil <- traits[,c(5:14,19:26)]
rownames(traits.fil) <- traits$Species
traits.fil$Habitat <- as.factor(traits.fil$Habitat)


# Su pc pamplona ho fatto Gower distance senza elevation
## Gower distance with group of traits
gaw.group <- gawdis(traits.fil, w.type = "optimized", 
                    groups = c(1,1,1,1,1,1,2,3,4,4,5,5,5,5,5,5,5,5), 
                    opti.maxiter = 500,
                    groups.weight=T)

# Save functional tree
fd.tree <- as.phylo(hclust(gaw.group)) 
write.tree(phy = fd.tree, file="./functional_tree.newick")
