#-----------------#
# ALPHA DIVERSITY #
#-----------------#
# Version 1.0


alphaRaster <- function(x, y, atree){
if (!require("raster")) install.packages("raster")
require(raster)
# if (!require("BAT")) install.packages("BAT")
# require(BAT)

# Convert raster to dataframe
data.1 <- as.data.frame(x, xy=F)
data.2 <- as.data.frame(y, xy=F)
coord <- as.data.frame(x, xy=T)[ ,1:2]

# Replace NA with 0
data.1[is.na(data.1)] <- 0
data.2[is.na(data.2)] <- 0

# Load alpha function from BAT
source("/Volumes/GoogleDrive/Il mio Drive/Odonata_SDM/R_script/Function/BetaRaster/BAT_Beta.R")

if(missing(atree)){
alpha.db <- data.frame(present = alpha(data.1),
                       future = alpha(data.2))
colnames(alpha.db) <- c("present", "future")
alpha.db$diff <- alpha.db$future - alpha.db$present
} else {
  alpha.db <- data.frame(present = alpha(data.1, atree),
                         future = alpha(data.2, atree))
  
  colnames(alpha.db) <- c("present", "future")
  alpha.db$diff <- alpha.db$future - alpha.db$present
  
}

a.present.raster <- rasterFromXYZ(cbind(coord, alpha.db$present))
a.future.raster <- rasterFromXYZ(cbind(coord, alpha.db$future))
a.diff.raster <- rasterFromXYZ(cbind(coord, alpha.db$diff))

return(results <- list(alpha = alpha.db,
                aplpha.raster = stack(a.present.raster, a.future.raster, a.diff.raster)))

}
