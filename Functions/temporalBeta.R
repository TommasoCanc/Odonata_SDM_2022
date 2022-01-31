#----------------#
# BETA DIVERSITY #
#----------------#
# Version 2.0

betaRaster <- function(x, y, betree = NA, bIndex = "Sorensen", Ncore = 1) {
  if (!require("raster")) install.packages("raster")
  require(raster)
  # if (!require("BAT")) install.packages("BAT")
  # require(BAT)
  if (!require("foreach")) install.packages("foreach")
  require(foreach)
  if (!require("picante")) install.packages("picante")
  require(picante)
  
  # Function to find the difference
  "%notin%" <- Negate("%in%")
  
  # Number of core
  cl <- parallel::makeCluster(Ncore)
  
  # Convert raster to dataframe
  data.1 <- as.data.frame(x, xy=F)
  data.2 <- as.data.frame(y, xy=F)
  coord <- as.data.frame(x, xy=T)[ ,1:2]
  
  #  Replace NA with 0
  # data.1[is.na(data.1)] <- 0
  # data.2[is.na(data.2)] <- 0
  
  beta.list <- list()
  
  for(i in 1:nrow(data.1) ){
    sp.data.1 <- names(data.1)[data.1[i,]==1]
    sp.data.2 <- names(data.1)[data.2[i,]==1]
    
    # Check for the shared species
    share <- na.omit(sp.data.1[sp.data.1 %in% sp.data.2]) # <- Shared species
    share.num <- rbind(rep(1,length(share)),rep(1,length(share)))
    colnames(share.num) <- share
    
    # Check for species in the data.1 but not in the data.2
    sp.data.1.1 <- na.omit(sp.data.1[sp.data.1%notin%sp.data.2])
    sp.data.1.1.num <- rbind(rep(1,length(sp.data.1.1 )),rep(0,length(sp.data.1.1 )))
    colnames(sp.data.1.1.num) <- sp.data.1.1
    
    # Check for species in the data.2 but not in the data.1
    sp.data.2.1 <- na.omit(sp.data.2[sp.data.2%notin%sp.data.1])
    sp.data.2.1.num <- rbind(rep(0,length(sp.data.2.1)),rep(1,length(sp.data.2.1)))
    colnames(sp.data.2.1.num)<-sp.data.2.1
    
    # Merge all the matrix
    beta.df <- cbind(share.num, sp.data.1.1.num, sp.data.2.1.num)
    
    beta.list[[i]] <- beta.df
  }
  
  # Parallel ----------------------------------- > Speed
  if(missing(betree)){
    doParallel::registerDoParallel(cl)
    beta.results.cl <- foreach(j = 1:length(beta.list), .combine = rbind) %dopar% {
      source("/Volumes/GoogleDrive/Il mio Drive/Odonata_SDM/R_script/Function/BetaRaster/BAT_Beta.R")
      beta.results <- beta(beta.list[[j]], func = bIndex)
    }
    # Close cluster
    parallel::stopCluster(cl)
  } else {
    doParallel::registerDoParallel(cl)
    beta.results.cl <- foreach(j = 1:length(beta.list), .combine = rbind, .multicombine=TRUE) %dopar% {
      source("/Volumes/GoogleDrive/Il mio Drive/Odonata_SDM/R_script/Function/BetaRaster/BAT_Beta.R")
      tree.prune <- t(data.frame(names(data.frame(beta.list[[j]]))))
      colnames(tree.prune) <- tree.prune
      # DOVREBBE FUNZIONARE
      if(ncol(tree.prune) >= 2){
        beta.results.1 <- beta(beta.list[[j]], tree = picante::prune.sample(tree.prune, betree), func = bIndex)
      } else {
        beta.results.2 <- list(Btotal = 0,
                               Brepl = 0,
                               Brich = 0)
      }
    }
    # Close cluster
    parallel::stopCluster(cl)
  }
  
  # Create vectors with beta results
  beta.results.df <- as.data.frame(beta.results.cl)
  btotal.df <- unname(unlist(beta.results.df$Btotal))
  brepl.df <-  unname(unlist(beta.results.df$Brepl))
  brich.df <-  unname(unlist(beta.results.df$Brich))
  
  # Convert dataframe to raster
  btotal.raster <- rasterFromXYZ(cbind(coord, btotal.df))
  brepl.raster <-  rasterFromXYZ(cbind(coord, brepl.df))
  brich.raster <-  rasterFromXYZ(cbind(coord, brich.df))
  
  # Return results
  return(results <- list(btotal = btotal.df,
                         brepl = brepl.df,
                         brich = brich.df,
                         beta.raster = stack(btotal.raster, brepl.raster, brich.raster))
  )                             
}
