################################################################################
#' @title sortFToPoly
#'
#' @author James Margrove 
#'
#' @description function to arange polygon points always so when plotted they 
#' are in the correc orentation 
#' 
#' @param coords four coordinates in a matrix, cbind, or data.frame




sortToPoly <- function(coords){
  source("./functions/pointDist.R")
 # coords <- plot_corners[plot_corners$Plot == k, c(5,6)]
  #coords <- dt
  d <-  0
  for(i in 2:4){
    d[i] <- pointDist(coords[1,], coords[i,])
  }

  d
  d2 <- numeric(4)
  for(i in c(1,3,4)){
    d2[i] <- pointDist(coords[2,], coords[i,])
  }
  d
  print(d)
  if(max(d, na.rm = T) > max(d2, na.rm = T)){
    coords <- coords[order(d),]
  } else {
    coords <- coords[order(d2),]
  }
  r4 <- coords[4,]
  coords[4,] <- coords[3,] 
  coords[3,] <- r4
  coords <- coords[,1:2]
  return(coords)
  
  
}


