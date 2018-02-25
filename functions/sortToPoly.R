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
  source("./functions/dist.R")
  d <-  0
  for(i in 2:4){
    d[i] <- dist(coords[1,], coords[i,])
  }
  
  d2 <- numeric(4)
  for(i in c(1,3,4)){
    d2[i] <- dist(coords[2,], coords[i,])
  }
  
  if(max(d) > max(d2)){
    coords <- coords[order(d),]
  } else {
    coords <- coords[order(d2),]
  }
  r4 <- coords[4,]
  coords[4,] <- coords[3,] 
  coords[3,] <- r4
  return(coords)
}
