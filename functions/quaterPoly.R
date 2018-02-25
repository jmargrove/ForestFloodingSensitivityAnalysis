################################################################################
#' @title quaterPoly
#' 
#' @author James Margrove 
#'
#' @description funciton that takes coordinates and partions plots 
#'
#' @param coords four sets of coordinates, 
#' @param graph outpust a ggplot2 graph of the 4 new polygons 


quaterPoly <- function(coords, graph = F){
  source("./functions/midPoint.R")
  source("./functions/sortToPoly.R")
  newcoords <- data.frame(x = as.numeric(), y = as.numeric())
  coords <- sortToPoly(coords)
  s = 1:4
  for(i in s){
    newdt <- coords[i,]
    for(j in s[-i]) {
      newdt <- rbind(newdt, midPoint(coords[i,], coords[j, ]))
    }
    newdt <- sortToPoly(newdt)
    newcoords <- rbind(newcoords, newdt)
  }
  newcoords$Q <- factor(rep(1:4, each = 4))
  if(!graph){
    return(newcoords)  
  } else if (graph){
    ggplot2::ggplot(newcoords, aes(x = x, y = y, fill = Q)) + 
      geom_polygon() + 
      theme_classic()
  }
}

