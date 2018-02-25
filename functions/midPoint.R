################################################################################
#' @title midPoint
#'
#' @author James Margrove 
#'
#' @description calculates the coordinates of a mid point 
#' 
#' @param p1 coordinates of point 1
#' @param p2 coordinates of point 2


midPoint <- function(p1, p2) {
  newX <- (p1[1] + p2[1])/2
  newY <- (p1[2] + p2[2])/2
  return(c(newX, newY))
}