################################################################################
#' @title sdist
#'
#' @author James Margrove 
#'
#' @description calculates the distance between two points
#' 
#' @param p1 coordinates of point 1
#' @param p2 coordinates of point 2

pointDist <- function(p1,p2){
  xvals <- (p2[1]-p1[1])^2
  yvals <- (p2[2]-p1[2])^2
  sumVals = xvals+yvals
  sqrt(sumVals)
}