################################################################################
#' @title sdist
#'
#' @author James Margrove 
#'
#' @description calculates the distance between two points
#' 
#' @param p1 coordinates of point 1
#' @param p2 coordinates of point 2

dist <- function(p1,p2){
  sqrt(sum(((p1[1]-p2[1]))^2, ((p1[2]-p2[2])^2)))
}