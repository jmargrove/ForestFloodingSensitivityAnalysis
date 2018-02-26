#####################################################################################
#' @title quadSplit
#' @author James Margrove 
#' 
#' @description split a polygon into as many seperate polyons as specified 
#' 
#' @param coords four coordinates specifying the polygon 
#'
#'


rm(list=ls())

source("./functions/sortToPoly.R")
source("./functions/midPoint.R")
require(ggplot2)
data <- expand.grid(x = c(100, 300), y = c(250, 475))
data[1:2, "x"] <- data[1:2,"x"] + 50
data[2:4, "y"] <- data[2:4,"y"] + 400
data <- sortToPoly(data)
ggplot(data, aes(x = x, y = y)) + geom_polygon()

quadSplit <- function(data, n) {
  linearEq <- function(p1,p2, n){
    x = seq(min(as.numeric(c(p1[1],p2[1]))), max(as.numeric(c(p1[1],p2[1]))), length = n + 1 )
    beta <- as.numeric((p1[2]-p2[2])/(p1[1]-p2[1]))
    alpha = as.numeric(p1[2] - beta*p1[1])
    y <- (beta*x) + alpha
    return(data.frame(x, y))
  }
  
  calcPoints <- function(n){
    res <- data.frame(x = as.numeric(), y = as.numeric())
    for(i in c(1,3)){
      res <- rbind(res, linearEq(data[i,], data[if(i == 4) 1 else i + 1,], n = n))
    }
    return(res)
  }
  
  
  
  # calculate the points 
  dt <- calcPoints(n = n)
  
  preds <- data.frame(x = as.numeric(), y = as.numeric())
  for(i in 1:(n+1)){
    preds <- rbind(preds, linearEq(dt[1:(n + 1),][i, ], dt[(n + 2):(n + 2 + n),][i, ], n = n))
  }
  
  m <- as.matrix(preds)
  dm <- as.matrix(stats::dist(m))

  pointys <- function(n){
    dt <- calcPoints(n = n)
    preds <- data.frame(x = as.numeric(), y = as.numeric())
    for(i in 1:(n+1)){
      preds <- rbind(preds, linearEq(dt[1:(n + 1),][i, ], dt[(n + 2):(n + 2 + n),][i, ], n = n))
    }
    return(preds)
  }
    
  preds <- pointys(n)
  nums <- c(1,2,n+2,n+3)
  plots <- data.frame(x = numeric(), y = numeric())
  s <- c(0:n^3)[rep(c(rep(TRUE, n), FALSE), n)][1:(n^2)]
  
  
  for(i in s){
    plots <- rbind(plots, sortToPoly(preds[nums+i,]))  
  }
  
  plots$Q <- factor(rep(1:((dim(plots)[1])/4), each = 4))

  return(plots)
}






plots <- quadSplit(data = data, n = 7)

ggplot(data, aes(x = x, y = y)) + 
  geom_polygon(data = plots, aes(x = x, y = y, fill = (Q)))

