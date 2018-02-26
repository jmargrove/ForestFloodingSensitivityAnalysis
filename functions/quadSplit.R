#####################################################################################
#' @title quadSplit
#' @author James Margrove 
#' 
#' @description split a polygon into as many seperate polyons as specified 
#' 
#' @param coords four coordinates specifying the polygon 
#' @param n number of splits or sqrt of new quadrats 
#'

quadSplit <- function(data, n, graph = FALSE) {
  source("./functions/sortToPoly.R")
  source("./functions/midPoint.R")
  
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

  if(graph){
    ggplot2::ggplot(plots, aes(x = x, y = y, fill = Q)) + 
    geom_polygon()  
  }
  
  return(plots)
}
