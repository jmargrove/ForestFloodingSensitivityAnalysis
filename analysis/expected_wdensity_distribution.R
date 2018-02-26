################################################################################
#' @title Expected wood density distribution  
#' @author James Margrove 
#' 
#' @discription a graph of the wood density hypothesis 
#'  

require(ggplot2)

data <- data.frame(point = c("lowlow", "lowhigh", "highhigh"), 
                   x = c(0,100,100),
                   y = c(0,0,100))

ggplot(data, aes(x = x, y = y)) + 
  geom_polygon() + 
  theme_classic() + 
  xlab(bquote("Wood density g" ~cm^-3 )) +
  ylab(expression("elevation (m) /n %->%")) 
