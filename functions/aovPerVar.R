################################################################################
# title: Calculation of ANOVA perecentage variance 
# author: James Margrove 


aovPerVar <- function(ma) {
  res <- ma[,1]/sum(ma[,1])*100
  names(res) <- row.names(ma)
  res
}