################################################################################
#'@title: Calculation of ANOVA perecentage variance 
#'@author: James Margrove 
#' 
#'@param ma which is a anova model 
#'
#'@description quickly calculates the precentage var for an anova mode or lm model 
#'
#'@return precentage variance of each anova


aovPerVar <- function(ma) {
  if(class(ma) == "lm"){
    ma <- car::Anova(ma)
  }
  res <- ma[,1]/sum(ma[,1])*100
  names(res) <- row.names(ma)
  
  return(res)
}