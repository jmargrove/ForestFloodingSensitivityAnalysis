# standardise 

stand <- function(data, var){
  (data[,var] - mean(data[,var], na.rm = T))/sd(data[,var], na.rm = T)
}