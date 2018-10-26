####dAIC()
#dAIC: a funciton to calculate the differance in AIC values to two models 

dAIC <- function(x1,x2) abs(diff(AIC(x1,x2)[,2]))