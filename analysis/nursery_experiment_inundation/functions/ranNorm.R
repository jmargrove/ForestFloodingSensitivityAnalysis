##### Title: the modified binplotfunction 
##### Author: James Margrove 
##### Date: 26 01 2018

ranNorm <- function(intercept = NULL, slope = 1, model = NULL){
  if(slope >= 2){par(mfcol=c((2),(1*slope)))}else{par(mfrow=c(1,2))} # orentation of the graphs 
  for(i in 1:slope){  # loop depends on how many slopes there are.
    MAIN <- paste("RanEffect:", intercept) # title
    ranEffects <- ranef(model)
    effect <- ranEffects[[intercept]][[i]] # exctaract the random effects 
    qqnorm(effect, main = MAIN); qqline(effect) # qqnorm graph, with line 
    pvalue <- round(shapiro.test(effect)[[2]], digit = 3) # pvalue
    plot(density(effect), main = paste("shp_test:",pvalue))}} # density plot with p-value 