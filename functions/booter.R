#####################################################################################
#'@title booty: a function for bootstrapping data
#'@author James Margrove
#'@param model model to be bootstrapped 
#'@param data - data for sampling with replacement 
#'@param preds - prediction data frame for the model
#'@param n - number of iterations of bootstraps 
#'@param coef - boolean, if true calculate coefs, if false calculate preds 
#'@param CI - string default value is 95% calculate 95% quatiles for distribution 
#'@param quantreg - calculate predictions for a quantile regression analysi 
#'@dependancies foreach 
#'@return bootstrapped predictions or confidence intervals for lm or quantreg 
#'
require(foreach)

booter <- function(
  model, data, preds, n, coef = FALSE,  CI = "95%", quantreg = FALSE) {
  if (!coef) {
    booty <- function() {
      random_row_numbers <- sample(1:dim(data)[1], replace = TRUE)
      random_row <- data[random_row_numbers, ]
      btm <- update(model, . ~ ., data = random_row)
      preds <- predict(btm, newdata = preds, type = "response", re.form = NA)
      if(! quantreg) {
        return (preds)
      } else {
        return (as.vector(preds))
      }
    }
  }
  if (coef == TRUE) {
    print("enter Coef")
    if(quantreg == FALSE){
      booty <- function() {
        random_row_numbers <- sample(1:dim(data)[1], replace = TRUE)
        random_row <- data[random_row_numbers, ]
        btm <- update(model, . ~ ., data = random_row)
        coef(btm) 
      }
    }
      if (quantreg == TRUE){
        print("enter quantreg")
        booty <- function() {
          random_row_numbers <- sample(1:dim(data)[1], replace = TRUE)
          random_row <- data[random_row_numbers, ]
          btm <- update(model, . ~ ., data = random_row)
          as.vector(coef(btm))
        }
    }
  }


  boots <- foreach(i = 1:n, .combine = cbind) %do% booty()
  if (CI == "95%") {
    apply(boots, 1, quantile, c(0.025, 0.975))
  }
  else if (CI == "SD") {
    apply(boots, 1, sd)
  }
}