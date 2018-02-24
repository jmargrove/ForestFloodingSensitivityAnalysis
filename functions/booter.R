#####################################################################################
# title: booty: a function for bootstrapping data
# author: James Margrove

booter <- function(model, data, preds, n, coef = FALSE,  CI = "95%", quantReg = FALSE) {
  if(!coef) {
    booty <- function() {
      random_row_numbers <- sample(1:dim(data)[1], replace = TRUE)
      random_row <- data[random_row_numbers, ]
      btm <- update(model, . ~ . , data = random_row)
      preds <- predict(btm, newdata = preds, type = "response", re.form = NA)
      if(! quantReg) {
        return (preds)
      } else {
        return (as.vector(preds))
      }
    }
  }
  if(coef == TRUE) {
    
    booty <- function() {
      random_row_numbers <- sample(1:dim(data)[1], replace = TRUE)
      random_row <- data[random_row_numbers, ]
      btm <- update(model, . ~ . , data = random_row)
      coef(btm)
    }
  }

  require(foreach)
  boots <- foreach(i = 1:n, .combine = cbind) %do% booty()
  if(CI == "95%") {
    apply(boots, 1, quantile, c(0.025, 0.975))
  } 
  else if(CI == "SD") {
    apply(boots, 1, sd)
  }
  
}