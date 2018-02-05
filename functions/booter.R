#####################################################################################
# title: booty: a function for bootstrapping data
# author: James Margrove

booter <- function(model, data, preds, n, coef = FALSE) {
  if(!coef) {
    booty <- function() {
      random_row_numbers <- sample(1:dim(data)[1], replace = TRUE)
      random_row <- data[random_row_numbers, ]
      btm <- update(model, . ~ . , data = random_row)
      predict(btm, newdata = preds, type = "response", re.form = NA)
    }
  }
  else {
    "needs to be coded lah"
  }

  require(foreach)
  boots <- foreach(i = 1:n, .combine = cbind) %do% booty()
  apply(boots, 1, quantile, c(0.025, 0.975))
}