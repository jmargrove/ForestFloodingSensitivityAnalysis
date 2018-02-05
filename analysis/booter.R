
booter <- function(model, data, preds, coef = FALSE) {
  if(coef) {
    random_row_numbers <- sample(1:dim(data)[1], replace = TRUE)
    random_row <- data[random_row_numbers, ]
    btm <- update(model, . ~ . , data = random_row)
    predict(btm, newdata = preds, type = "response", re.form = NA)
  }
  else {
    "needs to be coded lah"
  }

}