# testing the median/mead elevation values against the model predicted values 

# Clear work space 
rm(list=ls())

# Import packages 
require(ggplot2)

# Import data 
data <- read.table( "./data/Reuben_data/data_cleaned.txt", header = TRUE)
colnames(data)[which(colnames(data) == 'rr')] <- 'diff_mort'

# import model
load("./analysis/inundation_predicts_species_distributions/models/elevation_Vs_wooddensityN_diff_mortality.R")

# Predict elevation
data$predicted <- predict(model2, data, type = "response")
# do the predicted values match the actual elevation values 
ggplot(data, aes(x = predicted, y = elev), alpha = 2) + geom_point() +
  stat_smooth(method = lm)
head(data)
model <- lm(predicted ~ elev, data, subset = abn > 5)
summary(model)
# here we can include a spatial effect within gls 
data$x <- data$x + rnorm(dim(data)[1])
data$y <- data$y + rnorm(dim(data)[1])
require(nlme)
model1 <- gls(predicted ~ elev, data = data)
model2 <- gls(predicted ~ elev, data = data, correlation = corExp(form = ~ x + y))

summary(model1)
summary(model2)

# what about the means
preds_data <- data.frame(predicted = with(data, tapply(predicted, Species, median)), 
                         elevation = with(data, tapply(elev, Species, median)), 
                         abundance = with(data, tapply(elev, Species, length)))

# model
model1 <- lm(elevation ~ predicted, data = preds_data, subset = abundance > 3)
summary(model1)
0.8614  +   0.3016 * 1.96
0.8614  -   0.3016 * 1.96

# graphing the variables 
ggplot(preds_data[preds_data$abundance > 3,], aes(y = elevation, x = predicted)) + geom_point() + 
  stat_smooth(method = lm)



(preds_data$predicted -  preds_data$elevation)
