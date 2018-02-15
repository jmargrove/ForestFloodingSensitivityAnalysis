################################################################################
##### Predicting Reuben's plot data 
##### author: James Margrove 

# Clear work space 

rm(list=ls())

# Import packages 
require(ggplot2)

# Import data 
data <- read.table( "./data/Reuben_data/data_cleaned.txt", header = TRUE)

load("./models/pele_fsen_dden_Abundance")
summary(model)

####
data$p <- predict(model, data, type = "response")

model2 <- (lm( el ~ p, data = data[data$Forest != "A3",]))
summary(model2)

#### plot the model 
preds <- data.frame(p = seq(min(data$p), max(data$p), length = 100))
preds$el <- predict(model2, preds, type = "response")
preds$CI <- predict(model2, preds, type = "response", se.fit = TRUE)$se.fit * 1.96

head(data)

ggplot(preds, aes(x = p, y = el)) + 
  geom_violin(data = data, aes(x = p, y = el, fill = factor(p)), alpha = 0.3) + 
  geom_ribbon(aes(ymin = el - CI, ymax = el + CI), alpha = 0.2) + 
  geom_line() + 
  theme_classic() +
  xlab("predicted elevation") + 
  ylab("elevation of the 1ha plot")



ggplot(data, aes(x =p, y = el)) + 
  geom_violin(aes(group = p, color = factor(p)), scale = "width") + 
  geom_point() + geom_jitter() + stat_smooth(method = lm)

