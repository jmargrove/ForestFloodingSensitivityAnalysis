################################################################################
##### Predicting Reuben's plot data 
##### author: James Margrove 

# Clear work space 

rm(list=ls())

# Import packages 
require(ggplot2)
sp <- read.table("./data/riskratio_data.txt", header = TRUE)$sp[-c(4,7, 13)]
# Import data 
data <- read.table( "./data/Reuben_data/data_cleaned.txt", header = TRUE)

load("./models/pele_fsen_dden_Abundance")
summary(model)
dim(data)

# Predict elevation
data$p <- predict(model, data, type = "response")

model2 <- lm( el ~ p, data = data)
summary(model2)

#### plot the model 
preds <- data.frame(p = seq(min(data$p), max(data$p), length = 100))
preds$el <- predict(model2, preds, type = "response")
preds$CI <- predict(model2, preds, type = "response", se.fit = TRUE)$se.fit * 1.96

head(data)

ggplot(preds, aes(x = p, y = el)) + 
  geom_point(data = data, aes(x = p, y = el), alpha = 0.15) + 
  geom_ribbon(aes(ymin = el - CI, ymax = el + CI), alpha = 0.1) + 
  geom_line() + 
  theme_classic() +
  xlab("predicted elevation") + 
  ylab("elevation of the 1ha plot")


ggplot(preds, aes(x = p, y = el)) + 
  geom_violin(data = data, aes(x = round(p, 2), y = el, group = data$Species),  scale = "count", width = 10) + 
  geom_point(data = data, aes(x = p, y = el), alpha = 0.15) + 
  geom_ribbon(aes(ymin = el - CI, ymax = el + CI), alpha = 0.5) + 
  geom_line() + 
  theme_classic() +
  xlab("predicted elevation") + 
  ylab("elevation of the 1ha plot") +
  xlim(0, 55)



# 
e <- with(data, tapply(el, Species, mean))
p <- with(data, tapply(p, Species, mean))
a <- with(data, tapply(p, Species, length))

model3 <- (lm(e ~ p, weights = a))

preds <- data.frame(p = seq(min(p),max(p), length = 100))
preds$e <- predict(model3, preds, type = "response")
preds$CI <- predict(model3, preds, type = "response", se.fit = TRUE)$se.fit * 1.96

dt <- data.frame(e, p, a, sp)

ggplot(dt, aes(x = p, y = e)) + geom_point() + 
  geom_violin(data = data, aes(x = round(p, 2), y = el, group = data$Species),  scale = "count", width = 10) +
  geom_point(aes(size = a, alpha = 0.2)) +
  geom_ribbon(data = preds, aes(ymin = e - CI, ymax = e + CI), alpha = 0.2) + 
  geom_line(data = preds, aes(y = e + CI, x = p), alpha = 0.2) + 
  geom_line(data = preds, aes(y = e - CI, x = p), alpha = 0.2) + 
  geom_line(data = preds, aes(x = p, y = e)) +
  theme_classic() + 
  geom_text(aes(x = p, y = 75, label = sp, size = 15, angle = 60), position = position_jitterdodge(jitter.height = 3)) + 
  geom_vline(xintercept = p, alpha = 0.1) + 
  theme(legend.position="none") +
  xlab("Predicted elevation (m)") +
  ylab("Actual elevation (m)")
  

?position_dodge

args(position_jitterdodge)






