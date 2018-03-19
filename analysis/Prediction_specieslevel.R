################################################################################
##### Predicting Reuben's plot data 
##### author: James Margrove 

# Clear work space 
rm(list=ls())

# Import packages 
require(ggplot2)
require(simex)
source("./functions/booter.R")

# Import data 
sp <- read.table("./data/riskratio_data.txt", header = TRUE)$sp[-c(4,7, 13)]
data <- read.table( "./data/Reuben_data/data_cleaned.txt", header = TRUE)
with(data, tapply(dden, Species, mean))
# Import model 
load("./models/pele_fsen_dden_Abundance")
summary(model)
# Predict elevation
data$p <- predict(model, data, type = "response")
head(data)

# organsie data in to a data.frame
e <- as.vector(with(data, tapply(elev, Species, mean, na.rm = T)))
p <- as.vector(with(data, tapply(p, Species, mean)))
a <- (as.vector(with(data, tapply(p, Species, length))))
dden <- round(as.vector(with(data, tapply(dden, Species, mean))), 2)
dt <- data.frame(e, p, a, sp, dden)
rownames(dt) <- c()

# SD of the predictions 
preds <- data.frame(rr = as.vector(with(data, tapply(rr, Species, mean))), 
                    dden = as.vector(with(data, tapply(dden, Species, mean))))

# Run native model 
model3 <- lm(e ~ p, weight = a, dt)
summary(model3)

# 95% coef
coef <- summary(model3)$coefficients
coef[2,2]*1.96*c(1,-1) + coef[2,1]
coef[1,2]*1.96*c(1,-1) + coef[1,1]

# Calculate the R^2 manually, for all individuals within the plots 
SSE_H1 <- sum((data$elev - predict(model3, data, type = "response"))^2)
SSE_H0 <- sum((data$elev - mean(data$elev))^2)
R2 <- (1 - (SSE_H1/SSE_H0))
R2 

resid(lm(elev ~ p, data))[1:10]
(data$elev - data$p)[1:10]

# nudge text 
dt$nudge_text <- rep(0.5, 13)
dt$nudge_text[which(dt$sp == "Slep")] <- -1
dt$nudge_text[which(dt$sp == "Spar")] <- -1
dt$nudge_text[which(dt$sp == "Ptom")] <- 2
dt$nudge_text[which(dt$sp == "Smec")] <- -1
dt$nudge_text[which(dt$sp == "Swal")] <- -1
dt$nudge_text[which(dt$sp == "Sfal")] <- -1

preds <- data.frame(p = seq(min(p),max(p), length = 100))
preds$e <- predict(model3, preds, type = "response")
preds$CI <- predict(model3, preds, type = "response", se.fit = TRUE)$se.fit * 1.96
# jitter values 
j <- rep(c(130, 125), 7)[1:13] + 10

colnames(dt)[3] <- "Abundance"

p2 <- ggplot(dt, aes(x = p, y = e)) + 
  geom_vline(xintercept = p, alpha = 0.2, linetype = 2) +
  geom_boxplot(data = data, aes(x = p, y = elev, group = Species), width = 1) + 
  geom_point(aes(size = Abundance), fill = "grey") +
  geom_ribbon(data = preds, aes(ymin = e - CI, ymax = e + CI), alpha = 0.2) + 
  geom_line(data = preds, aes(y = e + CI, x = p), alpha = 0.2) + 
  geom_line(data = preds, aes(y = e - CI, x = p), alpha = 0.2) + 
  geom_line(data = preds, aes(x = p, y = e)) +
  theme_classic() + 
  geom_text(aes(x = p, y = j, label = sp, size = 20, angle = 60), nudge_x = dt$nudge_text, show.legend = FALSE) + 
  geom_point(aes(x = p, y = j), size = 0.2) + 
  theme(legend.position="top") +
  xlab("Predicted species elevation (asl m)") +
  ylab("Actual species mean elevation (asl m)") + 
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "#BC4B51") + 
  geom_hline(yintercept = 67.43401, linetype = 2, alpha = 0.2) 

p2


ggsave(p2, file = './graphs/Reuben_meanPlotElevation.png', 
       width = 4, 
       height = 4.2)


