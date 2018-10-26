################################################################################
##### Predicting Reuben's plot data 
##### author: James Margrove 

# Clear work space 

rm(list=ls())
Sys.setenv(LANG = "en")
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
# Prediction intervals 
data <- cbind(data, predict(model, data, type = "response", interval = "prediction", weights = data$abn))

head(data)


model2 <- lm( elev ~ p, data = data)
summary(model2)

#### plot the model 
preds <- data.frame(p = seq(min(data$p), max(data$p), length = 100))
preds$el <- predict(model2, preds, type = "response")
preds$CI <- predict(model2, preds, type = "response", se.fit = TRUE)$se.fit * 1.96
head(preds)

# To jitter names
j <- rep(c(140, 135, 130), 7)[1:13]
p1 <- ggplot(preds, aes(x = p, y = el)) + 
              geom_violin(data = data, aes(x = round(p, 2), y = el, group = data$Species),  scale = "count", width = 10) + 
              geom_point(data = data, aes(x = p, y = el), alpha = 0.15) + 
              geom_ribbon(aes(ymin = el - CI, ymax = el + CI), alpha = 0.5) + 
              geom_line() + 
              theme_classic() +
              xlab("Predicted elevation (asl m)") +
              ylab("Actual elevation (asl m)") + 
              geom_abline(intercept = 0, slope = 1, linetype = 2, color = "red")

p1

ggsave(p1, file = './graphs/Reuben_EachIndividualElevation.png', 
       width = 4, 
       height = 4)


# 
e <- with(data, tapply(elev, Species, mean))
p <- with(data, tapply(p, Species, mean))
a <- with(data, tapply(p, Species, length))
d <- with(data, tapply(dden, Species, mean))

mean_dden <- 0.463125

model3 <- (lm(e ~  p, weights = a))
summary(model3)

summary(update(model3, . ~ . + offset(1 * p)))
fit <- summary(model3)
t.value = (1-fit$coefficients[2,1])/fit$coefficients[2,2]
p.value = 2 * pt(t.value, df = df.residual(model3), lower.tail = FALSE)
p.value

# we test the hypothesis that the line is different from one finding that this no the case. 

preds <- data.frame(p = seq(min(p),max(p), length = 100))
preds$e <- predict(model3, preds, type = "response")
preds$CI <- predict(model3, preds, type = "response", se.fit = TRUE)$se.fit * 1.96
head(preds)

dt <- data.frame(e, p, a, sp)
dt$nudge_text <- rep(0.5, 13)
dt$nudge_text[which(dt$sp == "Slep")] <- -1
dt$nudge_text[which(dt$sp == "Spar")] <- -1
head(dt)

p2 <- ggplot(dt, aes(x = p, y = e)) + geom_line() 
              geom_violin(data = data, aes(x = round(p, 2), y = el, group = data$Species),  scale = "count", width = 10) +
              geom_point(aes(size = a, alpha = 0.33)) +
              geom_ribbon(data = preds, aes(ymin = e - CI, ymax = e + CI), alpha = 0.2) + 
              geom_line(data = preds, aes(y = e + CI, x = p), alpha = 0.2) + 
              geom_line(data = preds, aes(y = e - CI, x = p), alpha = 0.2) + 
              geom_line(data = preds, aes(x = p, y = e)) +
              theme_classic() + 
              geom_text(aes(x = p, y = j, label = sp, size = 15, angle = 60), nudge_x = dt$nudge_text) + 
              geom_point(aes(x = p, y = j), size = 0.2) + 
              geom_vline(xintercept = p, alpha = 0.2, linetype = 2) + 
              theme(legend.position="none") +
              xlab("Predicted species elevation (asl m)") +
              ylab("Actual species mean elevation (asl m)") + 
              geom_abline(intercept = 0, slope = 1, linetype = 2, color = "red") + 
              #ylim(65, 142) + 
              geom_hline(yintercept = 67.43401, linetype = 2, alpha = 0.2)

p2

ggsave(p2, file = './graphs/Reuben_meanPlotElevation.png', 
       width = 8, 
       height = 4)



################################################################################
# doing it for each 1 ha plot 

abn <- as.vector(with(data, tapply(Species, list(Species, Forest, ha4plot), length)))

df <- expand.grid(Species = levels(data$Species), Forest = levels(data$Forest), ha4Plot = 1:4)
df$abn <- abn
df$dden <- rep(with(data, tapply(dden, Species, mean)), 4*5)
df$rr <- rep(with(data, tapply(rr, Species, mean)), 4*5)
df$el <- rep(as.vector(with(data, tapply(el, list(Forest, ha4plot), mean))), each = 13)
df$pe <- predict(model, df, type = "response")

df <- df[!is.na(df$abn),]

ggplot(df, aes(x = pe, y = el)) + geom_point() + stat_smooth()

model4 <- (lm(el ~ pe, weight = abn, df))
summary(model4)
summary(update(model4, . ~ . + offset(1*pe)))

preds <- data.frame(pe = seq(min(df$pe),max(df$pe), length = 100))
preds$el <- predict(model4, preds, type = "response")
preds$CI <- predict(model4, preds, type = "response", se.fit = TRUE)$se.fit * 1.96




p3 <- ggplot(df, aes(x = pe, y = el))  + 
  geom_violin(data = data, aes(x = round(p, 2), y = el, group = data$Species),  scale = "count", width = 10) +
  geom_point(aes(size = abn, alpha = 0.2)) +
  geom_ribbon(data = preds, aes(ymin = el - CI, ymax = el + CI), alpha = 0.2) + 
  geom_line(data = preds, aes(y = el + CI, x = pe), alpha = 0.2) + 
  geom_line(data = preds, aes(y = el - CI, x = pe), alpha = 0.2) + 
  geom_line(data = preds, aes(x = pe, y = el)) +
  theme_classic() + 
  
  geom_text(data = dt, aes(x = p, y = j, label = sp, size = 5), angle = 60) + 
  
  geom_vline(xintercept = dt$p, alpha = 0.1) + 
  theme(legend.position="none") +
  xlab("Predicted elevation (asl m)") +
  ylab("Actual elevation (asl m)") + 
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "red")

p3

ggsave(p3, file = './graphs/Reuben_mean1haPlotElevation.png', 
       width = 4, 
       height = 4)

p1
p2
p3
