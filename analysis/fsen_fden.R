################################################################################
# title: flooding sensitivity response predicting species distributions 
# author: James Margrove 

# Clear work space 
rm(list = ls())

# Import packages
require(ggplot2)
source('./functions/booter.R')

# Import data 
rr_data <- read.table("./data/riskratio_data.txt", header = TRUE)
dden_data <- read.table("./data/dden_adult.txt", header = TRUE)

dden_data <- dden_data[order(dden_data$sp), ]
rr_data$dden <- dden_data$dden_adult

rr_data$fden <- cut(rr_data$dden, 
                    breaks = c(min(rr_data$dden)-.1, 
                               mean(rr_data$dden), 
                               max(rr_data$dden)), 
                    labels = c("low","high"))





# Exploration 
p1 <- ggplot(rr_data, aes(x = fden, y = log(rr))) + 
  geom_point() + 
  stat_smooth(method = lm, color = '#000000', size = 0.5) + 
  ylab("log(water inundation sensitivity)") + 
  xlab("log(adult wood density)") + 
  theme_classic()

p1

# Model the data 
rr_data$fden <- relevel(rr_data$fden, ref = "high")
?relevel
model <- lm(rr ~ fden, data = rr_data)
summary(model)
par(mfrow=c(2,2))
plot(model)

rr_data$resid <- residuals(model)
with(rr_data, tapply(resid, fden, var))*100

# The low wood density groups residuals had an order of magnitude greater variance than 
# the high wood density group. So we are going to log transform the data. 


model2 <- update(model, -rr + log(rr) ~ .)
summary(model2)
plot(model2)
rr_data$resid2 <- residuals(model2)
with(rr_data, tapply(resid2, fden, var))*100

# Evaluation 
preds <- data.frame(fden = c("low", "high"))

# Prediction  
preds$rr <- predict(model2, 
                    newdata = preds,
                    type = "response")

preds$CI <- predict(model2, 
                    newdata = preds, 
                    type = "response", 
                    se.fit = TRUE)$se.fit

# Plot the graph 
p1 <- ggplot(preds, aes(x = fden, y = exp(rr))) + 
             geom_point() + 
             geom_errorbar(aes(ymin = exp(rr - CI * 1.96), 
                              ymax = exp(rr + CI * 1.96)), 
                           width = 0.2,
                           alpha = 0.22) + 
  ylab("water inundation sensitivity") + 
  xlab(bquote("Wood density")) +
  theme_classic() +
  theme(legend.position = c(0.2, 0.85))


p1


# Boot strap for coef CIs 
coef_CI <- booter(model = model, 
                  data = rr_data, 
                  preds = preds,
                  coef = TRUE,
                  n = 5000)
coef_CI

# saving the plot 
ggsave(p1, file = "./graphs/fsen_vs_fden.png",
       width = 4, 
       height = 4)

