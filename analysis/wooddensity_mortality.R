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
p1 <- ggplot(rr_data, aes(x = fden, y = rr)) + 
  geom_point() + 
  stat_smooth(method = lm, color = '#000000', size = 0.5) + 
  ylab("log(water inundation sensitivity)") + 
  xlab("log(adult wood density)") + 
  theme_classic()

p1

# Model the data 
model <- lm(log(rr) ~ fden, data = rr_data)
summary(model)




# Evaluation 
preds <- data.frame(dden = seq(from = min(rr_data$dden), 
                               to = max(rr_data$dden), 
                               length = 100))

# Prediction  
preds$rr <- predict(model, 
                    newdata = preds,
                    type = "response")

preds$CI <- predict(model, 
                    newdata = preds, 
                    type = "response", 
                    se.fit = TRUE)$se.fit

# Plot the graph 
p1 <- ggplot(preds, aes(x = log(dden), y = rr)) + 
             geom_line() + 
             geom_ribbon(aes(ymin = rr - CI * 1.96, 
                             ymax = rr + CI * 1.96), 
                         alpha = 0.22) +
             geom_line(aes(x = log(dden), 
                           y = rr - CI * 1.96), 
                       linetype = 2) + 
             geom_line(aes(x = log(dden), 
                           y = rr + CI * 1.96), 
                       linetype = 2) + 
             geom_point(data = rr_data, 
                        aes(x = log(dden), 
                            y = log(rr))) + 
             geom_line() + 
             ylab("log(water inundation sensitivity)") + 
             xlab(bquote("log(Wood density g" ~cm^-3~")" )) +
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
ggsave(p1, file = "./graphs/log_fsen_vs_dden.png",
       width = 4, 
       height = 4)

