# wood density quantile regression 

source("./packages.R")
source("./analysis/wood_density_distribution/data_index.R")
source("./analysis/wood_density_distribution/function_index.R")

# analysis 
# Taus/quantiles of 
taus = c(0.025, 0.1, 0.5, 0.9, 0.975)

# Run the model 
quantModel <- rq(d ~ e, data = wood_density_data_178ha, tau = taus)
summary(quantModel)

# Prediction from the mode and confidence intervales bootstrapped 
pred <- data.frame(e = seq(min(wood_density_data_178ha$e), max(wood_density_data_178ha$e), length = 100))
preds <- expand.grid(e = pred$e, Quantile = factor(taus))
preds$d <- as.vector(predict(quantModel, pred, type = "response"))
CIQ <- booter(quantModel, data = wood_density_data_178ha, preds = pred, quantreg = TRUE, n = 100)
preds$CI025 <- CIQ[1,]
preds$CI975 <- CIQ[2,]

# Graphing the model 
cols <- c("#8CB369", "#F4E285", "#4C8577","#F4A259", "#BC4B51")
p1 <- ggplot(preds, aes(x = e, y = d, group = Quantile)) + 
  geom_point(data = wood_density_data_178ha, inherit.aes = F,  aes(x = e, y = d)) + 
  geom_ribbon(aes(ymin = CI025, ymax = CI975, group = Quantile, fill = Quantile), alpha = .74) + 
  geom_line() + 
  theme_classic() + 
  theme(legend.position = "top") + 
  scale_fill_manual(values = cols) + 
  ylim(0.35, 0.65)

p1
