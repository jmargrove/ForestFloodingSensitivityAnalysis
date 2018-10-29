# graphing and bootstrapping the quantile regression model 
rm(list = ls())

load("./analysis/wood_density_distribution/models/quanreg_models/wooddensity_VS_elevation_quantreg.R")
source("./analysis/wood_density_distribution/data/data_index.R")
source("./analysis/wood_density_distribution/function_index.R")
summary(quantModel)
# Prediction from the mode and confidence intervales bootstrapped 
pred <- data.frame(d = seq(min(wood_density_data_178ha$d), max(wood_density_data_178ha$d), length = 100))
taus <- c(0.025, 0.1, 0.5, 0.9, 0.975)
preds <- expand.grid(d = pred$d, Quantile = as.factor(taus))
preds$e <- as.vector(predict(quantModel, pred, type = "response"))
CIQ <- booter(quantModel, data = wood_density_data_178ha, preds = pred, quantreg = TRUE, n = 5000)
preds$CI025 <- CIQ[1,]
preds$CI975 <- CIQ[2,]

head(preds)
# Graphing the model
cols <- c("#8CB369", "#F4E285", "#4C8577","#F4A259", "#BC4B51")
p1 <- ggplot(preds[preds$d < 0.6, ], aes(x = d, y = exp(e), group = Quantile)) + 
  geom_point(data = wood_density_data_178ha[wood_density_data_178ha$d < 0.6, ], inherit.aes = F,  aes(x = d, y = e)) + 
  geom_ribbon(aes(ymin = exp(CI025), ymax = exp(CI975), group = Quantile, fill = Quantile), alpha = .74) + 
  geom_line()  +
  theme_bw() +
  ylab("Elevation (m asl)") + 
  xlab(bquote("Wood density g" ~cm^-3 )) +
  theme(legend.position = "top") + 
  scale_fill_manual(values = cols)

p1

ggsave(p1, 
       file = "./analysis/wood_density_distribution/graph_code/graphs/wooddensity_VS_elevation_quanReg.png", 
       width = 5, 
       height = 5
)

