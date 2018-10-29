# Combining gls result and quantile regression into one figure 
# Clear working space 
rm(list = ls())

gls_preds <- read.table('./analysis/wood_density_distribution/data/gls_predictions.txt', header = TRUE)
qr_preds <- read.table('./analysis/wood_density_distribution/data/quantreg_predictions.txt', header = TRUE)
qr_preds$Quantile <- as.factor(qr_preds$Quantile)
str(qr_preds)
# import data 
source("./analysis/wood_density_distribution/organisation.R")
# colors 
source('./colors.R')

# ggplot of the results 
p1 <- ggplot(gls_preds, aes(x = d, y = e)) + 
  geom_point(data = wood_density_data_178ha, aes(x = d, y = e), alpha = 0.5) + 
  geom_line() + 
  theme_bw() +
  geom_ribbon(aes(ymin = CI025, ymax = CI975), alpha = 0.5) + 
  ylab("Elevation (m asl)") + 
  xlab(bquote("Wood density g" ~cm^-3 )) +
  theme(legend.position = "top") + 
  scale_fill_manual(values = cols) + 
  xlim(NA, 0.62) + 
  ylim(NA, 150) +
  theme(text = element_text(size=20)) + 
  geom_line(data = qr_preds[qr_preds$Quantile != '0.5',], 
            aes(x = d, y = e, color = Quantile), linetype = 2, size = 1) +
  scale_color_manual(values = cols) 
  

p1

ggsave(p1, 
       file = "./analysis/wood_density_distribution/graph_code/graphs/wooddensity_VS_elevation_quanReg_gls.png", 
       width = 6, 
       height = 6
)
