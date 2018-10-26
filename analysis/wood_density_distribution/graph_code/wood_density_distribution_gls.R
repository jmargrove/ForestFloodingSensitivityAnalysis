#' @title graph: wood_density_distribution with gls 
#' @author James Margrove

# Clear workspace 
rm(list = ls())

# import model 
load('./analysis/wood_density_distribution/models/gls_models/model2.R')
# import data 
source("./analysis/wood_density_distribution/organisation.R")
# import functions 
source('./functions/booter.R')

# prediction frame for the model results 
d_seq <- with(wood_density_data_178ha, seq(min(d), max(d), length = 100))
preds <- data.frame(d = d_seq)
preds$e <- predict(model2, newdata = preds, type = "response")

# bootstrap the model
# n = 5000  #number of bootstrapping samples, if n is not defined 10 rounds will be done. See 'ternary' below
# 
# CI <- booter(model2,
#                 data = wood_density_data_178ha,
#                 preds = preds,
#                 n = if(n) n else 10)
# 
# save(CI, file = './analysis/wood_density_distribution/bootstrapped/wooddensity_VS_elevation_gls_n=5000.R')

# Load bootstrapped values
load('./analysis/wood_density_distribution/bootstrapped/wooddensity_VS_elevation_gls_n=5000.R')

# ggplot of the results 
p1 <- ggplot(preds, aes(x = d, y = e)) + 
  geom_point(data = wood_density_data_178ha, aes(x = d, y = e), alpha = 0.2) + 
  geom_line() + 
  theme_bw() +
  geom_ribbon(aes(ymin = CI[1,], ymax = CI[2,]), alpha = 0.5) + 
  ylab("Elevation (m asl)") + 
  xlab(bquote("Wood density g" ~cm^-3 )) +
  theme(legend.position = "top") + 
  scale_fill_manual(values = cols) + 
  xlim(NA, 0.62) + 
  ylim(NA, 150)

p1

ggsave(p1, 
       file = './analysis/wood_density_distribution/graph_code/graphs/wooddensity_VS_elevation_gls.png', 
       width = 5, 
       height = 5)

