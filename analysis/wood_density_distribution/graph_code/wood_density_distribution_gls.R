# graphs 
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
CI <- booter(model2, 
                data = wood_density_data_178ha, 
                preds = preds, 
                n = 10)

# ggplot of the results 
p1 <- ggplot(preds, aes(x = d, y = e)) + 
  geom_point(data = wood_density_data_178ha, aes(x = d, y = e, color = fd)) + 
  geom_line() + 
  theme_bw() +
  geom_ribbon(aes(ymin = CI[1,], ymax = CI[2,]), alpha = 0.5)

ggsave(p1, file = './analysis/wood_density_distribution/graph_code/graphs/wooddensity_VS_elevation_gls.png', width = 3, height = 3)

