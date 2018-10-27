# wood density quantile regression 

rm(list = ls())

source("./packages.R")
source("./analysis/wood_density_distribution/data/data_index.R")
<<<<<<< HEAD
source("./analysis/wood_density_distribution/function_index.R")
=======
>>>>>>> eddbaf3409778575f50f1e19684046df57ec3a8e

# Taus/quantiles of 
taus = c(0.025, 0.1, 0.5, 0.9, 0.975)

# Run the model, log-log version as it fits better
quantModel <- rq(log(e) ~ log(d), data = wood_density_data_178ha, tau = taus)
summary(quantModel)
<<<<<<< HEAD
plot(quantModel)

# save model for graphing
save(quantModel, file = "./analysis/wood_density_distribution/models/quanreg_models/wooddensity_VS_elevation_quantreg.R")

# bootstrapping confidence intervals 
coef_CI_quantreg <- booter(quantModel, data = wood_density_data_178ha, coef = TRUE, n = 5000, quantreg = TRUE)
coef_CI_quantreg

# graphic of the changing slope and intercept of quantile regression 
n = 15
taus2 <- seq(0.025, 0.975, length = n)
quantModel2 <- rq(log(e) ~ log(d), data = wood_density_data_178ha, tau = taus2)

slopes <- quantModel3$coefficients[2, ]
int <- quantModel3$coefficients[2, ]
# CI_slopes <- booter(quantModel2, data = wood_density_data_178ha, n = 5000, coef = TRUE, quantreg = TRUE)
# save(CI_slopes, file = './analysis/wood_density_distribution/bootstrapped/CI_quantreg_slopes_n=5000.R')
load(file = './analysis/wood_density_distribution/bootstrapped/CI_quantreg_slopes_n=5000.R')

taus_data <- data.frame(taus = taus2, 
                        slopes, 
                        int, 
                        CI025 = CI_slopes[1, seq(2, 30, by = 2)], 
                        CI975 = CI_slopes[2, seq(2, 30, by = 2)] )



p1 <- ggplot(taus_data, aes(x = taus, y = slopes)) + 
  geom_errorbar(aes(ymin = CI025, ymax = CI975), width = 0.025, color = "grey") + 
  geom_point() + 
  theme_bw() + 
  stat_smooth(size = 0.5, se = F, color = "red", linetype = 2, method = 'loess') + 
  xlab("Quantile") + 
  ylab("Beta")

p1 

ggsave(p1, file = "./analysis/wood_density_distribution/graph_code/graphs/quantreg_slopes_graph.png", 
       width = 5, 
       height = 5)

=======

# save model for graphing
save(quantModel, file = "./analysis/wood_density_distribution/models/quanreg_models/wooddensity_VS_elevation_quantreg.R")
>>>>>>> eddbaf3409778575f50f1e19684046df57ec3a8e
