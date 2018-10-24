# wood density quantile regression 

rm(list = ls())

source("./packages.R")
source("./analysis/wood_density_distribution/data/data_index.R")

# Taus/quantiles of 
taus = c(0.025, 0.1, 0.5, 0.9, 0.975)

# Run the model, log-log version as it fits better
quantModel <- rq(log(e) ~ log(d), data = wood_density_data_178ha, tau = taus)
summary(quantModel)

# save model for graphing
save(quantModel, file = "./analysis/wood_density_distribution/models/quanreg_models/wooddensity_VS_elevation_quantreg.R")
