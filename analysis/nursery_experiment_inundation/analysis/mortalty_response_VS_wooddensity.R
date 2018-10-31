# Does it fit with the wood density values 

# Clear working space 
rm(list = ls())
seedling_density <- read.table( './analysis/nursery_experiment_inundation/data/seedling_density.txt', header = TRUE)
slopes <- read.table('./analysis/nursery_experiment_inundation/data/survival_slope_coef.txt', header = TRUE)


head(seedling_density)
str(seedling_density)
# create a new data frame 
data <- seedling_density
data$slopes <- slopes$p

# quick graph of the data 
ggplot(data, aes(x = seed_density, y = slopes)) + 
  geom_point()

# ok model the data 
model <- lm(slopes ~ seed_density, data)
summary(model)

coef <- (summary(model)$coefficients)[2, 1:2]
# calculate the confidence intervals 
coef[1] + coef[2] * 1.96
coef[1] - coef[2] * 1.96
