# r-squared for the ontogenic stages as well as the does seedling wood density predict mortality 
traits <- read.table('./data/traits.txt', header = TRUE)
density_data <- read.table('./analysis/inundation_wooddensity_relationship/data/data_sap_adult_rr_density.txt', header = TRUE)
density_data <- merge(density_data, traits,  by = 'sp')[,c(1,2,3,4,11)]

model <- (lm(rr ~ dden, density_data))
source('./functions/booter.R')
booter(model, density_data, n = 5000, coef = TRUE)
summary(model)
###
summary(lm(adult_dden ~ dden, density_data))
summary(lm(sap_dden ~ dden, density_data))
summary(lm(adult_dden ~ sap_dden, density_data))
