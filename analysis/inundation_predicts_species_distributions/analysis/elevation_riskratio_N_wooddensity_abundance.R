# Clear working space 
rm(list = ls())

# Import the data 
source('analysis/inundation_predicts_species_distributions/data/data_index.R')
# model the data 
model <- lm(elev ~ dden * diff_mort, weights = abundance, riskratio)
summary(model)
# remove the interaction 
model2 <- lm(elev ~ dden + diff_mort, weights = abundance, riskratio)
summary(model2)
# remove each term just to check
model3 <- lm(elev ~ dden, weights = abundance, riskratio)
model4 <- lm(elev ~ diff_mort, weights = abundance,riskratio)
summary(model3)
summary(model4)

# plot the residuals 
par(mfrow=c(2,2))
plot(model2)

# best model clearly is model2 - save for graphing later
save(model2, file = './analysis/inundation_predicts_species_distributions/models/elevation_Vs_wooddensityN_diff_mortality_abundance.R')

# partial residual plots also require the same model but in glm 
model2_glm <- glm(elev ~ dden + diff_mort, riskratio, weights = abundance, family = "gaussian")
summary(model2_glm)

save(model2_glm, file = './analysis/inundation_predicts_species_distributions/models/elevation_Vs_wooddensityN_diff_mortality_GLM_abundance.R')

