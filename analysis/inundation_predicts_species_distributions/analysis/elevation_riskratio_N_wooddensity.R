# Clear working space 
rm(list = ls())

# Import the data 
source('analysis/inundation_predicts_species_distributions/data/data_index.R')
# model the data 
model <- lm(elev ~ dden * diff_mort, riskratio)
summary(model)
# remove the interaction 
model2 <- lm(elev ~ diff_mort + dden, riskratio)
summary(model2)

# remove each term just to check
model3 <- lm(elev ~ dden, riskratio)
model4 <- lm(elev ~ diff_mort, riskratio)
summary(model3)
summary(model4)

# plot the residuals 
par(mfrow=c(2,2))
plot(model2)

# best model clearly is model2 - save for graphing later
save(model2, file = './analysis/inundation_predicts_species_distributions/models/elevation_Vs_wooddensityN_diff_mortality.R')

# partial residual plots also require the same model but in glm 
model2_glm <- glm(elev ~ dden + diff_mort, riskratio, family = "gaussian")
summary(model2_glm)

save(model2_glm, file = './analysis/inundation_predicts_species_distributions/models/elevation_Vs_wooddensityN_diff_mortality_GLM.R')

# bootstrap the models to get the confidence intervals 
source('./functions/booter.R')
CI <- booter(model2, data = riskratio, n = 5000, coef = T)
CI
coef(model2)
