# inundation wood density relationship 
source('./analysis/inundation_predicts_species_distributions/data/data_index.R')

ggplot(riskratio, aes(x = dden, y = diff_mort)) + geom_point()

# univariate model of adult wood density predicting the inundation sensitivity 
model1 <- lm(diff_mort ~ dden, riskratio)
par(mfrow = c(2,2))
plot(model1)

# log-log model
model2 <- lm(log(diff_mort) ~ log(dden), riskratio)
summary(model2)
par(mfrow = c(2,2))
plot(model2)
par(mfrow = c(1,1))

save(model2, file = './analysis/inundation_wooddensity_relationship/models/inundation_VS_wooddensity_loglog.R')

# bootstrapping 
source('./functions/booter.R')
CI <- booter(model2, data = riskratio, n = 5000, coef = TRUE)
CI


  
