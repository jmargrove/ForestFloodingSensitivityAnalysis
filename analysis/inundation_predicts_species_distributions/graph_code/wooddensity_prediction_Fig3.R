# graph the mode elevation ~ wooddensity + riskratio
# Clear workspace
rm(list = ls())

# import prediction data frames 
source('./analysis/inundation_predicts_species_distributions/graph_code/prediction_dataframes.R')
load('./analysis/inundation_predicts_species_distributions/models/elevation_Vs_wooddensityN_diff_mortality.R')
load('./analysis/inundation_predicts_species_distributions/models/elevation_Vs_wooddensityN_diff_mortality_GLM.R')

# predict values first for riskratio
preds_wooddensity$elev <- predict(model2, newdata = preds_wooddensity, type = "response")
preds_wooddensity$CI025 <- preds_wooddensity$elev + predict(model2, 
                                                            newdata = preds_wooddensity, 
                                                            type = "response", 
                                                            se.fit = TRUE)$se.fit * -1.96

preds_wooddensity$CI975 <- preds_wooddensity$elev + predict(model2, 
                                                            newdata = preds_wooddensity, 
                                                            type = "response", 
                                                            se.fit = TRUE)$se.fit * 1.96

riskratio$partials_dd <- residuals(model2_glm, type = "partial")[, 1] + mean(riskratio$elev)

# plotting the data 
p1 <- ggplot(preds_wooddensity, aes(x = dden, y = elev)) + 
  geom_line() + 
  geom_point(data = riskratio, aes(y = elev, x = dden), alpha = 0.5) + 
  geom_ribbon(aes(ymin = CI025, ymax = CI975), alpha = 0.2) + 
  geom_point(data = riskratio, aes(y = partials_dd, x = dden), color = 'red') +
  theme_bw()

p1
