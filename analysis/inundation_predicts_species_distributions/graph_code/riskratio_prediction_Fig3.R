# graph the mode elevation ~ wooddensity + riskratio
# Clear workspace
rm(list = ls())

# import prediction data frames 
source('./analysis/inundation_predicts_species_distributions/graph_code/prediction_dataframes.R')
load('./analysis/inundation_predicts_species_distributions/models/elevation_Vs_wooddensityN_diff_mortality.R')
load('./analysis/inundation_predicts_species_distributions/models/elevation_Vs_wooddensityN_diff_mortality_GLM.R')

# predict values first for riskratio
preds_riskratio$elev <- predict(model2, newdata = preds_riskratio, type = "response")
preds_riskratio$CI025 <- preds_riskratio$elev + predict(model2, 
                                                        newdata = preds_riskratio, 
                                                        type = "response", 
                                                        se.fit = TRUE)$se.fit * -1.96

preds_riskratio$CI975 <- preds_riskratio$elev + predict(model2, 
                                                        newdata = preds_riskratio, 
                                                        type = "response", 
                                                        se.fit = TRUE)$se.fit * 1.96
# calculating the partial residuals 
riskratio$partials_rr <- residuals(model2_glm, type = "partial")[, 2] + mean(riskratio$elev)
# lines to join the partials 
partial_lines_data <- data.frame(x = rep(riskratio$diff_mort, 2), 
                                 y = c(riskratio$elev, riskratio$partials_rr))
# ANOVA type II to compare the variance 
var_res <- car::Anova(model2)[,1]
rr_explaied_variation <- paste('ANOVA:', round(var_res / sum(var_res) * 100, 1)[2], "%")

# jittering the species names 


# plotting the data 
p1 <- ggplot(preds_riskratio, aes(x = diff_mort, y = elev)) + 
  geom_line(data = partial_lines_data, aes(x = x, y = y, group = factor(x)), alpha = 0.3) + 
  geom_line() + 
  geom_point(data = riskratio, aes(y = elev, x = diff_mort), alpha = 0.5, color = 'red') + 
  geom_ribbon(aes(ymin = CI025, ymax = CI975), alpha = 0.2) + 
  theme_bw() + 
  geom_point(data = riskratio, aes(y = partials_rr, x = diff_mort), 
             color = 'black', 
             pch = 21, 
             fill = 'grey', size = 3) + 
  geom_text(aes(x = 0.12, y = 120, label = rr_explaied_variation), size = 5) + 
  ylab('E(elevation) m asl') + 
  xlab('Water inundation sensitivity') + 
  ylim(40, 120) + 
  stat_smooth(data = riskratio, aes(x = diff_mort, y = elev), 
              se = F, method = 'lm', color = 'red', 
              linetype = 2, size = 0.25) + 
  theme(text = element_text(size = 20)) + 
  geom_text(data = riskratio, aes(label = sp), nudge_y = rep(2.3, 16), nudge_x = rep(0.01, 15))

p1

ggsave(p1, 
       file = './analysis/inundation_predicts_species_distributions/graph_code/graphs/elevation_VS_riskration_C_woodensity.png', 
       width = 6, 
       height = 6)
