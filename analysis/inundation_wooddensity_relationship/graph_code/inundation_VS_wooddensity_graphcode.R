#
rm(list = ls())
# predictions for the graph
load('./analysis/inundation_wooddensity_relationship/models/inundation_VS_wooddensity_loglog.R')
source('./analysis/inundation_predicts_species_distributions/data/data_index.R')

# r2 is much more so I will go for the log-log options
preds <- expand.grid(dden = with(riskratio, seq(min(dden), max(dden), length = 100)))
preds$rr <- predict(model2, preds, type = 'response')
preds$CI025 <- preds$rr + predict(model2, preds, type = 'response', se.fit = T)$se.fit * -1.96
preds$CI975 <- preds$rr + predict(model2, preds, type = 'response', se.fit = T)$se.fit * 1.96

p1 <- ggplot(preds, aes(x = log(dden), y = (rr))) + geom_line() +
  geom_point(data = riskratio, aes(x = log(dden), y = log(diff_mort))) + 
  geom_ribbon(aes(ymin = (CI025), ymax = (CI975)), alpha = 0.333) + 
  theme_bw() + 
  ylab('log(riskratio (Mortality))') + 
  xlab(expression('log(Wood density)'~g~cm^3)) + 
  theme(text = element_text(size = 20))

p1

# Save the graph 
ggsave(p1, file = './analysis/inundation_wooddensity_relationship/graph_code/graphs/inundation_VS_wooddensity.png', 
       width = 6, 
       height = 6)
