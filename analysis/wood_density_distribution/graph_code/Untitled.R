# Predicting the adult distribution based on the inundation sensitivity 

# Clear workspace 
rm(list = ls())
# import data 
source('./analysis/inundation_predicts_species_distributions/data/data_index.R')

# import common packages 
source('./packages.R')

ggplot(riskratio, aes(x = diff_mort, y = elev)) + geom_point()
# now remove those with CI025 that are less than Zero 
ggplot(riskratio[riskratio$CI025 > 0,], aes(x = diff_mort, y = elev)) + 
  geom_point() +
  theme_bw() + 
  stat_smooth(method = 'lm', size = 0.2, color = 'black') + 
  ylab('E(elevation) m asl') + 
  xlab('Inundation sensitivity') + 
  geom_text(aes(label = 'R2: 0.66', x = 0.15, y = 120))


# linear model 
model <- lm(diff_mort ~ elev, riskratio, subset = CI025 > 0)
summary(model)
par(mfrow=c(2,2))
plot(model)

# boot for confidence intervals 
source('./functions/booter.R')
CI <- booter(model, data = riskratio, n = 5000, coef = TRUE)




