# modeling species distributions with inundation sensitivity

riskratio <- read.table('./analysis/inundation_predicts_species_distributions/data/riskratio.txt', header = TRUE)
head(riskratio)

# also require the wood density values 
riskratio$Wooddensity <- read.table('./data/dden_adult.txt', header = TRUE)$dden_adult


ggplot(riskratio[riskratio$CI025 > 0,], aes(x = diff_mort, y = pe)) + 
  geom_point() + 
  stat_smooth(method = 'lm', color = 'black', size = 0.5) + 
  theme_bw()

ggplot(riskratio, aes(x = Wooddensity, y = pe)) + 
  geom_point() + 
  stat_smooth(method = 'lm', color = 'black', size = 0.5) + 
  theme_bw()

ggplot(riskratio, aes(x = (Wooddensity), y =(diff_mort))) + 
  geom_point() + 
  stat_smooth(method = 'lm', color = 'black', size = 0.5) + 
  theme_bw()

ggplot(riskratio, aes(x = reorder(sp, Wooddensity), y =(diff_mort), color = sigDif)) + 
  geom_errorbar(aes(ymin = CI025, ymax = CI975), width = 0.2, color = 'darkgrey') + 
  geom_point() + 
  stat_smooth(method = 'lm', color = 'black', size = 0.5) + 
  theme_bw()



summary(lm(pe ~ Wooddensity, riskratio))
summary(lm(pe ~ diff_mort, riskratio))
summary(lm(diff_mort ~ Wooddensity, riskratio))

model <- lm(diff_mort ~ pe + Wooddensity, riskratio)
summary(model)
anova(model)
car::vif(model)

riskratio[riskratio$CI025 < 0, ]
