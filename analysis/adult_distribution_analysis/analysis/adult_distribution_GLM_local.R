#adult distribution glm
source('./analysis/adult_distribution_analysis/data/data_index.R')
str(species_occurance_data)

<<<<<<< HEAD
#Modelling occurance data as a binomial glm 
model1 <- glm(occurance ~ species * elev + I(elev^2), family = 'binomial', data = species_occurance_data)
summary(model1)

#Creating prediction data frame 
preds <- expand.grid(species = levels(species_occurance_data$species), 
                     elev = with(species_occurance_data, seq(min(elev), max(elev), length = 100)))
# Predicting values 
=======
# very quick model of the data 
model1 <- glm(occurance ~ species * elev + I(elev^2), family = 'binomial', data = species_occurance_data)
summary(model1)

preds <- expand.grid(species = levels(species_occurance_data$species), 
                     elev = with(species_occurance_data, seq(min(elev), max(elev), length = 100)))

>>>>>>> eddbaf3409778575f50f1e19684046df57ec3a8e
preds$occurance <- predict(model1, preds, type = 'response')
preds$CI025 <- preds$occurance + predict(model1, preds, type = 'response', se.fit = TRUE)$se.fit * -1.96
preds$CI975 <- preds$occurance + predict(model1, preds, type = 'response', se.fit = TRUE)$se.fit * 1.96

<<<<<<< HEAD

=======
>>>>>>> eddbaf3409778575f50f1e19684046df57ec3a8e
require(ggplot2)
# graph of the prediced values...
ggplot(preds, aes(x = elev, y = occurance, color = species)) + 
  geom_ribbon(aes(ymin = CI025, ymax = CI975), alpha = 0.25, linetype = 0) +
  facet_wrap(~species, scales = 'free_y') + 
  geom_line() + 
  theme_bw()
