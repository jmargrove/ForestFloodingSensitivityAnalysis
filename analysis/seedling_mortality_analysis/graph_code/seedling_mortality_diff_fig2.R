# the differeances
#graphing the seedling mortality data 
rm(list = ls())
# import bootstrapped data 

booted_seedling_mortality <- read.table('./analysis/seedling_mortality_analysis/bootstrapping/bootstrapping_parallel/ bootstrapped_seedling_mortality_glmer_nAGQ=1.txt', header = TRUE)

str(booted_seedling_mortality)
# import model
load('./analysis/seedling_mortality_analysis/models/seedling_mortality_model.R')
# prediction data.frame 
source('./analysis/seedling_mortality_analysis/data/prediction_species_inundation_interaction.R')
# prediction data.frame 
pelev_data <- read.table("./data/pelev_data.txt", header = TRUE)


mortality <- predict(r3, preds, type = "response", re.form = NA)

require(ggplot2)
pred_diff <- preds[1:16,]
pred_diff$diff_mort <- mortality[17:32] - mortality[1:16] 
pred_diff$CI025 <- as.numeric(booted_seedling_mortality[1, 33:48])
pred_diff$CI975 <- as.numeric(booted_seedling_mortality[2, 33:48])
pred_diff$pe <- pelev_data$pe
pred_diff$sigDiff <- rep("diff", 16)
pred_diff$sigDif[which(pred_diff$CI025 < 0)] <- 'no_diff'
pred_diff$Wooddensity <- read.table('./data/dden_adult.txt', header = TRUE)$dden_adult

write.table(pred_diff, file = './analysis/inundation_predicts_species_distributions/data/riskratio.txt')

cols <- c("#8CB369", "#F4E285", "#4C8577","#F4A259", "#BC4B51")


p1 <- ggplot(pred_diff, aes(x = reorder(sp, pe), y = diff_mort, color = sigDif)) + geom_point() + 
  geom_errorbar(aes(ymin = CI025, ymax = CI975), width = 0.2)  + 
  theme_bw() + 
  xlab("Species") + 
  ylab("Risk ratio (Mortality)") + 
  theme(axis.text.x = element_text(face = "italic", angle = 45, vjust = .7)) + 
  scale_color_manual(values = c("black", cols[c(4)])) + 
  geom_hline(aes(yintercept = 0), linetype = 2, col = cols[5]) + 
  theme(legend.position = c(0.1, 0.8)) 


p1


####
ggsave(p1, file = './analysis/seedling_mortality_analysis/graph_code/graphs/species_interaction_mortality_difference.png', 
              width = 8, 
              height = 4)



