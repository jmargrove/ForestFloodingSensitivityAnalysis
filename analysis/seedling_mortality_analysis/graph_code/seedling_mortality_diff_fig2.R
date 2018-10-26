# the differeances
#graphing the seedling mortality data 
rm(list = ls())
# import bootstrapped data 
booted_seedling_mortality <- read.table('./analysis/seedling_mortality_analysis/bootstrapping/bootstrapping_parallel/ bootstrapped_seedling_mortality_glmer.txt', header = TRUE)
str(booted_seedling_mortality)
# import model
load('./analysis/seedling_mortality_analysis/models/seedling_mortality_model.R')
# prediction data.frame 
source('./analysis/seedling_mortality_analysis/data/prediction_species_inundation_interaction.R')
# prediction data.frame 
pelev_data <- read.table("./data/pelev_data.txt", header = TRUE)


mortality <- predict(r3, preds, type = "response", re.form = NA)


pred_diff <- preds[1:16,]
pred_diff$diff_mort <- mortality[17:32] - mortality[1:16] 
pred_diff$CI025 <- as.numeric(booted_seedling_mortality[1, 33:48])
pred_diff$CI975 <- as.numeric(booted_seedling_mortality[2, 33:48])
pred_diff$pe <- pelev_data$pe
pred_diff$sigDiff <- rep("diff", 16)
pred_diff$sigDif[which(pred_diff$CI025 < 0)] <- 'no_diff'

cols <- c("#8CB369", "#F4E285", "#4C8577","#F4A259", "#BC4B51")

ggplot(pred_diff, aes(x = sp, y = diff_mort, color = sigDif)) + geom_point() + 
  geom_errorbar(aes(ymin = CI025, ymax = CI975), width = 0.2)  + 
  theme_bw() + 
  xlab("Species") + 
  ylab("risk ratio (Mortality))") + 
  theme(axis.text.x = element_text(face = "italic", angle = 45, vjust = .7)) + 
  scale_color_manual(values = c("black", cols[c(4)])) + 
  geom_hline(aes(yintercept = 0), linetype = 2, col = cols[5])

ggplot(pred_diff[pred_diff$CI025 > 0, ], aes(x = diff_mort, y = pe)) + 
  geom_point() + 
  stat_smooth(method = 'lm') + 
  theme_bw() 


summary(lm(pe ~ diff_mort, pred_diff, subset = CI025 > 0))  
