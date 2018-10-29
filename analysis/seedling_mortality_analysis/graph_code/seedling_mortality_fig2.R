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

preds$mortality <- predict(r3, preds, type = "response", re.form = NA)
preds$CI025 <- as.numeric(booted_seedling_mortality[1,1:32])
preds$CI975 <- as.numeric(booted_seedling_mortality[2,1:32])
preds$pe <- rep(pelev_data$pe, times = 2)
# graphing the data 

require(ggplot2)
cols <- c("#8CB369", "#F4E285", "#4C8577","#F4A259", "#BC4B51")


colnames(preds)[5] <- 'Water inundation'
levels(preds$`Water inundation`) <- c("Dry", "Wet")

# Graphing the results 
p1 <- ggplot(preds, aes(x =  reorder(sp, pe), y = mortality, group = `Water inundation`)) + 
  geom_errorbar(aes(ymin = CI025, ymax=CI975), width = 0.3, alpha = 0.2) + 
  theme_classic() + 
  geom_point(size = 4) +
  geom_point(size = 3, aes(color = `Water inundation`)) +
  theme(legend.position = c(0.25, 0.8)) + 
  xlab("Species") + 
  ylab("p(Mortality)") + 
  theme(axis.text.x = element_text(face = "italic", angle = 45, vjust = .7)) + 
  scale_color_manual(values = c("light grey", cols[c(5)])) + 
  theme(text = element_text(size=20))
p1


###
ggsave(p1, file = './analysis/seedling_mortality_analysis/graph_code/graphs/species_interaction_mortality_Fig2.png', 
       width = 13, 
       height = 6)
