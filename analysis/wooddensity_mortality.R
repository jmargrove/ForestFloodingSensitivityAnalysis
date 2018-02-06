################################################################################
# title: flooding sensitivity response predicting species distributions 
# author: James Margrove 

# IMPORT FUNCTIONS AND PACKAGES
require(ggplot2)

rr_data <- read.table("./data/riskratio_data.txt", header = TRUE)
dden_data <- read.table("./data/dden_adult.txt", header = TRUE)

dden_data <- dden_data[order(dden_data$sp),]
rr_data$dden <- dden_data$dden_adult
##### Exploration 
ggplot(rr_data, aes(x = log(dden), y = log(rr))) + 
  geom_point() + 
  stat_smooth(method = lm, color = '#000000', size = 0.5) + 
  ylab("log(water inundation sensitivity)") + 
  xlab("log(adult wood density)") + 
  theme_classic() + 
  


