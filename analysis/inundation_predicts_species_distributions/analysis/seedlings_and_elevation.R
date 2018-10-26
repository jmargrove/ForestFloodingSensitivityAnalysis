# modeling species distributions with inundation sensitivity

riskratio <- read.table('./analysis/inundation_predicts_species_distributions/data/riskratio.txt', header = TRUE)
head(riskratio)

# also require the wood density values 
riskratio$`Wood density` <- read.table('./data/')
