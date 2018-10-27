#data index 
#path <- '/home/majames/Documents/ForestFloodingSensitivityAnalysis/analysis/adult_distribution_analysis/'
path <- paste(getwd(), '/analysis/adult_distribution_analysis/', sep = '')
species_occurance_data <- read.table(paste(path, 'data/species_occurance_data.txt', sep = ""), header = TRUE)


