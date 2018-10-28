# Import and organise the data 
riskratio <- read.table('./analysis/inundation_predicts_species_distributions/data/riskratio.txt', header = TRUE)
riskratio$dden <- read.table('./analysis/inundation_predicts_species_distributions/data/wooddensity_adult.txt', header = TRUE)$dden_adult
riskratio$elev <- read.table('./analysis/inundation_predicts_species_distributions/data/species_elevational_distribution_predictions', header = TRUE)$pe

# calculating species abundances for the analysis 
large_plot_data <- read.table('./analysis/inundation_predicts_species_distributions/data/forestplot_160_spatial_data_UPDATE.txt', header = T)
head(large_plot_data)
abn <- with(large_plot_data, tapply(sp, sp, length))
riskratio$abundance <- abn[which(names(abn) %in% riskratio$sp)]
