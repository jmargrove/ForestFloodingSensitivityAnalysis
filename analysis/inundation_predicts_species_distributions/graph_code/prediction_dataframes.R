source('./analysis/inundation_predicts_species_distributions/data/data_index.R')

preds_riskratio <- expand.grid(diff_mort = seq(min(riskratio$diff_mort), max(riskratio$diff_mort), length = 100), 
                               dden = mean(riskratio$dden))

preds_wooddensity <- expand.grid(diff_mort = mean(riskratio$diff_mort), 
                                 dden = seq(min(riskratio$dden), max(riskratio$dden), length = 100))
