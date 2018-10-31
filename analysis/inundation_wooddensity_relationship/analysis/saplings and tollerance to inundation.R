# does the wood density of the saplings infer that there is a tolleracne 

source('./analysis/inundation_predicts_species_distributions/data/data_index.R')
sap_dden_data <- read.csv('./data/wood_density_saplings .csv')
seedling_data <- read.table('./data/data.txt', header = TRUE)
head(seedling_data)



data <- data.frame(
          sap_dden = with(sap_dden_data, tapply(bottom_density, Sp, mean)),
          adult_dden = with(riskratio, tapply(dden, sp, mean)),
           rr = riskratio$diff_mort,
           elev = riskratio$elev,
           sp = levels(as.factor(seedling_data$sp)))

summary(lm(sap_dden ~ adult_dden, data))

ggplot(data, aes(x = log(sap_dden), y = log(adult_dden))) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  geom_text(aes(label = sp))
  
model <- (lm(log(rr) ~ log(sap_dden), data))
source('./functions/booter.R')
booter(model, coef = TRUE, n = 5000, data = data)


write.table(data, file = './analysis/inundation_wooddensity_relationship/data/data_sap_adult_rr_density.txt')

     