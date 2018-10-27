#' @title  Wood_density_distribution with gls analysis 
#' @author James Margrove

#data 
source("./analysis/wood_density_distribution/organisation.R")
#packages
source("./packages.R")
#functions
source("./analysis/wood_density_distribution/function_index.R")

# graph of the data 
ggplot(wood_density_data_178ha, aes(x = d, y = e)) + geom_point(aes(color = fd)) + 
  stat_smooth(method = lm) + 
  theme_bw()

# step one. Simple linear model 
model1 <- lm(e ~ d, data = wood_density_data_178ha)
plot(model1, which= 1)
save(model2, file = './analysis/wood_density_distribution/models/lm_models/model1.R')

# model2 modeling the unequal residuals.
model2 <- gls(e ~ d, data = wood_density_data_178ha, 
              weights = varIdent(form = ~1 | fd))

summary(model2)
plot(model2)
save(model2, file = './analysis/wood_density_distribution/models/gls_models/model2.R')

# bootstrapping for the coef CI's
<<<<<<< HEAD

#coef_CI <- booter(model2, data = wood_density_data_178ha, coef = TRUE, n = 5000)
#save(coef_CI, file = './analysis/wood_density_distribution/bootstrapped/coef_CI_gls_wooddensity_VS_elevation.R')

load('./analysis/wood_density_distribution/bootstrapped/coef_CI_gls_wooddensity_VS_elevation.R')
coef_CI
=======
booter(model2, data = wood_density_data_178ha, )
?booter

>>>>>>> eddbaf3409778575f50f1e19684046df57ec3a8e


