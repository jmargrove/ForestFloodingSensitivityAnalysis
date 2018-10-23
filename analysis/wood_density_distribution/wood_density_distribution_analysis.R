# Q. does wood density have a positive correlation?

#data 
source("./analysis/wood_density_distribution/data_index.R")
#packages
source("./packages.R")
# functions
source("./analysis/wood_density_distribution/function_index.R")

# step one. Simple linear model 
model1 <- lm(d ~ e, data = wood_density_data_178ha)
summary(model1)
par(mfrow=c(2,2))
plot(model1)

ggplot(wood_density_data_178ha, aes(x = e, y = d)) + geom_point(aes(color = fe)) + 
  stat_smooth(method = lm) 

# clearly as wood density increases so does the elevation, but the issue is that the residuals are
# not equally varied. To fix, implement a gls, first take steps to control for spatial effect 

model2 <- gls(d ~ e, data = wood_density_data_178ha)
model3 <- gls(d ~ e, data = wood_density_data_178ha, correlation = corExp(form = ~x + y))
model4 <- gls(d ~ e, data = wood_density_data_178ha, correlation = corGaus(form = ~x + y))

plot(model2)
plot(model3)
plot(model4)

# ok so modelling the spatial effect does help. But then...

AIC(model2)
AIC(model3)
AIC(model4)

# model 4 AIC is the lowest value 

model4 <- gls(d ~ e, data = wood_density_data_178ha, 
              weights = varIdent(form = ~1 | fe),
              correlation = corGaus(form = ~x + y))

summary(model4)
plot(model4)


  
