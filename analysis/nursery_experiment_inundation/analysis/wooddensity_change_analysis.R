# Clear workspace 
rm(list = ls())

survival_data <- read.table("./analysis/nursery_experiment_inundation/data/nursery_experiment_data.txt", header = TRUE)
seedling_density <- read.table("./analysis/nursery_experiment_inundation/data/seedling_density.txt", header = TRUE)
str(survival_data)

seedling_wood_density <- numeric(800)
for(i in 1:10){
  seedling_wood_density[which(survival_data$sp == tolower(seedling_density$sp)[i])] <- seedling_density$seed_density[i]
}
survival_data$seedling_wood_density <- seedling_wood_density
survival_data$delta_wood_density <- with(survival_data, dden - seedling_wood_density)

head(survival_data)

hist(survival_data$delta_wood_density)

with(survival_data, tapply(delta_wood_density, list(sp, treat), mean, na.rm = T))

require(lme4)
require(lmerTest)
model <- lmer(delta_wood_density ~ log(treat+1)  + dia + (1|sp/mother) + (1|block), survival_data, subset = surv == 1)
summary(model)

preds <- expand.grid(dia = mean(survival_data$dia, na.rm = T), 
                     treat = seq(0, 21, length = 100))

preds$delta_wood_density <- predict(model, preds, type = 'response', re.form = NA)
require(ggplot2)

source('./functions/booter.R')
CI <- booter(model = model, data = survival_data, n = 5000, coef = TRUE, MEM = TRUE)


# require(remef)
# partial_data <- survival_data[!is.na(survival_data$dden) & survival_data$surv == 1,]
# partial_data$y_partial <- remef(model, fix = "dia", ran = "all", keep.intercept = TRUE)

CI_preds <- booter(model = model, data = survival_data, preds = preds, n = 5000)
write.table(CI_preds, file = './analysis/nursery_experiment_inundation/data/delta_wooddensity_95CIs.txt')

CI_preds <- read.table('./analysis/nursery_experiment_inundation/data/delta_wooddensity_95CIs.txt', header = TRUE)
preds$CI025 <- as.numeric(CI_preds[1,])
preds$CI975 <- as.numeric(CI_preds[2,])

int_diff <- as.vector(with(partial_data, tapply(y_partial, factor(treat), mean)) - preds[1,"delta_wood_density"])[1]
source('./colors.R')
p1 <- ggplot(preds, aes(x = treat, y = delta_wood_density)) + 
# 
#   geom_boxplot(data = partial_data, 
#                aes(x = treat, y = y_partial - int_diff, group = treat), 
#                color = cols[3], alpha = 0.2, width = 0.5) +
  geom_line() +
  theme_bw() +
  # ylim(-0.15, 0.25) +
  theme(text = element_text(size = 20)) + 
  xlab(expression(Inundation~frequency~days~cycle^-1)) +
  ylab(expression(delta~"wood density"~g~cm^3)) + 
geom_ribbon(aes(ymin = CI025, ymax = CI975), alpha = 0.25) 

p1

delta = preds[100,3] - preds[1,3] 
delta 

(((mean(seedling_density$seed_density)  + delta ) / mean(seedling_density$seed_density)) - 1) * 100

ggsave(p1, file = './analysis/nursery_experiment_inundation/graph_code/graphs/wood_density_treatment_change.png', 
       width = 6, 
       height = 6)

  
