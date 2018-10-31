# wood density values converging on the end values 

survival_data <- read.table("./analysis/nursery_experiment_inundation/data/nursery_experiment_data.txt", header = TRUE)
str(survival_data)

wooddensity_data <- read.table('./analysis/inundation_wooddensity_relationship/data/data_sap_adult_rr_density.txt')
dden_mat <- with(survival_data, tapply(dden, list(sp, treat), mean, na.rm = T))[-5,]

wooddensity_data <- wooddensity_data[which(tolower(wooddensity_data$sp) %in% levels(survival_data$sp)),]

rsquared <- numeric(8)
for(i in 1:8){
  m <- lm(wooddensity_data$adult_dden ~ dden_mat[, i])
  rsquared[i] <- summary(m)$r.squared
}

data <- data.frame(r = rsquared, treat = seq(0,21, by = 3))
summary(lm(r ~ treat + I(treat^2), data))

p1 <- ggplot(data, aes(x = treat, y = r)) + 
  geom_point() + 
  stat_smooth(method = lm, formula = y ~ x + I(x^2), color = 'black', size = 0.2) + 
  theme_bw() + 
  ylab("r-squared(adult ~ exp)") + 
  xlab(expression(Inundation~frequency~days~cycle^-1))  +
  theme(text = element_text(size = 20))

ggsave(p1, file = './analysis/nursery_experiment_inundation/graph_code/graphs/rsquared_wooddensity.png', 
       width = 6, 
       height = 6)
