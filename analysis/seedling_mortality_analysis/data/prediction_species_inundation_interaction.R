# prediction data frame for confidence intervals 
seedling_mortality_data <- read.table('./analysis/seedling_mortality_analysis/data/seedling_mortality_data.txt', header = TRUE)
preds <- with(seedling_mortality_data, expand.grid(dia = mean(dia, na.rm = T), 
                                                   ztopo = 0, 
                                                   f.time = '3', 
                                                   sp = levels(seedling_mortality_data$sp),
                                                   flood = levels(seedling_mortality_data$flood)))