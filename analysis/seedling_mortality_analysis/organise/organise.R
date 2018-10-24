#data index and organsise for analysis 

seedling_mortality_data <- read.table("./analysis/seedling_mortality_analysis/data/seedling_mortality_data.txt", header = T)
seedling_mortality_data <- seedling_mortality_data[seedling_mortality_data$f.time != 0,]# remove the initial time point
seedling_mortality_data$idia <- rep(seedling_mortality_data$dia[1:2048], times = 4) 
seedling_mortality_data$f.time <- factor(seedling_mortality_data$f.time)


