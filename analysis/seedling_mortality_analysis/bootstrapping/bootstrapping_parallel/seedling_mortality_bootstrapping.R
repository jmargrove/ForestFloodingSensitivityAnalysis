# bootstrapping code for 95% IC and the differances 
rm(list = ls())
# number of bootstraps 
n = 2000 
# route 
route <- paste(getwd(), "/", sep = "")
#route <- paste(getwd(), "/analysis/seedling_mortality_analysis/bootstrapping/bootstrapping_parallel/", sep = "")
# route on shorea 


# Import packages 
source(paste(route, 'packages.R', sep = ""))
# Import model 
load(paste(route, 'seedling_mortality_model.R', sep = ""))
# import data
seedling_mortality_data <- read.table(paste(route, 'seedling_mortality_data.txt', sep = ""), header = TRUE)
seedling_mortality_data$f.time <- as.factor(seedling_mortality_data$f.time)

# prediction data frame for confidence intervals 
preds <- with(seedling_mortality_data, expand.grid(dia = mean(dia, na.rm = T), 
                                                  ztopo = 0, 
                                                  f.time = '3', 
                                                  sp = levels(seedling_mortality_data$sp),
                                                  flood = levels(seedling_mortality_data$flood)))

# booty function for bootstrapping glmer 
booty <- function(data, model, preds, i) {
  print(paste("this is what i = ", i, sep = ""))
  random_row_numbers <- sample(1:dim(data)[1], replace = TRUE)
  random_row <- data[random_row_numbers, ]
  btm <- update(model, . ~ ., data = random_row)
  res <- predict(btm, preds, type = "response", re.form = NA)
  delta <- res[17:32] - res[1:16]
  res <- c(res, delta)
  names(res) <- c(as.character(preds$sp), paste("delta_", preds$sp[1:16], sep = ""))
  return(res)
}

# how many cores are there
number_of_cores <- detectCores()
print(paste('how many cores? ', number_of_cores, sep = ""))
# make the cluster 
cl <- makeCluster(number_of_cores)
# register the cores
registerDoParallel(cl)

# run the parallel bootstrap 
boots <- foreach(i = 1:n, 
                 # .export=c('function1', 'function2'), 
                 .packages='lme4', 
                 .combine = cbind) %dopar% booty(data = seedling_mortality_data,
                                                 model = r3,
                                                 preds = preds, 
                                                 i = i)
#stop the cluster
stopCluster(cl)
# calculate the confidence intervals
CI <- (apply(boots, 1, quantile, c(0.025, 0.975)))

write.table(CI, file = paste(route, "bootstrapped_seedling_mortality_glmer_nAGQ=1.txt"))

