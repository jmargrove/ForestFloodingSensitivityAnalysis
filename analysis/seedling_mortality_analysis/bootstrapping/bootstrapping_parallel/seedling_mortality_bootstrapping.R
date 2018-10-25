# bootstrapping code for 95% IC and the differances 

# route 
route <- './analysis/seedling_mortality_analysis/bootstrapping/bootstrapping_parallel/'

# Import packages 
source(paste(route, 'packages.R', sep = ""))
# Import model 
load(paste(route, 'seedling_mortality_model.R', sep = ""))
# Import bootstrapping 
source(paste(route, 'booter.R', sep = ""))
# import data
seedling_mortality_data <- read.table(paste(route, 'seedling_mortality_data.txt', sep = ""), header = TRUE)


# prediction data frame for confidence intervals 
preds <- with(seedling_mortality_data, expand.grid(dia = mean(dia, na.rm = T), 
                                                  ztopo = 0, 
                                                  f.time = '3', 
                                                  sp = levels(seedling_mortality_data$sp),
                                                  flood = levels(seedling_mortality_data$flood)))

booter_par <- function(model, data, preds, n = 10){
  booty <- function() {
    random_row_numbers <- sample(1:dim(data)[1], replace = TRUE)
    random_row <- data[random_row_numbers, ]
    btm <- update(model, . ~ ., data = random_row, nAGQ = 1)
    preds <- predict(btm, newdata = preds, type = "response", re.form = NA)
    delta <- pred[17:32] - preds[1:16]
    res <- c(preds, delta)
    names(res) <- c(preds$sp, paste("delta_", preds$sp[1:16], sep = ""))
    return(res)
  }
  
  # setting up the cores
  require(doSNOW)
  require(snow)
  require(parallel)
  number_of_cores <- detectCores()
  clust <- snow::makeCluster(number_of_cores, type = 'SOCK')
  clusterExport(clust, c("booty","data","preds"))
  
  # run the parallel bootstrap 
  require(foreach)
  boots <- foreach(i = 1:n, .combine = cbind) %dopar% booty()
  stopCluster()
  # return the confidence intervals
  return(apply(boots, 1, quantile, c(0.025, 0.975)))
}


CI <- booter_par(model = r3, data = seedling_mortality_data, preds = preds, n = 8)

write.table(CI, file = paste(route, "bootstrapped_seedling_mortality_glmer.txt"))
?write.table
