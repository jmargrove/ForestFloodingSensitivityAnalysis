################################################################################
#' @title Regional predictions of dipterocarps 
#' @author James Margrove 

# Clear workspace 
rm(list =ls())

# Needing errors in english 
Sys.setenv(LANG = "en")

# Import packages 
require(raster)
require(rgdal)
require(foreach)

# Functions 
quantCalc <- function(rainfall, spdist, CI = 0.025){
  if(class(spdist)[1] != "RasterLayer") return(NA)
  spdist_rainfall <- mask(rainfall, spdist)
  as.numeric(quantile(spdist_rainfall, CI))
}

# Importing the rainfall data from worldclim ...
# BIO12 - BIO17 are obviosly the most usefull ones
# For each species, calculate the BIO_rainfall 0.95 interval
# 12 - Annual precipitation 
# 13 - Precipitation of wettest month 
# 14 - Precipitation of driest month 
# 15 - Precipitation of seasonality coef var 
# 16 - Precipitation of Wettest quater 
# 17 - Precipitation of Driest quater 

# Import data 
spnames <- read.table("./data/species_names.txt", header = T)
spnames$spnames_file <- paste("./data/Dipterocarpaceae/", 
                              gsub("_", " ", spnames[,2]), 
                              ".asc", sep = "")


rainfall <- raster("./data/worldclim/wc2.0_bio_10m_13.tif")
# Importing the species dist file 
spdist <- raster(spnames$spnames_file[1])

# Setting all locations where the species does not occur to NA 
spdist[spdist != 1] <- NA

exp_spdist  <- disaggregate(spdist, fact = 6)
# run the quant
quantCalc(rainfall, exp_spdist)

loadSpData <- function(i){
  spdist <- try(raster(spnames$spnames_file[i]), silent = T)
  try(spdist[spdist != 1] <- NA, silent = T)
  tryCatch(disaggregate(spdist, fact = 6), error = function(e){ return (NA)})
}


# looping for each species 
wettest_month <- foreach(i = 1:41, .combine = rbind) %do% quantCalc(rainfall, loadSpData(i))

allRain <- function(i, CI = 0.025){
  filename <- paste("./data/worldclim/wc2.0_bio_10m_", i,".tif", sep = "")
  rainfall <- raster(filename)
  foreach(i = 1:41, .combine = rbind) %do% quantCalc(rainfall, loadSpData(i), CI)
}

CI025_data <- foreach(i = 13:17, .combine = cbind) %do% allRain(i)
CI975_data <- foreach(i = 13:17, .combine = cbind) %do% allRain(i, CI = 0.975)

colnames(CI975_data) <- c("wet_month", "dry_month", "co_var_25", "wet_quar", "dry_quar") # dry
colnames(CI025_data) <- c("wet_month", "dry_month", "co_var_75", "wet_quar", "dry_quar") # wet

# to data frames 
dist_data <- cbind(as.data.frame(CI025_data)[,c(2,3,5)], as.data.frame(CI975_data)[,c(1,3,4)])
dist_data$sp = sp_table$sp

sp_table <- read.table("./data/speceis_table.txt", header = T)


CI025_data$sp <- sp_table$sp
CI975_data$sp <- sp_table$sp
sen_dist_data <- merge(sp_table, dist_data, by = "sp")
write.table(sen_dist_data, file = "./data/sen_dist_data.txt")

### OK lm 
require(ggplot2)
ggplot(sen_dist_data, aes(x = dry_month, y = Drought)) + geom_point()
ggplot(sen_dist_data, aes(x = dry_quar, y = Drought)) + geom_point() + 
  stat_smooth(method = lm )


ggplot(sen_dist_data, aes(x = wet_month, y = Inundation)) + geom_point() + 
  stat_smooth(method = lm )

ggplot(sen_dist_data, aes(x = wet_quar, y = Inundation)) + geom_point() + 
  stat_smooth(method = lm )

ggplot(sen_dist_data, aes(x = wet_quar, y = wet_month)) + geom_point() + 
  stat_smooth(method = lm )

ggplot(sen_dist_data, aes(x = dden, y = wet_month)) + geom_point() + 
  stat_smooth(method = lm )

ggplot(sen_dist_data, aes(x = Drought, y = wet_month)) + geom_point() + 
  stat_smooth(method = lm )

ggplot(sen_dist_data, aes(x = Drought, y = wet_quar)) + geom_point() + 
  stat_smooth(method = lm )

ggplot(sen_dist_data, aes(x = Inundation, y = dry_month)) + geom_point() + 
  stat_smooth(method = lm)

ggplot(sen_dist_data, aes(x = Inundation, y = co_var_75)) + geom_point() + 
  stat_smooth(method = lm )



summary(lm(wet_month ~ Drought, sen_dist_data))
summary(lm(dry_month ~ Inundation + Drought, sen_dist_data))
summary(lm(wet_month ~ Inundation + Drought, sen_dist_data))
summary(lm(dry_quar ~ Inundation + Drought, sen_dist_data))
summary(lm(wet_quar ~ Drought, sen_dist_data))
summary(lm(dry_quar ~ Inundation, sen_dist_data))
summary(lm(dry_month ~ dden + elev, sen_dist_data))

# Ok these results are a bit strange, I wonder about range. 
# species with larger range of values have 
str(sen_dist_data)
summary(lm(cov_var))