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

# Import species names 
spnames <- read.table("./data/species_names.txt", header = T)
spnames$spnames_file <- paste("./data/Dipterocarpaceae/", 
                              gsub("_", " ", spnames[,2]), 
                              ".asc", sep="")


# Functions 
quantCalc <- function(rainfall, spdist, CI = 0.025){
  if(class(spdist)[1] != "RasterLayer") return(NA)
  spdist_rainfall <- mask(rainfall, spdist)
  #as.numeric(quantile(spdist_rainfall, CI))
  #range(spdist_rainfall@data@values, na.rm = T)
  length(which(!is.na(spdist_rainfall@data@values)))
}

loadSpData <- function(i){
  spdist <- try(raster(spnames$spnames_file[i]), silent = T)
  try(spdist[spdist != 1] <- NA, silent = T)
  tryCatch(disaggregate(spdist, fact = 6), error = function(e){ return (NA)})
}

rainfall <- raster("./data/worldclim/wc2.0_bio_10m_12.tif")

# mean_var <- foreach(i = 1:41, .combine = rbind) %do% quantCalc(rainfall, loadSpData(i))
# mean_var

area <- foreach(i = 1:41, .combine = rbind) %do% quantCalc(rainfall, loadSpData(i))
which(area == area[2])
sp_table$area <- area
sp_table <- read.table("./data/speceis_table.txt", header = T)

sp_table$range <- apply(mean_var, 1, diff)
sp_table$area <- area
with(sp_table, plot(range, area))

##
ggplot(sp_table, aes(x = Inundation, y = range)) + geom_point()
ggplot(sp_table, aes(x = Drought, y = range)) + geom_point()
ggplot(sp_table, aes(x = Drought, y = area)) + geom_point() + ylim(0, 5000)

summary(lm(range ~ Inundation + Drought, sp_table))
summary(lm(area ~ Inundation * Drought + dden + elev, sp_table))


for(i in 1:41) plot(loadSpData(i), main = i)
# 2
plot( boundaries(r, classes=TRUE) )
plot( boundaries(r, type="outer") )

# driest quarter 

rainfall <- raster("./data/worldclim/wc2.0_bio_10m_12.tif")
#i = 1
borderVals <- function(i){
    spdist <- tryCatch(raster(spnames$spnames_file[i]), error = function(e){return(NA)})
  try(spdist[spdist != 1] <- NA, silent = T)
  try(bon <- boundaries(spdist, type="inner") , silent = T)
  try(ag <- disaggregate(bon, fact = 6), silent = T)
  try(spdist_rainfall <- mask(rainfall, ag), silent = T)
  try(vals <- spdist_rainfall@data@values, silent = T)
  try(hist(vals, breaks = 25, main = i), silent = T)
  try(quantile(vals, c(0.1, 0.5, 0.9), na.rm = T), silent = T)
  #median(vals, na.rm = T)
}

driest <- foreach(i = 1:41, .combine = rbind) %do% borderVals(i)

sp_table <- read.table("./data/speceis_table.txt", header = T)
#driest[18] <- NA
#sp_table$dd <- as.vector(driest)

# inundation 
ggplot(sp_table, aes(y = driest[,1], x = Inundation )) + geom_point() +
  stat_smooth(method = lm)

summary(lm(driest[,1] ~ Inundation, sp_table))
summary(lm(driest[,2] ~ Inundation, sp_table))
summary(lm(driest[,3] ~ Inundation, sp_table))

ggplot(sp_table, aes(y = driest[,2], x = Inundation)) + geom_point() +
  stat_smooth(method = lm)

ggplot(sp_table, aes(y = driest[,3], x = Inundation)) + geom_point() +
  stat_smooth(method = lm)

# Drought
ggplot(sp_table, aes(y = driest[,1], x = Drought)) + geom_point() +
  stat_smooth(method = lm)

summary(lm(driest[,1] ~ Drought, sp_table))
summary(lm(driest[,2] ~ Drought, sp_table))
summary(lm(driest[,3] ~ Drought, sp_table))

ggplot(sp_table, aes(y = driest[,2], x = Inundation)) + geom_point() +
  stat_smooth(method = lm)

ggplot(sp_table, aes(y = driest[,3], x = Inundation)) + geom_point() +
  stat_smooth(method = lm)


# dden
sp_table$d1 <- driest[,1]
sp_table$d2 <- driest[,2]
sp_table$d3 <- driest[,3]

summary(lm(d1 ~ dden, weights = 1/area, sp_table))

ggplot(sp_table, aes(y = d1, x = dden, size = 1/area, weight = 1/area)) + geom_point() +
  stat_smooth(method = lm)

# simple lm with inundation 
summary(lm(d1 ~ Inundation, weights = 1/area, sp_table))
ggplot(sp_table, aes(y = d1, x = Inundation, size = 1/area, weight = 1/area)) + geom_point() +
  stat_smooth(method = lm)

ggplot(sp_table, aes(y = d2, x = Drought, size = 1/area, weight = 1/area)) + geom_point() +
  stat_smooth(method = lm)

####
m <- (lm(d2 ~ Drought, weights = 1/area, sp_table))
summary(m)
AIC(m)




summary(lm(d1 ~ dden, sp_table[sp_table$area < 2500,]))
summary(lm(d3 ~ dden, sp_table[sp_table$area < 2500,]))

summary(lm(d1 ~ Drought, sp_table[sp_table$area < 2500,]))



summary(lm(driest[,1] ~ dden, sp_table))
summary(lm(driest[,3] ~ dden, sp_table))

ggplot(sp_table, aes(y = driest[,2], x = dden)) + geom_point() +
  stat_smooth(method = lm)

ggplot(sp_table, aes(y = driest[,3], x = dden)) + geom_point() +
  stat_smooth(method = lm)



summary(lm(driest[,1] ~ Inundation + Drought, sp_table))

summary(lm(driest[,2] ~ Inundation, sp_table))

summary(lm(driest[,3] ~ Inundation * Drought, sp_table))




## Explore only my species 
rr_data <- read.table("./data/riskratio_data.txt", header = T)
rr_data


ndata <- merge(sp_table, rr_data, by = "sp")

dt <- read.table("./data/sen_dist_data.txt", header = T)

ndata <- merge(dt, rr_data, by = "sp")

ggplot(ndata, aes(y = dry_quar , x = rr)) + geom_point()

