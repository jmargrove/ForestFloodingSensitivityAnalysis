################################################################################
#' @title gls calculation of mean elevation 
#' @author: James Margrove 
#' @description calculating the mean elevation using gls
#' 

Sys.setenv(LANG = "en")
# Clear work space 
rm(list=ls())

# Import packages 
require(ggplot2)
require(nlme)
source("./functions/booter.R")

# Import data 
sp <- read.table("./data/riskratio_data.txt", header = TRUE)$sp[-c(4,7, 13)]
data <- read.table( "./data/Reuben_data/data_cleaned.txt", header = TRUE)
load("./models/pele_fsen_dden_Abundance")
head(data)

# Predict elevation
nullModel1 <- gls(elev ~ Species, data = data)

var1 <- Variogram(nullModel1, form = ~x + y, resType = "pearson")
plot(var1)

data$xp <- data$x <- rnorm(nrow(data), 0, 0.1)
data$yp <- data$y <- rnorm(nrow(data), 0, 0.1)

spatModel1 <- gls(elev ~ Species, data = data, correlation = corExp(form =  ~xp + yp))
spatModel2 <- gls(elev ~ Species, data = data, correlation = corGaus(form =  ~xp + yp))
AIC(spatModel1, spatModel2)
# no dif in AIC

spatModel3 <- gls(elev ~ Species, data = data, correlation = corExp(form =  ~xp + yp | Forest))
spatModel4 <- gls(elev ~ Species, data = data, correlation = corGaus(form =  ~xp + yp | Forest))
AIC(spatModel3, spatModel4)
summary(spatModel4)


var2 <- Variogram(spatModel1, form = ~x + y, resType = "pearson")
plot(var2)

summary(spatModel1)

# Ok so there is no spatial auto-correlation to account for in these plots 
