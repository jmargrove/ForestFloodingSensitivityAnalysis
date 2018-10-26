##########################################################################################
#' @title Spatial Analysis of wood density data 
#' @Author James Margrove 
#' @discription Quantile regression of the wood density and elvation data, + 
#' then model the spatial effect. 

# Clear work space
rm(list=ls())

# Import packages & functions 
Sys.setenv(LANG = "en")
require(quantreg)
require(ggplot2)
require(nlme)
source("./functions/booter.R")

# Import data 
data <- read.table("./data/wden_triangle_data.txt", header = TRUE)

# Taus/quantiles of 
taus = c(0.025, 0.1, 0.5, 0.9, 0.975)

# Run the model 
quantModel <- rq(e ~ d, data = data, tau = taus)
summary(quantModel)

# Prediction from the mode and confidence intervales bootstrapped 
pred <- data.frame(d = seq(min(data$d), max(data$d), length = 100))
preds <- expand.grid(d = pred$d, Quantile = factor(taus))
preds$e <- as.vector(predict(quantModel, pred, type = "response"))

predict(quantModel, pred, type = "response", se.fit = T)
?predict

CIQ <- booter(quantModel, data = data, preds = pred, quantReg = TRUE, n = 100)
preds$CI025 <- CIQ[1,]
preds$CI975 <- CIQ[2,]

# Graphing the model 
cols <- c("#8CB369", "#F4E285", "#4C8577","#F4A259", "#BC4B51")
p1 <- ggplot(preds[preds$d < 0.6,], aes(x = d, y = e, group = Quantile)) + 
  geom_point(data = data[data$d < 0.6,], inherit.aes = F,  aes(x = d, y = e)) + 
  geom_ribbon(aes(ymin = CI025, ymax = CI975, group = Quantile, fill = Quantile), alpha = .74) + 
  geom_line() + 
  theme_classic() + 
  ylab("Elevation (m asl)") + 
  xlab(bquote("Wood density g" ~cm^-3 )) +
  theme(legend.position = "top") + 
  scale_fill_manual(values = cols)
  

p1

# Save the graph
ggsave(p1, file = './graphs/woodDensity_triangle.png', 
       width = 4, 
       height = 4.2)


##########################################################################################
#' Now doing the spatial version of the analysis 
taus = c(0.5) # Median quantile 
quantModel <- rq(e ~ d, data = data, tau = taus)
summary(quantModel)
# Extract the residuals and fitted values from the model 
data$quantModelresid <- resid(quantModel)
data$quantModelfitted <- fitted(quantModel)

# Model the spatial data of the quantile 
spatModel1 <- gls(quantModelresid ~ 1, 
                        data = data, 
                        correlation = corExp(form = ~ x + y))

# Calculating the resisuals with the spatial effect 
dist_mat <- as.matrix(dist(cbind(data$x, data$y)))

calcVar <- function(d, phi, sigma){
 exp(-(d/phi)) * sigma^2
}

# variance matrix
var_mat <- calcVar(dist_mat, phi = spatModel1$modelStruct$corStruct, sigma = spatModel1$sigma)
# SQRT mat 
sq_mat <- t(chol(var_mat))
quantTaus <- c(0.05, 0.2, 0.5, 0.8, 0.95)

booting <- function(){
  new_e <- rnorm(nrow(sq_mat), mean = 0, sd = 1) 
  new_error_spat <- new_e %*% sq_mat
  # Assign old errors based on the order of the new errors to maintain the distribution 
  new_error_w_dist <- numeric(length(new_e))
  new_error_w_dist[order(new_error_spat)] <- sort(data$quantModelresid)
  ## Shit 
  data$new_y <- data$quantModelfitted + new_error_w_dist
  data$new_y
  # what about re ordering it by new ys
  #spat_order <- data$quantModelfitted + new_error_spat
  #data$new_y <- numeric(length(new_e))
  #data$new_y[order(spat_order)] <- sort(data$e)
  
  #with(data, plot(y = e, x = d))
  #with(data, plot(y = new_y, x = d))
  
  
  
  sim_model <- rq(new_y ~ d, data = data, tau = quantTaus)
  summary(sim_model)
  
  as.vector(predict(sim_model, data.frame(d = seq(min(data$d), max(data$d), length = 100))))
}

# Replicating the sampling 
boots <- replicate(100, booting())
dim(boots)
preds <- expand.grid(d = seq(min(data$d), max(data$d), length = 100), Quantile = factor(c(0.05, 0.2, 0.5, 0.8, 0.95)))
dim(preds)
# Calculating the mean, and quantiles 
preds$e <- apply(boots, 1, mean)
preds$CI025 <- apply(boots, 1, quantile, c(0.025))
preds$CI975 <- apply(boots, 1, quantile, c(0.975))

# Graphic of the data 
cols <- c("#8CB369", "#F4E285", "#4C8577","#F4A259", "#BC4B51")
p2 <- ggplot(preds[preds$d < 0.6,], aes(x = d, y = e, fill = Quantile)) + 
  geom_point(data = data[data$d < 0.6,], inherit.aes = F, aes(x = d, y = e), alpha = 0.5) + 
  geom_ribbon(aes(ymin = CI025, ymax = CI975), alpha = 0.75) + 
  theme_classic() + 
  ylab("Elevation (m asl)") + 
  xlab(bquote("Wood density g" ~cm^-3 )) +
  theme(legend.position = "top") + 
  scale_fill_manual(values = cols) +
  geom_line() 


p2

ggsave(p2, file = './graphs/woodDensity_triangle_spatial_new_erI.png', 
       width = 4, 
       height = 4.2)
