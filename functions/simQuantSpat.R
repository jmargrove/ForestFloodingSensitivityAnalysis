################################################################################
#' @title simQuantSpat
#' @author James Margrove
#' @description simulated function 

# Set langauge and clear workspace
Sys.setenv(LANG = "en")
rm(list=ls())

# Import packages 
require(ggplot2)
require(gtable)
require(grid)
require(quantreg)
require(ape)

# Import data 
pelev_data <- read.table("./data/pelev_data.txt", header = TRUE)
spatial_data <- read.table("./data/forestplot_160_spatial_data.txt", header = TRUE)
plotExtent <- read.table("./data/plotExtent.txt", header = TRUE)
# Reubens data (Sort out later )
#reu_data <- read.table( "./data/Reuben_data/data_cleaned.txt", header = TRUE)
# Cut the data to ensure that 
Xbreaks <- seq(plotExtent[1,"X50N"], plotExtent[3,"X50N"], length = 37)  
Ybreaks <- seq(plotExtent[1,"Y50N"], plotExtent[2,"Y50N"], length = 19)  

spatial_data$XCut <- cut(spatial_data$X50N, breaks = Xbreaks)
spatial_data$YCut <- cut(spatial_data$Y50N, breaks = Ybreaks)
str(spatial_data)
# Calculate average, elevation and wood density per square 
e <- as.vector(unlist(with(spatial_data, tapply(elev, list(XCut, YCut), mean, na.rm = TRUE))))
d <- as.vector(unlist(with(spatial_data, tapply(dden, list(XCut, YCut), mean, na.rm = TRUE))))
x <- as.vector(unlist(with(spatial_data, tapply(X50N, list(XCut, YCut), mean, na.rm = TRUE))))
y <- as.vector(unlist(with(spatial_data, tapply(Y50N, list(XCut, YCut), mean, na.rm = TRUE))))
spat_data <- data.frame(e, d, x, y)
spat_data <- spat_data[with(spat_data, which(!is.na(y))),]

#### Email 2 from robi
data = spat_data
taus = c(0.5)
quantModel <- rq(e ~ d, data = data, tau = taus)
summary(quantModel)
data$quantModelresid <- resid(quantModel)
ggplot(data, aes(x, y, color = quantModelresid)) + geom_point()


# Are wood density values auto-correlated?
# Calculate Morans I
coords_dist_mat <- as.matrix(dist(cbind(spat_data$x, spat_data$y)))
coords_dist_mat_inv <- 1/coords_dist_mat
diag(coords_dist_mat_inv) <- 0
# Calculate Moran's I
Moran.I(data$quantModelresid, coords_dist_mat_inv)
require(nlme)
# Ok so the residuals are quto correlated 
quantModelfitted <- fitted(quantModel)
plot(y = quantModelresid, x = quantModelfitted)
nullModel <- nlme::gls(quantModelresid ~ 1, 
                       data = data)


vario2 <- Variogram(nullModel, form = ~x + y, resType = "pearson")
plot(vario2, smooth = TRUE, ylim = c(0, 1.2))
#On the variogram we see that the semi-variance is clearly increasing with distance.
# Semi variance is, 

# Model using different spatial structures 
spatModel1 <- nlme::gls(quantModelresid ~ 1, 
                        data = data, 
                        correlation = corExp(form = ~ x + y))
spatModel2 <- nlme::gls(quantModelresid ~ 1, 
                        data = data, 
                        correlation = corGaus(form = ~ x + y))
spatModel3 <- nlme::gls(quantModelresid ~ 1, 
                        data = data, 
                        correlation = corSpher(form = ~ x + y))
spatModel4 <- nlme::gls(quantModelresid ~ 1, 
                        data = data, 
                        correlation = corLin(form = ~ x + y))

AIC(spatModel1)
AIC(spatModel2)
AIC(spatModel3)
AIC(spatModel4)
# Best model is model 1 corExp
summary(spatModel1)
vario4 <- Variogram(spatModel1, form = ~x + y)
plot(vario4, smooth = FALSE)



# this is very good completely flattens out the residuals 

summary(spatModel2)

gaussian(spatModel1)

?Variogram
str(spatModel1)
spatModel1$range



?gaussian
gaussian()
args(gaussian)


summary(spatModel)
str(spatModel$modelStruct$corStruct)

cg <- nlme::corGaus(form = quantModelresid ~ x + y)
icg <- nlme::Initialize(cg, data)
str(icg)
## OK 


# Extract distances 
dist_mat <- as.matrix(dist(cbind(x = data$x, y = data$y)))
var_mat <- ??
sigma <- t(chold(var_mat))
rnorm(nrow(data), 0, sd = sigma)



#############
simQuantSpat <- function(quantModel, coords, nsim = 10, coef = TRUE){
  # args 
  #quantModel <- mod
  #coords <- spat_data[,c(3,4)]
  #coef = FALSE
  #nsim = 100
  #
  taus <- quantModel$tau
  data <- quantModel$model
  colnames(coords) <- c("x", "y")
  data <- cbind(data, coords)
  n = nrow(data)
  x <- coords[,1]
  y <- coords[,2]
  data$quantModelresid <- resid(quantModel)
  data$quantModelfitted <- fitted(quantModel)
  # Run 
  spatModel <- nlme::gls(quantModelresid ~ 1, 
                         data = data, 
                         correlation = corGaus(form = ~ x + y))
  
  spatModelfitted <- fitted(spatModel)
  spatModelresid <- resid(spatModel)
  
  sigma <- spatModel$sigma
  simCoef <- function(){
    sim_error <- rnorm(n, mean = spatModelfitted, sd = sigma)
    data$new_y <- data$quantModelfitted + sim_error
    sim_quantModel <-  update(quantModel, new_y ~ . , data = data)
    summary(sim_quantModel)
    
    if(coef){
      coef_sim <- coef(sim_quantModel) 
      return(coef_sim)
    }
    else if(!coef){
      preds <- data.frame(d = seq(min(data[,"d"]), max(data[,"d"]), length = 100))
      e <- predict(sim_quantModel, newdata = preds)
      return(e)
    }
  }
  
  if(coef){
    sim_dist <- replicate(nsim, simCoef())
    sim_CI <- apply(sim_dist, 1, quantile, c(0.025, 0.975))
    sim_CI <- t(sim_CI) # CHECK!!! switch for binding
    sim_mean <- apply(sim_dist, 1, mean)
    sim_median <- apply(sim_dist, 1, median)
    sim_coefs <- cbind(sim_mean, sim_median, sim_CI)
    return(sim_coefs)
  } 
  else if(!coef){
    sim_dist <- replicate(nsim, simCoef())
    CI <- apply(sim_dist, 1, quantile, c(0.025, 0.975))
    mcoef <- apply(sim_dist, 1, mean)
    data.frame(mcoef, 
               CI025 = CI[1,], 
               CI975 = CI[2,], 
               d = seq(min(data[,"d"]), max(data[,"d"]), length = 100))
  }
}


#############
# current tau
taus = c(0.975)
# Data frame creation 
mod <- rq(e ~ d, data = spat_data, tau = taus)
summary(mod)
#simQuantSpat(quantModel = mod, coords = spat_data[,c(3,4)], coef = TRUE, nsim = 100)
#preds <- data.frame(d = seq(min(spat_data[,"d"]), max(spat_data[,"d"]), length = 100))
taus = c(0.025, 0.1, 0.5, 0.9, 0.975)
preds <- data.frame(mcoef = as.numeric(),
                    CI025 = as.numeric(),
                    CI975 = as.numeric(),
                    d = as.numeric(),
                    Q = as.numeric())
for(t in taus){
  mod <- rq(e ~ d, data = spat_data, tau = t)
  pred <- simQuantSpat(quantModel = mod, coords = spat_data[,c(3,4)], coef = FALSE, nsim = 1000)
  pred$Q <- rep(t, nrow(pred))
  preds <- rbind(preds, pred)
}

# Colors for CI ribbon 
cols <- c("#8CB369", "#F4E285", "#4C8577", "#F4A259", "#BC4B51")
# Ok basic done 
p1 <- ggplot(spat_data[spat_data$d < 0.55,], aes(x = d, y = e)) + 
  geom_ribbon(data = preds[preds$d < 0.55,], 
              inherit.aes = FALSE, 
              aes(x = d, ymin = CI025, ymax = CI975, group = Q,  fill = factor(Q)), 
              alpha = 0.77) + 
  geom_line(data = preds[preds$d < 0.55,], inherit.aes = FALSE, aes(x = d, y = mcoef, group = Q)) + 
  theme_classic() + 
  xlab(bquote("Wood density g" ~cm^-3 )) +
  ylab("elevation (asl m)") + 
  scale_fill_manual(values = cols) +
  geom_point(alpha = 0.5) +
  theme(legend.position = "top")
  

ggsave(p1, file = './graphs/simQuantSpat_wden_triangle.png', 
       width = 4, 
       height = 4.2)


