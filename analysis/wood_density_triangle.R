
######## author: James Margrove 

# Set langauge and clear workspace
Sys.setenv(LANG = "en")
rm(list=ls())

# Import packages 
require(ggplot2)
require(gtable)
require(grid)
require(quantreg)
require(ape)
require(McSpatial)
# Import data 
pelev_data <- read.table("./data/pelev_data.txt", header = TRUE)
spatial_data <- read.table("./data/forestplot_160_spatial_data.txt", header = TRUE)
plotExtent <- read.table("./data/plotExtent.txt", header = TRUE)
# Reubens data (Sort out later )
#reu_data <- read.table( "./data/Reuben_data/data_cleaned.txt", header = TRUE)
648/3
# Cut the data to ensure that 
Xbreaks <- seq(plotExtent[1,"X50N"], plotExtent[3,"X50N"], length = 37)  
Ybreaks <- seq(plotExtent[1,"Y50N"], plotExtent[2,"Y50N"], length = 19)  

spatial_data$XCut <- cut(spatial_data$X50N, breaks = Xbreaks)
spatial_data$YCut <- cut(spatial_data$Y50N, breaks = Ybreaks)

# Calculate average, elevation and wood density per square 
e <- as.vector(unlist(with(spatial_data, tapply(elev, list(XCut, YCut), mean, na.rm = TRUE))))
d <- as.vector(unlist(with(spatial_data, tapply(dden, list(XCut, YCut), mean, na.rm = TRUE))))
x <- as.vector(unlist(with(spatial_data, tapply(X50N, list(XCut, YCut), mean, na.rm = TRUE))))
y <- as.vector(unlist(with(spatial_data, tapply(Y50N, list(XCut, YCut), mean, na.rm = TRUE))))

spat_data <- data.frame(e, d, x, y)
spat_data <- spat_data[!is.na(d),]

require(ade4)
coords_dist <- dist(cbind(spat_data$x, spat_data$y))
dden_dist <- dist(spat_data$d)
mantel.rtest(coords_dist, dden_dist, nrepet = 9999)

# Are wood density values auto-correlated?
# Calculate Morans I
coords_dist_mat <- as.matrix(dist(cbind(spat_data$x, spat_data$y)))
coords_dist_mat_inv <- 1/coords_dist_mat
diag(coords_dist_mat_inv) <- 0
# Calculate Moran's I
Moran.I(spat_data$d, coords_dist_mat_inv)
# full data set is very autocorrelated, this is a major issue. And

# The Moran and Mantel tests show that there is spatial autocorrelation in the values. 
# We need to account for this by doing a quantreg accounting for this 

taus <- c(0.025, 0.1, 0.5, 0.9, 0.975)

linerEQ <- function(int, slope, x){
  (x*slope) + int
}

n = as.integer(dim(spat_data)[1]/3)

qrSpatial <- function(data){
  rown <- sample(1:dim(data)[1], replace = T, size = n)
  samp <- data[rown,]
  coords_dist <- dist(cbind(samp$x, samp$y))
  dden_dist <- dist(samp$d)
  mt <- mantel.rtest(coords_dist, dden_dist, nrepet = 999)
  if(mt$pvalue > 0.05){
    qrmod <- rq(e ~ d, data = samp, tau = taus)
    p <- predict(qrmod, data.frame(d = seq(min(data$d), max(data$d), length = 100)))
    return(as.vector(p))
  }
}

# Boot strap without the data
#dist <- foreach(i = 1:5000, .combine = cbind) %do% qrSpatial(data = spat_data)

# save the boot strap 
#write.table(dist, file = "./data/boots_triangle_wden.txt")
dist <- read.table("./data/boots_triangle_wden.txt", header = TRUE)
## how many were successsfully independent 
mdist <- apply(dist, 1, mean)

# prediction data frame 
preds <- expand.grid(d = seq(min(spat_data$d), max(spat_data$d), length = 100), taus = as.factor(taus))
preds$e <- apply(dist, 1, mean)
CI <- apply(dist, 1, quantile, c(0.025, 0.975))
preds$CI025 <- CI[1,]
preds$CI975 <- CI[2,]

# Colors for CI ribbon 
cols <- c("#8CB369", "#F4E285", "#4C8577","#F4A259", "#BC4B51")

# plot the results 
p1 <- ggplot(preds[preds$d < 0.6,], aes(x = d, y = e, group = taus)) + 
  geom_ribbon(aes(ymin = CI025, ymax = CI975, group = taus, fill = taus), alpha = 0.75) + 
  geom_point(data = spat_data[spat_data$d < 0.6, ], inherit.aes = FALSE, aes(x = d, y = e), alpha = 0.4) + 
  theme_classic() + 
  geom_line() + 
  xlab(bquote("Wood density g" ~cm^-3 )) +
  ylab("elevation (asl m)") + 
  scale_fill_manual(values = cols) + 
  theme(legend.position = "top")


p1

# Save the graph 
ggsave(p1, file = './graphs/wden_triangle.png', 
       width = 4, 
       height = 4.2)


ribbon_data <- data.frame(d = seq(min(spat_data$d), max(spat_data$d), length = 100))

ribbon_data$CI025 <- preds[preds$taus == "0.025",]$CI025
ribbon_data$CI975 <- preds[preds$taus == "0.975",]$CI975


p2 <- ggplot(preds[preds$d < 0.6,], aes(x = d, y = e, group = taus)) + 
  geom_ribbon(data = ribbon_data[ribbon_data$d < 0.6,], inherit.aes = F, aes(x = d, ymin = CI025, ymax = CI975), fill = "blue",alpha = 0.2) + 
  geom_point(data = spat_data[spat_data$d < 0.6, ], inherit.aes = FALSE, aes(x = d, y = e), alpha = 0.4) + 
  theme_classic() + 
  geom_line(aes(color = taus)) + 
  xlab(bquote("Wood density g" ~cm^-3 )) +
  ylab("elevation (asl m)") + 
  scale_color_manual(values = cols) + 
  theme(legend.position = "top")


p2

ggsave(p2, file = './graphs/wden_triangle_constant_envalope.png', 
       width = 4, 
       height = 4.2)
