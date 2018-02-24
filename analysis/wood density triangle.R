##### author: James Margrove 

Sys.setenv(LANG = "en")

# Import packages 
require(ggplot2)
require(gtable)
require(grid)

# Import data 
sp_dist_pred <- read.table("./data/species_dist_predictions.txt", header = TRUE)
spnames <- read.table("./data/species_names.txt", header = TRUE)
pelev_data <- read.table("./data/pelev_data.txt", header = TRUE)
spatial_data <- read.table("./data/forestplot_160_spatial_data.txt", header = TRUE)
spdata <- droplevels(spatial_data[spatial_data$sp %in% levels(pelev_data$sp),])

# Reubens data 
reu_data <- read.table( "./data/Reuben_data/data_cleaned.txt", header = TRUE)



# Cut the data to ensure that 
spatial_data$XCut <- cut(spatial_data$X50N, 36)
spatial_data$YCut <- cut(spatial_data$Y50N, 18)







e <- as.vector(unlist(with(spatial_data, tapply(elev, list(XCut, YCut), mean))))
d <- as.vector(unlist(with(spatial_data, tapply(dden, list(XCut, YCut), mean))))
a <- as.vector(unlist(with(spatial_data, tapply(dden, list(XCut, YCut), length))))


plot(y = e, d)
taus <- c(.025,0.1, 0.5, 0.9, .975)
xx <- seq(min(spatial_data$dden, na.rm = T),max(spatial_data$dden, na.rm = T), length = 100)
f <- coef(rq((e)~ d,tau=taus))
yy <- cbind(1,xx)%*%f
for(i in 1:length(taus)){
  lines(xx,yy[,i],col = "red")
}



rmod <- rq(e ~ d, tau = taus)
summary(rmod)

pred <- data.frame(d = seq(min(d, na.rm = T), max(d, na.rm = T), length = 100))
preds <- data.frame(d = rep(pred$d, 5))
preds$el <- as.vector(predict(rmod, pred, "response"))
preds$Quantile <- rep( factor(taus), each = 100)

data_plot <- data.frame(e, d)

summary(lm(e ~ d))


source("./functions/booter.R")

args(booter)

CI <- booter(model = rmod, data = data_plot, preds = pred, n = 1000, quantReg = TRUE)
preds$CI025 <- CI[1,]
preds$CI975 <- CI[2,]

cols <- c("#8CB369", "#F4E285", "#4C8577","#F4A259", "#BC4B51")

p2 <- ggplot(data = data_plot, aes(x = d, y = e)) + 
  geom_ribbon(data = preds, inherit.aes = F, 
              aes(x = d, ymin = CI025, ymax = CI975, group = Quantile, fill = Quantile), alpha = 0.75) + 
  geom_line(data = preds, aes(x = d, y = el, group = Quantile), linetype = 1) + 
  theme_classic() + 
  xlab(bquote("Wood density g" ~cm^-3 )) +
  ylab("elevation (asl m)") + 
  scale_fill_manual(values = cols) + 
  geom_point(alpha = 0.5) + 
  theme(legend.position = "top")

p2


ggsave(p2, file = './graphs/density_dist.png', 
       width = 4, 
       height = 4.2)

