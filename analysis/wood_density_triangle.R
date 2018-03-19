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
spatial_data <- read.table("./data/forestplot_160_spatial_data_UPDATE.txt", header = TRUE)
plotExtent <- read.table("./data/plotExtent.txt", header = TRUE)
reu_data <- read.table("./data/reu_160plot_combinded_data.txt", header = TRUE)

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
ggplot(spat_data, aes(x = d, y = e)) + geom_point()

# Divide reubens plot data up in to 1/4 ha areas 
with(reu_data, tapply(elev, list(Forest, ha4plot), mean))

for(i in levels(reu_data$Forest)){
  for(j in as.character(1:4)){
    subplot <- reu_data[reu_data$Forest == i & reu_data$ha4plot == j,]
    
    xr <- range(subplot[, "x"])
    yr <- range(subplot[, "y"])
    
    midPoint <- c(mean(range(subplot[, "x"])), mean(range(subplot[, "y"])))
    
    quarter1 <- subplot[subplot$x > xr[1] & subplot$x < midPoint[1] & 
                          subplot$y > yr[1] & subplot$y < midPoint[2],]
    
    quarter2 <- subplot[subplot$x > xr[1] & subplot$x < midPoint[1] & 
                          subplot$y < yr[2] & subplot$y > midPoint[2],]
    
    quarter3 <- subplot[subplot$x < xr[2] & subplot$x > midPoint[1] & 
                          subplot$y < yr[2] & subplot$y > midPoint[2],]
    
    quarter4 <- subplot[subplot$x < xr[2] & subplot$x > midPoint[1] & 
                          subplot$y > yr[1] & subplot$y < midPoint[2],]
    
    quarter1$Q <- rep("Q1", nrow(quarter1))
    quarter2$Q <- rep("Q2", nrow(quarter2))
    quarter3$Q <- rep("Q3", nrow(quarter3))
    quarter4$Q <- rep("Q4", nrow(quarter4))
    
    
    quaters <- rbind(quarter1, quarter2, quarter3, quarter4)
    
    new_row <- data.frame(e = with(quaters, tapply(elev, Q, mean)),
                          d = with(quaters, tapply(dden, Q, mean)), 
                          x = with(quaters, tapply(x, Q, mean)),
                          y = with(quaters, tapply(y, Q, mean)))
    
    spat_data <- rbind(spat_data, new_row)
  }
}


# how many species are there in total 

nuSp <- (unique(c(levels(spatial_data[!is.na(spatial_data$dden),]$fullname), 
                  levels(reu_data[!is.na(reu_data$dden),]$Species))))
# how many individuals in total 

nrow(reu_data) + nrow(spatial_data)

write.table(spat_data, file = "./data/wden_triangle_data.txt")
