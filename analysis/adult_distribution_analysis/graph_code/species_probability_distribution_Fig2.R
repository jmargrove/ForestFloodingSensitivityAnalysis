# graphing the species probability distribution 

Sys.setenv(LANG = "en")

# Import packages 
require(ggplot2)
require(gtable)
require(grid)

# Import data 
sp_dist_pred <- read.table("./data/species_dist_predictions.txt", header = TRUE)

#elevation_bootstrap <- read.table("./data/elevation_bootstrap.txt", header = TRUE)
spnames <- read.table("./data/species_names.txt", header = TRUE)

pelev_data <- read.table("./data/pelev_data.txt", header = TRUE)
spatial_data <- read.table("./data/forestplot_160_spatial_data.txt", header = TRUE)
spdata <- droplevels(spatial_data[spatial_data$sp %in% levels(pelev_data$sp),])
CI <- apply(elevation_bootstrap, 1, quantile, c(0.025, 0.975))
quantile(elevation_bootstrap[70,], c(0.025, 0.975))


write.table(CI, file = "analysis/adult_distribution_analysis/data/elevation_distribution_CI.txt")
elevation_CI <- read.table("analysis/adult_distribution_analysis/data/elevation_distribution_CI.txt", header = TRUE)

# Preperation of the main data frame of the curves
sp_dist_pred$spname <- with(spnames[spnames$sp %in% levels(pelev_data$sp),], rep(paste(Cap,species), each = 121))
sp_dist_pred$spname <- factor(sp_dist_pred$spname)
sp.levels <- levels(reorder(levels(sp_dist_pred$spname), pelev_data$pe))
sp_dist_pred$spname <- factor(sp_dist_pred$spname, levels=sp.levels)

# Adding confidence intervals to the data frame 
CIwhere <- c(rep(c(rep(TRUE, 121), rep(FALSE, 10)), 16), rep(FALSE, 16))
sp_dist_pred$CI025 <- CI[1,CIwhere]
sp_dist_pred$CI975 <- CI[2,CIwhere]


# Graph 
p1 <- ggplot(sp_dist_pred[sp_dist_pred$elev > min(spdata$elev) & sp_dist_pred$elev < max(spdata$elev),], aes(x = elev, y = p)) + 
  geom_ribbon(aes(ymin=CI025,ymax=CI975,linetype=NA), alpha =  0.22) + 
  geom_line() + 
  facet_wrap(~spname, scale="free_y") + 
  xlim(min(spdata$elev),max(spdata$elev)) + 
  theme_classic() + 
  theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black")) + 
  xlab("Elevation (m)") + ylab("p(occurance)") + 
  theme(legend.position="none") +
  theme(strip.text = element_text(face = "italic"))

p1

ggsave(p1, 
       file = './analysis/adult_distribution_analysis/graph_code/graphs/species_probability_distribution_Fig2.png',
       width = 8, 
       height = 5)

