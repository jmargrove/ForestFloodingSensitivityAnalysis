# graphing the species probability distribution 
rm(list = ls())

Sys.setenv(LANG = "en")

# Import packages 
require(ggplot2)
require(gtable)
require(grid)

# Import data 
load('./analysis/adult_distribution_analysis/data/elevation_distribution_CI_NEW.R')
pelev_data <- read.table("./SRF_spatial_analysis_elevation/SRF_spatial_analysis_elevation/pelev_data.txt", header = TRUE)
spatial_data <- read.table("./SRF_spatial_analysis_elevation/SRF_spatial_analysis_elevation/forestplot_160_spatial_data.txt", header = TRUE)
spdata <- droplevels(spatial_data[spatial_data$sp %in% levels(pelev_data$sp),])
sp_dist_pred <- read.table("./SRF_spatial_analysis_elevation/SRF_spatial_analysis_elevation/preds_curves.txt", header = TRUE)
spnames <- read.table("./data/species_names.txt", header = TRUE)

# Preperation of the main data frame of the curves
sp_dist_pred$spname <- with(spnames[spnames$sp %in% levels(pelev_data$sp),], rep(paste(Cap,species), each = 131))
sp_dist_pred$spname <- factor(sp_dist_pred$spname)
sp.levels <- levels(reorder(levels(sp_dist_pred$spname), pelev_data$pe))
sp_dist_pred$spname <- factor(sp_dist_pred$spname, levels=sp.levels)

# Adding confidence intervals to the data frame 
sp_dist_pred$CI025 <- CI[1, 1:(131*16)]
sp_dist_pred$CI975 <- CI[2, 1:(131*16)]

dim(sp_dist_pred)/16
dim(CI)/16
# Graph 
p1 <- ggplot(sp_dist_pred[sp_dist_pred$elev > min(spdata$elev) & sp_dist_pred$elev < max(spdata$elev),], aes(x = elev, y = p)) + 
  geom_ribbon(aes(ymin=CI025,ymax=CI975,linetype=NA), alpha =  0.22) + 
  geom_line() + 
  facet_wrap(~spname, scale="free_y", ncol = 8) + 
  theme_classic() + 
  theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black")) + 
  xlab("Elevation (m)") + ylab("p(occurance)") + 
  theme(legend.position="none") +
  theme(strip.text = element_text(face = "italic")) 

p1


?facet_wrap

ggsave(p1, 
       file = './analysis/adult_distribution_analysis/graph_code/graphs/species_probability_distribution_Fig2.png',
       width = 16, 
       height = 5)

