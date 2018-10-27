################################################################################
##### displaying the adult distributions 
##### author: James Margrove 

Sys.setenv(LANG = "en")

# Import packages 
require(ggplot2)
require(gtable)
require(grid)

# Import data 
sp_dist_pred <- read.table("./data/species_dist_predictions.txt", header = TRUE)

elevation_bootstrap <- read.table("./data/elevation_bootstrap.txt", header = TRUE)
spnames <- read.table("./data/species_names.txt", header = TRUE)

pelev_data <- read.table("./data/pelev_data.txt", header = TRUE)
spatial_data <- read.table("./data/forestplot_160_spatial_data.txt", header = TRUE)
spdata <- droplevels(spatial_data[spatial_data$sp %in% levels(pelev_data$sp),])
dim(elevation_bootstrap)
elevation_bootstrap[1:10,1:10]
CI <- apply(elevation_bootstrap, 1, quantile, c(0.025, 0.975))
quantile(elevation_bootstrap[70,], c(0.025, 0.975))
CI[,70]



#write.table(CI, file = "elevation_CI.txt")
elevation_CI <- read.table("./data/elevation_CI.txt", header = TRUE)

# Explore the results 
ggplot(spdata, aes(x = reorder(sp, elev), y = elev)) + geom_boxplot() + 
  geom_point(data = pelev_data, 
             aes(x = sp, y = pe), size = with(spdata, tapply(sp, sp, length)/100), 
             color = "red") + 
  theme_bw()


# Preperation of the main data frame of the curves
sp_dist_pred$spname <- with(spnames[spnames$sp %in% levels(pelev_data$sp),], rep(paste(Cap,species), each = 121))
sp_dist_pred$spname <- factor(sp_dist_pred$spname)
sp.levels <- levels(reorder(levels(sp_dist_pred$spname), pelev_data$pe))
sp_dist_pred$spname <- factor(sp_dist_pred$spname, levels=sp.levels)

# Grey for the ribbon 
colss <- rep("white", 16)
colss[c(1,12,6,13,15,16)] <- "light grey"

with(sp_dist_pred, tapply(sp, sp, length))
(dim(elevation_bootstrap)-16)/16
# Adding confidence intervals to the data frame 
CIwhere <- c(rep(c(rep(TRUE, 121), rep(FALSE, 10)), 16), rep(FALSE, 16))
length(CIwhere)
sp_dist_pred$CI025 <- CI[1,CIwhere]
sp_dist_pred$CI975 <- CI[2,CIwhere]
head(sp_dist_pred)

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

# Now create the graph of the output 2: 
dummy <- ggplot(data = sp_dist_pred, aes(x=elev, y = p)) + facet_wrap(~spname) + 
  geom_rect(aes(fill=rep(colss, 121)), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax = Inf)  + 
  theme_minimal() +
  scale_color_manual(values = colss) + scale_fill_manual(values = colss) + 
  theme(strip.text = element_text(face = "italic"))

#Create the functions and labels required for combining the two plots 
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(dummy)

# gtable 
gtable_select <- function (x, ...) 
{
  matches <- c(...)
  x$layout <- x$layout[matches, , drop = FALSE]
  x$grobs <- x$grobs[matches]
  x
}

# Creating the pannels for insertion 
panels <- grepl(pattern="panel", g2$layout$name)
strips <- grepl(pattern="strip-t", g2$layout$name)
g2$layout$t[panels] <- g2$layout$t[panels] - 1
g2$layout$b[panels] <- g2$layout$b[panels] - 1

# 
new_strips <- gtable_select(g2, panels | strips)
grid.newpage()

# Where the new pannels are going to go!
grid.draw(new_strips) 

#Combining the two plots...
gtable_stack <- function(g1, g2){
  g1$grobs <- c(g1$grobs, g2$grobs)
  g1$layout <- transform(g1$layout, z= z-max(z), name="g2") 
  g1$layout <- rbind(g1$layout, g2$layout) # combine
  g1
}

# Ideally you'd remove the old strips, for now they're just covered
new_plot <- gtable_stack(g1, new_strips)
grid.newpage()
grid.draw(new_plot)


### CI for the predicted elevations 
n = dim(CI)[2]
data.frame(sp = levels(spdata$sp), 
           CI025 = CI[1,c((n-15):n)],
           CI975 = CI[2,c((n-15):n)])

