################################################################################
##### displaying the adult distributions 
##### author: James Margrove 

Sys.setenv(LANG = "en")

# Import packages 
require(ggplot2)
require(gtable)
require(grid)

# Import data 
sp_dist_pred <- read.table("./data/preds_curves.txt", header = TRUE)
elevation_bootstrap <- read.table("./data/elevation_bootstrap.txt", header = TRUE)
spnames <- read.table("./data/species_names.txt", header = TRUE)

pelev_data <- read.table("./data/pelev_data.txt", header = TRUE)
spatial_data <- read.table("./data/forestplot_160_spatial_data.txt", header = TRUE)
spdata <- droplevels(spatial_data[spatial_data$sp %in% levels(pelev_data$sp),])
dim(elevation_bootstrap)
elevation_bootstrap[1:10,1:10]
CI <- apply(elevation_bootstrap, 1, quantile, c(0.025, 0.975))

#write.table(CI, file = "./data/elevation_CI.txt")
elevation_CI <- read.table("./data/elevation_CI.txt", header = TRUE)

# Preperation of the main data frame of the curves
sp_dist_pred$spname <- with(spnames[spnames$sp %in% levels(pelev_data$sp),],
                            rep(paste(Cap,species), each = 131))
sp_dist_pred$spname <- factor(sp_dist_pred$spname)
sp.levels <- levels(reorder(levels(sp_dist_pred$spname), pelev_data$pe))
sp_dist_pred$spname <- factor(sp_dist_pred$spname, levels=sp.levels)

# Grey for the ribbon 

dden_adult <- read.table("./data/dden_adult_new.txt", header = T)
dden_adult$fden <- with(dden_adult, cut(dden_adult, breaks = c(0, mean(dden_adult), 1), labels = c("low", "high")))

which()

colss <- rep("white", 16)
colss[c(2)] <- "light grey"
with(sp_dist_pred, tapply(sp, sp, length))
(dim(elevation_bootstrap)-16)/16
# Adding confidence intervals to the data frame 
#CIwhere <- c(rep(c(rep(TRUE, 129), rep(FALSE, 10)), 16))
#length(CIwhere)
#dim(CI)
sp_dist_pred$CI025 <- CI[1,1:2096]
sp_dist_pred$CI975 <- CI[2,1:2096]
head(sp_dist_pred)
sp_max <- with(sp_dist_pred, tapply(p, sp, max))
sp_elev <- sp_dist_pred[which(sp_dist_pred$p %in% sp_max), "elev"]
pMax_elev <- data.frame(sp_max, sp_elev, sp = levels(sp_dist_pred$spname))

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

cols <- c("#8CB369", "#F4E285", "#4C8577","#F4A259", "#BC4B51")
colss <- rep("#EFF7FF", 16)
colss[c(1,4,6,7,9,13,14)] <- "#6788B5"




dden_adult
# Now create the graph of the output 2: 
dummy <- ggplot(data = sp_dist_pred, aes(x=elev, y = p)) + 
  facet_wrap(~spname) + 
  geom_rect(aes(fill=rep(as.factor(1:16), each = 131)), alpha = 0.5, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax = Inf)  + 
  theme_minimal() +
  scale_fill_manual(values = colss) + 
  theme(strip.text = element_text(face = "italic"))

dummy

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

new_plot



ggsave(new_plot, file = "./graphs/sp_distribution_fig2a.png", 
       width = 9, 
       height = 5)

### CI for the predicted elevations 
n = dim(CI)[2]
data.frame(sp = levels(spdata$sp), 
           CI025 = CI[1,c((n-15):n)],
           CI975 = CI[2,c((n-15):n)])

