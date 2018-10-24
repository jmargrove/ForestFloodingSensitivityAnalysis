################################################################################
#' @title Interaction plots of species + inundation response 
#' @author James margrove 

# Import model 
load("./models/Model.RData")

# Import data 
boots <- read.table("./data/FLoodIntSp.txt", header = TRUE)
pelev_data <- read.table("./data/pelev_data.txt", header = TRUE)
dden_data <- read.table("./data/dden_adult_new.txt", header = TRUE)
data <- s3@frame

# Calculate confidence intervals 
CI <- apply(boots, 1, quantile, c(0.025, 0.975))

# Create high and low wood density factors 
dden_data$fden <- cut(dden_data$dden_adult, 
    breaks = c(0, mean(dden_data$dden_adult), 1), 
    labels = c("Low wood density", "High wood density"))

# Predictions for the graph
preds <- expand.grid(dia = mean(exp(data$`log(dia)`)), 
                     ztopo = 0, 
                     sp = levels(data$sp), 
                     flood = levels(data$flood), 
                     f.time = as.factor(3))

preds$p <- predict(s3, preds, type = "response", re.form = ~0)
preds$CI025 <- CI[1,] 
preds$CI975 <- CI[2,]
preds$fden <- rep(dden_data$fden, times = 2)
preds$pe <- rep(pelev_data$pe, times = 2)
preds$fden <- relevel(preds$fden, ref = "High wood density")
colnames(preds)[4] <- "Water inundation"
levels(preds$`Water inundation`) <- c("Dry", "Wet")

cols <- c("#8CB369", "#F4E285", "#4C8577","#F4A259", "#BC4B51")

# Graphing the results 
p1 <- ggplot(preds, aes(x = reorder(sp, pe), y = p, group = `Water inundation`)) + 
  facet_grid(~fden, space = "free", scale = "free") + 
  geom_errorbar(aes(ymin = CI025, ymax=CI975), width = 0.3, alpha = 0.2) + 
  theme_classic() + 
  geom_point(size = 4) + 
  geom_point(size = 3, aes(color = `Water inundation`)) + 
  theme(legend.position = c(0.25, 0.8)) + 
  xlab("Species") + 
  ylab("p(Mortality)") + 
  theme(axis.text.x = element_text(face = "italic", angle = 45, vjust = .7)) + 
  scale_color_manual(values = c("light grey", cols[c(5)]))
p1

p1
# Save graph 
ggsave(p1, file = "./graphs/spMortality_inundation_interaction_Fig2a.png", 
       width = 8, 
       height = 4)





