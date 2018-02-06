################################################################################
# title: flooding sensitivity response predicting species distributions 
# author: James Margrove 

# IMPORT FUNCTIONS AND PACKAGES
require(ggplot2)

##### IMPORT DATA 
riskratio_data <- read.table("./data/riskratio_data.txt", header = TRUE)
spdata <- read.table("./data/spdata.txt", header = TRUE)
riskratio_data$pe <- read.table("./data/pelev_data.txt", header = TRUE)$pe

##### Calculating the abundance 
abn <- with(spdata[spdata$sp %in% riskratio_data$sp,], tapply(sp, sp, length))
riskratio_data$Abundance <- abn[!is.na(abn)]

##### Explore 
ggplot(riskratio_data, aes(x = rr, y = pe, size = Abundance)) + 
  geom_point() + 
  stat_smooth(method = lm)

##### Model data with a weighted linear model 
model <- lm(pe ~ rr, weight = Abundance, data = riskratio_data)
summary(model)

##### model evaluation 
par(mfrow=c(2,2))
plot(model)

##### bootstrap the data 
preds <- expand.grid(rr = with(riskratio_data, 
                               seq(from = min(rr), 
                                   to = max(rr), 
                                   length = 100)))

preds$p <- predict(model, preds, type = "response")
preds$CI <- predict(model, preds, type = "response", se.fit = TRUE)$se.fit
colnames(preds)

##### Graph the predictions 
p1 <- ggplot(preds, aes(x = rr, y = p)) + 
        geom_line() + 
        geom_ribbon(aes(ymin = p - CI, ymax = p + CI), alpha = 0.22) +
        geom_line(aes(x=rr, y=p-CI), linetype = 2) + 
        geom_line(aes(x=rr, y=p+CI), linetype = 2) + 
        geom_point(data = riskratio_data, aes(x = rr, y = pe, size = Abundance)) + 
        geom_line() + 
        ylab("p(elevation) m") + 
        xlab("Water inundation sensitivity") +
        theme_classic() +
        theme(legend.position = c(0.2,0.85))

p1

ggsave(p1, file = './graphs/floodingsensitivity_pelevation.png', 
       width = 4, 
       height = 4)



