################################################################################
# title: flooding sensitivity response predicting species distributions 
# author: James Margrove 

# Clear work space 
rm(list = ls ())

# IMPORT FUNCTIONS AND PACKAGES
require(ggplot2)
require(car)

source("./functions/aovPerVar.R")

# IMPORT DATA 
riskratio_data <- read.table("./data/riskratio_data.txt", header = TRUE)
spdata <- read.table("./data/spdata.txt", header = TRUE)
riskratio_data$pe <- read.table("./data/pelev_data.txt", header = TRUE)$pe

# Calculating the abundance 
abn <- with(spdata[spdata$sp %in% riskratio_data$sp,], tapply(sp, sp, length))
riskratio_data$Abundance <- abn[!is.na(abn)]
dden_data <- read.table("./data/dden_adult.txt", header = TRUE)

dden_data <- dden_data[order(dden_data$sp), ]
riskratio_data$dden <- dden_data$dden_adult

# Explore 
ggplot(riskratio_data, aes(x = rr, y = pe, size = Abundance)) + 
  geom_point() + 
  stat_smooth(method = lm)

# Model data with a weighted linear model 
model <- lm(pe ~ rr + dden, weight = Abundance, data = riskratio_data)
summary(model)
vif(model)

# Anova test
ma <- Anova(model)

# Anova percentage variation 
aovPerVar(ma)

# model evaluation 
par(mfrow=c(2,2))
plot(model)

# bootstrap the data 
preds <- expand.grid(rr = with(riskratio_data, 
                               seq(from = min(rr), 
                                   to = max(rr), 
                                   length = 100)), 
                     dden = mean(riskratio_data$dden))

preds$p <- predict(model, preds, type = "response")
preds$CI <- predict(model, preds, type = "response", se.fit = TRUE)$se.fit

# Graph the predictions 
p1 <- ggplot(preds, aes(x = rr, y = p)) + 
        geom_line() + 
        geom_ribbon(aes(ymin = p - CI * 1.96, ymax = p + CI * 1.96), alpha = 0.22) +
        geom_line(aes(x = rr, y = p-CI * 1.96), linetype = 2) + 
        geom_line(aes(x = rr, y = p+CI * 1.96), linetype = 2) + 
        geom_point(data = riskratio_data, aes(x = rr, y = pe, size = Abundance), alpha = 0.3) + 
        geom_line() + 
        ylab("p(elevation) m") + 
        xlab("Water inundation sensitivity") +
        theme_classic() +
        theme(legend.position = c(0.2, 0.85))

p1

# Save plot to graphs file 
ggsave(p1, file = './graphs/pelevation_fsen_Abundance.png', 
       width = 4, 
       height = 4)

################################################################################
# bootstrap the data 
preds <- expand.grid(dden = with(riskratio_data, 
                               seq(from = min(dden), 
                                   to = max(dden), 
                                   length = 100)), 
                     rr = mean(riskratio_data$rr))

preds$p <- predict(model, preds, type = "response")
preds$CI <- predict(model, preds, type = "response", se.fit = TRUE)$se.fit
colnames(preds)

# Graph the predictions 
p2 <- ggplot(preds, aes(x = dden, y = p)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = p - CI * 1.96, ymax = p + CI * 1.96), alpha = 0.22) +
  geom_line(aes(x = dden, y = p-CI * 1.96), linetype = 2) + 
  geom_line(aes(x = dden, y = p+CI * 1.96), linetype = 2) + 
  geom_point(data = riskratio_data, aes(x = dden, y = pe, size = Abundance), alpha = 0.3) + 
  geom_line() + 
  ylab("p(elevation) m") + 
  xlab("Wood density") +
  theme_classic() +
  theme(legend.position = c(0.2, 0.85))

p2


# Save plot to graphs file 
ggsave(p2, file = './graphs/pelevation_dden_Abundance.png', 
       width = 4, 
       height = 4)



