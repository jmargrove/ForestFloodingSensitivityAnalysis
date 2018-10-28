################################################################################
# title: flooding sensitivity response predicting species distributions 
# author: James Margrove 

# Clear work space 
rm(list = ls())

# Import functions and packages
require(ggplot2)
require(car)
source("./functions/aovPerVar.R")

# Import data 
riskratio_data <- read.table("./data/riskratio_data.txt", header = TRUE)
spdata <- read.table("./data/spdata.txt", header = TRUE)
riskratio_data$pe <- read.table("./data/pelev_data.txt", header = TRUE)$pe
dden_data <- read.table("./data/dden_adult.txt", header = TRUE)

dden_data <- dden_data[order(dden_data$sp), ]
riskratio_data$dden <- dden_data$dden_adult

# Calculating the abundance 
abn <- with(spdata[spdata$sp %in% riskratio_data$sp,], tapply(sp, sp, length))
riskratio_data$Abundance <- abn[!is.na(abn)]

riskratio_data$fden <- cut(riskratio_data$dden, 
                           breaks = c(min(riskratio_data$dden)-.1, 
                                      mean(riskratio_data$dden), 
                                      max(riskratio_data$dden)), 
                           labels = c("low","high"))

# Explore 
ggplot(riskratio_data, aes(x = rr, y = pe)) + 
  geom_point() + 
  stat_smooth(method = lm)


ggplot(riskratio_data, aes(x = rr, y = dden)) + 
  geom_point() + 
  stat_smooth(method = lm)

# Model data with a weighted linear model 
model <- lm(rr ~ fden * pe,  riskratio_data)
summary(model)


# Check colinearity
vif(model)

# Anova test
ma <- Anova(model)

# Anova percentage variation 
aovPerVar(ma)

# model evaluation 
par(mfrow=c(2,2))
plot(model)

##### bootstrap the data 
preds <- expand.grid(pe = with(riskratio_data, 
                               seq(from = min(pe), 
                                   to = max(pe), 
                                   length = 100)),
                     fden = c("low" , "high"))

preds$p <- predict(model, preds, type = "response")
preds$CI <- predict(object = model, 
                    newdata = preds, 
                    type = "response", 
                    interval = "confidence",
                    level = 0.95,
                    se.fit = TRUE)$se.fit

# Colors
col1 <- "#C5283D"
col2 <-  "#255F85"

# Graph the predictions 
p1 <- ggplot(preds, aes(x = pe, y = p, fill = fden)) + 
  geom_ribbon(aes(ymin = p - CI * 1.96, ymax = p + CI * 1.96), alpha = 0.5) +
  geom_point(data = riskratio_data, aes(x = pe, y = rr, color = fden)) + 
  ylab("flooding sensitivity") + 
  geom_line(aes(color = fden), size = 1) + 
  geom_line(size = 0.33) + 
  geom_line(data = preds, aes(y = p - CI * 1.96, color = fden), linetype = 2, alpha = 0.222) + 
  geom_line(data = preds, aes(y = p + CI * 1.96, color = fden), linetype = 2, alpha = 0.222) + 
  xlab("pelevation (m)") +
  theme_classic() +
  theme(legend.position = c(0.2, 0.85)) +
  scale_fill_manual(values = c(col1, col2)) +
  scale_color_manual(values = c(col1, col2))

p1

# Save plot to graphs file 
ggsave(p1, file = './graphs/fsen_fden_pe1_interaction.png', 
       width = 4, 
       height = 4)



