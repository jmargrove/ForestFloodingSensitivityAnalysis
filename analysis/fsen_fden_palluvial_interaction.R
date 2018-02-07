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

# Calculating the abundance 
abn <- with(spdata[spdata$sp %in% riskratio_data$sp,], tapply(sp, sp, length))
riskratio_data$Abundance <- abn[!is.na(abn)]


dden_data <- dden_data[order(dden_data$sp), ]
riskratio_data$dden <- dden_data$dden_adult

riskratio_data$fden <- cut(riskratio_data$dden, 
                           breaks = c(min(riskratio_data$dden)-.1, 
                                      mean(riskratio_data$dden), 
                                      max(riskratio_data$dden)), 
                           labels = c("low","high"))



spde_preds <- read.table("./data/INLA_SPDE_predictions.txt", header = TRUE)

str(spde_preds)


sortout <- function(x){
  x/max(x)
}

spde_preds$pS <- unlist(with(spde_preds, tapply(p, sp, function(x) sortout(x))))

 
riskratio_data$pall <- spde_preds[which(spde_preds$elev == 0), ]$pS


ggplot(riskratio_data, aes(x = pall, y = rr)) + geom_point()
model <- (lm(rr ~ pall * fden, riskratio_data))

summary(model)


##### bootstrap the data 
preds <- expand.grid(pall = with(riskratio_data, 
                               seq(from = min(pall), 
                                   to = max(pall), 
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
p1 <- ggplot(preds, aes(x = pall, y = p, fill = fden)) + 
  geom_ribbon(aes(ymin = p - CI * 1.96, ymax = p + CI * 1.96), alpha = 0.5) +
  geom_point(data = riskratio_data, aes(x = pall, y = rr, color = fden)) + 
  ylab("flooding sensitivity") + 
  geom_line(aes(color = fden), size = 1) + 
  geom_line(size = 0.33) + 
  geom_line(data = preds, aes(y = p - CI * 1.96, color = fden), linetype = 2, alpha = 0.222) + 
  geom_line(data = preds, aes(y = p + CI * 1.96, color = fden), linetype = 2, alpha = 0.222) + 
  xlab("p(alluvial)") +
  theme_classic() +
  theme(legend.position = c(0.8, 0.85)) +
  scale_fill_manual(values = c(col1, col2)) +
  scale_color_manual(values = c(col1, col2))

p1

Save plot to graphs file 
ggsave(p1, file = './graphs/fsen_fden_pall_interaction.png', 
       width = 4, 
       height = 4)


