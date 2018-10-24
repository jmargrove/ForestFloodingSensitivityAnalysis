################################################################################
# title: Calculating the Risk ratios for the experiments
# author: James Margrove

# Clear work space
rm(list = ls())

# Imports
#load("./models/Model.Rdata")
load('./analysis/seedling_mortality_analysis/models/seedling_mortality_model.R')
data <- read.table("./data/data.txt", header = TRUE)

# Prediction data frame
pred1 <- expand.grid(
    sp = levels(data$sp),
    flood = c("dry", "wet"),
    ztopo = 0,
    dia = mean(data[data$f.time == 3, ]$dia, na.rm = TRUE),
    f.time = "3",
    blockL = 0,
    wl = 0
)

# Predict from model
pred1$p <- predict(r3, pred1, type = "response", re.form = ~0)
pred1$lo <- predict(r3, pred1, re.form = ~0)
# Calculate the risk ratio per species
rr <- with(pred1, tapply(p, sp, diff))

# Create data frame
riskratio_data <- data.frame(sp = levels(data$sp), rr = rr)

# Write table to data folder
write.table(riskratio_data, file = "./analysis/seedling_mortality_analysis/data/riskratio_data.txt")
