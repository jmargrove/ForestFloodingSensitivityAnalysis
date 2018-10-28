#' @title wood density sapling adult correlation
#' @author James Margrove

# set working directory
setwd("./analysis/wood_density_sapling_adult_correlation")
# import data
source("./data/index.R")

head(dden_adult_new)
head(dden_sapling)

# modelling the wood density values 
model <- lm(bottom_density ~ Sp + Dia, dden_sapling)
summary(model)

dden <- predict(model, newdata = data.frame(Sp = levels(dden_sapling$Sp), Dia = with(dden_sapling, tapply(Dia, Sp, mean))), type = "response")

# explore the data
data <- data.frame(sp = levels(dden_sapling$Sp), dden_adult = dden_adult_new$dden_adult, dden_sap = dden)
data$pe <- read.table("./data/pelev_data.txt", header = TRUE)$pe
data$rr <- read.table("./data/riskratio_data.txt", header = TRUE)$rr
spdata <- read.table("./data/spdata.txt", header = TRUE)
traits <-  read.table("./data/traits.txt", header = T)


# Calculating the abundance 
abn <- with(spdata[spdata$sp %in% data$sp,], tapply(sp, sp, length))
data$abn<- abn[!is.na(abn)]

summary(lm(pe ~ rr + dden_sap, data = data))
#summary(lm(I(bet*rr) ~ pe *  dden_sap, data = data))

data$bet <- c(0,1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0)

summary(lm(pe ~ rr + dden_sap, data))

ggplot(data, aes(x = pe, y = rr)) + geom_point() + geom_label(aes(label = sp))
ggplot(data, aes(y = dden_adult, x = dden_sap)) + geom_point() + geom_label(aes(label = sp))


with(data, plot(dden_adult, dden_sap))

summary(lm(dden_sap$dden ~ dden_adult_new$dden_adult))


require(ggplot2)


# looking at the labels 
ggplot(data, aes(y = dden_adult, x = dden_sap)) + geom_point() + 
  geom_label(aes(label = sp))

summary(lm(dden_adult ~ dden_sap, data))

# biggest outliers are, sgib, sacu, spau, pmal and smec. 
setwd("./")
getwd()

source("models/pele_fsen_dden_Abundance.R")

traits$sp <- tolower(traits$sp)

# traits and new dden data 
merge_data <- merge(traits, data, by = 'sp')
head(merge_data)

ggplot(merge_data, aes(x = dden_adult, y = dden)) + geom_point() + geom_label(aes(label = sp))
ggplot(merge_data, aes(x = dden_sap, y = dden)) + geom_point() + geom_label(aes(label = sp))
ggplot(merge_data, aes(x = dden_sap, y = dden_adult)) + geom_point() + geom_label(aes(label = sp))
summary(lm(dden_adult ~ dden, merge_data))
summary(lm(dden ~ dden_sap, merge_data))
summary(lm(dden_adult ~ dden_sap, merge_data))

# So the seedlings > saplings > adults 
# 0.20, 0.5, 0.625...
# therefore seedling values are converging on the adults 



