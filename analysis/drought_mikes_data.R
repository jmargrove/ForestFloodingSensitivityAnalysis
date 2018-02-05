################################################################################
# title: drought sensitivity response predicting species distributions 
# author: James Margrove 
# desc: analysis using the data from MOB's data on drought

rm(list=ls())

gcb_data <- read.table("./data/GCB2017.txt", header = TRUE)
ncc_data <- read.table("./data/NCC2017.txt", header = TRUE)
rr_data <- read.table("./data/riskratio_data.txt", header = TRUE)

spdata <- read.table("./data/spdata.txt", header = TRUE)
rr_data$pe <- read.table("./data/pelev_data.txt", header = TRUE)$pe
rr_data$dden <- read.table("./data/dden_adult.txt", header = TRUE)$dden

spdata <- read.table("./data/spdata.txt", header = TRUE)
riskratio_data$pe <- read.table("./data/pelev_data.txt", header = TRUE)$pe

##### Calculating the abundance 
abn <- with(spdata[spdata$sp %in% rr_data$sp,], tapply(sp, sp, length))
rr_data$Abundance <- abn[!is.na(abn)]


spn <- c("dry", "doxl", "Dzib", "Hfer", "Hner", "Hsan", "Kexc", "Pmal", "Ptom", "Sarg", 
  "Sbec", "SfalX", "Sfag", "Sgib", "Smadasfdc", "Smac", "Smec", "Sova", "Spar", "Spau")
gcb_data <- gcb_data[order(gcb_data$spp),]
gcb_data$sp <- spn


##### plot the data 
ggplot(dt, aes(x = rr, y = resist)) + geom_text(aes(label = sp))
ggplot(dt, aes(x = pe, y = resist)) + geom_text(aes(label = sp))


dt <- merge(rr_data, gcb_data, by=c("sp"))

##### correlation of the variables 
with(dt, cor(resist, rr))
with(dt, cor(resist, pe))


m <- lm(rr ~ resist, dt)
summary(m)

m2 <- lm(pe ~ resist, dt)
summary(m2)

##### looking at the days to death data for the other analysis 
ncc_data$sp <- c("Smaxly", "Doxl", "Spar", "Ptom", "Dry", "Kexc",  "Sbec", "Pmal", "Hner", "Sarg")

ndt <- merge(ncc_data, rr_data, by=c("sp"))
dim(ndt)
# not enough individuals at all here ffs
