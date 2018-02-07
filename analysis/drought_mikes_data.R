################################################################################
# title: drought sensitivity response predicting species distributions 
# author: James Margrove 
# desc: analysis using the data from MOB's data on drought, paper publsihed in 
# global change. Nature climate change data do not coinside enough - low 
# sample size. 
# EXPLORATION 

rm(list=ls())

# import 
require(ggplot2)
require(simex)

gcb_data <- read.table("./data/GCB2017.txt", header = TRUE)
ncc_data <- read.table("./data/NCC2017.txt", header = TRUE)
rr_data <- read.table("./data/riskratio_data.txt", header = TRUE)

spdata <- read.table("./data/spdata.txt", header = TRUE)
rr_data$pe <- read.table("./data/pelev_data.txt", header = TRUE)$pe
rr_data$dden <- read.table("./data/dden_adult.txt", header = TRUE)$dden

spdata <- read.table("./data/spdata.txt", header = TRUE)
rr_data$pe <- read.table("./data/pelev_data.txt", header = TRUE)$pe

### sd data for the rr 
int.boot <- read.table("./data/FLoodIntSp.txt", header = TRUE)
RiskDiff <- function(a){a[2]-a[1]}
bootRD <- apply(int.boot, 2, function(x){tapply(x, rep(1:16,2), RiskDiff)})
rr_data$SD <- apply(bootRD, 1, sd)


##### Calculating the abundance 
abn <- with(spdata[spdata$sp %in% rr_data$sp,], tapply(sp, sp, length))
rr_data$Abundance <- abn[!is.na(abn)]


spn <- c("dry", "doxl", "Dzib", "Hfer", "Hner", "Hsan", "Kexc", "Pmal", "Ptom", "Sarg", 
  "Sbec", "SfalX", "Sfag", "Sgiba", "SmacX", "Smac", "Smec", "Sova", "Spar", "Spau")
gcb_data <- gcb_data[order(gcb_data$spp),]
gcb_data$sp <- spn


dt <- merge(rr_data, gcb_data, by=c("sp"))
dt
##### plot the data 
ggplot(dt, aes(x = rr, y = resist)) + 
  geom_text(aes(label = sp)) + 
  stat_smooth(method =lm) + 
  xlab("flooding sensitivity") + 
  ylab("resistance (MOB paper in CGB") 


##### correlation of the variables 
with(dt, cor(resist, rr))
with(dt, cor(resist, pe))


m <- lm(resist ~ rr, dt)
summary(m)


m2 <- lm(pe ~ resist, dt)
summary(m2)

##### using the SIMEX
m3 <- lm(resist ~ rr, dt)
summary(m3)
si1 <- simex(model = m3, 
             measurement.error = dt$SD, 
             SIMEXvariable = "rr", 
             fitting.method = "quad", 
             asymptotic = FALSE, 
             B = 5000)

summary(si1)



##### looking at the days to death data for the other analysis 
ncc_data$sp <- c("Smaxly", "Doxl", "Spar", "Ptom", "Dry", "Kexc",  "Sbec", "Pmal", "Hner", "Sarg")

ndt <- merge(ncc_data, rr_data, by=c("sp"))
dim(ndt)

cor.test(ndt$rr, ndt$day)
summary(lm(day ~ pe, weight = Abundance, ndt))

# not enough species to run analysis 
