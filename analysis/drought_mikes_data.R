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

spdata <- read.table("./data/forestplot_160_spatial_data.txt", header = TRUE)
rr_data$pe <- read.table("./data/pelev_data.txt", header = TRUE)$pe
dden_data <- read.table("./data/dden_adult.txt", header = TRUE)
rr_data$dden <- dden_data[order(dden_data$sp),]$dden_adult
rr_data
rr_data$pe <- read.table("./data/pelev_data.txt", header = TRUE)$pe


##### Calculating the abundance 
abn <- with(spdata[spdata$sp %in% rr_data$sp,], tapply(sp, sp, length))
rr_data$Abundance <- abn[!is.na(abn)]

spn <- c("Dry", "doxl", "Dzib", "Hfer", "Hner", "Hsan", "Kexc", "Pmal", "Ptom", "Sarg", 
  "Sbec", "SfalX", "Sfag", "Sgib", "SmacX", "Smac", "Smec", "Sova", "Spar", "Spau")
gcb_data <- gcb_data[order(gcb_data$spp),]
gcb_data$sp <- spn


dt <- merge(rr_data, gcb_data, by=c("sp"))
dt

m <- (lm(rr ~  pe + dden + log(resist), dt))
summary(m)
anova(m)

with(dt, plot(resist, pe))

ggplot(dt, aes(x = rr, y = resist)) + geom_point() + geom_text(aes(label = sp))
summary(lm(resist ~ rr, dt))

require(vegan)
mat <- as.matrix(cbind(d = dt$resist, w = dt$rr))

mdm <- metaMDS(mat, )
mdm

p <- prcomp(mat)
summary(p)

p[1,]
str(p)
?metaMDS

?prcomp

biplot(p)
?ndm

as.data.frame(p$rotation[,1:2])


m <- prcomp(~ resist * rr, data = dt, scale = TRUE)
summary(m)
axes <- predict(m, dt)
axes

plot(axes[,2], dt$el)

m = (lm(pe ~ rr + dden, dt, weights = Abundance))
summary(m)
anova(m)

pred = data.frame()
predict(m, )

#### spdata 
spdt <- data.frame(sp = levels(spdata$sp), 
                   elev = with(spdata, tapply(elev, sp, median)), 
                   a = with(spdata, tapply(sp, sp, length)),
                   dden = with(spdata, tapply(dden, sp, median)))
head(spdt)

dt <- merge(spdt, gcb_data, by=c("sp"))
dt

### 
load("./models/pele_fsen_dden_Abundance")
summary(model)

coef <- summary(model)$coef
alpha = coef[1]
betaRR <- coef[2]
betaDD <- coef[3]

equation <- function(el, dden) {
  (el + (-1 * alpha) + (-1 * betaDD * dden))/(betaRR)
}

dt$rr <- equation(dt$elev, dt$dden)
dt

ggplot(dt, aes(x= resist, y =rr)) + geom_point() + geom_text(aes(label = sp))

dt

rr_data

summary(lm(resist ~ rr,  weights = a, data = dt))


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
