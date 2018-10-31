# modelling the change in wood density as a function of the carbon change 

rm(list=ls())
require(lmerTest)
require(lme4)
require(MuMIn)
require(remef)
require(ggplot2)


#Creating the variable from the other data I have and then making it
data <- read.table("./analysis/nursery_experiment_inundation/data/biomass_data.txt",header =T)
Ndata <- read.table("./analysis/nursery_experiment_inundation/data/carbon_intake_data.txt",header =T)
str(Ndata)
str(data)

###
Ndata$index <- with(Ndata, paste(sp, mother, block, treat, sep = ""))
data$index <- with(data, paste(sp, mother, block, treat, sep = ""))
newdata <- merge(Ndata, data, by = 'index')
###

str(newdata)

leafArea <- newdata$lBio.y/(newdata$slm.y*10000) # in meters of leaf area per plant  
TotalBio <- apply(data.frame(newdata$lBio.y,  newdata$rBio, newdata$sBio), 1, sum)# mg of biomass per plant 
Co2Rate <- newdata$A.y # rate of co2 intake per s-1 per m-2
newdata$carbon.per.sec2 <- leafArea*Co2Rate*1000 #nmol CO2 S-1
newdata$carbon.fraction2 <- newdata$carbon.per.sec2/(TotalBio) # nmol CO2 S-1 mg biomass-1



#Creating variables leaf: total biomass and root: stem biomass ratio 
newdata$leafTtotal <- newdata$lBio.y/TotalBio
newdata$rootTstem <- newdata$rBio/newdata$sBio


#Modelling the data. use step to speed the process, but essentially using dredge model AIC selection. Models selected with a AIC - 6 per variable 
mod1 <- lmer(Dden.y ~ (carbon.fraction2 + diameter.y + rgr.y + leafTtotal + rootTstem + slm.y + si.y + treat.y)^2 + (1|block.y) + (1|sp.y/mother.y), newdata)
st_mod1 <- step(mod1)
summary(mod1)
NAdata <- data[which(!is.na(data$Dden)),]
mod2 <- lmer(Dden ~ carbon.fraction2 + diameter + rootTstem + slm + si + diameter:rootTstem + slm:si + (1|block) + (1|sp/mother), NAdata, na.action = "na.pass")
summary(mod2)


#### think about these tomorrow (shall they be included)?
pred <- expand.grid(si = seq(min(data$si), max(data$si), length = 100), 
                    slm = seq(min(data$slm), max(data$slm), length = 100),
                    carbon.fraction2 = mean(data$carbon.fraction2),
                    diameter = mean(data$diameter),
                    rootTstem = mean(data$rootTstem))

pred$Dden <- predict(mod2, pred, type = "response", re.form = NA)
ggplot(pred,aes(x=si,y=slm,fill = Dden)) + geom_raster()


pred2 <- expand.grid(si = mean(data$si), 
                     slm = mean(data$slm),
                     carbon.fraction2 = mean(data$carbon.fraction2),
                     diameter = seq(min(data$diameter), max(data$diameter), length = 100),
                     rootTstem = seq(min(data$rootTstem), max(data$rootTstem), length = 100))
pred2$Dden <- predict(mod2, pred2, type = "response", re.form = NA)
ggplot(pred2,aes(x=diameter,y=rootTstem,fill = Dden)) + geom_raster()




pred$Dden <- predict(mod2, pred2, type = "response", re.form = NA)
ggplot(pred,aes(x=si,y=slm,fill = Dden)) + geom_raster()

d1 <- dredge(mod2, rank = "AIC")
head(d1)


#The final model 
mod2 <- lmer(Dden.y ~ carbon.fraction2 + diameter.y * sp.y + (1|block.y) + (1|mother.y), newdata)
summary(mod2)
source('./functions/booter.R')

CI_coef <- booter(mod2, data = newdata, ceof = TRUE, n = 10)
CI_coef

require(remef)

partial_data <- newdata[!is.na(newdata$Dden.y) & !is.na(newdata$carbon.fraction2) & !is.na(newdata$diameter.y),]
partial_data$y_partial <- remef(mod2, fix = c("diameter.y",
                                              "sp.ypmal", 
                                              "sp.yptom", 
                                              "sp.ysbec", 
                                              "sp.ysjoh", 
                                              "sp.yslep",
                                              "sp.yspar",
                                              "sp.yssem",
                                              "sp.yssmi",
                                              "sp.ysxan"), ran = "all", 
                                grouping = TRUE, keep.intercept = FALSE)

pred <- expand.grid(carbon.fraction2 = seq(min(data$carbon.fraction2, na.rm = T),
                                             max(data$carbon.fraction2, na.rm = T),
                                             length=100), 
                    sp.y = "dry",
                    diameter.y = mean(newdata$diameter.y, na.rm = T))

pred$Dden <- predict(mod2, pred, type = "response", re.form = NA)
CI <- booter(mod2, data = newdata, preds = pred, n = 5000, MEM = TRUE)
#write.table(CI, file = './analysis/nursery_experiment_inundation/data/carbon_intake_delta_wooddensity_CI')
read.table('./analysis/nursery_experiment_inundation/data/carbon_intake_delta_wooddensity_CI', header = TRUE)
pred$CI025 <- as.numeric(CI[1,])
pred$CI975 <- as.numeric(CI[2,])

diff_partial <- pred$Dden[1]
source('./colors.R')
# creating graphs 
p1 <- ggplot(pred,aes(x=carbon.fraction2,y=Dden)) +
  geom_point(data=partial_data, aes(x = carbon.fraction2, y = y_partial + diff_partial), alpha = 0.3) +
  # geom_point(data=newdata, aes(x = carbon.fraction2, y = Dden), alpha = 0.3, color = cols[4]) +
  geom_ribbon(aes(ymin = CI025, ymax = CI975), alpha = 0.2) + 
  geom_line() + 
  theme_classic() + 
  geom_hline(yintercept = 0, color = cols[5], linetype = 2) +
  ylab(expression(delta~'wood density'~g~cm^-3)) + 
  xlab("Carbon intake per biomass") +
  ylim(-0.25, 0.40) +
  theme(text = element_text(size = 20))

p1

ggsave(p1, file = './analysis/nursery_experiment_inundation/graph_code/graphs/delat_wooddensity_VS_carbon_intake_sp.png', 
       width = 6, 
       height = 6)
