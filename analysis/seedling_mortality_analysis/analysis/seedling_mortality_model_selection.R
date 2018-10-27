#' @title Seedling Mortality Model Selection and Evaluation
#' @author "James Margrove"
#' @date "15 Juli 2016"

# Clear workspace 
rm(list = ls())

# import route 
source('./analysis/seedling_mortality_analysis/route.R')
# import packages 
source(paste(route, "organise/organise.R", sep = ""))
# Import functions 
source('./functions/dAIC.R')
source('./functions/stand.R')
# Import packages 
source(paste(route, "packages.R", sep = ""))
#Modeling
#Standardize all the variables for analysis: ztopo, light, sand, 
seedling_mortality_data$Sztopo <- stand(data = seedling_mortality_data, var = 'ztopo')
seedling_mortality_data$Slight <- stand(data = seedling_mortality_data, var = 'light')
seedling_mortality_data$Ssand <- stand(data = seedling_mortality_data, var = 'sand')

# Random effects
#r1
#Random effects are mother nested within sp, and plot (wl) nested within block. 
#The fixed effects specified are those that we a) hypothesied to have an effect, 
#and from data exploration seed to have an effect.

r1 <- glmer(mortality ~ log(dia) + Sztopo +  f.time + flood + sp + sp:flood +
              (1|mother) + (f.time|blockL:wl),
            family = binomial, data = seedling_mortality_data, control=glmerControl(optimizer="nlminbw"))

#r2
#remove mother as a random effect. 
r2 <- update(r1, .~. - (1|mother))
dAIC(r1,r2)
AIC(r1,r2)

#Using mother as a added effect does not help. 
#r3
#remove the nexted effect of blockL/wl and replace with the plot effect 
r3 <- glmer(mortality ~ log(dia) + Sztopo +  f.time + flood + sp + sp:flood + (f.time|blockL:wl),
            family = binomial, data = seedling_mortality_data, control=glmerControl(optimizer="nlminbw"))
dAIC(r2,r3)
AIC(r2,r3)

#Block is not needed. R3 is the best 
#Fixed Effects Structure
#delta AIC must be more than 4AIC points for us to consider the more coomplex model better. 

m1 <- update(r3, .~. - log(dia) + dia)
dAIC(r3,m1)
AIC(r3,m1)
#NOTE:r3 is the best model still 

#Does the Are there any extra interactions with Diameter 
m2 <- update(r3, .~. + log(dia):f.time)
m3 <- update(r3, .~. + log(dia):flood)
m4 <- update(r3, .~. + log(dia):Sztopo)
m5 <- update(r3, .~. + log(dia):sp)
dAIC(m2,r3)
AIC(r3,m2,m3,m4,m5)

#NOTE:Reduces the model only by 2.07, hence stick with the original 
m6 <- update(m1, .~. + dia:f.time)
m7 <- update(m1, .~. + dia:flood)
m8 <- update(m1, .~. + dia:Sztopo)
m9 <- update(m1, .~. + dia:sp)
AIC(r3,m1,m6,m7,m8,m9)

summary(m3)
dAIC(m3,r3)
AIC(m3,r3)

# AIC
summary(m3)
dAIC(m3,r3)
AIC(m3,r3)

#Light
m4 <- update(r3, .~. + Slight)
m5 <- update(r3, .~. + Slight + Slight:Sztopo)
m6 <- update(r3, .~. + Slight + Slight:flood)
AIC(r3,m3,m4,m5)

#Sand
m7 <- update(r3, .~. + Ssand)
m8 <- update(r3, .~. + Ssand:Sztopo)
m9 <- update(r3, .~. + Ssand:flood)
AIC(r3,m7,m8,m9)

#Site
m10 <- update(r3, .~. + site)
m11 <- update(r3, .~. + site:Sztopo)
m12 <- update(r3, .~. + site:flood)
AIC(r3,m10,m11,m12)

# Other remaining model mixtures  
m13 <- update(r3, .~. - flood:sp) # flooding should be kept in the model. 
AIC(m13,r3)
dAIC(m13,r3)# rounded it is dAIC=5, keep this in 

require(MuMIn)
NARM_seedling_mortality_data <- seedling_mortality_data[!is.na(seedling_mortality_data$dia),] # removes rows with NAs in dia 
NARM_seedling_mortality_data <- NARM_seedling_mortality_data[!is.na(NARM_seedling_mortality_data$mortality),] # removes rows with NAs in dia 
r4 <- update(r3,.~. ,data=NARM_seedling_mortality_data, na.action="na.fail")
dr4 <- dredge(r4, rank="AIC", trace=TRUE)# dregde the rest to see what happens
dr4

r3 <- glmer(mortality ~ log(dia) + ztopo +  f.time + flood + sp + sp:flood + (f.time |blockL:wl),
            family = binomial, 
            data = seedling_mortality_data)

summary(r3)
save(r3, file = './analysis/seedling_mortality_analysis/models/seedling_mortality_model.R')

<<<<<<< HEAD
# testing the effect of inundation interaction with species 
rm_flood_species_int <- update(r3, . ~ . -sp:flood)
anova(rm_flood_species_int, r3)

=======
dAIC(rm_flood_species_int, r3)

rm_flood_species_int <- update(r3, . ~ . -sp:flood)
anova(rm_flood_species_int, r3)

load("./models/Model.RData")

s3
dAIC(s3, r3)
>>>>>>> eddbaf3409778575f50f1e19684046df57ec3a8e
