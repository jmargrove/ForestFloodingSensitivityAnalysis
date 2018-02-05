

### Wood density numbers
GlobalWD <- read.csv("~/Google Drive/Projects_Collaberations/INDFORSUS Plots/Data/Analysis 2016/GlobalWoodDensityDatabase.csv")
head(GlobalWD)

GlobalWD_sea<-subset(GlobalWD, Region=="South.East.Asia.tropical")
head(GlobalWD_sea)

WD_SEA_Genus <- with(GlobalWD_sea, aggregate(WoodDensity, by=list(Genus), function(x) mean(x)))
colnames(WD_SEA_Genus)<-c("Genus", "WD_Genus_Mean")
head(WD_SEA_Genus)

WD_SEA_Species <- with(GlobalWD_sea, aggregate(WoodDensity, by=list(Full.names), function(x) mean(x)))
colnames(WD_SEA_Species)<-c("Full.names", "WD_Species_Mean")
head(WD_SEA_Species)

Pele <- read.csv("~/Google Drive/Papers In Prep/James_Field_Study/Distributions33Species.csv")
head(Pele)
Pele$Full.names<-with(Pele, paste(Genus, Species, sep="."))

PeleWD <- merge(Pele, WD_SEA_Species, by=c("Full.names"), all.x=T)
head(PeleWD)
str(PeleWD)
PeleWD$WD_Species_Mean[12]<-0.31
PeleWD



with(PeleWD, plot(pele ~ WD_Species_Mean))

m1 <- with(PeleWD, lm(pele ~ WD_Species_Mean +I( WD_Species_Mean^2)+I( WD_Species_Mean^3)))
anova(m1)

# with(PeleWD, plot(WD_Species_Mean ~ pele))
#### 

## previous - now updated
# 1. take mean wood density of surrounding trees genus average
# Plot level mean biomass
#PlotWd <- with(IND4WD, aggregate(WD_Genus_Mean, by=list(Plot), function(x) mean(x, na.rm=T))) 
#colnames(PlotWd)<-c("Plot", "Pl_WD")
#PlotWd
#head(IND4WD)


### update with code from BIOMASS package
