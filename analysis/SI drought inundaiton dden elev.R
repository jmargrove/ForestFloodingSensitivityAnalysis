### looking again at mikeys stuff 

rm(list=ls())
require(ggplot2)
load("./models/pele_fsen_dden_Abundance")
summary(model)
dt <- read.table("./data/Resistance.txt", header = TRUE)
dt$resistance <- dt$resistance*-1

sp_names_dt <- data.frame(
  Species = c(
    "Shorea_argentifolia", "Shorea_macrophylla", "Durio_oxleyanus", "Koompassia_excelsa", 
    "Shorea_parvifolia", "Shorea_fallax", "Parashorea_tomentella", "Shorea_mercistopteryx",
    "Durio_zibethinus", "Shorea_beccariana", "Dryobalanops_lanceolata", "Shorea_pauciflora", 
    "Hopea_nervosa", "Hopea_ferruginea", "Hopea_sangal", "Parashorea_malaanonan", 
    "Shorea_faguetiana", "Shorea_gibbosa", "Shorea_ovalis", "Shorea_macroptera"), 
  spp = c("SA", "SM1", "DO", "KE", "SP", "SF", "PT", "SM3", "DZ", "SB", "DL", "SPC", "HN",
          "HF", "HS", "PM", "SFG", "SG", "SO", "SM2")
)

R <- data.frame(spp = c("DO", "SFG", "KE", "SP", 
                        "SM1", "SF", "PM", "PT", 
                        "SM2", "HF", "SPC", "SM3", 
                        "SA", "HS", "HN", "SG", 
                        "DL", "DZ", "SB", "SO"), 
                rank = 20:1)

# 
sp_names_dt <- merge(sp_names_dt, R, by = "spp")

data <- merge(dt, sp_names_dt, by = "spp")




# adding in species acrynomis 
data$sp <- paste(substring(data$Species, 1, 1), 
                 substring(data$Species, 
                           regexpr("_", as.character(data$Species)) + 1, 
                           regexpr("_", as.character(data$Species)) + 3), 
                 sep = "")

data[1, "sp"] <- "Dry"
str(data)

# now we need reubens data and the 160 ha forest plot 
spatial_data <- read.table("./data/forestplot_160_spatial_data_UPDATE.txt", header = TRUE)
reu_data <- read.table("./data/reu_160plot_combinded_data.txt", header = TRUE)

plot <- data.frame(elev = c(spatial_data$elev, 
                            reu_data$elev), 
                   dden = c(spatial_data$dden, 
                            reu_data$dden),
                   spp = factor(c(as.character(spatial_data$fullname), as.character(reu_data$Species))))

preds_data <- data.frame(elev = with(plot, tapply(elev, spp, mean)), 
                         dden = with(plot, tapply(dden, spp, mean)), 
                         a = with(plot, tapply(dden, spp, length)),
                         Species = levels(plot$spp))

coefs <- coef(model)
rrFun <- function(elev, dden){
  ((elev + (coefs[1]*-1) + (dden*coefs[3]*-1)))/coefs[2]
}

preds_data$rr <- rrFun(elev = as.numeric(preds_data$elev), dden = as.numeric(preds_data$dden))                    
dt <- merge(preds_data, data, by = "Species")
dt <- droplevels(dt)

ggplot(dt, aes(x = rr, y = dd)) + geom_point()

# So the hypothesis goes, in that we are controlling for an above ground limit, 
# then we should expect that there is a negative relationship with elevation. 
# and there is a negative realtionship between wood density and elevation 
dt <- dt[order(dt$rank),]
dt$dip_rank <- 1:11

write.table(data, file = "./data/resistance_drought.txt")

ggplot(dt, aes(y = elev, x = resistance)) + geom_point()

model2 <- lm(elev ~ resistance + dden, dt)
summary(model2)

residData <- as.data.frame(residuals(model2, type="partial"))

model2$residuals
?residuals
# ok 
anova(model2)
car::vif(model2)
plot(model2, which = 1)

### univariate analysis 
uniVar1 <- lm(elev ~ resistance, dt)
summary(uniVar1)
uniVar2 <- lm(elev ~ dden, dt)
summary(uniVar2)





summary(model2)
anova(model2)
source("./functions/booter.R")




save(model2, file = "./models/elev_resistance_dden.R")
source("./functions/aovPerVar.R")
aov_percent <- aovPerVar(model2)

preds <- data.frame(resistance = seq(from = min(dt$resistance), 
                                     to = max(dt$resistance), 
                                     length = 100), 
                    dden = mean(dt$dden))

preds$elev <- predict(model2, preds, type = "response")
preds$CI <- (predict(model2, preds, type = "response", se.fit = T)$se.fit *1.96)

p1 <- ggplot(data = preds, aes(x = resistance, y = elev)) + 
  geom_ribbon(aes(ymin = elev - CI, ymax = elev + CI), alpha = 0.2) + 
  geom_line() + 
  geom_point(data = dt, aes(x = resistance, y = elev)) + 
  theme_classic() + 
  ylab("Elevation (m asl)") +
  xlab("Drought sensitivity") + 
  geom_text(aes(x = -0.00001, y = 100, 
                label = paste("ANOVA: ", round(aov_percent[1], 1), "%", sep = "")), size = 3) 

p1




ggsave(p1, file = './graphs/pelev_dip_rank_dden.png', 
       width = 4, 
       height = 4)


##
preds <- data.frame(resistance = mean(dt$resistance), 
                    dden = seq(min(dt$dden), max(dt$dden), length = 100))
preds$elev <- predict(model2, preds, type = "response")
preds$CI <- (predict(model2, preds, type = "response", se.fit = T)$se.fit *1.96)
p2
p2 <- ggplot(data = preds, aes(x = dden, y = elev)) + 
  geom_ribbon(aes(ymin = elev - CI, ymax = elev + CI), alpha = 0.2) + 
  geom_line() + 
  geom_point(data = dt, aes(x = dden, y = residData$dden+mean(dt$elev))) + 
  theme_classic() +
  ylab("Elevation (m asl)") +
  xlab("Wood density g cm-3") +
  geom_text(aes(x = 0.44, y = 100, 
                label = paste("ANOVA: ", round(aov_percent[2], 1), "%", sep = "")), size = 3) 

p2

ggsave(p2, file = './graphs/pelev_dden_Drought.png', 
       width = 4, 
       height = 4)
