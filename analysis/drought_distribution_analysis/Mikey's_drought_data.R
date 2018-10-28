### looking again at mikeys stuff 

rm(list=ls())
require(ggplot2)
load("./models/pele_fsen_dden_Abundance")
summary(model)
dt <- read.table("./data/Resistance.txt", header = TRUE)
dt$resistance <- dt$resistance * -1

 
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
  geom_text(aes(x = -0.00001, y = 100, size = 1,
                label = paste("ANOVA: ", round(aov_percent[1], 1), "%", sep = "")), size = 3) 

p1


### ok new figure with partials and raw points 
model2p <- glm(elev ~ resistance + dden, family = "gaussian", dt)
summary(model2p)
par <- cbind(dt, par_res = resid(model2p, type = "partial")[,1] + mean(dt$elev))




pd <- data.frame(line = rep(dt$resistance, times = 2), 
                 e = c(dt$elev, par$par_res), 
                 d = c(rep(0, dim(dt)[1]), par$par_res - dt$elev))
                 

hj <- rep(-0.15, dim(dt)[1])
vj <- rep(0.5, dim(dt)[1])
vj[which(dt$sp == "Sgib")] <- -0.5
vj[which(dt$sp == "Sarg")] <- -0.5
vj[which(dt$sp == "Pmal")] <- -0.25
hj[which(dt$sp == "Ptom")] <- 1.25
vj[which(dt$sp == "Ptom")] <- -0.25
hj[which(dt$sp == "Spar")] <- 1.25




pp1 <- ggplot(data = preds, aes(x = resistance, y = elev)) + 
  geom_line(data = pd, inherit.aes = F, aes(x = line, y = e, group = factor(line)), color = "grey") + 
  geom_ribbon(aes(ymin = elev - CI, ymax = elev + CI), alpha = 0.2) + 
  geom_line() + 
  geom_point(data = par, aes(x = resistance, y = par_res), size = 3, pch = 21, fill = "grey") + 
  geom_point(data = dt, aes(x = resistance, y = elev), color = "red", pch = 21, fill = "lightgrey") + 
    stat_smooth(data = dt, aes(x = resistance, y = elev), color = "red", method = lm, size = 0.5, alpha = 0.5, linetype = 2, se = F)+ 
  theme_bw() + 
  ylab("Elevation (m asl)") +
  xlab("Drought sensitivity") + 
  geom_text(aes(x = 0.0001, y = 100, 
                label = paste("ANOVA: ", round(aov_percent[1], 1), "%", sep = "")), size = 3) + 
  geom_text(data = dt, aes(x = resistance, y = elev, label = dt$sp),
            size = 3,
            hjust = hj, vjust = vj, fontface = "italic")

pp1

ggsave(pp1, file = './graphs/pelev_dip_rank_dden_partial.png', 
       width = 4, 
       height = 4)



### now for wood density 
### ok new figure with partials and raw points 
preds <- data.frame(resistance = mean(dt$resistance), 
                    dden = seq(min(dt$dden), max(dt$dden), length = 100))
preds$elev <- predict(model2, preds, type = "response")
preds$CI <- (predict(model2, preds, type = "response", se.fit = T)$se.fit *1.96)
model2p <- glm(elev ~ resistance + dden, family = "gaussian", dt)
summary(model2p)
par <- cbind(dt, par_res = resid(model2p, type = "partial")[,2] + mean(dt$elev))




pd <- data.frame(line = rep(dt$dden, times = 2), 
                 e = c(dt$elev, par$par_res), 
                 d = c(rep(0, dim(dt)[1]), par$par_res - dt$elev))


hj <- rep(-0.15, dim(dt)[1])
vj <- rep(0.5, dim(dt)[1])
vj[which(dt$sp == "Sgib")] <- -0.5
vj[which(dt$sp == "Hsan")] <- -0.75
vj[which(dt$sp == "Sarg")] <- -0.5
vj[which(dt$sp == "Pmal")] <- -0.25
hj[which(dt$sp == "Ptom")] <- 1.25
hj[which(dt$sp == "Hsan")] <- 1
vj[which(dt$sp == "Ptom")] <- -0.25
vj[which(dt$sp == "Smac")] <- 1.30
hj[which(dt$sp == "Smac")] <- 0.25
vj[which(dt$sp == "Pmal")] <- 0.5





pp1 <- ggplot(data = preds, aes(x = dden, y = elev)) + 
  geom_line(data = pd, inherit.aes = F, aes(x = line, y = e, group = factor(line)), color = "grey") + 
  geom_ribbon(aes(ymin = elev - CI, ymax = elev + CI), alpha = 0.2) + 
  geom_line() + 
  geom_point(data = par, aes(x = dden, y = par_res), size = 3, pch = 21, fill = "grey") + 
  geom_point(data = dt, aes(x = dden, y = elev), color = "red", pch = 21, fill = "lightgrey") + 
  stat_smooth(data = dt, aes(x = dden, y = elev), color = "red", method = lm, size = 0.5, alpha = 0.5, linetype = 2, se = F)+ 
  theme_bw() + 
  ylab("Elevation (m asl)") +
  xlab(bquote("Wood density g" ~cm^-3 )) + 
  geom_text(aes(x = 0.65, y = 100, 
                label = paste("ANOVA: ", round(aov_percent[2], 1), "%", sep = "")), size = 3) + 
  geom_text(data = dt, aes(x = dden, y = elev, label = dt$sp), 
            size = 3,
            hjust = hj, vjust = vj, fontface = "italic")

pp1

ggsave(pp1, file = './graphs/pelevpartial.png', 
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


## Ok, the same models are good for doing this backwards, now can we try just using teh dip rank and the rr

#### OK but what about, rank and rr - the predicted 
### based on this I can predict the species drought sensitivity, and the species flooding sensitivity.
### this kindof means that i could make predictions about the whole plot? 
# So lets predict the drought sensitivity for all the dips, at equal wood densitites
# because wood density is a component of the distributions, for equal wood densittes what is the drought sensitivitoies

summary(model)
summary(model2)
source("./functions/aovPerVar.R")
aovPerVar(model)
aovPerVar(model2)
plot(model, which = 1)

coefs <- coef(model)
rrFun <- function(elev, dden){
  (((elev + (coefs[1]*-1) + (dden*coefs[3]*-1)))/coefs[2]) 
}

preds_data$rr <- rrFun(dden = as.numeric(preds_data$dden), elev = as.numeric(preds_data$elev))                  

coefs <- coef(model2)
rrFun <- function(elev, dden){
  (((elev) + (coefs[1]*-1) + (dden*coefs[3]*-1)))/coefs[2] 
}

preds_data$dd <- rrFun(dden = as.numeric(preds_data$dden), elev = as.numeric(preds_data$elev))                    

ggplot(preds_data, aes(x = rr, y = dd)) + geom_point() + 
  xlab("Inundation sensitivity") + 
  ylab("Drought tolleracne") + 
  stat_smooth(method = lm, formula = y ~ x)


summary(lm(elev ~ dd, preds_data))



# Ok so according to this, where wood density is accounted for, there is a tradeoff between drought sensitivity and 
# inundation sensitivity. 

# wait, so can I predict elevation with dden, rr.x, and rank. 
rr_data <- read.table("./data/riskratio_data.txt", header = T)
colnames(rr_data)[2] <- "rr_O"
fin_data <- merge(rr_data, dt, by = "sp")
str(fin_data)
fin_data



#### OK there we go
moo1 <- (lm(resistance ~ dden + rr_O, fin_data))

summary(moo1)

moo1p <- (glm(resistance ~ dden + rr_O, family = "gaussian", fin_data))

parMoo <- cbind(fin_data, par_rr = resid(moo1p, type = "partial")[,2] + mean(fin_data$resistance))
summary(moo1)
anova(moo1)

aovPerVar(moo1)
pred_moo <- expand.grid(rr_O = seq(min(fin_data$rr_O), max(fin_data$rr_O), length = 100), 
                        dden = mean(fin_data$dden))

pred_moo$resist <- predict(moo1, pred_moo, type = "response")
pred_moo$CI <- predict(moo1, pred_moo, type = "response", se.fit = T)$se.fit * 1.96

gglines <- data.frame(lines = rep(fin_data$rr_O, times = 2), 
                      end_pts = c(fin_data$resistance, parMoo$par_rr))

hj <- rep(-0.25, 8)
vj <- rep(0, 8)
hj[which(fin_data$sp == "Ptom")] <- 1.25
hj[which(fin_data$sp == "Sbec")] <- 0.5
vj[which(fin_data$sp == "Sbec")] <- -1.5

p3 <- ggplot(pred_moo, aes(x = rr_O, y = resist)) + 
  stat_smooth(data = fin_data, aes(x = rr_O, y = resistance), method = lm, size = 0.5, color = "red", linetype = 2, se = F) + 
  geom_line(data = gglines, inherit.aes = F, aes(x = lines, y = end_pts, group = lines), color = "grey") +
  geom_ribbon(aes(ymin = resist - CI, ymax = resist + CI), alpha = 0.2) +
  geom_line() + 
  geom_point(data = fin_data, aes(x = rr_O, y = resistance), pch = 21, color = "red", fill = "lightgrey") + 
  geom_point(data = parMoo, aes(x = rr_O, y = par_rr), size = 3, pch = 21, fill = "grey") +
  theme_bw() + 
  ylab("Drought sensitivity") + 
  xlab("Water inundation sensitivity") + 
  geom_text(data = fin_data, aes(x = rr_O, y = resistance, label = sp), 
            size = 3,
            hjust = hj, vjust =vj, fontface = "italic")



p3



ggsave(p3, file = './graphs/Drought_WIS.png', 
       width = 4, 
       height = 4)


# wait, so can I predict elevation with dden, rr.x, and rank. 
rr_data <- read.table("./data/riskratio_data.txt", header = T)
colnames(rr_data)[2] <- "rr_O"
fin_data <- merge(rr_data, dt, by = "sp")
str(fin_data)
fin_data



#### OK there we go
moo1 <- (lm(resistance ~ dden + rr_O, fin_data))
moo1p <- (glm(resistance ~ dden + rr_O, family = "gaussian", fin_data))

parMoo <- cbind(fin_data, par_rr = resid(moo1p, type = "partial")[,1] + mean(fin_data$resistance))
summary(moo1)
anova(moo1)

aovPerVar(moo1)
#####
pred_moo2 <- expand.grid(dden = seq(min(fin_data$dden), max(fin_data$dden), length = 100), 
                         rr_O = mean(fin_data$rr_O))

pred_moo2$resist <- predict(moo1, pred_moo2, type = "response")
pred_moo2$CI <- predict(moo1, pred_moo2, type = "response", se.fit = T)$se.fit *1.96

gglines <- data.frame(lines = rep(fin_data$dden, times = 2), 
                      end_pts = c(fin_data$resistance, parMoo$par_rr))

hj <- rep(-0.25, 8)
vj <- rep(0, 8)
hj[which(fin_data$sp == "Ptom")] <- 1.25
hj[which(fin_data$sp == "Smac")] <- -.05
hj[which(fin_data$sp == "Pmal")] <- 1.05
hj[which(fin_data$sp == "Dry")] <- 1.25
hj[which(fin_data$sp == "Sgib")] <- 1.25
#hj[which(fin_data$sp == "Sbec")] <- 0.5
vj[which(fin_data$sp == "Spar")] <- -1.5
vj[which(fin_data$sp == "Smac")] <- 1

p4 <- ggplot(pred_moo2, aes(x = dden, y = resist)) + 
  stat_smooth(data = fin_data, aes(x = dden, y = resistance), method = lm, size = 0.5, color = "red", linetype = 2, se = F) + 
  geom_line(data = gglines, inherit.aes = F, aes(x = lines, y = end_pts, group = lines), color = "grey") +
  geom_ribbon(aes(ymin = resist - CI, ymax = resist + CI), alpha = 0.2) +
  geom_line() + 
  geom_point(data = fin_data, aes(x = dden, y = resistance), pch = 21, color = "red", fill = "lightgrey") + 
  geom_point(data = parMoo, aes(x = dden, y = par_rr), size = 3, pch = 21, fill = "grey") +
  theme_bw() + 
  ylab("Drought sensitivity") + 
  xlab(bquote("Wood density g" ~cm^-3 )) + 
  geom_text(data = fin_data, aes(x = dden, y = resistance, label = sp), 
            size = 3,
            hjust = hj, vjust =vj, fontface = "italic")



p4

ggsave(p4, file = './graphs/Drought_WoodDensity.png', 
       width = 4, 
       height = 4)


#####
pred_moo2 <- expand.grid(dden = seq(min(fin_data$dden), max(fin_data$dden), length = 100), 
                        rr_O = mean(fin_data$rr_O))

pred_moo2$resist <- predict(moo1, pred_moo2, type = "response")
pred_moo2$CI <- predict(moo1, pred_moo2, type = "response", se.fit = T)$se.fit *1.96

p4 <- ggplot(pred_moo2, aes(x = dden, y = resist)) + 
  geom_ribbon(aes(ymin = resist - CI, ymax = resist + CI), alpha = 0.2) +
  geom_line() + 
  geom_point(data = fin_data, aes(x = dden, y = resistance)) + 
  theme_classic() + 
  ylab("Drought sensitivity") + 
  xlab(bquote("Wood density g" ~cm^-3 ))

p4
ggsave(p4, file = './graphs/Drought_InundationSensitivity.png', 
       width = 4, 
       height = 4)


# 
moo2 <- (lm( elev ~ rr_O + resistance, fin_data))
summary(moo2)
anova(moo2)
car::vif(moo2)



### what about when we only use the predicted values for the three


pelev <- read.table("./data/pelev_data.txt", header = TRUE)

new_dt <- merge(pelev, fin_data, by = "sp")


moo2 <- (lm(pe ~ resistance, new_dt))
summary(moo2)
with(new_dt, plot(pe, rr_O))

### INLA 
require(INLA)


inla()


