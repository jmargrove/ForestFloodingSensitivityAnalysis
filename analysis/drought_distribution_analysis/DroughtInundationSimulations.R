################################################################################
#' Simulation of the inundation and drought data 
#' author: James Margrove 

# Remove the data 
rm(list=ls())
# Load models 
load("./models/elev_resistance_dden.R")
load("./models/pe_rr_dden_NoAbundance")


summary(model2)
str(model4)
summary(model4)
model4$coefficients

sd(data$elev+52, na.rm = T)

calcNewResp = function(m, dden, elev, err, dden_sd = 0.132861/sqrt(41), elev_sd = 9.025599) {
  coefs <- coef(m)
  int <- coefs[1]
  dden <- dden #+ rnorm(n = length(elev), mean = 0, sd = dden_sd)
  elev <- elev# + rnorm(n = length(elev), mean = 0, sd = elev_sd)
  error <- rnorm(n = length(elev), mean = 0, sd = err)
  slp1 <- coefs[2] 
  slp2 <- coefs[3] 
  resistance = (int + error + (slp2)*dden - elev) / slp1 * -1
  return(data.frame(dden = dden, elev = elev, res = resistance))
}

calcNewResp(model4, 0.62, 70)

# Import data 
spdata <- read.table("./data/spdata.txt", header = T)
str(spdata)

# Calculate the median elevation & wood density 
elev_median <- with(spdata, tapply(elev, sp, median))
sp_dden <- with(spdata, tapply(dden, sp, median))
data <- data.frame(elev = elev_median, dden = sp_dden)
# risk ratio of model4 is the inundaiton response 

data$rr <- calcNewResp(model4, data$dden, data$elev + 52, err = 0)$res
data$resistance <- calcNewResp(model2, data$dden, data$elev + 52, err = 0)$res



require(foreach)
n = 500
#

data
datasims_inun <- foreach(i = 1:n, .combine = rbind) %do% calcNewResp(model4, 
                                                                       data$dden, 
                                                                       data$elev + 52, 
                                                                       err = 14.04)
# 
dim(datasims_inun)
sims_inun <- data.frame(elev = rep(data$elev, n),
                   dden = rep(data$dden, n),
                   inun = as.vector(inundation_data))



summary(model4)
sims_res <- foreach(i = 1:n, .combine = rbind) %do% calcNewResp(model2, 
                                                                       data$dden, 
                                                                       data$elev + 52, 
                                                           err = 7.561)


dim(sims_res)
# 
# sims_res <- data.frame(elev = rep(data$elev, n), 
#                    dden = rep(data$dden, n), 
#                    res = as.vector(res_data))

str(sims_res)

dt <- data.frame(inun = datasims_inun[, "res"], 
                 res = sims_res[, "res"], 
                 elev = datasims_inun[, "elev"], 
                 dden = datasims_inun[, "dden"])

#dt <- cbind(sim_res, sims_inun[, "res"])
dt$res <- dt$res - mean(dt$res, na.rm  = T)
dt$inun <- dt$inun - mean(dt$inun, na.rm  = T)


colnames(dt)[4] <- "Wood density"

head(dt)

p1 <- ggplot(dt, aes(x = inun, y = res, color = `Wood density`)) + geom_point(alpha = 0.75) + 
  theme_bw() + 
  xlab("Inundation Sensitivity") + 
  ylab("Drought Sensitivity") + 
  geom_vline(xintercept = 0, linetype = 2, color = "#941C2F") + 
  geom_hline(yintercept = 0, linetype = 2, color = "#941C2F") + 
  theme(legend.position = "top") + 
  scale_color_gradient(high = "#0B2644", 
                       # mid = "#F3A738", 
                       low = "#D3E7FF") +
  geom_point(data = data, aes(x = rr - mean(rr, na.rm  = T), 
                              y = resistance - mean(resistance, na.rm  = T)), fill = "red",  
             color = "black", size = 3, pch = 21) + 
  annotate("segment", x = -0.5, xend = 0.5, y = -0.0003, yend = 0.0003, colour = "black", size=1, arrow=arrow()) + 
  geom_text(data = data.frame(x = 0.32, y = 0.00045, l = "Trade-off between wood \n density and hydraulic sensitity"), 
            inherit.aes = F, aes(x = x, y = y, label = l))

p1

ggsave(p1, file = "./graphs/inun_dro_sen.png", 
       width = 5, 
       height = 5.2)

# 

p2 <- ggplot(dt, aes(x = inun, y = res, color = elev)) + geom_point(alpha = 0.75) + 
  theme_bw() + 
  xlab("Inundation Sensitivity") + 
  ylab("Drought Sensitivity") + 
  geom_vline(xintercept = 0, linetype = 2, color = "#941C2F") + 
  geom_hline(yintercept = 0, linetype = 2, color = "#941C2F") + 
  theme(legend.position = "top") + 
  scale_color_gradient(high = "#EF5B00", 
                       low = "#EFCDB8")

p2

ggsave(p2, file = "./graphs/inun_dro_sen_elev.png", 
       width = 5, 
       height = 5.2)


# Table of specie s
fullname_data <- read.table("./data/species_names.txt", header = T)
table <- data.frame(
  sp = names(apply(res_data, 1, mean)),
  dden = data$dden,
  elev = data$elev,
  Drought = apply(res_data, 1, mean),
  sd = apply(res_data, 1, sd),
  Inundation = apply(inundation_data, 1, mean),
  sd = apply(inundation_data, 1, sd)
)


table <- merge(fullname_data[, c(1,2)], table, by = "sp")

write.table(table, file = "./data/speceis_table.txt")
