################################################################################
# title: organising reubens data for analysis 
# author: James Margrove 

rm(list=ls())

# Import packages
require(ggplot2)

# Import data 
a1_data <- read.table("./data/Reuben_data/A1.txt", header = TRUE)
a2_data <- read.table("./data/Reuben_data/A2.txt", header = TRUE)
a3_data <- read.table("./data/Reuben_data/A3.txt", header = TRUE)
s1_data <- read.table("./data/Reuben_data/S1.txt", header = TRUE)
s2_data <- read.table("./data/Reuben_data/S2.txt", header = TRUE)
s3_data <- read.table("./data/Reuben_data/S3.txt", header = TRUE)

data <- rbind(a1_data, a2_data, a3_data, s1_data, s2_data, s3_data)

# Points with elevation 
elev <- read.csv("./data//Reuben_data/pointsWithElevation.csv")
elev$plot <- paste(elev$Forest, elev$id, sep = "")

# Species of interest
spp <- c("Dryobalanopslanceolata", "Parashoreamalaanonan", "Parashoreatomentella", 
         "Shorealeprosula", "Shoreamacroptera", "Shoreaparvifolia", 
         "Shoreasmithiana",  "Shoreaseminis", "Shoreawaltonii",  
         "Shoreamecistopteryx", "Shoreabeccariana",  "Shoreapauciflora", 
         "Shoreaxanthophylla", "Shoreafalciferoides", "Shoreagibosa",
         "Shoreaacumitisima")
spp <- spp[order(spp)]

# Filter for species of interest above 50cm, and those with different lower limits
sxanDT <- data[data$Species == "Shoreaxanthophylla" & data$Diam2000 > 30 & 
                 data$Diam2000 < 50,]
sbecDT <- data[data$Species == "Shoreabeccariana" & data$Diam2000 > 30 & 
                 data$Diam2000 < 50,]
smacDT <- data[data$Species == "Shoreamacroptera" & data$Diam2000 > 40 & 
                 data$Diam2000 < 50,]
data <- data[data$Diam2000 > 50 & data$Species %in% spp,]
data <- rbind(data, sxanDT, sbecDT, smacDT)

# Add in the elevations of the 1 ha plots 
el <- numeric(dim(data)[1])
for(i in 1:dim(data)[1]){
  pt <- which(elev$plot == data[i,"Forest"] & elev$ha4plot == data[i, "ha4plot"])
  el[i] <- elev[pt, "sepilok_DT"]
}

# subtract the bottom of the plot 
data$el <- el - 57.83 

# Add in the wood density 
dden_data <- read.table('./data/dden_adult.txt', header = T)
dden_data <- dden_data[order(dden_data$sp),]
#dden_data <- dden_data[-c(4, 7),]
head(dden_data)

dden <- numeric(dim(data)[1])
for(i in 1:16){
  pt <- which(data$Species == spp[i])
  dden[pt] <- dden_data[i, "dden_adult"]
}

data$dden <- dden 
head(data)
# Add in the wood density 
rr_data <- read.table('./data/riskratio_data.txt', header = T)

rr <- numeric(dim(data)[1])
for(i in 1:dim(rr_data)[1]){
  pt <- which(data$Species == spp[i])
  rr[pt] <- rr_data[i, "rr"]
}

data$rr <- rr 

tail(data, 20)
mrr <- with(data, tapply(rr, Species, mean))
mrr[!is.na(mrr)]
rr_data$rr

# Drop unused levels 
data <- droplevels(data)

#Double check that everything matches up 
# wood density 
sp <- with(data, tapply(dden, Species, mean))
dden_data$dden_adult[-c(4,7, 13)] == sp[order(names(sp))]

# rr 
sp <- with(data, tapply(rr, Species, mean))
rr_data$rr[-c(4,7, 13)] == sp[order(names(sp))]

# 1 ha plot elevation 
head(data)
pt <- as.vector(with(data, tapply(el, list(Forest, ha4plot), mean)))
dden_data$dden_adult[-c(4,7, 13)] == sp[order(names(sp))]
pt == as.vector(with(elev, tapply(sepilok_DT, list(plot, ha4plot), mean))) - 57.83

# Elevations are paire correctly to plots 
# wood densities are paird to species 
# water innundation sensitivites are paired to species 

# remove A3 from the analysis because it is within the 160ha forest plot 
data <- data[data$Forest != "A3",]

# Drop unused levels 
data <- droplevels(data)


# save the data frame 
write.table(data, "./data/Reuben_data/data_cleaned.txt")




