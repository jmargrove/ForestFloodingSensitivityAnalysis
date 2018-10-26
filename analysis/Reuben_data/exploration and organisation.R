################################################################################
# title: organising reubens data for analysis 
# author: James Margrove 

rm(list=ls())

# Import packages
require(ggplot2)

# Import data 
a1_data <- read.table("./data/Reuben_data/A1.txt", header = TRUE)
a2_data <- read.table("./data/Reuben_data/A2.txt", header = TRUE)
#a3_data <- read.table("./data/Reuben_data/A3.txt", header = TRUE)
s1_data <- read.table("./data/Reuben_data/S1.txt", header = TRUE)
s2_data <- read.table("./data/Reuben_data/S2.txt", header = TRUE)
s3_data <- read.table("./data/Reuben_data/S3.txt", header = TRUE)
data <- rbind(a1_data, a2_data, s1_data, s2_data, s3_data)

data$unqeID <- paste(data$`X10x10`, data$`X20x20`, data$ha4plot, data$Forest, sep = "")

# Points with elevation 
#elev <- read.csv("./data/Reuben_data/pointsWithElevation.csv")
elev <- read.table("./data/Reuben_data/plots10x10_elevation.txt", header = TRUE)
head(elev)

with(elev, tapply(zmean, Forest, mean))
ggplot(elev, aes(zmean)) + geom_density(aes(fill = Forest))

head(elev)
e <- numeric()
x <- numeric()
y <- numeric()
for(i in 1:nrow(data)){
#  i = 1
  n <- which(elev$unqeID == data$unqeID[i])
  n
  
  e[i]<- elev$zmean[n]
  x[i] <- elev$x[n]
  y[i] <- elev$y[n]
}


data$x <- x
data$y <- y
data$elev <- e
head(data)
tail(data)
length(which(e == 0))
length(which(is.na(e)))

head(data)
ggplot(data, aes(elev)) + geom_density(aes(fill = Forest))


# Species of interest
spp <- c("Dryobalanops_lanceolata", "Parashorea_malaanonan", "Parashorea_tomentella", 
         "Shorea_leprosula", "Shorea_macroptera", "Shorea_parvifolia", 
         "Shorea_smithiana",  "Shorea_seminis", "Shorea_waltonii",  
         "Shorea_mecistopteryx", "Shorea_beccariana",  "Shorea_pauciflora", 
         "Shorea_xanthophylla", "Shorea_falciferoides", "Shorea_gibosa",
         "Shorea_acumitisima")
spp <- spp[order(spp)]

# Filter for species of interest above 50cm, and those with different lower limits
sxanDT <- data[data$Species == "Shorea_xanthophylla" & data$Diam2000 > 30 & 
                 data$Diam2000 < 50,]
head(sxanDT)
sbecDT <- data[data$Species == "Shorea_beccariana" & data$Diam2000 > 30 & 
                 data$Diam2000 < 50,]
head(sbecDT)

smacDT <- data[data$Species == "Shorea_macroptera" & data$Diam2000 > 40 & 
                 data$Diam2000 < 50,]
data <- data[data$Diam2000 > 50 & data$Species %in% spp,]
data <- data[data$Diam2000 > 50,]
data <- rbind(data, sxanDT, sbecDT, smacDT)
head(data)

# Add in the wood density 
dden_data <- read.table('./data/dden_adult_new.txt', header = T)
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
head(data)
#Double check that everything matches up 
# wood density 
sp <- with(data, tapply(dden, Species, mean))
dden_data$dden_adult[-c(4,7, 13)] == sp[order(names(sp))]

# rr 
sp <- with(data, tapply(rr, Species, mean))
rr_data$rr[-c(4,7, 13)] == sp[order(names(sp))]

#min Reuben plot elevation 
min(pt, na.rm = TRUE)


# Abundance 
for(i in 13:1) {
  data$Species <- relevel(data$Species, ref = sort(levels(data$Species))[i])
}



abn <- with(data, tapply(elev, Species, length))
ABN <- numeric(dim(data)[1])

for(i in 1:13){
  a <- which(data$Species == levels(data$Species)[i])
    ABN[a] <- abn[i]
}

data$abn <- ABN
with(data, tapply(abn, Species, mean)) == abn

# Elevations are paire correctly to plots 
# wood densities are paird to species 
# water innundation sensitivites are paired to species 

# remove A3 from the analysis because it is within the 160ha forest plot 
data <- data[data$Forest != "A3",]
data
data <- data[!(data$Forest == "A2" & data$ha4plot %in% c(2,3)),]
data <- droplevels(data)


data <- droplevels(data)


data <- data[!is.na(data$el),]


# save the data frame 
write.table(data, "./data/Reuben_data/data_cleaned.txt")
