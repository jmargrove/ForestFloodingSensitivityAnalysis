################################################################################
# title: organising reubens data for full triangle analysis  
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
data <- rbind(a1_data, a2_data, s1_data, s2_data, s3_data)
data$unqeID <- paste(data$`X10x10`, data$`X20x20`, data$ha4plot, data$Forest, sep = "")

# Points with elevation 
#elev <- read.csv("./data/Reuben_data/pointsWithElevation.csv")
elev <- read.table("./data/Reuben_data/plots10x10_elevation.txt", header = TRUE)


e <- numeric(nrow(data))
x <- numeric(nrow(data))
y <- numeric(nrow(data))
for(i in 1:nrow(data)){
  n <- which(elev$unqeID == data$unqeID[i])
  e[i]<- elev$zmean[n]
  x[i] <- elev$x[n]
  y[i] <- elev$y[n]
}

data$x <- x
data$y <- y
data$elev <- e
data$elev
# select all dipterocarps 
data <- data[data$Family == "Dipterocarpaceae",]
data <- droplevels(data)
length(data[data$y == 0,1])

# extracting those with the correct minimum limits 
min_lim <- read.table("./data/plot_min_DBH_limits.txt", header = TRUE)
dt <- data[0,]

for(i in 1:nrow(min_lim)){
  if(!is.na(min_lim[i, 3])){
    n <- which(data$Species == as.character(min_lim[i, 2]) & 
                 data$Diam2000 > as.numeric(min_lim[i, 3]))
    df <- data[n,]
    
    if(nrow(df) != 0){
      dt <- rbind(dt, df)
    }
  }
}

data <- dt
data <- droplevels(data)
# createAcrynms 
sp <- character(nrow(data))
for(i in 1:nrow(data)){
  g = substring(data$Species[i], 1, 1)
  pos <- regexpr("_", as.character(data$Species[i]))[1]
  s = substring(data$Species[i], pos+1, pos+3)
  sp[i] <- paste(g, s, sep = "")
}
sp[sp == "Dlan"] <- "Dry"

data$sp <- factor(sp)

levels(data$Species)

spatial_data <- read.table("./data/forestplot_160_spatial_data.txt", header = TRUE)
bigPlot <- levels(spatial_data$fullname)
reubenPlot <- levels(data$Species)

unqSp <- unique(c(bigPlot, reubenPlot))
unqSp




# Add in the wood density 
wddb_data <- read.table("./data/GlobalWoodDensityDatabase.txt", header = TRUE)
wden_data <- data.frame(Species = unqSp, dden = numeric(length(unqSp)))
wden_data$Genus <- substring(wden_data$Species, 0, regexpr("_", wden_data$Species) - 1)

for(i in 1:length(unqSp)){
  d = wddb_data[(which(wddb_data$Species == unqSp[i])), "dden"]
  if(length(d) > 0){
    wden_data[i, "dden"] <- mean(d) 
  } else {
    wden_data[i, "dden"] <- NA 
  }
}

wden_data[16,"dden"] <- 0.52 # from philipson 
gen_means <- with(wden_data, tapply(dden, Genus, mean, na.rm = T))
missing_wden_values <- wden_data[is.na(wden_data$dden),] 
missing_wden_values


for(i in 1:nrow(missing_wden_values)){
  wden_data[as.numeric(rownames(missing_wden_values))[i], "dden"] <- as.numeric(gen_means[which(names(gen_means) == missing_wden_values[i,"Genus"])])
  
}



#Hopea_sangal 
#Shorea_agamii
#Shorea_confusa
#Shorea_symingtonii
#Shorea_waltonii

sp <- character(nrow(wden_data))
for(i in 1:nrow(wden_data)){
  g = substring(wden_data$Species[i], 1, 1)
  pos <- regexpr("_", as.character(wden_data$Species[i]))[1]
  s = substring(wden_data$Species[i], pos+1, pos+3)
  sp[i] <- paste(g, s, sep = "")
}
sp[sp == "Dlan"] <- "Dry"
wden_data$sp <- sp
wden_data
wden_data <- wden_data[order(wden_data$sp),]
wden_data

#### so we have a table of wood density values 
#### Combine spatial data with rebens plot data 

# add in the dden for the 160 plot data 
str(spatial_data)
for(i in 1:nrow(spatial_data)){
  #i = 980
  pt <- which(wden_data$Species == as.character(spatial_data$fullname[i]))
  print(i)
  if(length(pt) != 0){
    spatial_data[i, "dden"] <- wden_data[pt, "dden"]
  }
}

with(spatial_data, tapply(dden, sp, mean))


write.table(spatial_data, file = "./data/forestplot_160_spatial_data_UPDATE.txt")
### now add in the wood densities 

data$dden <- numeric(nrow(data))
for(i in 1:nrow(data)){
  pt <- which(wden_data$Species == as.character(data$Species[i]))
  data[i, "dden"] <- wden_data[pt, "dden"]
}

with(data, tapply(dden, Species, mean))
data$elev


write.table(data, file="./data/reu_160plot_combinded_data.txt")
