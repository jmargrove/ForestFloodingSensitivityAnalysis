################################################################################
# title: organising reubens data for analysis 
# author: James Margrove 


a1_data <- read.table("./data/Reuben_data/A1.txt", header = TRUE)
a2_data <- read.table("./data/Reuben_data/A2.txt", header = TRUE)
a3_data <- read.table("./data/Reuben_data/A3.txt", header = TRUE)
s1_data <- read.table("./data/Reuben_data/S1.txt", header = TRUE)
s2_data <- read.table("./data/Reuben_data/S2.txt", header = TRUE)
s3_data <- read.table("./data/Reuben_data/S3.txt", header = TRUE)
# Filter species of interest
head(a1_data)

# Points with elevation 
elev <- read.csv("./data//Reuben_data/pointsWithElevation.csv")

elev$plot <- paste(elev$Forest, elev$id, sep = "")






spp <- c("Dryobalanopslanceolata", "Parashoreamalaanonan", "Parashoreatomentella", 
         "Shorealeprosula", "Shoreamacroptera", "Shoreaparvifolia", 
         "Shoreasmithiana",  "Shoreaseminis", "Shoreawaltonii",  
         "Shoreamecistopteryx", "Shoreabeccariana",  "Shoreapauciflora", 
         "Shoreaxanthophylla", "Shoreafalciferoides", "Shoreagibosa",
         "Shoreaacumitisima")


data <- rbind(a1_data, a2_data, a3_data, s1_data, s2_data, s3_data)
head(data)

spn <- with(data[data$Species %in% spp && data$Diam2000 > 50,], tapply(Species, Species, length))
df <- with(data[data$Species %in% spp && data$Diam2000 > 50,], tapply(Species, list(Species, Forest), length))


dt <- df[rownames(df) %in% spp,]

df <- with(data[data$Species %in% spp,], tapply(Species, list(Species, Forest), length))

str(df)

with(elev, tapply(sepilok_DT, plot, mean))

dt <- df[rownames(df) %in% spp,]
n = as.vector(dt)
#n[is.na(n)] <- 0
sp = rep(rownames(dt)[order(rownames(dt))], 6)
el <- rep(with(elev, tapply(sepilok_DT, plot, mean)), each = 14)

dden <- read.table('./data/dden_adult.txt', header = T)
dden <- dden[order(dden$sp),]
dden <- dden[-c(4, 7),]

wd <- rep(dden$dden_adult, 6)

rr_data <- read.table("./data/riskratio_data.txt", header = TRUE)[-c(4, 7), ]
rr <- rep(rr_data$rr, 6)

data <- data.frame(sp, n, el, wd, rr)

data[!is.na(data$n),]

m <- (lm(el ~ wd + rr, data[!is.na(data$n),]))
car::vif(m)


require(ggplot2)

ggplot(data[!is.na(data$n),], aes(x = rr, y = el)) + geom_point(alpha = 0.1) + 
  stat_smooth(method = lm)

ggplot(data[!is.na(data$n),], aes(x = wd, y = el, color = rr, size = n*10)) + geom_point() + 
  stat_smooth(method = lm) + 
  theme_classic()


data




