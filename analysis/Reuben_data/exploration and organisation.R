################################################################################
# title: organising reubens data for analysis 
# author: James Margrove 


a1_data <- read.table("./data/Reuben_data/A1.txt", header = TRUE)
a2_data <- read.table("./data/Reuben_data/A2.txt", header = TRUE)
a3_data <- read.table("./data/Reuben_data/A3.txt", header = TRUE)
s1_data <- read.table("./data/Reuben_data/S1.txt", header = TRUE)
s2_data <- read.table("./data/Reuben_data/S2.txt", header = TRUE)
s3_data <- read.table("./data/Reuben_data/S3.txt", header = TRUE)

levels(a2_data$Species)
# Filter species of interest

spp <- c("Dryobalanopslanceolata", "Parashoreamalaanonan", "Parashoreatomentella", 
         "Shorealeprosula", "Shoreamacroptera", "Shoreaparvifolia", 
         "Shoreasmithiana",  "Shoreaseminis", "Shoreawaltonii",  
         "Shoreamecistopteryx", "Shoreabeccariana",  "Shoreapauciflora", 
         "Shoreaxanthophylla", "Shoreafalciferoides", "Shoreagibosa",
         "Shoreaacumitisima")


data <- rbind(a1_data, a2_data, a3_data, s1_data, s2_data, s3_data)

spn <- with(data[data$Species %in% spp,], tapply(Species, Species, length))
df <- with(data[data$Species %in% spp,], tapply(Species, list(Species, Forest), length))


dt <- df[rownames(df) %in% spp,]

df <- with(data[data$Species %in% spp,], tapply(Species, list(Species, ha4plot, Forest), length))

df
dt <- df[rownames(df) %in% spp,]
str(a1_data)





dt


