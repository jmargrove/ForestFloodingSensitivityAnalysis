#data index 
route <- paste(getwd(), "/", sep = "")
species_occurance_data <- read.table(source(paste(route, '../data/species_occurance_data.txt', sep = "")), header = TRUE)