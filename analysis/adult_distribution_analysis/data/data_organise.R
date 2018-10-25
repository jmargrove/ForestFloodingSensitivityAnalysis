#data organising for the SPDE model

# import the data 
data <- read.table('data/forestplot_160_spatial_data_UPDATE.txt', header = TRUE)
species <- read.table('data/dden_adult_new.txt', header = TRUE)$sp
# make teh data frame for probs

occurance <- rep(0, dim(data)[1])
fun <- function(i){
  occurance[which(data$sp == as.character(i))] <- 1
  data$occurance <- occurance
  data$species <- i
  return(data)
}

species_occurance_data <- foreach(i = species, .combine = rbind) %do% fun(i)

str(species_occurance_data)
ggplot(species_occurance_data, aes(x = elev, y = occurance, color = species)) + 
  facet_wrap(~species) + 
  geom_point() + 
  stat_smooth()

write.table(species_occurance_data, file = './analysis/adult_distribution_analysis/data/species_occurance_data.txt')


