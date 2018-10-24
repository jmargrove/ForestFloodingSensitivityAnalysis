# organisation 
# additional organisation file of the data
source("./analysis/wood_density_distribution/data/data_index.R")
# create a variable for the groupings of elevation patches 
desired_number_observations <- round(dim(wood_density_data_178ha)[1] / 10, 0)
wood_density_data_178ha$fd <- cut2(wood_density_data_178ha$d, m = desired_number_observations)
desired_number_observations2 <- round(dim(wood_density_data_178ha)[1] / 20, 0)
wood_density_data_178ha$fd2 <- cut2(wood_density_data_178ha$d, m = desired_number_observations2)
