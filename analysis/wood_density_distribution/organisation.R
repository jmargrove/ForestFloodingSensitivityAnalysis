# organisation 
source("./analysis/wood_density_distribution/data_index.R")

# create a variable for the groupings of elevation patches 
wood_density_data_178ha$fe <- cut(wood_density_data_178ha$e, breaks = 10)