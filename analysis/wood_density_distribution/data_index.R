# importing the required data
# RAW data files
pelev_data <- read.table("./data/pelev_data.txt", header = TRUE)
spatial_data <- read.table("./data/forestplot_160_spatial_data_UPDATE.txt", 
                           header = TRUE)
plotExtent <- read.table("./data/plotExtent.txt", header = TRUE)
reu_data <- read.table("./data/reu_160plot_combinded_data.txt", header = TRUE)

