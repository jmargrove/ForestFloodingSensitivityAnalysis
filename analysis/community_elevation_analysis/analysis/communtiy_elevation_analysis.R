#' @title: "NDMS of species and elevation"
#' @author: "James Margrove"
#' @date: "30 November 2016"
#' @output: html_document

require(vegan)
require(ggplot2)
require(raster)
require(Hmisc)

source('./analysis/community_elevation_analysis/data/data_index.R')
large_plot_data <- subset(large_plot_data, sp != "Unkn" & mort != 1 & DBH > 50)
large_plot_data <- droplevels(large_plot_data)
str(large_plot_data)

#Creating the factor elevation,
expected_n_bands <- 12
large_plot_data$felv25 <- cut2(large_plot_data$elev, g = expected_n_bands, m = 100)
bands <- cut2(large_plot_data$elev, g = expected_n_bands, m = 100, onlycuts = TRUE)
bands[c(1,length(bands))] <- c(55.9, 137.9)
# number of bands used
length(bands)-1

sd(with(large_plot_data, tapply(sp, felv25, length)))

#Creating the matrix for the analysis 
matSp <- as.matrix(with(large_plot_data, tapply(sp, list(felv25,sp), length)))
matSp[is.na(matSp)] <- 0
### How many of the speceis occur at each of the top brackets 
SP <- c("Dry","Pmal","Ptom","Sacu","Sbec","Sfal","Sgib","Slep","Smac","Smec","Spar","Spau","Ssem","Ssmi","Swal","Sxan")

# at high elevations 
length(which(matSp[12,SP] != 0))
# at the lowest elevation 
length(which(matSp[1,SP] != 0))

#Runing NMDS from the vegan package
m1 <- metaMDS(matSp)


md1 <- m1$points[,1] # we want the sites 



#Import the map
r <- raster("analysis/community_elevation_analysis/data/Sepilok160_Qgis.tif") #This is the sepilok, map
e <- extent(603945.651,605788.486,  647133.509,  648054.815) # extent
plot160c <- crop(x = r, e)

max(plot160c@data@values)
plot160c <- plot160c-min(plot160c@data@values) 
plot160c <- (plot160c/max(plot160c@data@values))*(137.9 - 55.9) + 55.9
rVals <- plot160c@data@values # the values of the raster 
comPlot <- plot160c # new file for the community plot

#Adding in the new values per band
for(i in 1:length(bands)-1){
  cVals <- which(rVals >= bands[i] & rVals <= bands[i+1])
  comPlot@data@values[cVals] <- md1[i]}


#Using ggplot raster to map the image 
ggCom <- as.data.frame(comPlot, xy = TRUE)
colnames(ggCom) <- c("x","y","z")

###
brk <- round(seq(min(comPlot@data@values), 0.97, length = 5),2)

### graph  
ggplot(ggCom) + geom_raster(aes(x, y, fill=z)) + 
  scale_fill_gradient(low="#232d29", high="#6A9113", name = "", breaks = brk)  


#Export as a raster 
writeRaster(comPlot, "analysis/community_elevation_analysis/data/community_analysis_Fig1.tif", format = "GTiff")



