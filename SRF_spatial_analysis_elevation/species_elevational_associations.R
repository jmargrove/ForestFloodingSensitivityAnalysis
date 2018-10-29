################################################################################
##### Title: species elevational associations with INLA-SPDE
##### Author: James Margrove 

# Clear workspace 
rm(list=ls())

# Import packages 
require(INLA)
require(doSNOW)
require(plyr)
require(parallel)
require(ggplot2)
require(arm)

spelev <- read.table("./data/forestplot_160_spatial_data.txt",header = T)
# coordinates 
spelev$Xm <- spelev$X50N - min(spelev$X50N)
spelev$Ym <- spelev$Y50N - min(spelev$Y50N)

#The species of interest: those that are also in the field experiment 
sp <- c("Dry","Sxan","Ssmi","Ptom","Slep" ,"Sbec","Sacu" ,"Sgib", "Pmal" , "Spar" ,"Smac" ,"Smec", "Spau", "Ssem","Sfal","Swal")
sp <- sort(sp)

# Processing data for analysis 
xxx <- lapply(sp, function(x) {
  species <- x
  dt <- spelev[spelev$sp == species,] 
  nind <- length(spelev[spelev$sp == species,]$sp) 
  dt$sp <- factor(dt$sp)
  nt <- spelev[spelev$sp != species,] 
  nt$sp <- factor(nt$sp)
  nt$occ <- rep(0, length(nt$sp))
  dt$occ <- rep(1, length(dt$sp))
  dt <- rbind(dt, nt)
  dt$FocSp <- factor(rep(species, length(dt$sp)))
  dt$abun <- rep(nind, length(dt$sp))
  dt
})

data <- NULL
for(i in 1:16) {data <- rbind(data, xxx[[i]])}

# Sampling data to test this out 
sdata <- ddply(data,.(FocSp),function(x) x[sample(nrow(x),400),])

# Organising data for INLA
# coords and the mesh 
coords <- sdata[,c("Xm","Ym")]
head(coords)
m1 <- inla.mesh.2d(coords, max.edge = c(200, 500), 
                   cutoff =100, 
                   offset=c(200,400))

# spde
spde <- inla.spde2.matern(m1, alpha=2)

#Making the data stack, only for modeling the spatial effect
A <- inla.spde.make.A(m1, loc=as.matrix(coords))

#Stack object incorporating the data 
stk<- inla.stack(tag = "stk", 
                 data=list(occ=sdata$occ), 
                 A=list(A, 1), 
                 effects=list(list(i=1:spde$n.spde),
                              data.frame(int=1,
                                         elev=sdata$elev,
                                         FocSp=sdata$FocSp)))
# set up the Clcuter 
ncores <- detectCores()
getDoParWorkers()# 16 cores 
nClust <- makeCluster(ncores, type="SOCK")
registerDoSNOW(nclust)
clusterExport(nclust, c("join.stack","spde","inla","inla.stack.data","inla.stack.A"))


#Running the INLA model 
model1 <- inla(occ ~ 0 + int + elev + I(elev^2) + FocSp + FocSp:elev +
                 f(i, model=spde),
               family="binomial",  data=inla.stack.data(stk), control.predictor=list(A=inla.stack.A(stk)),
               control.fixed = list(expand.factor.strategy="inla"), 
               num.threads = ncores)

# 
stopCluster(nClust)

# Extracting the coeficents 
cf1 <- spc3$summary.fixed[,1]
# sorting coef in to a data frame 
coefVals <- data.frame(int=c(cf1[1]+cf1[4:19]), 
                       x1=c(cf1[2]+cf1[20:35]), 
                       x2=rep(cf1[3], 16),
                       sp=sp)

# Calculating the maximum point of occurance (pele)
pele <- (-(coefVals[,2])/((coefVals[,3])*2))

#########################################################################
# Calculating the quadratic equations for each species from the model 

# extract the coeficents from the model 
cf1 <- spc3$summary.fixed[,1]

#make a table of teh coef once they have been added to teh intercept etc..
coefVals <- data.frame(int=c(cf1[1]+cf1[4:19]), 
                       x1=c(cf1[2]+cf1[20:35]), 
                       x2=rep(cf1[3], 16),
                       sp=sp)

#running a loop to calculate the curves. 0 to 80 is the elevation gradient. also invlogit after to transform 
dtf <- sapply(1:16, function(x){
  cf1 <- as.numeric(coefVals[x,])
  spcf1 <- function(x) cf1[1] + cf1[2]*x + cf1[3]*x^2
  invlogit(spcf1(0:80))})

dtf <- as.vector(dtf)# turn in to a vector 

# creating a data frame of teh predicted values from above 
preds <- data.frame(sp=rep(sp, each=81), p=dtf, elev=rep(0:80, times=16))

# plotting out the lines for each species
ggplot(preds,aes(x=elev,y=p,color=sp)) + geom_line() + facet_wrap(~sp, scale="free_y") + xlim(0,80) +
  theme_classic() + theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black")) + xlab("Elevation (m)") + ylab("p(occurance)")


########### calculating the derivitive and setting to zero to work out the most probable elevation a species is at 
pele <- (-(coefVals[,2])/((coefVals[,3])*2)) # derivitive equation set to zero 
pele[pele<0] <- 0 # if any species have a negative elevation, change to 0 (the lowest point)




################################################################################
##### Now sample from the posterior distribution to get the Credible intervals 
ncores <- detectCores()
getDoParWorkers()# 16 cores 
nClust <- makeCluster(ncores, type="SOCK")
registerDoSNOW(nclust)
clusterExport(nclust, c("spde","inla","inla.stack.data","inla.stack.A"))

#
model2 <- inla(occ ~ 0 + int + elev + I(elev^2) + FocSp + FocSp:elev +
                 f(i, model=spde),
               family="binomial",  data=inla.stack.data(stk), 
               control.predictor=list(A=inla.stack.A(stk), 
                                      compute = TRUE),
               control.fixed = list(expand.factor.strategy="inla"), 
               num.threads = ncores)

# Stop the cluster. 
stopCluster(nClust)

# This is a function to sample from the posterior marginal and then recalculate the coef (similar to above)
FunBoots <- function(x){
  s = inla.posterior.sample(1, result=model2) # sample new coef
  s = s[[1]]$latent # latent 
  cf2 <- tail(s, 35) # take the coefs from the end of this vector 
  
  
  coef=rownames(cf2) # teh coef names 
  cf2[20:35] <- cf2[20:35][order(coef[20:35])] # reorder
  cf2[4:19] <- cf2[4:19][order(coef[4:19])] # reorder ... don't know why they're not in alphabetical order 
  
  # same as above, creating a table of the coef
  coefVals <- data.frame(int=c(cf1[1]+cf1[4:19]), 
                         x1=c(cf1[2]+cf1[20:35]), 
                         x2=rep(cf1[3], each=16),
                         sp=sp)
  
  #calculate the actual curves, same as above 
  dtf <- sapply(1:16, function(x){
    cf1 <- as.numeric(coefVals[x,])
    spcf1 <- function(x) cf1[1] + cf1[2]*x + cf1[3]*x^2
    invlogit(spcf1(0:80))})
  dtf <- as.vector(dtf)
  pele <- (-(coefVals[,2])/((coefVals[,3])*2)) # calcualting the derivitive set to zero 
  pele[pele<0] <- 0 # set to zero any -ve elevations 
  dtf <- c(dtf,pele)# joinig these two vectors together  
  dtf}# returning these 


##### posterior sampling 5000 times  
ncores <- detectCores()
getDoParWorkers()# 16 cores 
nclust <- makeCluster(ncores, type="SOCK")
registerDoSNOW(nclust)
clusterExport(nclust, c("spde",
                        "inla",
                        "inla.stack.data",
                        "inla.stack.A", 
                        "inla.posterior.sample", 
                        "model2", 
                        "invlogit", 
                        "FunBoots", 
                        "foreach"))


post.samp2 <- foreach(i = 1:5000, .combine="cbind") %dopar% FunBoots() # foreach loop, cbind to store 
stopCluster(nClust)
write.table(post.samp2, "post.samp5000.txt") # save the table 

