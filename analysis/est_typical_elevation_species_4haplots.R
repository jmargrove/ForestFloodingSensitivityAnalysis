################################################################################
#' @title Estimate typical elevation of co-occuring species in 4ha plots 
#' @author James Margrove
#'
#'
#'


# Need to flip coordinates so that thay are consistent, everything is in the right configuration
# this is quite important so that all the plots are the same config

# make a mesh suitable for all coordinates 
coords <- sdata[,12:13]
head(coords)
m1 <- inla.mesh.2d(coords, max.edge = c(200, 500), 
                   cutoff =100, 
                   offset=c(200,400))

# now start to add in the repl code


#params
nplots <- (dim(sdata)/nSp)[1]


#### spde model matern 
spde <- inla.spde2.matern(mesh=m1, alpha=2)

#### Making the data stack, only for modeling the spatial effect
A <- inla.spde.make.A(mesh=m1, loc=as.matrix(coords),
                      repl=rep(1:nSp, each=nFsp), n.repl=nSp)# there are one for each FocSp
####
s.index <- inla.spde.make.index(name="spatial.field", n.spde=spde$n.spde, n.repl=nSp)
str(s.index)

####
stk<- inla.stack(tag = "stk", 
                 data=list(elev=sdata$elev), 
                 A=list(A, 1), 
                 effects=list(s.index,
                              data.frame(int=1,
                                         sp)))


# Running the INLA model 
model <- inla(elev ~ 0 + int + sp + 
               f(spatial.field, model=spde, replicate=spatial.field.repl), 
             family="binomial",  data=inla.stack.data(stk), control.predictor=list(A=inla.stack.A(stk)),
             control.fixed = list(expand.factor.strategy="inla"), 
             control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))
