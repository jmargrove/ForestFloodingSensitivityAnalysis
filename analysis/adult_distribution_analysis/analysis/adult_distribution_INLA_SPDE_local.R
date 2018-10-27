# adult distribution INLA SPDE analysis

source('./analysis/adult_distribution_analysis/data/data_index.R')
str(species_occurance_data)

require(INLA)
coords <- with(species_occurance_data, cbind(X, Y))
mesh <- inla.mesh.2d(coords, max.edge = c(300, 500), 
                                  cutoff = 100, 
                                  offset=c(200,400))

plot(mesh)
points(coords, col = 'red', pch = 21, cex = 0.1)

# priors 
rho0 <- 100
sig0 <- 0.3


# spde
spde <- inla.spde2.pcmatern(mesh, 
                            alpha = 2,
                            prior.range = c(rho0, 0.05), 
                            prior.sigma = c(sig0, 0.05))

# A
A <- inla.spde.make.A(mesh, loc=as.matrix(coords))

# stk 
stk <- inla.stack(tag = "stk", 
                  data = list(occurance = species_occurance_data$occurance), 
                  A = list(A, 1), 
                  effects = list(list(i = 1:spde$n.spde),
                               data.frame(int = 1,
                                          elev = species_occurance_data$elev,
                                          species = species_occurance_data$species)))

# formula 
formula <- occurance ~ 0 + int + species * elev + I(elev^2) + f(i, model = spde)

# inla model 
model1 <- inla(formula, 
               data = inla.stack.data(stk),
               control.predictor = list(A = inla.stack.A(stk)),
               control.fixed = list(expand.factor.strategy = "inla"), 
               family = "binomial", 
               inla.call = "remote",
               num.threads = 4)

# summary 
summary(model1)
