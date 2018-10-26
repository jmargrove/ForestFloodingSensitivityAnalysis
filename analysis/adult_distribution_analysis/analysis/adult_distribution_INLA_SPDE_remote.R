# adult distribution INLA SPDE analysis
rm(list = ls())
path <- '/home/majames/Documents/ForestFloodingSensitivityAnalysis/adult_distribution_analysis/'
#pathe <- './'

source(paste(path, 'data/data_index.R', sep = ""))
str(species_occurance_data)

# plot(mesh_samples_for_testing[["setup1"]]$mesh)
# points(coords, col = 'red', pch = 21, cex = 0.1)

require(INLA)
coords <- with(species_occurance_data, cbind(X, Y))

source(paste(path, 'analysis/testing_meshes.R', sep = ""))

diff_setups <- names(mesh_samples_for_testing)


# priors 
for(setup in diff_setups){
  rho0 <- mesh_samples_for_testing[[setup]]$priors$rho0
  sig0 <- mesh_samples_for_testing[[setup]]$priors$sig0
  # spde
  spde <- inla.spde2.pcmatern(mesh_samples_for_testing[[setup]]$mesh, 
                              alpha = 2,
                              prior.range = c(rho0, 0.05), 
                              prior.sigma = c(sig0, 0.05))
  # A
  A <- inla.spde.make.A(mesh_samples_for_testing[[setup]]$mesh, loc=as.matrix(coords))
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
  
  # setting up the core 
  require(doParallel)
  # how many cores are there
  number_of_cores <- detectCores()
  print(paste('how many cores? ', number_of_cores, sep = ""))
  # make the cluster 
  cl <- makeCluster(number_of_cores)
  # register the cores
  registerDoParallel(cl)
  
  # inla model 
  model1 <- inla(formula, 
                 data = inla.stack.data(stk),
                 control.predictor = list(A = inla.stack.A(stk)),
                 control.fixed = list(expand.factor.strategy = "inla"), 
                 family = "binomial", 
                 num.threads = number_of_cores)
  # summary 
  save(model1, file = paste(path, 'results/', setup, '.R'), sep = "")
  
}
