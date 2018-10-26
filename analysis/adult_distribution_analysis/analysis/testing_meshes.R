# meshes for testing the outcomes of INLA-SPDE
require(INLA)
mesh_samples_for_testing <- list(
  setup1 = list(mesh = inla.mesh.2d(coords, 
                                    cutoff = 100, 
                                    max.edge = c(900, 800), 
                                    offset=c(500,1000)), 
                priors = list(
                  rho0 = 100,
                  sig0 = 0.3
                )),
  setup2 = list(mesh = inla.mesh.2d(coords, 
                                    max.edge = c(100, 250), 
                                    cutoff = 50, 
                                    offset=c(500,1000)), 
                priors = list(
                  rho0 = 100,
                  sig0 = 0.3
                )),
  setup3 = list(mesh = inla.mesh.2d(coords, 
                                    max.edge = c(100, 250), 
                                    cutoff = 25, 
                                    offset=c(500,1000)), 
                priors = list(
                  rho0 = 100,
                  sig0 = 0.3
                )),
  setup4 = list(mesh = inla.mesh.2d(coords, 
                                    max.edge = c(100, 250), 
                                    cutoff = 25, 
                                    offset=c(500,1000)), 
                priors = list(
                  rho0 = 100,
                  sig0 = 0.3
                )),
  setup5 = list(mesh = inla.mesh.2d(coords, 
                                    max.edge = c(50, 100), 
                                    cutoff = 10, 
                                    offset=c(500,1000)),
                priors = list(
                  rho0 = 100,
                  sig0 = 0.3
                ))
)