# Calculating the confidence intervals from the posterior sampling in INLA SPDE

post_sample <- read.table('./SRF_spatial_analysis_elevation/SRF_spatial_analysis_elevation/post.samp5000.txt', header = TRUE)

# calculate the Confidence intervals
CI <- apply(as.matrix(post_sample), 1, quantile, c(0.025, 0.975))
save(CI, file = './analysis/adult_distribution_analysis/data/elevation_distribution_CI_NEW.R')

