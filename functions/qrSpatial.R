#' @author James Margrove
#' @description calculates a quantile regression without using spatialy autocorrelated samples 
#' @param data spatial data set with 
#' @param nsamp sample size from data default: 1/3 


qrSpatial <- function(data, nsamp = 1 / 3){
  rown <- sample(1:dim(data)[1],
    replace = T,
    size = as.integer(dim(data)[1] / nsamp))
  samp <- data[rown, ]
  coords_dist <- dist(cbind(samp$x, samp$y))
  dden_dist <- dist(samp$d)
  mt <- mantel.rtest(coords_dist, dden_dist, nrepet = 999)

  if (mt$pvalue > 0.05){
    qrmod <- rq(e ~ d, data = samp, tau = taus)
    p <- predict(qrmod,
      data.frame(d = seq(min(data$d), max(data$d),
      length = 100)))
    return(as.vector(p))
  }
}