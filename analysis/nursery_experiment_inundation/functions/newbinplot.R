##### Title: the modified binplotfunction 
##### Author: James Margrove 
##### Date: 26 01 2018
require(arm)

newbinplot <-   function (x, y, nclass = NULL, xlab = "Expected Values", ylab = "Average residual", 
                          main = "", cex.pts = 0.8, col.pts = 1, 
                          col.int = "gray", ...)  {
  par(mfrow=c(1,3))
  n <- length(x)
  if (is.null(nclass)) {
    if (n >= 100) {
      nclass = floor(sqrt(length(x)))
    }
    if (n > 10 & n < 100) {
      nclass = 10
    }
    if (n <= 10) {
      nclass = floor(n/2)
    }
  }
  aa <<- data.frame(binned.resids(x, y, nclass)$binned) # result, global 
  plot(range(aa$xbar), range(aa$ybar, aa$X2se, -aa$X2se, na.rm = TRUE), 
       xlab = xlab, ylab = ylab, type = "n", main = "", ...)
  abline(0, 0, lty = 2)
  lines(aa$xbar, aa$X2se, col = col.int)
  lines(aa$xbar, -aa$X2se, col = col.int)
  points(aa$xbar, aa$ybar, pch = 19, cex = cex.pts, col = col.pts)
  lines(loess.smooth(x = aa$xbar, y = aa$ybar), col = "red") # additional line of code here for the loess smoother 
  plot(density(as.numeric(aa$ybar)), main = main) # density
  #qqnorm(aa$ybar, main = ""); qqline(aa$ybar) # qqnorm 
  car::qqPlot(aa$ybar, col = "black", pch = 19, cex = .75, grid = FALSE, ylab = "Sample Quantiles", xlab = "Theoretical Quantiles")
  par(mfrow=c(1,1))
}
