library(kwb.fcr)

## get parameters
norm95 <- function(par, distName){
  sampleData <- sapply(1:10, function(i){
    quantile(
      x = rdist(
        dist_name = distName,
        value_1 = par[1],
        value_2 = par[2],
        n = 1E05, seed = 1),
      probs = c(0.025, 0.975))
  }
  )
  test <- rowMeans(sampleData)
  vorgabe <- c(10,100)

  sum((test - vorgabe)^2)
}

#
distName <- "logderived"
lowerLimit <- c(0, 0.001)

par_out <- optim(par = c(5, 100),
                 fn = norm95,
                 distName = distName,
                 lower = lowerLimit,
                 method = "L-BFGS-B",
                 control = list(maxit = 1000))$par

v <- rdist(
  dist_name = distName,
  value_1 = par_out[1],
  value_2 = par_out[2],
  n = 1E07, shift = 0, seed = 1)
d <- density(v, bw = "SJ", adjust = 1)

{
  dev.new(noRStudioGD = TRUE, width = 4, height = 2.5)
  par(mar = c(2.1, 2.1, 0.1, 0.1))
  plot(x = d$x, y = d$y, type = "l",
       main = "",
       xlab = "",
       ylab = "",
       lwd = 3, xlim = c(-50,200),
       yaxs = "i",
       ylim = c(0, max(d$y) * 1.1),
       yaxt = "n",
       col = rgb(0, 112, 150, maxColorValue = 255))
  mtext(text = "Density", side = 2, line = 1)
  abline(v = 0, lty = "dashed", lwd = 2)
}




