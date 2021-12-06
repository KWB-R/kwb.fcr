#' Plot cumulative sum of RQ after application and compare it with initial state
#'
#' @param v0 Vector of initial risk quotient distribution
#' @param v Vector of final risk quotient distribution of x years
#' @param year_x Numarical value of the last year of the risk assessment
#' @param xmin Minimum value of x-Axis
#' @param xmax Maximumg value of x-Axis
#'
#' @return
#' Cumulative sums are plotted and the underlying table of quantiles of the
#' initial and final risk quotients
#'
#' @export
#' @importFrom stats quantile
#' @importFrom graphics abline axis lines legend
#' @importFrom grDevices rgb
#'
CumSumSoil<- function(
  v0, v, year_x, xmin = 0.001, xmax = 10)
{

  quants0 <- quantile(x = v0, probs =seq(0.00, 1, by = 0.01))
  quants <- quantile(x = v, probs =seq(0.00, 1, by = 0.01))

  plot(x = quants, y = 100:0, type = "n", ylim = c(0,100),
       xlab = "Risk Quotient (PEC/PNEC)",
       ylab = "Cumulative Sum [%]", lwd = 2, xlim = c(xmin,xmax), xaxs = "i",
       yaxs = "i", col = "black", log = "x", axes = FALSE)

  # background colors
  rect(xleft = 1, xright = xmax, ybottom = 0, ytop = 100,
       border = FALSE, col = rgb(218,96,96, maxColorValue = 255))
  rect(xleft = 0.01, xright = 1, ybottom = 0, ytop = 100,
       border = FALSE, col = rgb(242,206,142, maxColorValue = 255))
  rect(xleft = xmin, xright = 0.01, ybottom = 0, ytop = 100,
       border = FALSE, col = rgb(169,209,142,  maxColorValue = 255))
  abline(v = rep(x = 1:10, 11) * rep(x = 10^seq(-5,5,1), each = 10),
         col = "gray60", lty = "dashed")
  # axis
  axis(1, at = c(0.0001,0.001, 0.01, 0.1, 1, 10, 100, 1000),
       labels = c(0.0001,0.001, 0.01, 0.1, 1, 10, 100, 1000))
  axis(2, at = c(0, 20, 40, 60, 80,100), labels = c(0, 20, 40, 60, 80,100), las = 2)

  # data
  lines(x = quants0, y = 100:0, col = "black", lwd = 2, lty = "dotted")
  lines(x = quants, y = 100:0, col = "steelblue", lwd = 2, lty = "solid")

  legend(x = "bottomleft",
         legend = c("Background (t=0)",
                    paste0("Fert. application (t=", year_x, ")")),
         lwd = 2, col = c("black", "steelblue"),
         lty = c("dotted", "solid"),
         cex = 0.8, seg.len = 3, bty = "n")

  data.frame("quantile_initial" = quants0, "quantiles_end" = quants)
}

#' Plot cumulative sum of RQ after application and compare it with initial state
#'
#' @param mat_xRow A matrix where every row is a distribution for one x value
#' @param ymin Minimum value of y-Axis
#' @param ymax Maximumg value of y-Axis
#' @param resolution The widths of quantiles used for shading (default is 1%)
#' @param xlab,ylab,main Óptional definitions of axis and title
#'
#' @return
#' A plot with overlapping transperant quantiles
#'
#' @export
#' @importFrom stats quantile
#' @importFrom graphics polygon lines
#' @importFrom grDevices rgb
#'
shadingPlot <- function(
  mat_xRow, ymin = 0, ymax = max(mat_xRow), resolution = 0.01,
  xlab = "", ylab = "", main = ""
){

  x_values <- nrow(mat_xRow)
  # calculation of quantiles
  quants <- apply(mat_xRow, 1, quantile, seq(0, 1, by = resolution))
  nQuants <- nrow(quants)
  Tcol <- rgb(0,130,188, alpha = (1000/nQuants + 2), maxColorValue = 255)

  plot(x = 1:x_values, y = 1:x_values, type = "n", ylim = c(ymin, ymax),
       xlab = xlab, ylab = ylab, main = main)
  # plot quantiles
  for(i in 0:(nQuants %/% 2)){
    polygon(x = c(1:x_values, x_values:1),
            y = c(quants[1+i,], rev(quants[nQuants - i,])),
            col = Tcol, border = NA)
  }
  # plot median
  lines(x = 1:x_values, y = quants[(nQuants %/% 2) + 1,],
        col = rgb(0,130,188, maxColorValue = 255), lwd = 1)
}
