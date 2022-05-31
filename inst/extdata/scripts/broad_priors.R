x <- seq(0.1,150, by = 0.1)

# no knowledge distribution (broad)
nkd_b <- dunif(x = x, min = 25, max = 125)

# no knowledge distribution (narrow)
nkd_n<- dunif(x = x, min = 30, max = 90)

# Case 1: same variance independent of mean value
# substance 1 distribution
s1d <- dnorm(x = x, mean = 50, sd = 5)
# substance 2 distribution
s2d <- dnorm(x = x, mean = 80, sd = 5)

# leading to a risky situation
risk_range <- c(0, 75)
# plot Distributions
{ # plot Distributions
  ymax <- max(c(nkd_b, nkd_n,s1d, s2d))
  plot(x = x, y = nkd_b, type = "l", ylim = c(0,ymax), lwd = 2,
       ylab = "Density", xlab = "Concentration [mg/kg]", lty = "twodash",
       main = "Concentration in Product")
  lines(x = x, y = nkd_n, col = "gray60", lwd = 2, lty = "twodash")
  lines(x = x, y = s1d, col = "steelblue", lwd = 2, lty = "dashed")
  lines(x = x, y = s2d, col = "orange", lwd = 2, lty = "dashed")
  rect(xleft = risk_range[1], xright = risk_range[2],
       ybottom = par("usr")[3], ytop = par("usr")[4], col = "red", density = 10)
  legend("topright", title = "Distributions",
         legend = c("No knoweldge (broad)", "No knoweldge (narrow)",
                    "Substance 1", "Substance 2"),
         lty = c(rep("twodash",2), rep("dashed", 2)), lwd = 2,
         col = c("black", "gray60", "steelblue", "orange"), cex = 0.8, bty = "n")
}

# under- and overestimation of probability for nkd - Broad
breaks <- seq(0,150, 10)

diff1 <- nkd_b - s1d
p_groups <- cut(x = x, breaks = breaks)
prob_diff <- round(sapply(unique(p_groups), function(y){
  sum(diff1[p_groups %in% y]) * 0.1
}) * 100, 2)

real_risk <- sum(s1d[x >= risk_range[1] & x <= risk_range[2]]) * 0.1 * 100
nkr_b <- sum(nkd_b[x >= risk_range[1] & x <= risk_range[2]]) * 0.1 * 100
nkr_n <- sum(nkd_n[x >= risk_range[1] & x <= risk_range[2]]) * 0.1 * 100
# underestimation of probability
{
  par(mar = c(4.1, 4.1, 4.1, 0.3))
  plot(x = x, y = diff1, type = "l", lwd = 2, col = "steelblue",
       lty = "dashed", xaxs = "i", xlab = "Substance Property",
       ylab = "Density Difference (to 'No knowledge - broad')")
  abline(h = 0)
  rect(xleft = risk_range[1], xright = risk_range[2],
       ybottom = par("usr")[3], ytop = par("usr")[4], col = "red", density = 10)

  cols <- sapply(abs(prob_diff)/100, function(value){
    rgb(red = 1 - value * 0.1, green = 1 - value, blue = 1 - value)
  })
  rect(xleft = breaks[1:(length(breaks)-1)],
       xright = breaks[2:length(breaks)],
       ybottom = par("usr")[4] + 1 * par("cxy")[2],
       ytop = par("usr")[4] + 2 * par("cxy")[2],
       col = cols, xpd = TRUE)
  text(x =  breaks + unique(abs(diff(breaks))) * 0.5,
       y = par("usr")[4] + 2 * par("cxy")[2],
       labels = paste0(round(prob_diff,0), "%"), pos = 3, xpd = TRUE, cex = 0.8)
  text(x =  par("usr")[1] - 2.5 * par("cxy")[1],
       y = par("usr")[4] + 2 * par("cxy")[2],
       labels = "Probability\nDifference", pos = 3, xpd = TRUE, cex = 0.8)
  legend("bottomright", legend = c(
    paste0("Substance Data -> ", round(real_risk,1), "%"),
    paste0("No Knowledge, broad -> ", round(nkr_b,1), "%")), cex = 0.8,
    title = "Risk Probability")
}

# under- and overestimation of probability for nkd - narrow
breaks <- seq(0,150, 10)

diff <- nkd_n - s1d
p_groups <- cut(x = x, breaks = breaks)
prob_diff <- round(sapply(unique(p_groups), function(y){
  sum(diff[p_groups %in% y]) * 0.1
}) * 100, 2)

real_risk <- sum(s1d[x >= risk_range[1] & x <= risk_range[2]]) * 0.1 * 100
nkr_b <- sum(nkd_b[x >= risk_range[1] & x <= risk_range[2]]) * 0.1 * 100
nkr_n <- sum(nkd_n[x >= risk_range[1] & x <= risk_range[2]]) * 0.1 * 100
# underestimation of probability
{
  par(mar = c(4.1, 4.1, 4.1, 0.3))
  plot(x = x, y = diff, type = "l", lwd = 2, col = "steelblue",
       lty = "dashed", xaxs = "i", xlab = "Substance Property",
       ylab = "Density Difference (to 'No knowledge - broad')")
  abline(h = 0)
  rect(xleft = risk_range[1], xright = risk_range[2],
       ybottom = par("usr")[3], ytop = par("usr")[4], col = "red", density = 10)

  cols <- sapply(abs(prob_diff)/100, function(value){
    rgb(red = 1 - value * 0.1, green = 1 - value, blue = 1 - value)
  })
  rect(xleft = breaks[1:(length(breaks)-1)],
       xright = breaks[2:length(breaks)],
       ybottom = par("usr")[4] + 1 * par("cxy")[2],
       ytop = par("usr")[4] + 2 * par("cxy")[2],
       col = cols, xpd = TRUE)
  text(x =  breaks + unique(abs(diff(breaks))) * 0.5,
       y = par("usr")[4] + 2 * par("cxy")[2],
       labels = paste0(round(prob_diff,0), "%"), pos = 3, xpd = TRUE, cex = 0.8)
  text(x =  par("usr")[1] - 2.5 * par("cxy")[1],
       y = par("usr")[4] + 2 * par("cxy")[2],
       labels = "Probability\nDifference", pos = 3, xpd = TRUE, cex = 0.8)
  legend("bottomright", legend = c(
    paste0("Substance Data -> ", round(real_risk,1), "%"),
    paste0("No Knowledge, narrow -> ", round(nkr_n,1), "%")), cex = 0.8,
    title = "Risk Probability")
}


diff <- nkd_b - s2d
p_groups <- cut(x = x, breaks = breaks)
prob_diff <- round(sapply(unique(p_groups), function(y){
  sum(diff[p_groups %in% y]) * 0.1
}) * 100, 2)

real_risk <- sum(s2d[x >= risk_range[1] & x <= risk_range[2]]) * 0.1 * 100
nkr_b <- sum(nkd_b[x >= risk_range[1] & x <= risk_range[2]]) * 0.1 * 100
nkr_n <- sum(nkd_n[x >= risk_range[1] & x <= risk_range[2]]) * 0.1 * 100
# underestimation of probability
{
  par(mar = c(4.1, 4.1, 4.1, 0.3))
  plot(x = x, y = diff, type = "l", lwd = 2, col = "steelblue",
       lty = "dashed", xaxs = "i", xlab = "Substance Property",
       ylab = "Density Difference (to 'No knowledge - broad')")
  abline(h = 0)
  rect(xleft = risk_range[1], xright = risk_range[2],
       ybottom = par("usr")[3], ytop = par("usr")[4], col = "red", density = 10)

  cols <- sapply(abs(prob_diff)/100, function(value){
    rgb(red = 1 - value * 0.1, green = 1 - value, blue = 1 - value)
  })
  rect(xleft = breaks[1:(length(breaks)-1)],
       xright = breaks[2:length(breaks)],
       ybottom = par("usr")[4] + 1 * par("cxy")[2],
       ytop = par("usr")[4] + 2 * par("cxy")[2],
       col = cols, xpd = TRUE)
  text(x =  breaks + unique(abs(diff(breaks))) * 0.5,
       y = par("usr")[4] + 2 * par("cxy")[2],
       labels = paste0(round(prob_diff,0), "%"), pos = 3, xpd = TRUE, cex = 0.8)
  text(x =  par("usr")[1] - 2.5 * par("cxy")[1],
       y = par("usr")[4] + 2 * par("cxy")[2],
       labels = "Probability\nDifference", pos = 3, xpd = TRUE, cex = 0.8)
  legend("bottomright", legend = c(
    paste0("Substance Data -> ", round(real_risk,1), "%"),
    paste0("No Knowledge, broad -> ", round(nkr_b,1), "%")), cex = 0.8,
    title = "Risk Probability")
}

# under- and overestimation of probability for nkd - narrow
diff <- nkd_n - s2d
p_groups <- cut(x = x, breaks = breaks)
prob_diff <- round(sapply(unique(p_groups), function(y){
  sum(diff[p_groups %in% y]) * 0.1
}) * 100, 2)

# underestimation of probability
{
  par(mar = c(4.1, 4.1, 4.1, 0.3))
  plot(x = x, y = diff, type = "l", lwd = 2, col = "steelblue",
       lty = "dashed", xaxs = "i", xlab = "Substance Property",
       ylab = "Density Difference (to 'No knowledge - broad')")
  abline(h = 0)
  rect(xleft = risk_range[1], xright = risk_range[2],
       ybottom = par("usr")[3], ytop = par("usr")[4], col = "red", density = 10)

  cols <- sapply(abs(prob_diff)/100, function(value){
    rgb(red = 1 - value * 0.1, green = 1 - value, blue = 1 - value)
  })
  rect(xleft = breaks[1:(length(breaks)-1)],
       xright = breaks[2:length(breaks)],
       ybottom = par("usr")[4] + 1 * par("cxy")[2],
       ytop = par("usr")[4] + 2 * par("cxy")[2],
       col = cols, xpd = TRUE)
  text(x =  breaks + unique(abs(diff(breaks))) * 0.5,
       y = par("usr")[4] + 2 * par("cxy")[2],
       labels = paste0(round(prob_diff,0), "%"), pos = 3, xpd = TRUE, cex = 0.8)
  text(x =  par("usr")[1] - 2.5 * par("cxy")[1],
       y = par("usr")[4] + 2 * par("cxy")[2],
       labels = "Probability\nDifference", pos = 3, xpd = TRUE, cex = 0.8)
  legend("bottomright", legend = c(
    paste0("Substance Data -> ", round(real_risk,1), "%"),
    paste0("No Knowledge, narrow -> ", round(nkr_n,1), "%")), cex = 0.8,
    title = "Risk Probability")
}
