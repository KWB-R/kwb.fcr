df_risk <- output[[1]]
colnames(df_risk)

hist(df_risk$FertRisk)
asd <- cor(df_risk, method = "spearman")
round(sort(asd[,"FertRisk"]), 2)


var_x <- "K_d"
var_y <- "DT50"
var_col <- "FertRisk"

x <- df_risk[[var_x]]
y <- df_risk[[var_y]]
y <- y  * # to g/(ha*a)
  365 * # d/a
  1700 * # kg/m3
  2000 / # m3/ha
  1000 # mg/g
z <- df_risk[[var_col]]


xBreaks <- quantile(x = x, probs = seq(0,1,0.05))
yBreaks <- quantile(x = y, probs = seq(0,1,0.05))

plot(x = x , y = y ,
     type = "n", xaxs = "i", yaxs = "i",
     ylim = range(yBreaks[1:(length(yBreaks) - 1)]),
     xlim = range(xBreaks[1:(length(xBreaks)-1)]), xlab = "pH", ylab = "Initial concentration in soil in mg/kg")
abline(v = xBreaks)
abline(h = yBreaks)

xCuts <- cut(x = x, breaks = xBreaks, include.lowest = TRUE)
yCuts <- cut(x = y, breaks = yBreaks, include.lowest = TRUE)





for(i in seq_along(z)){
  alpha <- floor(z[i])
  xLeft <- as.numeric(xCuts[i])
  yBottom <- as.numeric(yCuts[i])

  rect(xleft = xBreaks[xLeft], xright = xBreaks[xLeft + 1],
       ybottom = yBreaks[yBottom], ytop = yBreaks[yBottom + 1],
       col = rgb(red = 192, green = 0, blue = 0, alpha = alpha, maxColorValue = 255),
       border = NA)

}


sum(z < 1)
summary(x[z < 1])
summary(x[z > 1])

############
boxplot(x[z <= 1], x[z > 1], col = rgb(0, 102, 157, maxColorValue = 255),
        outline = FALSE, range = 10, xlab = "",
        ylab = "pH")
mtext(text = c("RQ < 1 ", "RQ \u2265 1 "), side = 1, at = c(1, 2), line = 2)
mtext(text = c("\u0394 RQ < 1 ", "\u0394 RQ \u2265 1 "), side = 1, at = c(1, 2), line = 2)


x <- df_risk[["pH"]]
xquant <- round(quantile(x, probs = c(0.1, 0.9)), 1)
x_min <- seq(xquant[1], xquant[2], by = 0.01)

i <- 1

rq <- df_risk[["FertRisk"]]

y <- c()
for(i in seq_along(x_min)){
  rq_x <- rq[x >= x_min[i]]

  q_relevant <- quantile(rq_x, c(0.95))
  y <- c(y, mean(rq_x[rq_x > q_relevant]))
}
x_min[115]

par(mar = c(4.1, 4.1, 0.5, 0.5))
plot(x = x_min, y = y, type = "l", lwd = 1,
     col = rgb(0, 102, 157, maxColorValue = 255),
     ylab = bquote(RQ[max]~'for'~pH~higher~as~minimal~ph), xlab = "minimal pH")
lines(x = x_min[1:115], y = y[1:115],  lwd = 3,
      col = rgb(164, 176, 43, maxColorValue = 255))
lines(x = x_min[115:length(x_min)], y = y[115:length(x_min)],  lwd = 3,
      col = rgb(0, 102, 157, maxColorValue = 255))

lines(x = c(0, x_min[115]), y = c(2,2), lwd = 2, lty = "dotted")
lines(x = c(x_min[115],x_min[115]), y = c(0, 2), lwd = 2, lty = "dotted")
mtext(text = x_min[115], side = 1, line = 1, at = x_min[115])


inc_ph <- order(x)

high_rq <- cumsum(as.numeric(rq[inc_ph] > 2))

plot(x = x[inc_ph], y = high_rq )
