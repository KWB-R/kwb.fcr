# nickel bio concentration factor. data from Kühne and Goldbach 2004
# Pages 70 and 79
asd <- data.frame(
  "cs" = c(4.41, 3.18, 10.3, 8.12, 4.53, 4.81, 13.1, 4.81),
  "cp" = c(0.14, 0.29, 0.22, 0.19, 0.27, 0.27, 0.23, 0.23),
  "plant" = c(rep("wheat", 3), "maize", rep("wheat", 3), "maize"))

asd$bcf <- asd$cp / asd$cs

asd$cp_log <- log10(asd$cs)
asd$bcf_log <- log10(asd$bcf)
reg <- lm(bcf_log ~ cp_log, asd)

{
  dev.new(noRStudioGD = TRUE, width = 5.36, height = 3.09)
  par(mar = c(4.1, 4.1, 0.2, 0.2))
  plot(x = asd$cp_log, y = asd$bcf_log, pch = 19, cex = 1.3,
       xlab = bquote(log[10]~(c[soil])),
       col = rgb(0,112,150, maxColorValue = 255),
       ylab = bquote(log[10]~(BCF)), xlim = log10(c(2,30)), ylim = c(-2,-1))
  abline(reg, lwd = 2, lty = "dotted")
}
summary(reg)


## plot concentrations
m <- mean(asd$cp)
yRange <- m + m * c(-1, 1)
m <- mean(asd$cs)
xRange <- m + m * c(-1, 1)

{
  dev.new(noRStudioGD = TRUE, width = 5.36, height = 3.09)
  par(mar = c(4.1, 4.1, 0.2, 0.2))
  plot(y = asd$cp, x = asd$cs,
       ylim = yRange,
       xlim = xRange, pch = 19, cex = 1.3,
       col = rgb(0,112,150, maxColorValue = 255),
       xlab = bquote(c[soil]~mg/kg),
       ylab = bquote(c[plant]~mg/kg~dw))
}


###############################################################################
# Pb
asd <- data.frame(
  "cs" = c(17.3, 17.7, 29.1, 23.6, 36.7, 47.1, 67.3, 47.1),
  "cp" = c(0.11, 0.15, 0.26, 0.07, 0.08, 0.17, 0.07, 0.1),
  "plant" = c(rep("wheat", 3), "maize", rep("wheat", 3), "maize"))


###############################################################################
# Zn
asd <- data.frame(
  "cs" = c(32.4, 32.9, 44.1, 40.8, 77.3, 19.1, 26.6, 19.1),
  "cp" = c(24.8, 30.9, 17.8, 22.3, 34.2, 22.5, 17.4, 21),
  "plant" = c(rep("wheat", 3), "maize", rep("wheat", 3), "maize"))

asd$bcf <- asd$cp / asd$cs

asd$cp_log <- log10(asd$cs)
asd$bcf_log <- log10(asd$bcf)
reg <- lm(bcf_log ~ cp_log, asd)

{
  dev.new(noRStudioGD = TRUE, width = 5.36, height = 3.09)
  par(mar = c(4.1, 4.1, 0.2, 0.2))
  plot(x = asd$cp_log, y = asd$bcf_log, pch = 19, cex = 1.3,
       xlab = bquote(log[10]~(c[soil])),
       col = rgb(0,112,150, maxColorValue = 255),
       ylab = bquote(log[10]~(BCF)), xlim = log10(c(10,80)), ylim = c(-0.7,0.2))
  abline(reg, lwd = 2, lty = "dotted")
}
summary(reg)


## plot concentrations
m <- mean(asd$cp)
yRange <- m + m * c(-1, 1)
m <- mean(asd$cs)
xRange <- m + m * c(-1, 1)

{
  dev.new(noRStudioGD = TRUE, width = 5.36, height = 3.09)
  par(mar = c(4.1, 4.1, 0.2, 0.2))
  plot(y = asd$cp, x = asd$cs,
       ylim = yRange,
       xlim = xRange, pch = 19, cex = 1.3,
       col = rgb(0,112,150, maxColorValue = 255),
       xlab = bquote(c[soil]~mg/kg),
       ylab = bquote(c[plant]~mg/kg~dw))
}

