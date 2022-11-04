input_path <- "Y:/WWT_Department/Projects/NextGen/Data-Work packages/WP2/QCRA/fcr/input"

siteName <- "germanMix"
pollutantNames <- c("as", "cd", "cr", "cu", "hg", "ni", "pb", "zn", "benzo", "pcdd")
pollutantNames <-  "cd"

output <- list()
for(pollutantName in pollutantNames){

  dat_0 <- kwb.fcr::read_fcr_input(input_path = input_path,
                                   pollutantName = pollutantName,
                                   siteName = siteName,
                                   fertilizerName = "none")

  # for longterm application -----------------------------------------------------
  fcr_out0 <- kwb.fcr::longterm_PEC(dat = dat_0$dat,
                                    info = dat_0$info,
                                    years = 1,
                                    nFields = 100000,
                                    use_mixing_factor = FALSE,
                                    PNECwater_c_i = TRUE,
                                    food_only = TRUE,
                                    growing_period = 180,
                                    t_res = 10,
                                    traceBackVariables = FALSE)

  output[[pollutantName]] <- fcr_out0
}

################################### Sorption ###################################

sName <- pollutantNames[1]
{
  print(sName)
  vName <- "K_d"
  v <- output[[sName]]$model_variables[,vName]
  v <- log10(v)
  stat <- summary(v)
  dev.new(noRStudioGD = TRUE, width = 5.36, height = 3.09)
  par(mar = c(4.1, 2.1, 2.1, 0.1))
  hist(x = v,
       col = rgb(0,112,150, maxColorValue = 255),
       breaks = 30, xlim = c(-2,8), main = "Sorption",
       xlab = bquote({log[10](K[d])}), border = "white",  yaxt = "n", ylab = "")
  abline(v = par("usr")[1])
  abline(h = par("usr")[3])
  mtext(text = "Frequency", side = 2, line = 0.4)
  legend("topright", legend = paste0(signif(stat, 2), " (",names(stat), ")"),
         bty = "n", cex = 1)
}


############################### Plant Uptake ###################################
{
  print(sName)
  vName <- "BCF"
  v <- output[[sName]]$model_variables[,vName]
  stat <- summary(v)
  dev.new(noRStudioGD = TRUE, width = 5.36, height = 3.09)
  par(mar = c(4.1, 2.1, 2.1, 0.1))
  hist(x = v,
       col = rgb(0,112,150, maxColorValue = 255),
       breaks = 500000, xlim = c(0,5), main = "Plant Uptake",
       xlab = bquote({BCF}), border = "white",  yaxt = "n", ylab = "")
  abline(v = par("usr")[1])
  abline(h = par("usr")[3])
  mtext(text = "Frequency", side = 2, line = 0.4)
  legend("topright", legend = paste0(signif(stat, 2), " (",names(stat), ")"),
         bty = "n")
}


############################### Atmospheric Deposition #########################
{
  print(sName)
  vName <- "D_air" # in mg/(kg*d)
  use_log <- FALSE
  v <- output[[sName]]$model_variables[,vName]
  v <- v * # to g/(ha*a)
    365 * # d/a
    1700 * # kg/m3
    2000 / # m3/ha
    1000 # mg/g

  if(use_log){
    v <- log10(v)
  }
  stat <- summary(v)
  dev.new(noRStudioGD = TRUE, width = 5.36, height = 3.09)
  par(mar = c(4.1, 2.1, 2.1, 0.1))
  hist(x = v,
       col = rgb(0,112,150, maxColorValue = 255),
       breaks = 200, main = "Atmospheric Deposition",
       xlab = bquote(Deposition~~(over(g, ha%*%a))), xlim = c(0, 0.002),
       border = "white", yaxt = "n", ylab = "")
  abline(v = par("usr")[1])
  abline(h = par("usr")[3])
  mtext(text = "Frequency", side = 2, line = 0.4)
  legend("topright", legend = paste0(signif(stat, 2), " (",names(stat), ")"),
         bty = "n", cex = 1)
}

quantile(v, 0.95)

############################### Volatilization rate ############################
sName <- pollutantNames[1]
{
  print(sName)
  vName <- "k_volat" # in mg/(kg*d)
  use_log <- TRUE
  v <- output[[sName]]$model_variables[,vName]

  if(use_log){
    v <- log10(v)
  }
  stat <- summary(v)
  dev.new(noRStudioGD = TRUE, width = 5.36, height = 3.09)
  par(mar = c(4.1, 2.1, 2.1, 0.1))
  hist(x = v,
       col = rgb(0,112,150, maxColorValue = 255),
       breaks = 30, main = "Volatilization rate",
       xlab = bquote(log[10]~(k[volat]~'in'~d^-1)),
       border = "white", yaxt = "n", ylab = "")
  abline(v = par("usr")[1])
  abline(h = par("usr")[3])
  mtext(text = "Frequency", side = 2, line = 0.4)
  legend("topleft", legend = paste0(signif(stat, 2), " (",names(stat), ")"),
         bty = "n", cex = 1)
}


############################### Biodegredation ############################
sName <- pollutantNames[1]
{
  print(sName)
  vName <- "DT50" # in mg/(kg*d)
  use_log <- FALSE
  v <- output[[sName]]$model_variables[,vName]

  if(use_log){
    v <- log10(v)
  }
  stat <- summary(v)
  dev.new(noRStudioGD = TRUE, width = 5.36, height = 3.09)
  par(mar = c(4.1, 2.1, 2.1, 0.1))
  hist(x = v,
       col = rgb(0,112,150, maxColorValue = 255), xlim = c(0,410000),
       breaks = 3000, main = "Biodegradation",
       xlab = bquote(Half-life~'in'~d),
       border = "white", yaxt = "n", ylab = "")
  abline(v = par("usr")[1])
  abline(h = par("usr")[3])
  mtext(text = "Frequency", side = 2, line = 0.4)
  legend("topright", legend = paste0(signif(stat, 2), " (",names(stat), ")"),
         bty = "n", cex = 1)
}

#################### initial concentration for groundwater simulation
sName <- pollutantNames[1]
{
  print(sName)
  vName <- "c_i"
  v <- output[[sName]]$model_variables[,vName]

  stat <- summary(v)
  dev.new(noRStudioGD = TRUE, width = 5.36, height = 3.09)
  par(mar = c(4.1, 2.1, 2.1, 0.1))
  hist(x = v,
       col = rgb(0,112,150, maxColorValue = 255),
       breaks = 100, xlim = c(0,2), main = "Initial Concentration",
       xlab = "Concentration in mg/kg soil", border = "white",  yaxt = "n", ylab = "")
  abline(v = par("usr")[1])
  abline(h = par("usr")[3])
  mtext(text = "Frequency", side = 2, line = 0.4)

  abline(v = output[[sName]]$model_variables[1,"PNEC_soil"],
         col = rgb(197, 210, 70, maxColorValue = 255), lwd = 3)
  text(x = output[[sName]]$model_variables[1,"PNEC_soil"], y = par("usr")[4]/2,
       labels = bquote(PNEC[Soil]), cex = 0.8, adj = c(-0.2, 1))
}


#################### pH ######
sName <- pollutantNames[1]
{
  print(sName)
  vName <- "pH"
  v <- output[[sName]]$model_variables[,vName]

  stat <- summary(v)
  dev.new(noRStudioGD = TRUE, width = 5.36, height = 3.09)
  par(mar = c(4.1, 2.1, 2.1, 0.1))
  hist(x = v,
       col = rgb(0,112,150, maxColorValue = 255),
       breaks = 20, xlim = c(2,12), main = "Soil pH-Value",
       xlab = "pH", border = "white",  yaxt = "n", ylab = "")
  abline(v = par("usr")[1])
  abline(h = par("usr")[3])
  mtext(text = "Frequency", side = 2, line = 0.4)
  legend("topright", legend = paste0(signif(stat, 2), " (",names(stat), ")"),
         bty = "n", cex = 1)
}

sName <- pollutantNames[1]
{
  print(sName)
  vName <- "f_oc"
  v <- output[[sName]]$model_variables[,vName] * 100

  stat <- summary(v)
  dev.new(noRStudioGD = TRUE, width = 5.36, height = 3.09)
  par(mar = c(4.1, 2.1, 2.1, 0.1))
  hist(x = v,
       col = rgb(0,112,150, maxColorValue = 255),
       breaks = 40, xlim = c(0,12), main = "Soil Organic Carbon",
       xlab = "Organic carbon content in %", border = "white",  yaxt = "n", ylab = "")
  abline(v = par("usr")[1])
  abline(h = par("usr")[3])
  mtext(text = "Frequency", side = 2, line = 0.4)
  legend("topright", legend = paste0(signif(stat, 2), " (",names(stat), ")"),
         bty = "n", cex = 1)
}

sName <- pollutantNames[1]
{
  print(sName)
  vName <- "rain"
  v <- output[[sName]]$model_variables[,vName] * 365 * 1000

  stat <- summary(v)
  dev.new(noRStudioGD = TRUE, width = 5.36, height = 3.09)
  par(mar = c(4.1, 2.1, 2.1, 0.1))
  hist(x = v,
       col = rgb(0,112,150, maxColorValue = 255),
       breaks = 40, xlim = c(0,3000), main = "Precipitation",
       xlab = "Yearly precipitation in mm/a", border = "white",  yaxt = "n", ylab = "")
  abline(v = par("usr")[1])
  abline(h = par("usr")[3])
  mtext(text = "Frequency", side = 2, line = 0.4)
  legend("topright", legend = paste0(signif(stat, 2), " (",names(stat), ")"),
         bty = "n", cex = 1)
}
