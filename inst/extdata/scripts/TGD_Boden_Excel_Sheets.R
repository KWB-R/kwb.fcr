library(readxl)
library(dplyr)
FertID <- data.frame(
  "ID" = c(1:10),
  "Name" = c("Entw. Kl?rschlamm", "Struvite aus Schlamm",
             "Struvite aus Schlammwasser", "Schlammasche", "AshDec",
             "Minerald?nger techn. P-S?ure", "Rohphosphat", "Minerald?nger",
             "Decadmierter Minerald.", "Nur Deposition"),
  "Eng" = c("dewatered sewage sludge", "struvite from sludge",
            "struvite from sludge water", "sludge ash", "AshDec - treated",
            "mineral fertilizer (technical P-acid)", "phosphate rock",
            "mineral fertilizers", "mineral fertilizers (decadmiation)",
            "deposition only"),
  "Color" = c(hsv(h = c(seq(0.5514184,1, 0.1), seq(0.0514184,0.55, 0.1))[1:9],
                  s = c(1,1,0.8),
                  v = c(0.7372549,1)),
              hsv(0,0,0)),
  "lineType" = c(rep("solid", 9), "dashed"),
  stringsAsFactors = FALSE)

comps <- data.frame("comp" = c("soil", "leach", "human"),
                    "deutsch" = c("Bodenorganismen", "Grundwasser", "Mensch"),
                    "english" = c("Soil organisms", "groundwater", "human"),
                    stringsAsFactors = FALSE)

if(FALSE){
  ##############################################################################
  ################################ Daten Laden #################################
  ##############################################################################
  Sub <- read_excel(path = "Y:/AUFTRAEGE/UFO-Phorw?rts/Data-Work packages/Risikomodell/R-Skript/Substance_Sheet.xlsx",
                    sheet = "Input", col_names = TRUE, na = "NA")

  Env <- read_excel(path = "Y:/AUFTRAEGE/UFO-Phorw?rts/Data-Work packages/Risikomodell/R-Skript/Environment_Sheet.xlsx",
                    sheet = "Input", col_names = TRUE, na = "NA")
  ########################## Process Data ########################################
  Substance <- "Benzo"
  HighestFert(Sub = Sub, Env = Env, Substance = Substance)
  Fertilizer <- 1
  start_with_0 <- FALSE
  K_d <- "other"
  BCF <- "other"
  vPNEC_organisms <- F # is the PNEC leachate variable? (QSAR for organic pollutants)

  a <- TGD_model(Sub = Sub, Env = Env, runs = 100, years = 10,
                 Fertilizer = Fertilizer, vPNEC_organisms = vPNEC_organisms,
                 Substance = Substance, K_d = K_d, BCF = BCF, t0 = F,
                 OutputLite = T, detailed_year = 1)


  # Degredation Plots ---------------------------------------------------------
  for(Substance in c("CBZ", "CPF", "DCF", "ETD", "ETE", "SMX", "BZF", "LVF", "MTP", "CFX", "CTC")){
    dev.new(noRStudioGD = TRUE, width = 8, height = 5)
    par(mar = c(4.1, 4.1,3,1), las = 0)


    DegredationPlot(Sub = Sub, Env = Env, years = 10,
                    vPNEC_organisms = TRUE, Substance = Substance)

    savePlot(
      filename =
        paste0("Y:/AUFTRAEGE/UFO-Phorw?rts/Data-Work packages/Risikomodell/Results/Plots/",
               Substance, "_degredation"), type = "wmf")
    dev.off()
  }








  cor(x = c(a$year_1$Input$K_d[1], a$year_2$Input$K_d[1], a$year_3$Input$K_d[1],
            a$year_4$Input$K_d[1], a$year_5$Input$K_d[1], a$year_6$Input$K_d[1]), y = a$c30_leach[1:6,1])

  summary(unlist(a$c30_leach[1,]))
  summary(unlist(a$c30_leach[2,]))
  summary(unlist(a$c30_leach[3,]))
  summary(a$year_1$Input$c_0)
  cor(x = a$year_1$Input$log_K, y = a$year_1$Input$K_d)

  apply(a$c30, 1, mean)
  cor(x = unlist(a$c30[10,]), y = a$year_1$Input$c_0)

  polyplot(input = a$c30_leach, years = 3, ymax = 0.2)



  vioplot(input = a$c30_leach, years = c(1,2,3,5,10), color = "black", ymax = 50, corpusFactor_violin = 1)







}
######################### Functions ############################################
TGD_model <- function(
  Sub = Sub,
  Env = Env,
  Substance,
  Fertilizer = 1,
  runs = 10000, # runs used for Monte-Carlo-Simulation
  years = 10, # Number of years investigated
  K_d = "adapt2", # K_d - value is adapted using pH and f_oc
  BCF = "adapt2", # BCF - value is adapted using pH and f_oc
  OutputLite = FALSE, # TRUE only returns PEC values --> necessary if many iterations are used
  detailed_year = years, # one year where input and output details are returned for worst case analysis
  t0 = FALSE, # return of the initial situation without any fertizer application?
  show_progress = FALSE,
  set_initial_concentration = FALSE, # PNEC for soil organisms can be set as inital concentration with "PNEC"
  sensPEC = FALSE, # if True most sensible Compartment is calculated (by median values)
  vPNEC_organisms = FALSE # if "TRUE" PNEC_soilorgasnisms is calculated with PNEC water, K_OC and Organic carbon content (QSAR)
)
{
  if(show_progress == TRUE){
    # create process report data
    progress <- round(seq(1, 100, length.out = years),1)
  }

  # combine substance dependend and environmental data
  Sub <- Sub[,-c(2,3)] # this columns are not important for the function
  Sub <- Sub[,c(1,grep(pattern = Substance, x = colnames(Sub), ignore.case = F))]
  Env <- Env[,c(1,4:8)]
  colnames(Sub) <- colnames(Env)
  Input <- rbind(Env, Sub)
  Input$pID <- 1:nrow(Input)

  # select fertilizer
  all_fert <- which(grepl(pattern = "_fert_", x = Input$Parameter))
  fert_selected <- which(Input$Parameter == paste0("c_fert_", Fertilizer) |
                           Input$Parameter == paste0("p_fert_", Fertilizer))
  Input$Parameter[fert_selected] <-
    gsub(pattern = paste0("_", Fertilizer), x = Input$Parameter[fert_selected], replacement = "")
  remaining <- all_fert[!(all_fert %in% fert_selected)]
  Input <- Input[-remaining,]

  # function to truncate normal distribution
  rtnorm <- function(n, mean, sd, a = -Inf, b = Inf){
    qnorm(p = runif(n = n,
                    min = pnorm(q = a, mean = mean, sd = sd),
                    max = pnorm(q = b, mean = mean, sd = sd)),
          mean = mean, sd = sd)
  }

  # initial concentration in soil ----------------------------------------------
  # (not part of the loop, only at the Beginning important)
  init <- which(Input[,1] == "c_i")

  if(set_initial_concentration == "PNEC"){
    c_i <- unlist(Input[which(Input[,1] == "PNEC_organisms"), 2])
  } else {

    if(Input$Distribution[init] == "none"){
      c_i <- rep(Input$Value_1[init], runs)
    } else if(Input$Distribution[init] == "uniform"){
      set.seed(0)
      c_i <- runif(n = runs,
                   min = min(Input[init,2:3], na.rm = TRUE),
                   max = max(Input[init,2:3], na.rm = TRUE))
    } else if(Input$Distribution[init] == "normal"){
      set.seed(0)
      c_i <- rnorm(n = runs, mean = Input$Value_1[init],
                   sd = Input$Value_2[init])
    } else if(Input$Distribution[init] == "tnormal"){
      set.seed(0)
      c_i <- rtnorm(n = runs, mean = Input$Value_1[init],
                    sd = Input$Value_2[init], a = 0)
    } else if(Input$Distribution[init] == "lognormal"){
      set.seed(0)
      c_i <- rlnorm(n = runs, meanlog = Input$Value_1[init],
                    sdlog = Input$Value_2[init])
    } else if(Input$Distribution[init] == "gamma"){
      set.seed(0)
      c_i <- rgamma(n = runs, shape = Input$Value_1[init],
                    rate = Input$Value_2[init])
    }
    c_i <- c_i + Input$shift[init]
  }
  # delete from Input table, so it is not part of the loop
  Input <- Input[-init,]
  # ----------------------------------------------------------------------------
  # worst case szenarios are treated as no distribution --> first value is used
  # for calculations
  Input$Distribution[Input$Distribution == "case"] <- "none"

  # devide data into distribution groups
  InputList <- list()
  InputList$const <- Input[Input$Distribution == "none" & !is.na(Input$Value_1), 1:2]
  InputList$unif <- Input[Input$Distribution == "uniform" & !is.na(Input$Value_1), c(1:4,6,7)]
  InputList$tnorm <- Input[Input$Distribution == "tnormal" & !is.na(Input$Value_1), c(1:4,6,7)]
  InputList$norm <- Input[Input$Distribution == "normal" & !is.na(Input$Value_1), c(1:4,6,7)]
  InputList$logn <- Input[Input$Distribution == "lognormal" & !is.na(Input$Value_1), c(1:4,6,7)]
  InputList$gamm <- Input[Input$Distribution == "gamma" & !is.na(Input$Value_1), c(1:4,6,7)]
  InputList$names <- c(InputList$const[[1]], InputList$unif[[1]],
                       InputList$tnorm[[1]], InputList$norm[[1]],
                       InputList$logn[[1]], InputList$gamm[[1]])

  # empty list for Monte-Carlo Simulation
  MCS <- list()
  # empty data frame for average concentration
  # of first 180 days (-> humans)
  # of first 30 days (-> Organisms and Groundwater)
  MCS$c180 <- data.frame(matrix(nrow = years, ncol = runs))
  MCS$human <- data.frame(matrix(nrow = years, ncol = runs))
  MCS$c30 <- data.frame(matrix(nrow = years, ncol = runs))
  MCS$c30_leach <- data.frame(matrix(nrow = years, ncol = runs))

  for(year in 1:years){

    if(show_progress == TRUE){
      rect(xleft = 0, xright = progress[year],
           ybottom = 11 - Fertilizer - 0.4, ytop = 11 - Fertilizer + 0.4,
           col = rgb(0,130,188, maxColorValue = 255), border = FALSE)
      rect(xright = 100, xleft = progress[year],
           ybottom = 11 - Fertilizer - 0.4, ytop = 11 - Fertilizer + 0.4,
           col = "white", border = FALSE)
      text(x = 50, y = 11 - Fertilizer, labels = paste0(progress[[year]], " %"))
    }

    # create Monte-Carlo-data --> data.frame with n rows,
    # constant Values without distribution
    p <- data.frame(
      matrix(data = rep(InputList$const[[2]], times = 1, each = runs),
             nrow = runs, ncol = nrow(InputList$const), byrow = FALSE))

    # seed depends on the first value of each parameter to prevent correlations
    # between parameters (times  10 000 to get different integers)

    # uniform distributed values
    if(nrow(InputList$unif) > 0){
      Unif <- apply(InputList$unif[,c(2,3,5,6)], 1,
                    function(x){
                      if(x[[3]] == "constant"){
                        set.seed(as.numeric(x[[4]]))
                      } else {
                        rm(.Random.seed, envir=globalenv())
                      }
                      runif(n = runs,
                            min = min(as.numeric(c(x[[1]], x[[2]]))),
                            max = max(as.numeric(c(x[[1]], x[[2]]))))})

      Unif <- t(Reduce(f = "+", x = list(t(Unif), InputList$unif[[4]])))

      p <- cbind(p, Unif)
    }

    # truncated normal distributed values (only positive values)
    if(nrow(InputList$tnorm) > 0){

      TNorm <- apply(InputList$tnorm[,c(2,3,5,6)], 1,
                     function(x){
                       if(x[[3]] == "constant"){
                         set.seed(as.numeric(x[[4]]))
                       } else {
                         rm(.Random.seed, envir=globalenv())
                       }
                       rtnorm(n = runs,
                              mean = as.numeric(x[[1]]),
                              sd = as.numeric(x[[2]]),
                              a = 0)})
      TNorm <- t(Reduce(f = "+", x = list(t(TNorm), InputList$tnorm[[4]])))
      p <- cbind(p, TNorm)
    }

    # normal distributed values
    if(nrow(InputList$norm) > 0){
      Norm <- apply(InputList$norm[,c(2,3,5,6)], 1,
                    function(x){
                      if(x[[3]] == "constant"){
                        set.seed(as.numeric(x[[4]]))
                      } else {
                        rm(.Random.seed, envir=globalenv())
                      }
                      rnorm(n = runs,
                            mean = as.numeric(x[[1]]),
                            sd = as.numeric(x[[2]]))})
      Norm <- t(Reduce(f = "+", x = list(t(Norm), InputList$norm[[4]])))
      p <- cbind(p, Norm)

    }

    # lognormal distributed values
    if(nrow(InputList$logn) > 0){
      Logn <- apply(InputList$logn[,c(2,3,5,6)], 1,
                    function(x){if(x[[3]] == "constant"){
                      set.seed(as.numeric(x[[4]]))
                    } else {
                      rm(.Random.seed, envir=globalenv())
                    }
                      rlnorm(n = runs,
                             meanlog = as.numeric(x[[1]]),
                             sdlog = as.numeric(x[[2]]))})
      Logn <- t(Reduce(f = "+", x = list(t(Logn), InputList$logn[[4]])))
      p <- cbind(p, Logn)
    }

    # gamma distributed values
    if(nrow(InputList$gamm) > 0){
      Gamm <- apply(InputList$gamm[,c(2,3,5,6)], 1,
                    function(x){
                      if(x[[3]] == "constant"){
                        set.seed(as.numeric(x[[4]]))
                      } else {
                        rm(.Random.seed, envir=globalenv())
                      }
                      rgamma(n = runs,
                             shape = as.numeric(x[[1]]),
                             rate = as.numeric(x[[2]]))})
      Gamm <- t(Reduce(f = "+", x = list(t(Gamm), InputList$gamm[[4]])))
      p <- cbind(p, Gamm)
    }

    colnames(p) <- InputList$names
    # --------------------------------------------------------------------------

    # add fertilizer
    if(t0 == TRUE){
      p$c_add <- 0
    } else {
      p$c_add <- (p$c_fert * p$p_app) / (p$d * p$rho_soil * 10000)
    }

    # add end concentration of previous year
    p$c_0 <- p$c_add + c_i

    # calculation mixing factor
    p$MF <- 1 + (p$v_G * p$m_d / (p$rain * 365 * p$f_inf * p$l_field))

    # calculation of the remaining parameter: k and Deposition
    # k_volat and k_leach and k_plant
    if(!("k_volat" %in% colnames(p) &
         "k_leach" %in% colnames(p))){

      if(!("K_SoilWater" %in% colnames(p))) {
        # pH and c_org as variables
        if(K_d == "adapt1") {
          p$K_d <- 10^(p$log_K +
                         p$c_pH * p$pH +
                         p$c_oc * log10(p$f_oc * 100) +
                         p$c_c * log10(p$c_0))
        } else if(K_d == "adapt2"){
          p$c_water <-
            10^(p$log_K +
                  p$c_pH * p$pH +
                  p$c_oc * log10(p$f_oc * 100) + # f_oc in %
                  p$c_c * log10(p$c_0))
          p$K_d <- p$c_0 * 1000 / p$c_water # Kd in [(mg/Kg)/(?g/L) * [1000 ?g/mg]] --> [L/Kg]
        }
        if(!("K_d" %in% colnames(p))){
          p$K_d <- p$f_oc * p$K_oc
        }
      }
      if(!("K_H" %in% colnames(p))) {p$K_H <- p$p * p$M / p$sol}
      if(!("K_AirWater" %in% colnames(p))) {p$K_AirWater <- p$K_H / (p$R * p$temp)}
      p$K_SoilWater <-
        (p$f_air * p$K_AirWater) +
        (p$f_water) +
        (p$f_solid * p$rho_solid * p$K_d / 1000)
    }

    if(!("k_volat" %in% colnames(p))){
      p$rez_k_volat <- 1 / (
        (1 / (p$k_aslAir * p$K_AirWater) +
           1 / (p$k_aslSoilAir * p$K_AirWater + p$k_aslSoilWater)) *
          p$K_SoilWater * p$d)
      p$k_volat <- 1 / p$rez_k_volat
    }

    if(!("k_leach" %in% colnames(p))){
      p$k_leach <- (p$f_inf * p$rain) / (p$K_SoilWater * p$d)
    }

    if(!("K_SoilWater" %in% colnames(p))){
      p$K_SoilWater <-  (p$f_inf * p$rain) / (p$k_leach * p$d)
    }

    # k_plant
    if(!("k_plant" %in% colnames(p))){
      if(BCF == "adapt2"){
        # pH and c_org as variables
        p$c_plant <-
          10^(p$log_BCF +
                p$c_pH_BCF * p$pH +
                p$c_oc_BCF * log10(p$f_oc * 100) + # f_oc in %
                p$c_c_BCF * log10(p$c_0))
        p$BCF <- p$c_plant / p$c_0
      } else if(BCF == "adapt1") {
        p$BCF <- 10^(p$log_BCF +
                       p$c_pH_BCF * p$pH +
                       p$c_oc_BCF * log10(p$f_oc * 100) +
                       p$c_c_BCF * log10(p$c_0))
      }
      p$k_plant <- (p$BCF * p$Y * p$DM_plant/100) / (p$t_g * p$d * p$rho_soil)
    }

    # k_bio
    if(!("k_bio" %in% colnames(p))){
      if("Degradability" %in% colnames(p)){
        if(!("K_p" %in% colnames(p))) {p$K_d <- p$f_oc * p$K_oc}
        p$DT50 <- DT50approx[p$K_d[1] > DT50approx$minK_p &
                               p$K_d[1] < DT50approx$maxK_p, p$Degradability[1]]
      }
      p$k_bio <- log(2)/p$DT50
    }

    # Overall k with k_plant
    if(!("k" %in% colnames(p))){
      p$k1 <- p$k_bio + p$k_leach + p$k_volat + p$k_plant
    }

    # Overall k without k_plant
    if(!("k" %in% colnames(p))){
      p$k2 <- p$k_bio + p$k_leach + p$k_volat
    }

    # Deposition
    if(!("D_air" %in% colnames(p))) {p$D_air <- p$D_air_tot / (p$d * p$rho_soil)}

    if(vPNEC_organisms){
      p$PNEC_organisms <- p$PNEC_leachate * (p$K_oc * 0.0104 + 0.174)
    }

    # Dynamic of substance concentration in the soil with and withoug k_plant
    output1 <- mapply(
      function(D_air, k1, c_0)
        D_air / k1 - (D_air / k1 - c_0) * exp(-k1 * seq(from = 0, to = 180, by = 1)),
      p$D_air, p$k1, p$c_0)

    # c_0 for calculation of the second half of the year is the concentration
    # on day 180 (last row of output1)
    output2 <- mapply(
      function(D_air, k2, c_0)
        D_air / k2 - (D_air / k2 - c_0) * exp(-k2 * seq(from = 1, to = 185, by = 1)),
      p$D_air, p$k2, output1[nrow(output1),])

    output <- rbind(output1, output2)

    # assign colnames and rownames to the dataframes
    colnames(output) <- paste0("MC_", 1:ncol(output))
    rownames(output) <- seq(from = 0, to = 365, by = 1)
    MCS[[year+4]] <- list() # "+4" to leave out the previous defined c180 and c30 list entries
    if(OutputLite){
      if(year == detailed_year){
        MCS[[year+4]]$Input <- p
        MCS[[year+4]]$Output <- output
      }
    } else {
      MCS[[year+4]]$Input <- p
      MCS[[year+4]]$Output <- output
    }

    # Further Characteristic numbers ------------------------------------------
    # average concentration within the first 180 and 30 days
    MCS$c180[year,] <- p$D_air / p$k1 +
      1 / (p$k1 * 180) * (p$c_0 - p$D_air / p$k1) * (1 - exp(- p$k1 * 180))

    MCS$c30[year,] <- p$D_air / p$k1 +
      1 / (p$k1 * 30) * (p$c_0 - p$D_air / p$k1) * (1 - exp(- p$k1 * 30))

    # Human consumption
    MCS$human[year,] <- MCS$c180[year,] * p$BCF * p$f_resorbed * p$m_wheat * 2
    # instead of deviding PNEC by 2 for only food uptake, PEC is multiplied with 2

    # ------------------------------------------
    # TGD eq. 24: porewater concentration equals soil concentration devided
    # by the soil to water partition coefficient K_d
    MCS$c30_leach[year,] <- (MCS$c30[year,] * p$rho_soil) / p$K_SoilWater

    # define the remaining oncentration for the upcoming year
    c_i <- output[nrow(output),]
    # --------------------------------------------------------------------------

  }

  if(sensPEC){
    round(c("origin_leach" =
              median(p$PNEC_leachate) * median(p$K_SoilWater) / median(p$rho_soil),
            "origin_human" = median(p$PNEC_human) / median(p$BCF) /
              median(p$f_resorbed) / median(p$m_wheat),
            "origin_soilOrganisms" = median(p$PNEC_organisms)), 2)
  } else {
    if(OutputLite){
      names(MCS)[5] <- paste0("year_", detailed_year)
    } else {
      names(MCS)[(1:years)+4] <- paste0("year_", 1:years)
    }
    MCS
  }
}


# plotting Functions

violin <- function(
  input, # a data.frame with years as rows, and values in columns
  year, # the selected year
  color,
  corpusFactor = 200,
  limitLow = 0, limitUp = 1 # propbability limits (defualt 0 <= Valtue <= 100 %)
){
  y_val <- density(unlist(input[year,]))$x
  x_val <- density(unlist(input[year,]))$y
  width <- y_val[2] - y_val[1]
  prob <- x_val * width

  l <- which(cumsum(prob) >= limitLow & cumsum(prob) <= limitUp)

  polygon(y = c(y_val[l], rev(y_val[l])),
          x = c(year + prob[l]*corpusFactor, rev(year - prob[l]*corpusFactor)),
          col = color, border = color, lwd = 2)
}

# line plot of the mean values in combination with vertical density plot at
# spefecied years
# ------------------------------------------------------------------------------
vioplot <- function(
  input,
  years,
  color,
  ymin = 0,
  ymax = max(input),
  corpusFactor_violin = 100,
  limitLow_violin = 0,
  limitUp_violin = 1,
  xlab = "Jahre",
  ylab = "Konzentration",
  main = "",
  PNEC = NA,
  plotMean = TRUE
){
  # plot the mean (or empty plot if plotMean = FALSE)
  plot(x = 1:max(years), y = rowMeans(input)[1:max(years)], type = "l",
       col = color,lwd = 2, ylim = c(ymin, ymax),xlab = xlab, ylab = ylab,
       main = main)

  # add PNEC
  abline(h = PNEC, col = "red")

  # add the violins vor each year
  for(year in years){
    violin(input = input, year = year, color = color, corpusFactor = corpusFactor_violin,
           limitLow = limitLow_violin, limitUp = limitUp_violin)
  }
}

# plot with 1 - 99, 10 - 90 and 25 - 75 % qunatiles over the years
polyplot <- function(input, years, ymin = 0, ymax = max(input), PNEC = NA,
                     xlab = "Jahre", ylab = "", main = ""){

  # color definition
  col1 <- rgb(red = 118, green = 217, blue = 255, maxColorValue = 255)
  col2 <- rgb(red = 64, green = 146, blue = 232, maxColorValue = 255)
  col3 <- rgb(red = 45, green = 99, blue = 255, maxColorValue = 255)
  col4 <- rgb(red = 6, green = 11, blue = 232, maxColorValue = 255)

  # calculation of quantiles
  quants <- apply(X = input, MARGIN = 1, FUN = quantile,
                  c(0.01,0.1,0.25,0.5,0.75,0.9,0.99))

  # plot it all
  plot(x = 1:years, y = rowMeans(input), type = "n", lwd = 2,
       ylim = c(ymin, ymax),xlab = xlab, ylab = ylab,  main = main)

  polygon(x = c(1:years, years:1), y = c(quants[1,], rev(quants[7,])), col = col1, border = NA)
  polygon(x = c(1:years, years:1), y = c(quants[2,], rev(quants[6,])), col = col2, border = NA)
  polygon(x = c(1:years, years:1), y = c(quants[3,], rev(quants[5,])), col = col3, border = NA)

  lines(x = 1:years, y = quants[4,], col = col4, lwd = 2)
  abline(h = PNEC, col = "red")

  legend("topright", legend = c("1 - 99 %", "10 - 90 %", "25 - 75 %"), border = NA,
         fill = c(col1, col2, col3),  bty = "n", cex = 0.8, title = "Quantile")

  legend("top", legend = c("Median", "PNEC"), bty = "n",
         col = c(col4, "red"), lwd = c(2,1), cex = 0.8)
}

# plot showing the density of the values over the year by the transperancy
# of the colors
shadingPlot <- function(input, years, ymin = 0, ymax = max(input), PNEC = NA,
                        xlab = "Jahre", ylab = "", main = "", resolution = 0.01){
  # calculation of quantiles
  quants <- apply(X = input, MARGIN = 1,
                  FUN = quantile, seq(0.00, 1, by = resolution))
  nQuants <- nrow(quants)
  Tcol <- rgb(0,130,188, alpha = (1000/nQuants + 2), maxColorValue = 255)


  plot(x = 1:years, y = 1:years, type = "n", ylim = c(ymin, ymax),
       xlab = xlab, ylab = ylab, main = main)
  # plot quantiles
  for(i in 0:(nQuants %/% 2)){
    polygon(x = c(1:years, years:1), y = c(quants[1+i,], rev(quants[nQuants - i,])),
            col = Tcol, border = NA)
  }
  # plot median
  lines(x = 1:years, y = quants[(nQuants %/% 2) + 1,],
        col = rgb(0,130,188, maxColorValue = 255), lwd = 1)
  # add PNEC
  abline(h = PNEC, col = "red")
  text(x = 0, y = PNEC, labels = "PNEC", col = "red", adj = c(0,1))
}

# cumulative sum plot for data frames created with "RunTGD_worstCase" function
CumSum100 <- function(
  df,
  Fertilizer,
  xmin = 0.001,
  xmax = 10,
  xlab = "Risikoquotient (PEC/PNEC)",
  ylab = "Anteil der ?berschreitung des Risikoquotienten [%]",
  german = TRUE,
  VerticalLines = TRUE)
{

  FertCols <- c(1, which(colnames(df) %in% FertID$Eng[Fertilizer]))
  quants <- apply(df[,FertCols], 2, quantile,
                  probs =seq(0.00, 1, by = 0.01), na.rm = TRUE)


  plot(x = quants[,1], y = 100:0, type = "l", ylim = c(0,100),
       xlab = xlab,
       ylab = ylab, lwd = 2, xlim = c(xmin,xmax), xaxs = "i",
       yaxs = "i", col = "black", log = "x", axes = FALSE)
  rect(xleft = 1, xright = xmax, ybottom = 0, ytop = 100,
       border = FALSE, col = rgb(245,220,220, maxColorValue = 255))
  rect(xleft = 0.01, xright = 1, ybottom = 0, ytop = 100,
       border = FALSE, col = rgb(245,245,180, maxColorValue = 255))
  rect(xleft = xmin, xright = 0.01, ybottom = 0, ytop = 100,
       border = FALSE, col = rgb(210,240,210,  maxColorValue = 255))
  if(VerticalLines)
  {
    abline(v = rep(x = 1:10, 11) * rep(x = 10^seq(-5,5,1), each = 10),
           col = "gray60", lty = "dashed")

  }

  topspace <- 100 / (par("plt")[4] - par("plt")[3]) * (1 - par("plt")[4])
  leftspace <- 100 / (par("plt")[2] - par("plt")[1]) * (1 - par("plt")[2])

  if(german){
    legend(x = par("xaxp")[1] / 2, y =  100 + topspace / 1.1,
           legend = c("Hintergrund (t=0)", FertID$Name[Fertilizer]),
           lwd = 2, col = c("black", FertID$Color[Fertilizer]),
           lty = c("solid", FertID$lineType[Fertilizer]),
           xpd = TRUE, ncol = 3, cex = 0.8, seg.len = 2, bty = "n")
  } else {
    legend(x = par("xaxp")[1] / 2, y =  100 + topspace / 1.1,
           legend = c("background (t=0)", FertID$Eng[Fertilizer]),
           lwd = 2, col = c("black", FertID$Color[Fertilizer]),
           lty = c("solid", FertID$lineType[Fertilizer]),
           xpd = TRUE, ncol = 3, cex = 0.8, seg.len = 2, bty = "n")
  }

  if(german == TRUE){
    axis(1, at = c(0.0001,0.001, 0.01, 0.1, 1, 10, 100, 1000),
         labels = c("0,0001","0,001", "0,01", "0,1", "1", "10", "100", "1000"))
  } else {
    axis(1, at = c(0.0001,0.001, 0.01, 0.1, 1, 10, 100, 1000),
         labels = c(0.0001,0.001, 0.01, 0.1, 1, 10, 100, 1000))
  }
  axis(2, at = c(0, 20, 40, 60, 80,100), labels = c(0, 20, 40, 60, 80,100), las = 2)

  if(length(FertCols) > 0){
    for(i in 2:ncol(quants)){
      lines(x = quants[,i], y = 100:0, col = FertID$Color[Fertilizer[i-1]],
            lwd = 2, lty = FertID$lineType[Fertilizer[i-1]])
    }
  }

  lines(x = quants[,1], y = 100:0, col = "black", lwd = 2)
  abline(v = 1, col = "red")

  quants
}


rewrite_env <- function(table, # the enironment data table
                        parameter, # one of the parameters listed in the Parameter column
                        distribution,  # "none" (no distribution),
                        # "uniform", "normal", "tnormal" (at truncated normal distribution at 0),
                        # "lognormal" or "gamma"
                        random = "constant", # "constant" (one randomisation process at t = 0), else
                        # Markov-Chain (--> randomization for every year)
                        shift = 0, # only important for lognormal and gamma distribution the shift
                        # the minimum of the distribution
                        value_1, # first value of distribution
                        # fixed-value ("none"), minimum ("uniform"), mean ("normal", "tnormal"),
                        # logmean ("lognormal"), shape ("gamma")
                        value_2 # second value of distribution
                        # NA ("none"), maximum ("uniform"), standard deviation ("normal", "tnormal"),
                        # log-standard deviation ("lognormal"), rate ("gamma")
)
{
  RowNo<- which(table$Parameter == parameter)

  # extract old version
  ex <- table[RowNo,]
  ex$Parameter <- paste0(parameter, "_old")

  # rewrite new version
  table$Distribution[RowNo] <- distribution
  table$shift[RowNo] <- shift
  table$random[RowNo] <- random
  table$Value_1[RowNo] <- value_1
  table$Value_2[RowNo] <- value_2

  # keep old version with "_old" suffix
  table <- rbind(table, ex)

  table
}

rewrite_sub <- function(table, # the enironment data table
                        parameter, # one of the parameters listed in the Parameter column
                        substance, # one of the substances
                        distribution,  # "none" (no distribution),
                        # "uniform", "normal", "tnormal" (at truncated normal distribution at 0),
                        # "lognormal" or "gamma"
                        random = "constant", # "constant" (one randomisation process at t = 0), else
                        # Markov-Chain (--> randomization for every year)
                        shift = 0, # only important for lognormal and gamma distribution the shift
                        # the minimum of the distribution
                        value_1, # first value of distribution
                        # fixed-value ("none"), minimum ("uniform"), mean ("normal", "tnormal"),
                        # logmean ("lognormal"), shape ("gamma")
                        value_2 # second value of distribution
                        # NA ("none"), maximum ("uniform"), standard deviation ("normal", "tnormal"),
                        # log-standard deviation ("lognormal"), rate ("gamma")
)
{
  RowNo<- which(table$Parameter == parameter)
  colNo <- data.frame(
    Name = paste0(substance, c("_dist", "_shift", "_random", "_1", "_2")))

  colNo$number <- NA
  for(i in 1:nrow(colNo)){
    colNo$number[i] <- which(colnames(table) == colNo$Name[i])
  }

  # extract old version
  ex <- table[RowNo,]
  ex$Parameter <- paste0(parameter, "_old")

  # rewrite new version
  table[RowNo, colNo$number[1]] <- distribution
  table[RowNo, colNo$number[2]] <- shift
  table[RowNo, colNo$number[3]] <- random
  table[RowNo, colNo$number[4]] <- value_1
  table[RowNo, colNo$number[5]] <- value_2

  # keep old version with "_old" suffix
  table <- rbind(table, ex)

  table
}



RunTGD <- function(
  runs = 5000,
  years = 100,
  Substance,
  Fertilizer = 1:6,
  K_d = "fixed", # adapt1 oder adapt2
  BCF = "fixed",
  FertID = FertID, # adapt1 oder adapt2
  set_initial_concentration = FALSE,
  vPNEC_organisms = FALSE,
  PPCP = FALSE)
{

  # this is just for progress information -------------------------
  pcol <- rgb(0,130,188, maxColorValue = 255)
  par(mar = c(1,15,1,1))
  plot(x = 0, y = 0, type = "n", xlim = c(0,100), ylim = c(0,12),
       main = "", xlab = "", ylab = "", axes = FALSE)
  mtext(text = c("Vorbelastung", FertID$Name), side = 2, line = 0,  at = 11:1, las = 1)
  # ---------------------------------------------------------------
  if(PPCP){
    t0_soil <- TGD_model(Sub = Sub,
                         Env = Env,
                         runs = runs,
                         years = 1,
                         Fertilizer = 10,
                         Substance = Substance,
                         K_d = K_d,
                         BCF = BCF,
                         OutputLite = FALSE,
                         t0 = FALSE,
                         set_initial_concentration = set_initial_concentration,
                         vPNEC_organisms = vPNEC_organisms)
  } else {
    t0_soil <- TGD_model(Sub = Sub,
                         Env = Env,
                         runs = runs,
                         years = 1,
                         Fertilizer = 10,
                         Substance = Substance,
                         K_d = K_d,
                         BCF = BCF,
                         OutputLite = FALSE,
                         t0 = TRUE,
                         set_initial_concentration = set_initial_concentration,
                         vPNEC_organisms = vPNEC_organisms)
  }
  # ---------------------------------------------------------------
  t0 <- data.frame("t0_soil" = unlist(t0_soil$c30),
                   "t0_leach" = unlist(t0_soil$c30_leach),
                   "t0_human" = unlist(t0_soil$human),
                   "t0_groundwater" = unlist(t0_soil$c30_leach)/t0_soil$year_1$Input$MF)

  # this is just for progress information -------------------------
  rect(xleft = 0, xright = 100, ybottom = 10.6, ytop = 11.4, col = pcol, border = FALSE)
  # ---------------------------------------------------------------


  t100 <- list()
  for(i in 1:10){
    if(i %in% Fertilizer){
      t100_soil <- TGD_model(Sub = Sub,
                             Env = Env,
                             runs = runs,
                             years = years,
                             Fertilizer = i,
                             Substance = Substance,
                             K_d = K_d,
                             BCF = BCF,
                             OutputLite = TRUE,
                             show_progress = TRUE,
                             set_initial_concentration = set_initial_concentration,
                             vPNEC_organisms = vPNEC_organisms)

      t100[[i]] <- data.frame("t100_soil" = unlist(t100_soil$c30[years,]),
                              "t100_leach" = unlist(t100_soil$c30_leach[years,]),
                              "t100_human" = unlist(t100_soil$human[years,]),
                              "t100_groundwater" =
                                unlist(t100_soil$c30_leach[years,])/
                                t0_soil$year_1$Input$MF)
    } else {
      t100[[i]] <- NA
      # this is just for progress information -------------------------
      abline(h = 11 - i, lty = "dashed")
      # ---------------------------------------------------------------
    }
  }
  names(t100) <- FertID$Eng

  a <- do.call(cbind, t100)
  a <- cbind(t0, a)

  soil <- a %>% select(ends_with("soil"))/t0_soil$year_1$Input$PNEC_organisms[1]
  leach <- a %>% select(ends_with("leach"))/t0_soil$year_1$Input$PNEC_leachate[1]
  human <- a %>% select(ends_with("human"))/t0_soil$year_1$Input$PNEC_human[1]
  groundwater <- a %>%
    select(ends_with("groundwater"))/
    t0_soil$year_1$Input$PNEC_leachate[1]

  Input <- t0_soil$year_1$Input
  constantInput <- apply(Input[,apply(Input, MARGIN = 2, sd) == 0],
                         MARGIN = 2, mean)
  variableInput <- Input[,apply(Input, MARGIN = 2, sd) > 0]


  colnames(soil) <-
    colnames(leach) <-
    colnames(human) <- c("t0", FertID$Eng[Fertilizer])

  par(mar = c(5.1,4.1,4.1,2.1))
  list("constantInput" = constantInput,
       "variableInput" = variableInput,
       "soil" = soil,
       "leach" = leach,
       "human" = human,
       "groundwater" = groundwater)
}


# Eintr?ge (?ber 100 Jahre) und Austragsraten (zum Zeitpunkt t = 0)
SoilInputOutput <- function(
  Sub,
  Output = TRUE,
  df,
  Substance,
  FertID,
  Fertilizer,
  mean_app = 60,
  sd_app = 10,
  german = TRUE)
{
  Fertilizer
  Sub <- Sub[, -c(2,3)]
  Eintr <- Sub[c(starts_with(vars = Sub$Parameter, match = "c_fert"),
                 which(Sub$Parameter == "D_air")),
               c(1,starts_with(vars = colnames(Sub), match = Substance))]
  Eintr <- Eintr[c(Fertilizer, nrow(Eintr)),] # the Fertilizers + last row (Deposition)

  if(Output == TRUE){
    layout(matrix(c(1,1,2), 3, 1, byrow = TRUE))
    Austr <- df[,colnames(df) %in% c("k_plant", "k_bio", "k_volat", "k_leach")]

    Austr[,colnames(Austr) %in% c("k_bio", "k_volat", "k_leach")] <-
      Austr[,colnames(Austr) %in% c("k_bio", "k_volat", "k_leach")] * 365

    Austr[,colnames(Austr) == "k_plant"] <-
      Austr[,colnames(Austr) == "k_plant"] * 180

    Austr <- Austr[,apply(Austr, 2, sum) > 0]
  }

  sum_field <- data.frame(matrix(nrow = 1000, ncol = sum(Eintr[[5]] == "normal")))
  for(i in 1:1000){
    sum_field [i,] <- apply(X = Eintr[Eintr[[5]] == "normal",], 1, function(X){
      sum(rnorm(n = 100, mean = as.numeric(X[[2]]), sd = as.numeric(X[[3]])) *
            rnorm(n = 100, mean = mean_app, sd = sd_app)) / # fertilizer application
        1000}) #  /1000 --> mg in g/ha

  }
  colnames(sum_field) <- FertID$Eng[Fertilizer]
  # Deposition
  if(Eintr[nrow(Eintr),5] == "lognormal"){
    sum_field$Dep <- rlnorm(n = 1000, meanlog = as.numeric(Eintr[nrow(Eintr),2]),
                            sdlog = as.numeric(Eintr[nrow(Eintr),3]))*
      365*100*340/1000*100*100 # mal 365 Tage, * 100 Jahre *
    # 340 kg/m? --> Density and depth, / 1000 mg/g, *100 m , * 100 m
  } else if(Eintr[nrow(Eintr),5] == "gamma"){
    sum_field$Dep <- rgamma(n = 1000, shape= as.numeric(Eintr[nrow(Eintr),2]),
                            rate = as.numeric(Eintr[nrow(Eintr),3]))*
      365*100*340/1000*100*100 # mal 365 Tage, * 100 Jahre *
    # 340 kg/m? --> Density and depth, / 1000 mg/g, *100 m , * 100 m
  } else {
    sum_field$Dep <- 0
  }



  if(german){
    par(mar = c(4.1, 13.1, 1,1))
    boxplot(sum_field, outline = FALSE, col = rgb(0,130,188, maxColorValue = 255),
            range = 1.5, horizontal = TRUE, las = 1,
            names = c(FertID$Name[Fertilizer], "Deposition"),
            xlab = "Modellierter Schadstoffeintrag in 100 Jahren [g/ha]")
    if(Output){
      boxplot(Austr, outline = FALSE, col = "forestgreen",
              range = 1.5, horizontal = TRUE, las = 1,
              names = colnames(Austr),
              xlab = "Austrags-Rate [1/a]")
    }
  } else {
    par(mar = c(4.1, 15.1, 1,1))
    boxplot(sum_field, outline = FALSE,
            col = rgb(129,169,187, maxColorValue = 255),
            range = 1.5, horizontal = TRUE, las = 1,
            names = c(FertID$Eng[Fertilizer], "deposition"),
            xlab = "Modeled pollutant input in 100 years [g/ha]")

    if(Output){
      boxplot(Austr, outline = FALSE, col = "forestgreen",
              range = 1.5, horizontal = TRUE, las = 1,
              names = colnames(Austr),
              xlab = "Output-rate [1/a]")
    }
  }
}


# Spearmam-Korrelation zwischen variablen Parametern und Konzentrationen im Boden
MCS_Spearman <- function(
  input,
  Fertilizer,
  varPar = input$variableInput,
  compartment = "soil",
  ylab = "Rangkorrelationskoeffizient",
  plot_t0 = TRUE,
  german = TRUE,
  FertID = FertID,
  Plot = TRUE
){

  # select table form list
  inputTable <- input[[which(names(input) == compartment)]]

  corTable <- cbind(inputTable, input$variableInput)
  corTable <- cor(corTable, method = "spearman", use = "pairwise.complete.obs")


  varRows <- which(colnames(corTable) %in% c(varPar, "c_0"))
  FertCols <- which(colnames(corTable) %in% FertID$Eng[FertID$ID %in% Fertilizer])

  if(Plot){
    NotAvail <-  which(!FertID$Eng[FertID$ID %in% Fertilizer] %in% colnames(corTable))
    if(length(NotAvail) > 0)
      stop(paste0(FertID$Name[NotAvail], " (D?nger ", NotAvail," ) ist nicht vorhanden"))

    rect_width <- 0.8 / length(Fertilizer)

    plot(x = 0, y = 0, type = "n", xlim = c(0.5,length(varPar)+0.5),
         ylim = c(-1,1), xlab = "",
         ylab = ylab, axes = FALSE)

    axis(1, at = 1:length(varPar), labels = colnames(corTable)[varRows], lwd = 0)
    if(german == TRUE){
      axis(2, at = c(-1, -0.5, 0, 0.5, 1), labels = c("-1","-0,5","0","0,5","1"))
    } else {
      axis(2, at = c(-1, -0.5, 0, 0.5, 1), labels = c(-1, -0.5, 0, 0.5, 1))
    }

    for(i in 1:length(varPar)){
      rect(xleft = i-0.4, xright = i + 0.4,
           ybottom = -1, ytop = 1,
           col = "gray90",  border = NA)
      for(j in 1:length(Fertilizer)){
        rect(xleft = i-0.4 + (j-1) * rect_width, xright = i - 0.4 + j * rect_width,
             ybottom = 0, ytop = corTable[varRows[i],FertCols[j]],
             col = FertID$Color[Fertilizer[j]],  border = NA)
      }

      if(plot_t0 == TRUE){
        lines(x = c(i-0.4, i+0.4), y = c(corTable[varRows[i],1],
                                         corTable[varRows[i],1]),
              col = "red")
      }
    }

    abline(h = 0)
    if(german){
      legend(x = 0, y =  1.5, legend = FertID$Name[Fertilizer],
             fill = FertID$Color[Fertilizer], lwd = NA,
             border = NA,  bty = "n", xpd = TRUE, ncol = 3, cex = 0.8, seg.len = 1)
    } else {
      legend(x = 0, y =  1.5, legend = FertID$Eng[Fertilizer],
             fill = FertID$Color[Fertilizer], lwd = NA,
             border = NA,  bty = "n", xpd = TRUE, ncol = 3, cex = 0.8, seg.len = 1)
    }
  }
  corTable
}

plotDegredation <- function(
  result_df, # a list crteated with function TGD_model
  years = 3, # the number of years that are shown
  plotLegend = T
){


  g1 <- rgb(47,86,26,maxColorValue = 255)
  g2 <- rgb(94,173,53,maxColorValue = 255)
  g3 <- rgb(155,215,124,maxColorValue = 255)

  result_df <- result_df[starts_with("year", vars = names(result_df))]
  AllYears <-
    apply(X = result_df[[1]]$Output, 1, quantile, c(0.1,0.25,0.5, 0.75,0.9))
  if(years > 1){
    for(i in 2:years){
      AllYears <-
        cbind(
          AllYears,
          apply(X = result_df[[i]]$Output, 1, quantile, c(0.1,0.25,0.5, 0.75,0.9)))
    }
  }

  TimeInYears <- seq(0,years,length.out = years*366) # 365 days plus day 0

  plot(x = TimeInYears,
       y = AllYears[3,], ylim = c(0,max(AllYears[5,])),
       ylab = "PEC Boden [mg/kg]", xlab = "Jahre",
       type = "l", xaxt = "n",
       col = g1, lwd = 2)
  polygon(x = c(TimeInYears, rev(TimeInYears)), y = c(AllYears[1,], rev(AllYears[5,])),
          col = g3, border = NA)
  polygon(x = c(TimeInYears, rev(TimeInYears)), y = c(AllYears[2,], rev(AllYears[4,])),
          col = g2, border = NA)
  lines(x = TimeInYears, y = AllYears[3,], col = g1, lwd = 2)
  axis(1, at = 0:years, labels = 0:years)
  if(plotLegend){
    legend(x = 0, y =  max(AllYears[5,]) + 0.2 * max(AllYears[5,]),
           seg.len = 1, horiz = T, xpd = T,
           legend = c("Median", "25 - 75 % Perzentil", "10 - 90 % Perzentil"),
           lwd = c(2, NA, NA),
           fill = c(NA, g2, g3), col = c(g1, NA, NA), border = FALSE, bty = "n")
  }
}

# Combination of TGD_model and plotDegredation (embedded)
DegredationPlot <- function(
  Sub = Sub,
  Env = Env,
  runs = 10000,
  years = 10,
  vPNEC_organisms = vPNEC_organisms,
  Substance = Substance,
  K_d = "other",
  BCF = "other",
  plotLegend = T
){
  # use fertilizer with highest pollutant concentration
  Fertilizer <- HighestFert(Sub = Sub, Env = Env, Substance = Substance)

  result_df <- TGD_model(Sub = Sub, Env = Env, runs = runs, years = years,
                         Fertilizer = Fertilizer, vPNEC_organisms = vPNEC_organisms,
                         Substance = Substance, K_d = K_d, BCF = BCF, t0 = F,
                         OutputLite = F)

  plotDegredation(result_df = result_df, years = years, plotLegend = plotLegend)
}


# Further Functions ------------------------------------------------------------

sensible_PEC <- function(Sub, Env, Substance, K_d, BCF){
  TGD_model(Sub = Sub, Env = Env, runs = 100000, years = 1,
            Fertilizer = 10,
            Substance = Substance, K_d = K_d, BCF = BCF, t0 = FALSE,
            OutputLite = FALSE,
            show_progress = FALSE, set_initial_concentration = FALSE, sensPEC = TRUE)
}


variableInput <- function(Sub = Sub, Env = Env, Substance = Substance){
  Sub <- Sub[,-c(2,3)] # this columns are not important for the function
  Sub <- Sub[,c(1,grep(pattern = Substance, x = colnames(Sub), ignore.case = F))]
  Env <- Env[,c(1,4:8)]
  colnames(Sub) <- colnames(Env)
  Input <- rbind(Env, Sub)
  Input$Parameter[which(Input$Distribution != "none" & Input$random != "random")]
}

# Individual variable analysis, threshold analysis
IndVarAnalysis <- function(
  TGD_list = result,
  variable = "c_fert", # all variables from "TGD_list$variableInput" possible
  comp, # "leach", "soil" or "human"
  correlatingFert = "deposition only", # depends on the calculated fertilizers
  RQ_threshold = 1, # the Risk quotient that is used for seperation of high and low values (1 by default)
  constant_bw = TRUE, # Either the groups have the same bandwith (default) or the same amount of entries
  PlotQuotient = F,
  xlabel = expression("Concentration in fertilizer [mg/kg P"[2]*"O"[5]*"]"),
  xlim = c(0,max(n.comp$right))
){

  cv <- which(colnames(TGD_list$variableInput) == variable) # column of variable
  cc <- which(names(TGD_list) == comp) # list entry of compartment
  cf <- which(colnames(TGD_list[[cc]]) == correlatingFert) # column of fertilizer

  corSpear <- cor(x = TGD_list$variableInput[[cv]], y = TGD_list[[cc]][[cf]],
                  method = "spearman", use = "pairwise.complete.obs")

  low <- TGD_list$variableInput[[cv]][TGD_list[[cc]][[cf]] < RQ_threshold]
  high <- TGD_list$variableInput[[cv]][TGD_list[[cc]][[cf]] >= RQ_threshold]

  # create data.frame with distinction between high an low RQ
  df <- data.frame("Value" = c(low, high),
                   "RQ" = c(rep("low", length(low)), rep("high", length(high))))

  # create groups with same band with (1) or create groups with same amount of data (2)
  # (1)
  if(constant_bw){
    s <- seq(0, max(df$Value), length.out = 1000) # grouping  series (1)
  } else {
    df <- df %>% arrange(Value)
    s <- df$Value[seq(1, nrow(df), length.out = 1000)] # grouping series (2)
  }

  # assign bandwiths (min, mean and max values) to existing data frame
  df$mid <- NA
  df$left <- NA
  df$right <- NA
  df$left[df$Value == s[1]] <- s[1]
  df$mid[df$Value == s[1]] <- (s[1] + s[2]) / 2
  df$right[df$Value == s[1]] <- s[2]

  for(i in 1:(length(s)-1)){
    df$left[df$Value > s[i] & df$Value <= s[i+1]] <- s[i]
    df$mid[df$Value > s[i] & df$Value <= s[i+1]] <- (s[i] + s[i+1]) / 2
    df$right[df$Value > s[i] & df$Value <= s[i+1]] <- s[i+1]
  }

  # aggregate (grouped by bandwidth) and join data frames
  n.low <- df %>% filter(RQ == "low") %>% group_by(mid, left, right) %>%
    summarize("low" = n())
  n.high <- df %>% filter(RQ == "high") %>% group_by(mid, left, right) %>%
    summarize("high" = n())

  # the data has to be ranked, depending on positive of negative correlation
  if(corSpear > 0){
    n.comp <- full_join(x = n.low, y = n.high, by = c("mid", "left", "right")) %>%
      arrange(mid)
  } else {
    n.comp <- full_join(x = n.low, y = n.high, by = c("mid", "left", "right")) %>%
      arrange(desc(mid))
  }

  # turn NA values into 0, calculation of the quotient:
  # values exceeding the RQ_threshold / all values
  n.comp[is.na(n.comp)] <- 0
  n.comp <- n.comp %>% mutate("comp" = low + high,
                              "lowShare" = low / comp,
                              "highShare" = 1 - lowShare)
  n.comp$cumSumLow <- cumsum(n.comp$low)
  n.comp$cumSumHigh <- cumsum(n.comp$high)
  n.comp$quotient <- n.comp$cumSumHigh / (n.comp$cumSumLow + n.comp$cumSumHigh)


  plot(x = 0, y = 0, type = "n", ylim = c(0,100),
       xlim = xlim, frame.plot = F,
       xlab = xlabel,
       ylab = paste0("Probability of RQ >", RQ_threshold," [%]"), xaxs = "i", yaxs = "i")
  rect(xleft = xlim[1], xright = xlim[2], ybottom = 0, ytop = 10, col = rgb(220,230,242, maxColorValue = 255), border = NA)
  rect(xleft = xlim[1], xright = xlim[2], ybottom = 10, ytop = 33, col = rgb(185,205,229, maxColorValue = 255), border = NA)
  rect(xleft = xlim[1], xright = xlim[2], ybottom = 33, ytop = 66, col = rgb(149,179,215, maxColorValue = 255), border = NA)
  rect(xleft = xlim[1], xright = xlim[2], ybottom = 66, ytop = 90, col = rgb(55,96,146, maxColorValue = 255), border = NA)
  rect(xleft = xlim[1], xright = xlim[2], ybottom = 90, ytop = 100, col = rgb(37,64,97, maxColorValue = 255), border = NA)
  abline(v = 0)
  Likelihood <- data.frame(
    "Likelihood" = c("very unlikely", "unlikely", "about as likely as not", "likely"),
    "Value" = NA)

  if(sum(n.comp$quotient<0.1) > 0){
    if(sum(n.comp$quotient >= 0.1) > 0){
      Likelihood[1,2] <- n.comp$mid[nrow(n.comp[n.comp$quotient<0.1,])]
      lines(x = rep(Likelihood[1,2],2), y = c(0,10), col = "red", lwd = 2)
    }
  }
  if(sum(n.comp$quotient<0.33) > 0){
    if(sum(n.comp$quotient >= 0.33) > 0){
      Likelihood[2,2] <- n.comp$mid[nrow(n.comp[n.comp$quotient<0.33,])]
      lines(x = rep(Likelihood[2,2],2),  y = c(0,33), col = "red", lwd = 2)
    }
  }
  if(sum(n.comp$quotient<0.66) > 0){
    if(sum(n.comp$quotient >= 0.66) > 0){
      Likelihood[3,2] <- n.comp$mid[nrow(n.comp[n.comp$quotient<0.66,])]
      lines(x = rep(Likelihood[3,2],2), y = c(0,66), col = "red", lwd = 2)
    }
  }
  if(sum(n.comp$quotient<0.9) > 0){
    if(sum(n.comp$quotient >= 0.9) > 0){
      Likelihood[4,2] <- n.comp$mid[nrow(n.comp[n.comp$quotient<0.9,])]
      lines(x = rep(Likelihood[4,2],2),  y = c(0,90), col = "red", lwd = 2)
    }
  }
  lines(x = n.comp$mid, y = n.comp$quotient*100, lwd = 3)

  list("Spearman Correlation" = corSpear,
       "Likelihood" = Likelihood,
       "RQ threshold table" = n.comp
  )
}

HighestFert <- function(Sub = Sub, Env = Env, Substance){
  Sub <- Sub[,-c(2,3)] # this columns are not important for the function
  Sub <- Sub[,c(1,grep(pattern = Substance, x = colnames(Sub), ignore.case = F))]
  all_fert <- which(grepl(pattern = "_fert_", x = Sub$Parameter))
  which(Sub[all_fert,2] == max(Sub[all_fert,2]))

}

