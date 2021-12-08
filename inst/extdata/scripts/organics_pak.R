
# read data
dat <- kwb.fcr::read_fcr_input(path = "inst/extdata/input/general", pollutantName = "organics")
info <- kwb.fcr::additional_substanc_info(path = "inst/extdata/input/general", pollutantName = "organics")

# for longterm application -----------------------------------------------------
fcr_out <- kwb.fcr::longterm_PEC(dat = dat,
                                 info = info,
                                 years = 100,
                                 nFields = 1000,
                                 use_mixing_factor = FALSE,
                                 food_only = TRUE,
                                 growing_period = 180,
                                 t_res = 10,
                                 traceBackVariables = FALSE)

dev.new(noRStudioGD = T, height = 5, width = 9)
kwb.fcr::shadingPlot(
  mat_xRow = fcr_out$PEC[["soil"]],
  ymin = 0, ymax = max(fcr_out$PEC[["soil"]]),
  resolution = 0.01, ylab = "Concentration [mg/kg]",
  xlab = "Years of Application", main = "PAH")
kwb.fcr::shadingPlot(
  mat_xRow = fcr_out$PEC[["porewater"]],
  ymin = 0, ymax = max(fcr_out$PEC[["porewater"]]),
  resolution = 0.01, ylab = "Concentration [µg/L]",
  xlab = "Years of Application", main = "PAH")
kwb.fcr::shadingPlot(
  mat_xRow = fcr_out$PEC[["human"]],
  ymin = 0, ymax = max(fcr_out$PEC[["human"]]),
  resolution = 0.01, ylab = "Concentration [mg/d]",
  xlab = "Years of Application", main = "PAH")

kwb.fcr::CumSumSoil(
  v0 = fcr_out$model_variables[,"c_i"] / fcr_out$model_variables[,"PNEC_soil"],
  v = fcr_out$PEC[["soil"]][100,] / fcr_out$model_variables[,"PNEC_soil"],
  year_x = 100, xmax = 1E08)
kwb.fcr::CumSumSoil(
  v0 = fcr_out$model_variables[,"c_water"] / fcr_out$model_variables[,"PNEC_water"],
  v = fcr_out$PEC[["porewater"]][100,] / fcr_out$model_variables[,"PNEC_water"],
  year_x = 100, xmin = 0.0001, xmax = 1E08)
kwb.fcr::CumSumSoil(
  v0 = fcr_out$model_variables[,"c_human"] / fcr_out$model_variables[,"PNEC_human"],
  v = fcr_out$PEC[["human"]][100,] / fcr_out$model_variables[,"PNEC_human"],
  year_x = 100, xmax = 1E08)

summary(fcr_out$model_variables[,"k_volat"])
summary(fcr_out$model_variables[,"k_bio"])
summary(fcr_out$model_variables[,"k_leach"])
summary(fcr_out$model_variables[,"k_plant"])

# 1.
dat$PNEC_water$value_1 <- 0.05 # für PAK
dat$PNEC_soil$value_1 <- 0.71 # für PAK
dat$PNEC_human$value_1 <- 5.96 # für PAK

# 2.
summary(fcr_out$model_variables[,"DT50"])
hist(fcr_out$model_variables[,"DT50"])
dat$DT50$value_1 <- 100
dat$DT50$value_2 <- 4000

# 3.
dat$BCF$value_1 <- 0
dat$BCF$value_2 <- 1

# 4.
dat$K_oc$value_1 <- 1E05
dat$K_oc$value_2 <- 1E07

# 5.
dat$c_i$value_1 <- 0.2
dat$c_i$value_2 <- 0.1
dat$c_i$distribution <- "tnormal"
dat$c_i$

# 6. Deposition
dat$k_volat$value_1 <- 1E-06
dat$k_volat$value_2 <- 1E-05
dat$k_volat$distribution <- "tnormal"
dat$k_volat$shift <- 0
dat$k_volat$site_specific <- FALSE

# 6. Deposition
dat$D_air_tot$value_1 <- 0.0015
dat$D_air_tot$value_2 <- 0.0005
dat$D_air_tot$distribution <- "tnormal"

# 7. Fertilizer
dat$c_fert$value_1 <- 0
dat$c_fert$value_2 <- 0
dat$c_fert$distribution <- "uniform"



