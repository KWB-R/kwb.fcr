# test functions

# read data
dat <- kwb.fcr::read_fcr_input(path = "inst/extdata/input", pollutantName = "cd")
info <- kwb.fcr::additional_substanc_info(path = "inst/extdata/input", pollutantName = "cd")

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

dev.new()
kwb.fcr::shadingPlot(
  mat_xRow = fcr_out$PEC[["soil"]] / fcr_out$model_variables[,"PNEC_soil"],
  ymin = 0, ymax = 1,
  resolution = 0.01)

kwb.fcr::CumSumSoil(
  v0 = fcr_out$model_variables[,"c_i"] / fcr_out$model_variables[,"PNEC_soil"],
  v = fcr_out$PEC[["soil"]][100,] / fcr_out$model_variables[,"PNEC_soil"],
  year_x = 100)
kwb.fcr::CumSumSoil(
  v0 = fcr_out$model_variables[,"c_water"] / fcr_out$model_variables[,"PNEC_water"],
  v = fcr_out$PEC[["porewater"]][100,] / fcr_out$model_variables[,"PNEC_water"],
  year_x = 100)
kwb.fcr::CumSumSoil(
  v0 = fcr_out$model_variables[,"c_human"] / fcr_out$model_variables[,"PNEC_human"],
  v = fcr_out$PEC[["human"]][100,] / fcr_out$model_variables[,"PNEC_human"],
  year_x = 100, xmax = 100)

plot(density(x = fcr_out$PEC[["soil"]][100,]), lwd = 2)

# for one single year ----------------------------------------------------------
# prepare data
# define initial concentration (only for the first year)
c_i <- kwb.fcr::rdist(value_1 = dat$c_i$value_1,
                      value_2 = dat$c_i$value_2,
                      n = 100,
                      dist_name = dat$c_i$distribution,
                      shift = dat$c_i$shift,
                      seed = 0)

p <- kwb.fcr::oneYear_matrix(dat = dat,
                             c_i = c_i,
                             nFields = 100,
                             use_mixing_factor = FALSE)

p <- kwb.fcr::add_variables(p = p,
                            info = info)

# PECs in this year
PEC_soil <- kwb.fcr::get_PEC_soil(p = p, d = 30)
PEC_human <- kwb.fcr::get_PEC_human(p = p, d = 180, food_only = T)
PEC_porewater <- kwb.fcr::get_PEC_porewater(p = p, d = 30)

# temporal concentration course in year i
output <- kwb.fcr::one_year(p = p,
                            growing_period = 180,
                            t_res = 1)
# end concentration of year i
c_i <- output[nrow(output),]


