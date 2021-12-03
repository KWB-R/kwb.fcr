# test functions

# read data
dat <- kwb.fcr::read_fcr_input(path = "inst/extdata/input", pollutantName = "cd")
info <- kwb.fcr::additional_substanc_info(path = "inst/extdata/input", pollutantName = "cd")

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
                             firstYear = FALSE,
                             use_mixing_factor = FALSE)

p <- kwb.fcr::add_variables(p = p,
                            info = info)

# PECs in year i
PEC_soil <- kwb.fcr::get_PEC_soil(p = p, d = 30)
PEC_human <- kwb.fcr::get_PEC_human(p = p, d = 180, food_only = T)
PEC_porewater <- kwb.fcr::get_PEC_porewater(p = p, d = 30)


# temporal concentration course in year i
output <- kwb.fcr::one_year(p = p,
                            growing_period = 180,
                            t_res = 10)
# end concentration of year i
c_i <- output[nrow(output),]
