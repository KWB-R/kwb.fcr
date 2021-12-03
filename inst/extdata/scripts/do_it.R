# test functions

# read data
dat <- kwb.fcr::read_fcr_input(path = "inst/extdata/input", pollutantName = "cd")
info <- kwb.fcr::additional_substanc_info(path = "inst/extdata/input", pollutantName = "cd")

# for longterm application -----------------------------------------------------
asd <- longterm_PEC(dat = dat,
                    info = info,
                    years = 5,
                    nFields = 100000,
                    use_mixing_factor = FALSE,
                    food_only = TRUE,
                    growing_period = 180,
                    t_res = 100, return_variables = T)

# plot a site specific variable
plot(x = asd$model_variables[[1]][,"rain"],
     y = asd$model_variables[[2]][,"rain"])

# plot a site independent variable
plot(x = asd$model_variables[[1]][,"c_fert"],
     y = asd$model_variables[[2]][,"c_fert"])

asd <- longterm_PEC(dat = dat,
                    info = info,
                    years = 20,
                    nFields = 100000,
                    use_mixing_factor = FALSE,
                    food_only = TRUE,
                    growing_period = 180,
                    t_res = 100, return_variables = F)
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


