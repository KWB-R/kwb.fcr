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

p <- kwb.fcr::add_variables(p = p, info = info)


