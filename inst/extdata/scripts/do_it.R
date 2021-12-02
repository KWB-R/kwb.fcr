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

c_out <- kwb.fcr::temp_c_profile(
  conti_input = p[,"D_air"],
  output_rate = p[,"k1"],
  c_i = p[,"c_0"],
  t_max = 180, t_res = 30, t_beg = 0)

c_out2 <- kwb.fcr::temp_c_profile(
  conti_input = p[,"D_air"],
  output_rate = p[,"k2"],
  c_i = c_out[nrow(c_out),],
  t_max = 180, t_res = 30, t_beg = 180)

rbind(c_out, c_out2)

