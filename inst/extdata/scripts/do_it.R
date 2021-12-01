# test functions

dat <- kwb.fcr::read_fcr_input(path = "inst/extdata/input", pollutantName = "cd")

# define initial concentration (only for the first year)
nFields <- 100
firstYear <- FALSE
use_mixing_factor

c_i <- kwb.fcr::rdist(value_1 = dat$c_i$value_1,
               value_2 = dat$c_i$value_2,
               n = nFields,
               dist_name = dat$c_i$distribution,
               shift = dat$c_i$shift,
               seed = 0)


# from here on per year
p <- sapply(dat, function(x){
  if(x[["site_specific"]] & !is.na(x[["site_specific"]])){
    s <- x[["pID"]]
  } else {
    s <- NULL
  }
  rdist(value_1 = x[["value_1"]],
        value_2 = x[["value_2"]],
        n = nFields,
        dist_name = x[["distribution"]],
        shift = x[["shift"]],
        seed = s)
  })

# yearly inital concentaiton c_0
if(firstYear){
  c_0 <- c_i
} else {
  c_add <- # in mg / (kg * a)
    (p[,"c_fert"] * p[,"p_app"]) / # in mg / (ha * a)
    (p[,"d"] * p[,"rho_soil"] * 10000)
  # sum of concentration from previous year and added pollutant
  c_0 <- c_add + c_end # c_end not defined yet
}

if(use_mixing_factor){
  MF <- 1 + (p[,"v_G"] * p[,"m_d"] /
               (p[,"rain"] * 365 * p[,"f_inf"] * p[,"l_field"]))
} else {
  MF <- 1
}








