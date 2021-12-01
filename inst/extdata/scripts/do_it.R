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


# calculation of the remaining parameters: k and Deposition
# k_volat and k_leach and k_plant
if(!("k_volat" %in% colnames(p) &
     "k_leach" %in% colnames(p))){

  if(!("K_H" %in% colnames(p))) {
    p <- kwb.fcr::add_Henry(p = p)
  }

  if(!("K_AirWater" %in% colnames(p))) {
    p <- kwb.fcr::add_AirWater(p = p)
  }

  if(!("K_SoilWater" %in% colnames(p))){
    if(!("K_d" %in% colnames(p))){
      p <- kwb.fcr::add_Kd(p = p, sub_info = info)
    }
    p <- kwb.fcr::add_SoilWater(p = p)
  }
}

if(!("k_volat" %in% colnames(p))){
 p <- kwb.fcr::add_kvolat(p = p)
}

if(!("k_leach" %in% colnames(p))){
  p <- kwb.fcr::add_kleach(p = p)
}

if(!("K_SoilWater" %in% colnames(p))){
  p <- kwb.fcr::add_SoilWater_reverse(p = p)
}

# k_plant
if(!("k_plant" %in% colnames(p))){
  if(!("BCF" %in% colnames(p))){
    p <- kwb.fcr::add_bcf(p = p, sub_info = info)
  }
  p <- kwb.fcr::add_kplant(p = p)
}

###############################################################################
# k_bio
if(!("k_bio" %in% colnames(p))){
  if(!("DT50" %in% colnames(p))){
    # DT50 schätzen mit KOW (siehe TGD)
  }
  p$k_bio <- log(2)/p$DT50
}

# Overall k with and without k_plant
  p <- cbind(p, "k1"=  p[,"k_bio"] + p[,"k_leach"] +
               p[,"k_volat"] + p[,"k_plant"])
  p <- cbind(p, "k2"=  p[,"k_bio"] + p[,"k_leach"] + p[,"k_volat"])


# Deposition
if(!("D_air" %in% nms)) {
  p$D_air <- p$D_air_tot / (p$d * p$rho_soil)
  }

if(vPNEC_organisms){
  p$PNEC_organisms <- p$PNEC_leachate * (p$K_oc * 0.0104 + 0.174)
}







