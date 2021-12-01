#' Estimate Sorption Coefficient
#'
#' Estimation is based on the FCR prepared Monte Carlo table
#'
#' @param p Parameter table created with [oneYear_matrix()]
#' @param sub_info The table containing additional substance information loaded
#' with [additional_substanc_info()]
#'
#' @return The Parameter table extended by a column for the sorption coefficient
#'
#' @export
#'
add_Kd <- function(p, sub_info){

  Kd_regType <- sub_info$value[sub_info$info == "Kd_regType"]

  if(Kd_regType != "no"){
    p <- cbind(p, "K_d" = kwb.fcr::Kd_regression(
      constant = p[,"const_K_d"],
      beta_ph = p[,"beta_pH"],
      beta_org = p[,"beta_oc"],
      beta_conc = p[,"beta_c"],
      regType = Kd_regType,
      pH = p[,"pH"],
      org_c = p[,"f_oc"] * 100,
      conc = p[,"c_0"]))
  } else {
    if(!("K_oc" %in% colnames(p))){
      if("K_ow" %in% colnames(p)){
        stop(paste("If no Sorption coefficient (K_d) and no organic carbon",
                   "soprtion (K_oc) is available, at least octanol-water",
                   "coefficient (K_ow) must be provided"))
      }
      if(p[,"K_ow"] >= 1 & p[,"K_ow"] < 7.5){
        p <- cbind(p, "K_oc" = 10^(0.81 * p[,"K_ow"] + 0.1))
      } else if(p[,"K_ow"] < 1) {
        p <- cbind(p, "K_oc" = 10^(0.52 * p[,"K_ow"] + 1.02))
      }
      print("Note: The estimation of K_oc based on K_ow is very uncertain for polar substances")
    }
    p <- cbind(p, "K_d" = p[,"f_oc"] * p[,"K_oc"])
  }
  p
}

#' Estimate Henry Coefficient
#'
#' Estimation is based on the FCR prepared Monte Carlo table
#'
#' @param p Parameter table created with [oneYear_matrix()]
#'
#' @return The Parameter table extended by a column for Henry coefficient.
#' Unit is (Pa*m3)/mol
#'
#' @export
#'
add_Henry <- function(p){
  cbind(p, "K_H" = p[,"p"] * p[,"M"] / p[,"sol"])
}

#' Estimate Air-Water partition coefficient
#'
#' Estimation is based on the FCR prepared Monte Carlo table
#'
#' @param p Parameter table created with [oneYear_matrix()]
#'
#' @return The Parameter table extended by a column for the partition
#' coefficient. Unit is m³/m³.
#'
#' @export
#'
add_AirWater <- function(p){
  cbind(p, "K_AirWater" = p[,"K_H"] / (p[,"R"] * p[,"temp"]))
}


#' Estimate Soil-Water partition coefficient
#'
#' Estimation is based on the FCR prepared Monte Carlo table
#'
#' @param p Parameter table created with [oneYear_matrix()]
#'
#' @return The Parameter table extended by a column for the partition
#' coefficient. Unit is m³/m³.
#'
#' @export
#'
add_SoilWater <- function(p){
  cbind(p, "K_SoilWater" = (p[,"f_air"] * p[,"K_AirWater"]) +
          p[,"f_water"] +
          p[,"f_solid"] * p[,"rho_solid"] * p[,"K_d"] / 1000)
}

#' Estimate Soil-Water partition coefficient starting from infitrlation rate
#'
#' Estimation is based on the FCR prepared Monte Carlo table
#'
#' @param p Parameter table created with [oneYear_matrix()]
#'
#' @return The Parameter table extended by a column for the partition
#' coefficient. Unit is m³/m³.
#'
#' @export
#'
add_SoilWater_reverse <- function(p){
  cbind(p, "K_SoilWater" = (p[,"f_inf"] * p[,"rain"]) /
          (p[,"k_leach"] * p[,"d"]))
}

#' Estimate Bio concentration factor
#'
#' Estimation is based on the FCR prepared Monte Carlo table
#'
#' @param p Parameter table created with [oneYear_matrix()]
#' @param sub_info The table containing additional substance information loaded
#' with [additional_substanc_info()]
#'
#' @return The Parameter table extended by a column for the sorption coefficient
#'
#' @export
#'
add_bcf <- function(p, sub_info){
  BCF_regType <- sub_info$value[sub_info$info == "BCF_regType"]

  cbind(p, "BCF" = kwb.fcr::Kd_regression(
    constant = p[,"const_BCF"],
    beta_ph = p[,"gamma_pH"],
    beta_org = p[,"gamma_oc"],
    beta_conc = p[,"gamma_c"],
    regType = BCF_regType,
    pH = p[,"pH"],
    org_c = p[,"f_oc"] * 100,
    conc = p[,"c_0"]))
}

#' Calculate volatisation rate
#'
#' Calculation is based on the FCR prepared Monte Carlo table
#'
#' @param p Parameter table created with [oneYear_matrix()]
#'
#' @return The Parameter table extended by a column for the partition
#' coefficient. Unit is 1/d.
#'
#' @export
#'
add_kvolat <- function(p){

    first_quo <- 1 / (p[,"k_aslAir"] * p[,"K_AirWater"])

    second_quo <-
      1 / (p[,"k_aslSoilAir"] * p[,"K_AirWater"] + p[,"k_aslSoilWater"])

    overal_factor <- p[,"K_SoilWater"] * p[,"d"]

    cbind(p, "k_volat" = ((first_quo + second_quo) * overal_factor)^-1)
}

#' Calculate infiltration rate
#'
#' Calculation is based on the FCR prepared Monte Carlo table
#'
#' @param p Parameter table created with [oneYear_matrix()]
#'
#' @return The Parameter table extended by a column for the partition
#' coefficient. Unit is 1/d.
#'
#' @export
#'
add_kleach <- function(p){
  cbind(p, "k_leach" = (p[,"f_inf"] * p[,"rain"]) /
          (p[,"K_SoilWater"] * p[,"d"]))
}

#' Calculate plant uptake rate
#'
#' Calculation is based on the FCR prepared Monte Carlo table
#'
#' @param p Parameter table created with [oneYear_matrix()]
#'
#' @return The Parameter table extended by a column for the partition
#' coefficient. Unit is 1/d.
#'
#' @export
#'
add_kplant <- function(p){
  cbind(p, "p$k_plant" = (p[,"BCF"] * p[,"Y"] * p[,"DM_plant"] / 100) /
          (p[,"t_g"] * p[,"d"] * p[,"rho_soil"]))
}





