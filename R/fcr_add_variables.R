#' Query and complementation of input variables
#'
#' @param p The input data table created with [read_fcr_input()]
#' @param info Additional input data information, created with
#' [additional_substanc_info()]
#'
#' @return
#' The table p exetended by new variable columns needed for the risk assessment
#'
#' @export
#'
add_variables <- function(
  p, info
){
  # Deposition (must be first because eventually needed for K_d regression)
  if(!("D_air" %in% colnames(p))){
    p <- add_deposition(p = p)
  }

  if(!("k_volat" %in% colnames(p) &
       "k_leach" %in% colnames(p))){

    if(!("K_H" %in% colnames(p))) {
      p <- add_Henry(p = p)
    }

    if(!("K_AirWater" %in% colnames(p))) {
      p <- add_AirWater(p = p)
    }

    if(!("K_SoilWater" %in% colnames(p))){
      if(!("K_d" %in% colnames(p))){
        p <- add_Kd(p = p, sub_info = info)
      }
      p <- add_SoilWater(p = p)
    }
  }

  if(!("k_volat" %in% colnames(p))){
    p <- add_kvolat(p = p)
  }

  if(!("k_leach" %in% colnames(p))){
    p <- add_kleach(p = p)
  }

  if(!("K_SoilWater" %in% colnames(p))){
    p <- add_SoilWater_reverse(p = p)
  }

  # k_plant
  if(!("k_plant" %in% colnames(p))){
    if(!("BCF" %in% colnames(p))){
      p <- add_bcf(p = p, sub_info = info)
    }
    p <- add_kplant(p = p)
  }

  ###############################################################################
  # k_bio
  if(!("k_bio" %in% colnames(p))){
    if(!("DT50" %in% colnames(p))){
      p <- add_DT50(p = p)
    }
    p <- add_kbio(p = p)
  }

  # Overall k with and without k_plant
  p <- cbind(p, "k1"=  p[,"k_bio"] + p[,"k_leach"] +
               p[,"k_volat"] + p[,"k_plant"])
  p <- cbind(p, "k2"=  p[,"k_bio"] + p[,"k_leach"] + p[,"k_volat"])



  if(!("PNEC_soil" %in% colnames(p))){
    p <- add_PNEC_soil(p = p)
  }
  p
}

#' Estimate Sorption Coefficient
#'
#' Estimation is based on the FCR prepared Monte Carlo table
#'
#' Theoretically the initial concentration in soil can become negative if the
#' pollutant concentration in fertilizes is negative. A negative concentration
#' makes obviously no sense in a real situation. However, long-term impact is
#' assessed, a broad concentration range of the pollutant also leads to more
#' uncertain results. Negative concentrations in one year are averaged out.
#' For the regression of the sorption coefficient, the concentration must be
#' positive. Thus, negative values are increased to the concentration in
#' top soil that would appear after one day of deposition.
#'
#' @param p Parameter table created with [oneYear_matrix()]. Note: D_air variable
#' must be available in p if K_d is estimated with linear regression.
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
    # negative concentrations are increased for K_d regression to the concentration
    # that would result after one day of deposition
    # this is possible if fertilizer concentration is negative as a result of
    # high uncertainty
    too_low <- which(p[,"c_0"] <= 0)
    if(length(too_low) > 0){
      p[too_low,"c_0"] <- p[too_low,"D_air"]
    }


    p <- cbind(p, "K_d" = Kd_regression(
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

#' Estimate Soil-Water partition coefficient starting from infiltration rate
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

#' Estimate bio concentration factor
#'
#' Estimation is based on the FCR prepared Monte Carlo table
#'
#' @param p Parameter table created with [oneYear_matrix()]
#' @param sub_info The table containing additional substance information loaded
#' with [additional_substanc_info()]
#'
#' @return The Parameter table extended by a column for the bio concentration
#' factor.
#'
#' @export
#'
add_bcf <- function(p, sub_info){
  BCF_regType <- sub_info$value[sub_info$info == "BCF_regType"]

  cbind(p, "BCF" = Kd_regression(
    constant = p[,"const_BCF"],
    beta_ph = p[,"gamma_pH"],
    beta_org = p[,"gamma_oc"],
    beta_conc = p[,"gamma_c"],
    regType = BCF_regType,
    pH = p[,"pH"],
    org_c = p[,"f_oc"] * 100,
    conc = p[,"c_0"]))
}

#' Estimate biological half-life
#'
#' Estimation is based on the FCR prepared Monte Carlo table
#'
#' The estimation is based on the K_d value of a substance according to the
#' technical guidance document on risk assessment Part II (table 8). If there
#' is a distribution of K_d values, the median K_d is used for the estimation.
#' In this function there is no distinction between biodegradbility classes.
#' All substances are assumend to be inherently biodegradable to encourage the
#' user of further half-life research.
#'
#' @param p Parameter table created with [oneYear_matrix()]
#' @param sub_info The table containing additional substance information loaded
#' with [additional_substanc_info()]
#'
#' @return The Parameter table extended by a column for the bio concentration
#' factor.
#'
#' @export
#' @importFrom stats median
#'
add_DT50 <- function(p, sub_info){
  dt50 <- NA

  if(median(p[,"K_d"]) <= 10000){
    dt50 <- 30000
  }
  if(median(p[,"K_d"])  <= 1000){
    dt50 <- 3000
  }
  if(median(p[,"K_d"])  <= 100){
    dt50 <- 300
  }
  cbind(p, "DT50" = dt50)
}

#' Calculate volatilization rate
#'
#' Calculation is based on the FCR prepared Monte Carlo table
#'
#' @param p Parameter table created with [oneYear_matrix()]
#'
#' @return The Parameter table extended by a column for the volatilization rate.
#' Unit is 1/d.
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
#' @return The Parameter table extended by a column for the infiltration rate.
#' Unit is 1/d.
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
#' @return The Parameter table extended by a column for the plant uptake rate.
#' Unit is 1/d.
#'
#' @export
#'
add_kplant <- function(p){
  cbind(p, "k_plant" = (p[,"BCF"] * p[,"Y"] * p[,"DM_plant"] / 100) /
          (p[,"t_g"] * p[,"d"] * p[,"rho_soil"]))
}

#' Calculate biodegredation rate
#'
#' Calculation is based on the FCR prepared Monte Carlo table
#'
#' @param p Parameter table created with [oneYear_matrix()]
#'
#' @return The Parameter table extended by a column for the biodegredation rate.
#' Unit is 1/d.
#'
#' @export
#'
add_kbio <- function(p){
  cbind(p, "k_bio" = log(2) / p[,"DT50"])
}

#' Calculate soil mass related daily atmospheric deposition
#'
#' Calculation is based on the FCR prepared Monte Carlo table
#'
#' @param p Parameter table created with [oneYear_matrix()]
#'
#' @return The Parameter table extended by a column for the atmospheric
#' Deposition. Unit is mg/(kg * d).
#'
#' @export
#'
add_deposition <- function(p){
  cbind(p, "D_air" =  p[,"D_air_tot"] / (p[,"d"] * p[,"rho_soil"]))
}


#' Estimate Predicted no-effect concentration for soil organisms
#'
#' Calculation is based on the FCR prepared Monte Carlo table
#'
#' @param p Parameter table created with [oneYear_matrix()]
#'
#' @return The Parameter table extended by a column for the Predicted no-effect
#' concentration for soil organisms. Unit is mg/(kg Dry Matter).
#'
#' @export
#'
add_PNEC_soil <- function(p){
  cbind(p, "PNEC_soil" =
          (p[,"PNEC_water"] / 1000) * (p[,"K_oc"] * 0.0104 + 0.174))
}






