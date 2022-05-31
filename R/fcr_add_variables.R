#' Query and complementation of input variables
#'
#' @param p The input data table created with [read_fcr_input()]
#' @param info Additional input data information, created with
#' [additional_substanc_info()]
#'
#' @return
#' The table p extended by new variable columns needed for the risk assessment
#'
#' @export
#'
add_variables <- function(
  p, info
){

  # Helper function calling a column-adding function in case of missing column
  call_if_missing <- function(data, column, fun, ...) {
    if (column %in% names(data)) {
      return(data)
    }
    # Call the function with the main argument data and further args if given
    do.call(fun, c(list(data), list(...)))
  }

  # Deposition (must be first because eventually needed for K_d regression)
  p <- call_if_missing(p, "D_air", add_deposition)

  if(!("k_volat" %in% names(p) && "k_leach" %in% names(p))){

    p <- call_if_missing(p, "K_H", add_Henry)
    p <- call_if_missing(p, "K_AirWater", add_AirWater)
    p <- call_if_missing(p, "K_SoilWater", function(p) {
      add_SoilWater(p = call_if_missing(p, "K_d", add_Kd, sub_info = info))
    })
  }

  p <- call_if_missing(p, "k_volat", add_kvolat)
  p <- call_if_missing(p, "k_leach", add_kleach)
  p <- call_if_missing(p, "K_SoilWater", add_SoilWater_reverse)

  # k_plant
  p <- call_if_missing(p, "k_plant", function(p) {
    add_kplant(p = call_if_missing(p, "BCF", add_bcf, sub_info = info))
  })

  ###############################################################################
  # k_bio
  p <- call_if_missing(p, "k_bio", function(p) {
    add_kbio(p = call_if_missing(p, "DT50", add_DT50))
  })

  # Overall k with and without k_plant
  x <- kwb.utils::createAccessor(p)

  k2 <- x("k_bio") + x("k_leach") + x("k_volat")

  p <- cbind(p, k1 = k2 + x("k_plant"), k2 = k2)

  call_if_missing(p, "PNEC_soil", add_PNEC_soil)
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

  x <- kwb.utils::createAccessor(p)

  if(Kd_regType != "no"){

    # negative concentrations are increased for K_d regression to the concentration
    # that would result after one day of deposition
    # this is possible if fertilizer concentration is negative as a result of
    # high uncertainty
    too_low <- which(x("c_0") <= 0)

    if(length(too_low) > 0){
      p[["c_0"]][too_low] <- x("D_air")[too_low]
    }

    # update the accessor function
    x <- kwb.utils::createAccessor(p)

    p <- cbind(p, "K_d" = Kd_regression(
      constant = x("const_K_d"),
      beta_ph = x("beta_pH"),
      beta_org = x("beta_oc"),
      beta_conc = x("beta_c"),
      regType = Kd_regType,
      pH = x("pH"),
      org_c = x("f_oc") * 100,
      conc = x("c_0"))
    )

  } else {

    if(!("K_oc" %in% colnames(p))){

      if("K_ow" %in% colnames(p)){
        stop(
          "If no Sorption coefficient (K_d) and no organic carbon",
          "soprtion (K_oc) is available, at least octanol-water",
          "coefficient (K_ow) must be provided"
        )
      }

      K_ow <- x("K_ow")

      if(K_ow >= 1 && K_ow < 7.5){
        p <- cbind(p, "K_oc" = 10^(0.81 * K_ow + 0.1))
      } else if(K_ow < 1) {
        p <- cbind(p, "K_oc" = 10^(0.52 * K_ow + 1.02))
      }

      print("Note: The estimation of K_oc based on K_ow is very uncertain for polar substances")
    }

    # update the accessor function
    x <- kwb.utils::createAccessor(p)

    p <- cbind(p, "K_d" = x("f_oc") * x("K_oc"))
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
  x <- kwb.utils::createAccessor(p)
  cbind(p, "K_H" = x("p") * x("M") / x("sol"))
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
  x <- kwb.utils::createAccessor(p)
  cbind(p, "K_AirWater" = x("K_H") / (x("R") * x("temp")))
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
  x <- kwb.utils::createAccessor(p)
  cbind(p, "K_SoilWater" =
    x("f_air") * x("K_AirWater") +
    x("f_water") +
    x("f_solid") * x("rho_solid") * x("K_d") / 1000
  )
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
  x <- kwb.utils::createAccessor(p)
  cbind(p, "K_SoilWater" = (x("f_inf") * x("rain")) / (x("k_leach") * x("d")))
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

  x <- kwb.utils::createAccessor(p)

  cbind(p, "BCF" = BCF_regression(
    constant = x("const_BCF"),
    beta_ph = x("gamma_pH"),
    beta_org = x("gamma_oc"),
    beta_conc = x("gamma_c"),
    regType = BCF_regType,
    pH = x("pH"),
    org_c = x("f_oc") * 100,
    conc = x("c_0")))
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

  med <- median(kwb.utils::selectColumns(p, "K_d"))

  if(med <= 10000){
    dt50 <- 30000
  }
  if(med <= 1000){
    dt50 <- 3000
  }
  if(med <= 100){
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

  x <- kwb.utils::createAccessor(p)

  # quotients 1 and 2
  q1 <- 1 / (x("k_aslAir") * x("K_AirWater"))
  q2 <- 1 / (x("k_aslSoilAir") * x("K_AirWater") + x("k_aslSoilWater"))

  overal_factor <- x("K_SoilWater") * x("d")

  cbind(p, "k_volat" = 1 / ((q1 + q2) * overal_factor))
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
  x <- kwb.utils::createAccessor(p)
  cbind(p, "k_leach" = (x("f_inf") * x("rain")) / (x("K_SoilWater") * x("d")))
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
  x <- kwb.utils::createAccessor(p)
  cbind(p, "k_plant" =
    (x("BCF") * x("Y") * x("DM_plant") / 100) /
    (x("t_g") * x("d") * x("rho_soil"))
  )
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
  x <- kwb.utils::createAccessor(p)
  cbind(p, "k_bio" = log(2) / x("DT50"))
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
  x <- kwb.utils::createAccessor(p)
  cbind(p, "D_air" =  x("D_air_tot") / (x("d") * x("rho_soil")))
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
  x <- kwb.utils::createAccessor(p)
  cbind(p, "PNEC_soil" = x("PNEC_water") / 1000 * (x("K_oc") * 0.0104 + 0.174))
}
