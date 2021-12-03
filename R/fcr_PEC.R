#' Calculation of the pollutant concentation course in top soil
#'
#' Estimation is based on the FCR prepared Monte Carlo table. The output
#' rate is split into one rate with plant uptake during plant growth and
#' one rate without plant uptake during winter time.
#'
#' A time period can be considered as growing period if 1) plants grow and 2)
#' plants will be harvested after growing. Otherwise the uptaken pollutants will
#' return to the soil.
#'
#' @param p Parameter table created with [oneYear_matrix()]
#' @param growing_period Numeric value specifiyng the number of days with plant
#' growth
#' @param t_res Temporal resolution to be returned in the output matrix
#' (no effect on the calculation)
#'
#' @return
#' Matrix with number of columns equal to the rows of p and number of rows
#' depending on the defined timesteps with t_res
#'
#' @export
#'
one_year <- function(p, growing_period, t_res){
  c_out <- temp_c_profile(
    conti_input = p[,"D_air"],
    output_rate = p[,"k1"],
    c_i = p[,"c_0"],
    t_max = growing_period, t_res = t_res, t_beg = 0)

  c_out2 <- temp_c_profile(
    conti_input = p[,"D_air"],
    output_rate = p[,"k2"],
    c_i = c_out[nrow(c_out),],
    t_max = 365 - growing_period, t_res = t_res, t_beg = growing_period)

  rbind(c_out, c_out2)
}

#' Predicted soil concentration
#'
#' Average predicted environmental concentration (PEC) over d days after
#' fertilizer application. The Calculation is based on the FCR prepared Monte
#' Carlo table
#'
#' @param p Parameter table created with [oneYear_matrix()]
#' @param d The number of days after fertilizer application. Somewhere between
#' 1 and the number of days of the growing period, since plant uptake is
#' considered
#'
#' @return Numeric vector of concentrations per field in mg/kg soil
#'
#' @export
#'
get_PEC_soil <- function(p, d){
  p[,"D_air"] / p[,"k1"] +
    1 / (p[,"k1"] * d) *
    (p[,"c_0"] - p[,"D_air"] / p[,"k1"]) *
    (1 - exp(- p[,"k1"] * d))
}

#' Predicted human consumption
#'
#' Predicted human consumption via crop consumption. The calculation is based
#' on the average predicted soil concentration within the growing period.
#'
#' @param p Parameter table created with [oneYear_matrix()] and extended with
#' [add_variables()]
#' @param d The number of days of the growing period.
#' @param food_only If TRUE, the predicted human consumption via food is
#' multiplied by 2 to compensate the disregard of water consumption
#'
#' @return Numeric vector of human consumption via crops in µg/d
#'
#' @export
#'
get_PEC_human <- function(p, d, food_only){
  PEC_soil <- get_PEC_soil(p = p, d = d)

  v_out <- PEC_soil * p[,"BCF"] * p[,"m_crop"] * p[,"f_resorbed"]
  if(food_only){
    v_out * 2
  } else {
    v_out
  }
}

#' Predicted pore water concentration
#'
#' Average predicted environmental concentration (PEC) over d days after
#' fertilizer application. The Calculation is based on the FCR prepared Monte
#' Carlo table
#'
#' @param p Parameter table created with [oneYear_matrix()] and extended with
#' [add_variables()]
#' @param d The number of days after fertilizer application. Somewhere between
#' 1 and the number of days of the growing period, since plant uptake is
#' considered
#'
#' @return Numeric vector of concentrations per field in µg/L
#'
#' @export
#'
get_PEC_porewater <- function(p, d){
  get_PEC_soil(p = p, d = d) * p[,"rho_soil"] / p[,"K_SoilWater"]
}







