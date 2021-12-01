#' Creates a matrix with all Input Data for one year Concentration calculation
#'
#' The information of the Excel input files are used to prepare a Monte Carlo
#' based data input for the calculation of a yearly concentration dynamic.
#'
#' @param dat List with all the input variables. This list is produced by
#' function [read_fcr_input()] from the Excel sheets.
#' @param c_i The initial concentration before fertilizer application
#' @param nFields The number Monte Carlo Simulations. This can be seen as
#' different agriculatural fields with different charactersitics.
#' @param firstYear We will see
#' @param use_mixing_factor If TRUE, a mixing factor for porewater dilution by
#' unpolluted groundwater is used to get the Risk quotient for Groundwater. The
#' defahutl is FALSE. In that case pore water concentration is identical with
#' groundwater concentration as is the approach in the Technical Guidance
#' Document
#'
#' @return
#' A Matrix with all available input data for the Assessment. The columns
#' represent the input variabls, the rows correspond to the definied number of
#' fields aka. number of Monte Carlo cycles.
#'
#' @export
#'
oneYear_matrix <- function(
  dat, c_i, nFields, firstYear = FALSE, use_mixing_factor = FALSE
){

  p <- create_mcs_input(data_list = dat, nFields = nFields)

  if(firstYear){
    c_0 <- c_i
  } else {
    c_add <- # in mg / (kg * a)
      (p[,"c_fert"] * p[,"p_app"]) / # in mg / (ha * a)
      (p[,"d"] * p[,"rho_soil"] * 10000)
    # sum of concentration from previous year and added pollutant
    c_0 <- c_add + c_i # c_end not defined yet
  }
  p <- cbind(p, c_0)

  if(use_mixing_factor){
    MF <- 1 + (p[,"v_G"] * p[,"m_d"] /
                 (p[,"rain"] * 365 * p[,"f_inf"] * p[,"l_field"]))
  } else {
    MF <- 1
  }
  cbind(p, MF)
}

#' List of Variables into Monte Carlo simulation input table
#'
#' @param data_list A list of input variable tables. Each table must have
#' columns "value_1", "value_2", "distribution", "site_specific", "pID" and
#' "shift"
#' @param nFields Number of Monte Carlo Cycles
#'
#' @return
#' A matrix with all input variables from the list
#'
#' @export
#'
create_mcs_input <- function(
  data_list, nFields
){
  sapply(data_list, function(x){
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
}

