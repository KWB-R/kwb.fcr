#' Estimate the soil sorption coefficient in two different ways
#'
#' Estimation via direct or indirect logarithmic linear regression including
#' pH, organic carbon and soil concentration. (log10 is used for all variables)
#'
#' The constant value, the regression parameter (beta) and the soil
#' characteristics can be single numeric values or vectors of the same length.
#'
#' @param constant Log constant in linear Kd regression
#' @param beta_ph,beta_org,beta_conc  Log coefficient for pH, organic carbon and soil concentration
#' in log-linear Kd regression
#' @param regType Chacter specifying the way the K_d value is calculated. Either
#' "direct" which means that the regression directly leads to the sorption
#' coefficient or"indirect" which means the regression estimates a concentration
#' in pore water and the sorption coefficient is subsequently be calculated by
#' the quotient of soil and pore water concentration.
#' @param pH pH value
#' @param org_c Organic carbon content in percent.
#' @param conc Soil concentration in mg/kg
#'
#' @return
#' A vector of sorption coefficients in L/kg
#'
#' @export
Kd_regression <- function(
  constant, beta_ph, beta_org, beta_conc, regType = "direct", pH, org_c, conc
){
  reg_out <- # either this is K_d (-> direct in L/kg) or this is the concentration in porewater (-> indirect in µg/L)
    10^(beta_ph * pH + beta_org * log10(org_c) + beta_conc * log10(conc) + constant)
  if(regType == "direct"){
    reg_out
  } else if(regType == "indirect"){
    conc * 1000 / reg_out
  }
}

#' Estimate the bio concentration factor in two different ways
#'
#' Estimation via direct or indirect logarithmic (log 10?) linear regression
#' including pH, organic carbon and soil concentration
#'
#' The constant value, the regression parameters (beta) and the soil
#' characteristics can be single numeric values or vectors of the same length.
#'
#' @param constant Log constant in linear Kd regression
#' @param beta_ph,beta_org,beta_conc  Log coefficient for pH, organic carbon and soil concentration
#' in log-linear Kd regression
#' @param regType Chacter specifying the way the K_d value is calculated. Either
#' "direct" which means that the regression directly leads to the sorption
#' coefficient or"indirect" which means the regression estimates a concentration
#' in pore water and the sorption coefficient is subsequently be calculated by
#' the quotient of soil and pore water concentration.
#' @param pH pH value
#' @param org_c Organic carbon content in percent.
#' @param conc Soil concentration in mg/kg
#'
#' @return
#' A vector of bio concentration factors without unit. It is defined as
#' concentration in plant dry matter divided by concentration in soil.
#'
#' @export
BCF_regression <- function(
  constant, beta_ph, beta_org, beta_conc, regType = "direct",
  pH, org_c, conc
){
  reg_out <- # either this is BCF (-> direct) or this is the concentration in plants (-> indirect in mg/kg DM)
    10^(beta_ph * pH + beta_org * log10(org_c) + beta_conc * log10(conc) + constant)

  if(regType == "direct"){
    reg_out
  } else if(regType == "indirect"){
    reg_out / conc
  }
}
