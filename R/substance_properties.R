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

  exponents <- (beta_ph * pH + beta_org * log10(org_c) + beta_conc * log10(conc) + constant)

  reg_out <- # either this is K_d (-> direct in L/kg) or this is the concentration in porewater (-> indirect in µg/L)
    10^exponents

  if(regType == "indirect"){
    reg_out <- conc * 1000 / reg_out
  }

  a <- quantile(pH, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
  b <- quantile(org_c, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
  c <- quantile(log10(org_c), c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
  d <- quantile(beta_ph, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
  e <- quantile(beta_org, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
  f <- quantile(beta_conc, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
  g <- quantile(constant, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
  h <- quantile(exponents, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
  i <- quantile(reg_out, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))

  message(a)
  message(b)
  message(d)
  message(d)
  message(e)
  message(f)
  message(g)
  message(h)
  message(i)


  return(reg_out)
}

summary_ <- function(series){
  q <- quantile(series, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
  message(q)
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
