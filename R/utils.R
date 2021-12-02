#' Truncate normal distribution
#'
#' Truncate normal distribution at a minimum or maximum value
#'
#' @param n Number of samples to be drawn
#' @param mean Mean value of normal distribution
#' @param sd Standard deviation of Normal distribution
#' @param a Minimum of distribution (Default is -Inf)
#' @param b Maximum of distribution (Default is Inf)
#'
#' @return numeric vector of length n with randomly drawn numbers of the
#' truncated normal distribution
#'
#' @export
#' @importFrom stats pnorm qnorm
#'
rtnorm <- function(
  n, mean, sd, a = -Inf, b = Inf
){
  # Helper function to avoid repetition of mean = mean, sd = sd
  pn <- function(q) pnorm(q = q, mean = mean, sd = sd)

  qnorm(
    p = runif(n = n, min = pn(a), max = pn(b)),
    mean = mean,
    sd = sd
  )
}

#' Random samples from specified distribution
#'
#' Draw n samples from uniform, normal, truncated normal (-> positive only),
#' lognormal or gamma distributions based on two input values
#'
#' The values spefecified in valua_1 and value_2 represent min and max for
#' "uniform", mean and standard deviation for "normal" and "tnormal", log
#' mean and log standard devation for "lognormal" and shape and rate for
#' "gamma", respectively. The shift value is especially interisting for lognormal
#' or gamma distributions.
#'
#' @param value_1,value_2 Distribution parameters (see Details)
#' @param n Number of samples to be drawn
#' @param dist_name Character vector specifying the Name of the distribution.
#' Either "none", "uniform", "normal" "tnormal" (for truncated), "lognormal" or
#' "gamma"
#' @param shift An numeric value defining a subsequent shift of the distribution
#' The default is 0 (-> no shift)
#' @param seed A numeric value to set the seed for random selection. The default
#' is NULL -> no seed
#'
#' @return Numeric vector of length n with randomly drawn samples of the
#' specified distribution
#'
#' @export
#' @importFrom stats runif rnorm rlnorm rgamma
#'
rdist <- function(
  value_1, value_2, n, dist_name, shift = 0, seed = NULL
){
  set.seed(seed)

  v_out <- switch(
    dist_name,
    none = rep(value_1, n),
    uniform = runif(n = n, min = value_1, max = value_2),
    normal = rnorm(n = n, mean = value_1, sd = value_2),
    tnormal = rtnorm(n = n, mean = value_1, sd = value_2, a = 0),
    lognormal = rlnorm(n = n, meanlog = value_1, sdlog = value_2),
    gamma = rgamma(n = n, shape = value_1, rate = value_2)
  )

  # re-initialize seed -> no seed
  set.seed(NULL)
  v_out + shift
}
