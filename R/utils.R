#' Random samples from specified distribution
#'
#' Draw n samples from uniform, normal, truncated normal (-> positive only),
#' lognormal or gamma distributions based on two input values
#'
#' @param dist_name Character vector specifying the Name of the distribution.
#' Either "none", "uniform", "normal" "tnormal" (for truncated), "lognormal",
#' "gamma", "derived", "tderived" or "logderived"
#' @param value_1,value_2 Distribution parameters based on dist_name: \cr
#' "none": value_1 is used as constantvalue, value_2 not used \cr
#' "normal": value_1 is mean, value_2 is standard deviation \cr
#' "tnormal": value_1 is mean, value_2 is standard deviation \cr
#' "lognormal": value_1 is log mean, value_2 is standard log deviation
#' (both natural logarithms) \cr
#' "gamma": value_1 is shape, value_2 is rate \cr
#' "uniform": value_1 is minimum, value_2 is maximum \cr
#' "derived": value_1 is minimum of uniform distribution, value_2 is maximum of
#' uniform distribution. Uniform distribution accounts for 95% of all data \cr
#' "tderived": value_1 is minimum of uniform distribution, value_2 is maximum of
#' uniform distribution. Uniform distribution accounts for 95% of all data \cr
#' "logderived": value_1 is minimum of log-uniform distribution, value_2 is
#' maximum of log-uniform distribution (values not log-scaled). Log-Uniform
#' distribution accounts for 95% of all data \cr
#'
#' @param n Number of samples to be drawn
#' @param shift An numeric value defining a subsequent shift of the distribution
#' The default is 0 (-> no shift)
#' @param seed A numeric value to set the seed for random selection. The default
#' is NULL -> no seed
#'
#' @return Numeric vector of length n with randomly drawn samples of the
#' specified distribution
#'
#' @details
#'
#' The values specified in value_1 and value_2 represent min and max for
#' "uniform", mean and standard deviation for "normal" and "tnormal", log
#' mean and log standard deviation (both natural logarithm) for "lognormal",
#' shape and rate for"gamma". The shift value is especially interesting for lognormal
#' or gamma distributions.
#'
#' @export
#' @importFrom stats runif rnorm rlnorm rgamma
#'
rdist <- function(
    dist_name, value_1, value_2, n, shift = 0, seed = NULL
){
  set.seed(seed)

  v_out <- switch(
    dist_name,
    none = rep(value_1, n),
    uniform = runif(n = n, min = value_1, max = value_2),
    normal = rnorm(n = n, mean = value_1, sd = value_2),
    tnormal = rtnorm(n = n, mean = value_1, sd = value_2, a = 0),
    lognormal = rlnorm(n = n, meanlog = value_1, sdlog = value_2),
    gamma = rgamma(n = n, shape = value_1, rate = value_2),
    derived = rderived(n = n, min = value_1, max = value_2),
    tderived = rderived(n = n, min = value_1, max = value_2, a = 0),
    logderived = rlderived(n = n, min = value_1, max = value_2)
  )

  if(is.null(v_out)){
    stop(paste0(dist_name, " is not one of the defined distributions."))
  }

  # re-initialize seed -> no seed
  set.seed(NULL)
  v_out + shift
}

#' Distribution derived by comparable circumstances
#'
#' This function combines a uniform distribution from minimum to maximum with
#' the upper and lower tail of a normal distribution.
#'
#' @param n Number of samples to be drawn
#' @param min Minimum value of normal comparables
#' @param max Maximum value of normal comparables
#' @param a Minimum of distribution (Default is -Inf)
#'
#' @return numeric vector of length n with randomly drawn values according to
#' the distribution
#'
#' @details
#' 95%, 2.5% and 2.5% of all drawn values originate from the uniform
#' distribution, the lower tail, and the upper tail of a normal distribution,
#' respectively. The normal distribution is characterized by the mean of
#' minimum and maximum. The standard deviation is derived from the 2.5% and
#' 97.5% quantile definied by the minimum and maximum values.
#' (sd = (max - mean) / 1.959963)
#'
#' The number of Fields is multiplied by 38 for the uniform distribution, while
#' it is multiplied by 1 for the tails of the normal distribution (38/40 = 95%).
#' Finally, the defined number of fields is randomly drawn from the vector.
#'
#' @export
#' @importFrom stats runif
#'
rderived <- function(
  n, min, max, a = -Inf
){

  mean <- mean(c(min,max))
  sd <- (max - mean) / 1.959963
  sample(
    x = c(sample(x = runif(min, max, n = n * 38)),
          rtnorm(n = n, mean = mean, sd = sd, b = mean - 1.959963 * sd, a = a),
          rtnorm(n = n, mean = mean, sd = sd, a = mean + 1.959963 * sd))[1:n],
    size = n)

}

#' Log-Distribution derived by comparable circumstances
#'
#' This function draws random values from a uniform distribution (minimum to
#' maximum) combined with the upper and lower tail of a log normal
#' distribution. Minimum and maximum values are transformed to a log10 scale
#' first.
#'
#' @param n Number of samples to be drawn
#' @param min Minimum value of normal comparables
#' @param max Maximum value of normal comparables
#'
#' @return numeric vector of length n with randomly drawn values according to
#' the distribution
#'
#' @details
#' 95%, 2.5% and 2.5% of all drawn values originate from the log-uniform
#' distribution, the lower tail, and the upper tail of a log-normal distribution,
#' respectively. The log-normal distribution is characterized by the mean of
#' log-minimum and log-maximum. The standard deviation is derived from the 2.5% and
#' 97.5% quantile defined by the minimum and maximum values.
#' sd = (log10(max) - log10(mean)) / 1.959963
#'
#' The number of Fields is multiplied by 38 for the uniform distribution, while
#' it is multiplied by 1 for the tails of the log-normal distribution
#' (38 / 40 = 95%)
#' Finally, the defined number of fields is randomly drawn from the vector.
#'
#' @export
#' @importFrom stats runif
#'
rlderived <- function(
  n, min, max
){
  min <- log10(min)
  max <- log10(max)

  mean <- mean(c(min,max))
  sd <- (max - mean) / 1.959963

  sample(
    x = 10^c(sample(x = runif(min, max, n = n * 38)),
       rtnorm(n = n , mean = mean, sd = sd, b = mean - 1.959963 * sd),
       rtnorm(n = n, mean = mean, sd = sd, a = mean + 1.959963 * sd)),
    size = n)

}

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
#' @importFrom stats pnorm qnorm runif
#'
rtnorm <- function(
  n, mean, sd, a = -Inf, b = Inf
){
  qnorm(p = runif(n = n,
                  min = pnorm(q = a, mean = mean, sd = sd),
                  max = pnorm(q = b, mean = mean, sd = sd)),
        mean = mean, sd = sd)
}
