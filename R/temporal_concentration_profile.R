#' Calculation of the pollutant concentration in top soil
#'
#'
#' @param conti_input Numeric vector of atmospheric depositions in mg/kg topsoil
#' @param output_rate Numeric vector of absolute pollutant decay rates.
#' @param c_i Numeric vector of initial concentrations in top soil
#' @param t_max Numeric value with the last considered timestep (unit
#' depends on the unit of the decay rate)
#' @param t_res Temporal resolution (numeric value) of the timesteps returned
#' (unit depends on the unit of the decay rate)
#' @param t_beg Numeric value of the first considered timestep. This value does
#' not influence the calculation itself, it is only used for row names.
#'
#' @return
#' Matrix with columns corresponding to the length of the provided
#' numeric vectors and rows depending on the defined timesteps with
#' t_max and t_res
#'
#' @export
#'
temp_c_profile <- function(
  conti_input, output_rate, c_i, t_max, t_res, t_beg = 0
){

  t1 <- system.time(result_1 <- temp_c_profile_v1(
    conti_input, output_rate, c_i, t_max, t_res, t_beg
  ))

  t2 <- system.time(result_2 <- temp_c_profile_v2(
    conti_input, output_rate, c_i, t_max, t_res, t_beg
  ))

  cat("temp_c_profile_v1():\n")
  print(t1)

  cat("temp_c_profile_v2():\n")
  print(t2)

  if (! all.equal(t1, t2)) {
    stop("temp_c_profile_v1() and temp_c_profile_v2 returned different results!")
  }

  t1
}

temp_c_profile_v1 <- function(conti_input, output_rate, c_i, t_max, t_res, t_beg = 0){
  mat_out <- mapply(
    function(IN, OUT, START)
      IN / OUT -
      (IN / OUT - START) *
      exp(- OUT * unique(c(seq(from = 0, to = t_max, by = t_res), t_max))),
    conti_input, output_rate, c_i)

  dimnames(mat_out) <-
    list(paste0("t",
                unique(c(seq(from = 0, to = t_max,by = t_res), t_max) + t_beg)),
         paste0("n", 1:ncol(mat_out)))

  mat_out
}

temp_c_profile_v2 <- function(
  conti_input, output_rate, c_i, t_max, t_res, t_beg = 0
){
  times <- seq(from = 0, to = t_max, by = t_res)

  # Number of elements in conti_input, output_rate and c_i
  n <- length(output_rate)

  # Create matrix by putting time vectors n-times on top of each other
  times_matrix <- matrix(rep(times, n), nrow = n, byrow = TRUE)

  # Calculate matrix of factors (= probabilities?)
  p <- exp(-output_rate * times_matrix)

  # Calculate output matrix
  mat_out <- p * c_i + (1 - p) * conti_input / output_rate

  dimnames(mat_out) <- list(
    paste0("n", seq_len(n)),
    paste0("t", times + t_beg)
  )

  t(mat_out)
}
