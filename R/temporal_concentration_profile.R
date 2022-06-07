#' Calculation of the pollutant concentration in top soil
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
  times <- unique(c(seq(from = 0, to = t_max, by = t_res), t_max))

  # Number of elements in conti_input, output_rate and c_i
  n <- length(output_rate)

  # Create matrix by putting time vectors n-times on top of each other
  times_matrix <- matrix(rep(times, n), nrow = n, byrow = TRUE)

  # Calculate matrix of factors (= probabilities?)
  p <- exp(-output_rate * times_matrix)

  # Calculate output matrix
  mat_out <- p * c_i + (1 - p) * conti_input / output_rate

  dimnames(mat_out) <- list(
    paste0("n", 1:n),
    paste0("t", times + t_beg)
  )

  t(mat_out)
}
