#' Calculation of the pollutant concentation in top soil
#'
#'
#' @param conti_input Numeric vector of atmospheric depositions in mg/kg topsoil
#' @param output_rate Numeric vector of absolute pollutant decay rates.
#' @param c_i Numeric vector of inital concentrations in top soil
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
temp_c_profile <- function(conti_input, output_rate, c_i, t_max, t_res, t_beg = 0){
  mat_out <- mapply(
    function(IN, OUT, START)
      IN / OUT - (IN / OUT - START) *
      exp(-OUT * seq(from = 0, to = t_max, by = t_res)),
    conti_input, output_rate, c_i)

  dimnames(mat_out) <-
    list(paste0("t", seq(from = 0 + t_beg,
                         to = t_max + t_beg,
                         by = t_res)),
         paste0("n", 1:ncol(mat_out)))
  mat_out
}




