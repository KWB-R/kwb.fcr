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

  if (!all.equal(result_1, result_2)) {
    stop("temp_c_profile_v1() and temp_c_profile_v2 returned different results!")
  }

  result_1
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
