#' Risk quotients and high risk scenarios with and without fertilization
#'
#' @param fertPEC A vector of predicted environmental concentrations at the
#' end of simulation with fertilization
#' @param noFertPEC A vector of predicted environmental concentrations at the
#' end of simulation without fertilization. Both vectors must be based on the
#' same environmental conditions during simulation.
#' @param PNEC A numeric value, defining the predicted no-effect concentration
#'
#' @return
#' Data frame with PECs, Risk Quotient and identification of high-risk scenarios
#' for both simulations, with and without fertilization, as well as the
#' risk quotient difference due to fertilization
#'
#' @export
#'
get_risk <- function(
    fertPEC, noFertPEC, PNEC
){
  df_out <- data.frame(
    "Fert" = fertPEC,
    "noFert" = noFertPEC
  )
  df_out["FertRisk"] <- df_out$Fert / PNEC
  df_out["noFertRisk"] <- df_out$noFert / PNEC

  df_out["highRisk"] <- df_out$FertRisk > 1
  df_out["highRisk_without"] <- df_out$noFertRisk > 1

  df_out["riskIncrease"] <- df_out$FertRisk - df_out$noFertRisk
  df_out
}

#' Risk Aggregation
#' Calculation of Delta Risk Quotient and Maximum Risk
#'
#' @param df_risk A dataframe created by [get_risk()]
#'
#' @details
#' The Delta Risk Quotient (RQ) is the average increase of high-risk scenarios
#' caused by fertilization. A high-risk scenarios are defined as scenraios with
#' a final RQ higher than 1. The increase refers to a comparison between two
#' similar scenarios, the only difference is the application of fertilizer.
#'
#' The maximum risk is the average RQ of the upper 5% of the RQ-Distribution.
#' It is meant to describe the upper tail of the distribution.
#'
#' Further information can be found in the report of the Eurpean Horizon 2020
#' project Nextgen "Assessment and risk analysis of NextGen demo case solutions"
#' (p.151)
#'
#' \href{https://mp.watereurope.eu/media/publications/D2.1_NextGen_LCA_Risk.pdf}{Nextgen: Deliverable 2.1}
#'
#' @return
#' A list of 2, the delta risk quotient and the maximum risk
#'
#'
#' @export
#'
risk_aggregation <- function(df_risk){
  relevant_increases <- df_risk$riskIncrease[df_risk$highRisk]
  delta_RQ <- mean(relevant_increases)

  q_relevant <- quantile(df_risk$FertRisk, c(0.95))
  highest_5 <- df_risk$FertRisk[df_risk$FertRisk >= q_relevant]
  Risk_max <- mean(highest_5)

  list("delta_RQ" = round(delta_RQ, 3),
       "Risk_max" = round(Risk_max, 1),
       "Risk" = risk_interpretation(delta_RQ = delta_RQ, risk_max = Risk_max))
}

#' Risk Interpretation
#' Assigns a risk to the aggregated risk parameters
#'
#' @param delta_RQ Numeric vector of length 1: Difference of of high-risk
#' simulations between scenarios with and without fertilization
#' @param risk_max Numeric vector of length 1: Mean value of the Upper five
#' percent of RQ distribution
#'
#' @details
#' Further information can be found in the report of the Eurpean Horizon 2020
#' project Nextgen "Assessment and risk analysis of NextGen demo case solutions"
#' (p.151)
#'
#' \href{https://mp.watereurope.eu/media/publications/D2.1_NextGen_LCA_Risk.pdf}{Nextgen: Deliverable 2.1}
#'
#' @return
#' A list of 2, the delta risk quotient and the maximum risk
#'
#' @export
#'
risk_interpretation <- function(
    delta_RQ, risk_max
){
  delta_RQ_limits <- c(0.01, 0.1, 1, Inf)
  risk_max_limits <- c(2, 5, Inf)

  i <- min(which(delta_RQ < delta_RQ_limits))
  j <- min(which(risk_max < risk_max_limits))

  interpretation <- factor(
    c(
      "negligible", "negligible", "acceptable", "increasing concern",
      "negligible", "acceptable", "increasing concern", "unacceptable",
      "acceptable", "increasing concern", "unacceptable", "unacceptable"),
    levels = c("negligible", "acceptable", "increasing concern", "unacceptable"),
    ordered = TRUE)

  r_table <- cbind(
    expand.grid(delta_RQ_limits, risk_max_limits),
    "risk" = interpretation)

  r_table$risk[r_table$Var1 == delta_RQ_limits[i] &
    r_table$Var2 == risk_max_limits[j]]
}



#' #' Risk interpretation
#' #'
#' #' @param df_risk A dataframe created by [get_risk()]
#' #'
#' #' @return
#' #' A numeric vector with the following information:
#' #' 1) The number of simulations that end up in a high risk (RQ > 1) with
#' #' fertilization,
#' #' 2) The proportion of how many of those high-risk simulations are only due to
#' #' fertilization,
#' #' 3) Mini
#' #'
#' #' @importFrom stats sd
#' #'
#' risk_aggregation_v0 <- function(df_risk){
#'   # How many high risk scenarios only appear because of fertilization?
#'   n_highRisk <- c("n_highRisk" = sum(df_risk$highRisk))
#'   caused_by_fert <-
#'     c("caused_by_fert" =
#'         (n_highRisk - sum(df_risk$highRisk_without)) / n_highRisk
#'     )
#'
#'   # risk quotients and increases
#'   rq <- df_risk$FertRisk
#'   inc<- df_risk$riskIncrease
#'   relevant_rq <-rq[df_risk$highRisk]
#'   relevant_inc <- inc[df_risk$highRisk]
#'
#'   range_relevant <- range(relevant_rq)
#'   names(range_relevant) <- c("min_relevant", "max_relevant")
#'   q_relevant <- quantile(rq, c(0.95))
#'   highest_5 <- rq[rq > q_relevant]
#'
#'
#'   stats <- c(
#'     "mean_all" = mean(rq),
#'     "sd_all" = sd(rq),
#'     "mean_highest_5" = mean(highest_5),
#'     "mean_relevant" = mean(relevant_rq),
#'     "sd_relevant" = sd(relevant_rq),
#'     "mean_delta_all" = mean(inc),
#'     "sd_delta_all" = sd(inc),
#'     "mean_delta_relevant" = mean(relevant_inc),
#'     "sd_delta_relevant" = sd(relevant_inc)
#'   )
#'
#'   cv_fert_relevant <-
#'     c("cv_fert_relevant" =
#'         unname(stats["sd_delta_relevant"] / stats["mean_relevant"]))
#'
#'   cv_fert_delta <-
#'     c("cv_fert_delta" =
#'         unname(stats["sd_delta_relevant"] / stats["mean_delta_relevant"]))
#'
#'   c(n_highRisk, caused_by_fert, range_relevant,q_relevant, stats, cv_fert_relevant, cv_fert_delta)
#' }
