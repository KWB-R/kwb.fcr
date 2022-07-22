result_aggregation <- function(fcr, fcr0){

  RQ_s <- fcr_out$PEC[["soil"]][100,] / fcr_out$model_variables[,"PNEC_soil"]
  RQno_s <- fcr_out0$PEC[["soil"]][100,] / fcr_out$model_variables[,"PNEC_soil"]
  RQ0_s <- fcr_out0$model_variables[,"c_i"] / fcr_out$model_variables[,"PNEC_soil"]

  RQ_w <- fcr_out$PEC[["porewater"]][100,] / fcr_out$model_variables[,"PNEC_water"]
  RQno_w <- fcr_out0$PEC[["porewater"]][100,] / fcr_out$model_variables[,"PNEC_water"]
  RQ0_w <- fcr_out0$model_variables[,"c_water"] / fcr_out$model_variables[,"PNEC_water"]


  c("soil" = fcr_parameters(RQ = RQ_s, RQno = RQno_s, RQ0 = RQ0_s),

    "water" = fcr_parameters(RQ = RQ_w, RQno = RQno_w, RQ0 = RQ0_w),

    "dep_impact" = round(cor(x = fcr_out$PEC[["soil"]][100,],
            y = fcr_out$model_variables[,"D_air"],
            method = "spearman"), 2))
}

fcr_parameters <- function(RQ, RQno, RQ0){

  high_risk <- which(RQ > 1 & RQ > RQ0)
  if(length(high_risk) == 0L){
    r95 <- risk_inc <- 0
    inc_abs <- 0
  } else {
    delta <- RQ[high_risk]  -  RQno[high_risk]

    r95 <- signif(quantile(x = RQ[high_risk], probs = 0.95), 3)
    # Absolute increase of risk (95th Quantile of all high-risk scenarios)
    inc_abs <- signif(quantile(x = delta, probs = 0.95), 3)
    # Increase of risk in % (95th Quantile of all high-risk scenarios)
    risk_inc <- inc_abs  / r95 * 100
  }
  # Increase of high-risk situations in %
  situation_inc <- if(length(high_risk) > 0L){
    signif(
      (1 - sum(RQno > 1 & RQno > RQ0) / sum(RQ > 1 & RQ > RQ0)) * 100,
      digits = 3)
  } else {
    0
  }

  round(c("q95" = unname(r95),
          "RQ_delta_abs" = unname(inc_abs),
          "RQ_delta_spec" = unname(risk_inc),
          "high_risk" = unname(situation_inc)
          ), 3)
}

interprate_parameters <- function(
  high_risk_th = c(1, 10, 100),
  severity_th = c(10, 100, 1000),
  rq_increase_th = c(1, 10, 100),
  df_in
){
  mat <- matrix(data = c(1,1,1,2,2,2,2,3,2,2,3,4,2,3,4,5), nrow = 4, ncol = 4, byrow = T)

  c_column <- as.numeric(
    cut(x = df_out$soil.RQ_delta_spec, breaks = c(0, rq_increase_th, Inf), include.lowest = T))
  c_row <- as.numeric(
    cut(x = df_out$soil.high_risk, breaks = c(0, high_risk_th, Inf), include.lowest = T))

  df_in$soil_class <- NA
  for(i in seq_along(c_row)){
    df_in$soil_class[i] <- mat[c_row[i], c_column[i]]
  }

  c_column <- as.numeric(
    cut(x = df_out$water.RQ_delta_spec, breaks = c(0, rq_increase_th, Inf), include.lowest = T))
  c_row <- as.numeric(
    cut(x = df_out$water.high_risk, breaks = c(0, high_risk_th, Inf), include.lowest = T))


  df_in$water_class <- NA
  for(i in seq_along(c_row)){
    df_in$water_class[i] <- mat[c_row[i], c_column[i]]
  }

  df_in$soil_class <- sapply(seq_along(df_in$soil_class), function(i){
    add <- max(which(c(-Inf, severity_th) < df_in$soil.q95[i])) - 1
    if(df_in$dep_impact[i] > 0.7){
      add <- 0
    }
    df_in$soil_class[i] + add
  })

  df_in$water_class <- sapply(seq_along(df_in$water_class), function(i){
    add <- max(which(c(-Inf, severity_th) < df_in$water.q95[i])) - 1
    if(df_in$dep_impact[i] > 0.7){
      add <- 0
    }
    df_in$water_class[i] + add
  })

  df_in$soil_class[df_in$soil_class > 5] <- 5
  df_in$water_class[df_in$water_class > 5] <- 5
  df_in
}

input_path <- "Y:/WWT_Department/Projects/NextGen/Data-Work packages/WP2/QCRA/fcr/input"

siteName <- "braun"
fertilizerName <- "sludgePhorw"
pollutantNames <- c("as", "cd", "cr", "cu", "hg", "ni", "pb", "zn", "benzo", "pcdd")
pollutantNames <- c("as","cd", "cr", "cu", "hg", "ni", "pb", "zn")
pollutantNames <- c("cr")

output <- list()
for(pollutantName in pollutantNames){

  print(pollutantName)
  dat_0 <- kwb.fcr::read_fcr_input(input_path = input_path,
                                   pollutantName = pollutantName,
                                   siteName = siteName,
                                   fertilizerName = "none")
  dat_in <- kwb.fcr::read_fcr_input(input_path = input_path,
                                    pollutantName = pollutantName,
                                    siteName = siteName,
                                    fertilizerName = fertilizerName)

  # for longterm application -----------------------------------------------------
  fcr_out0 <- kwb.fcr::longterm_PEC(dat = dat_0$dat,
                                    info = dat_0$info,
                                    years = 100,
                                    nFields = 10000,
                                    use_mixing_factor = FALSE,
                                    food_only = TRUE,
                                    growing_period = 180,
                                    t_res = 10,
                                    traceBackVariables = FALSE)
  fcr_out <- kwb.fcr::longterm_PEC(dat = dat_in$dat,
                                   info = dat_in$info,
                                   years = 100,
                                   nFields = 10000,
                                   use_mixing_factor = FALSE,
                                   food_only = TRUE,
                                   growing_period = 180,
                                   t_res = 10,
                                   traceBackVariables = FALSE)
  output[[pollutantName]] <- result_aggregation(fcr = fcr, fcr0 = fcr0)
}

df_out <- data.frame(do.call(rbind, output))
interprate_parameters(df_in = df_out,
                      high_risk_th = c(1, 10, 100),
                      rq_increase_th = c(1,10,100),
                      severity_th = c(10, 100, 1000))







