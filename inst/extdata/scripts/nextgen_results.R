get_risk <- function(
    fertPEC, noFertPEC,  PNEC
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


risk_aggregation <- function(df_risk){
  # How many high risk scenarios only appear because of fertilization?
  n_highRisk <- c("n_highRisk" = sum(df_risk$highRisk))
  caused_by_fert <-
    c("caused_by_fert" =
        (n_highRisk - sum(df_risk$highRisk_without)) / n_highRisk
    )

  # risk quotients and increases
  rq <- df_risk$FertRisk
  inc<- df_risk$riskIncrease
  relevant_rq <-rq[df_risk$highRisk]
  relevant_inc <- inc[df_risk$highRisk]

  range_relevant <- range(relevant_rq)
  names(range_relevant) <- c("min_relevant", "max_relevant")
  q_relevant <- quantile(rq, c(0.95))
  highest_5 <- rq[rq > q_relevant]


  stats <- c(
    "mean_all" = mean(rq),
    "sd_all" = sd(rq),
    "mean_highest_5" = mean(highest_5),
    "mean_relevant" = mean(relevant_rq),
    "sd_relevant" = sd(relevant_rq),
    "mean_delta_all" = mean(inc),
    "sd_delta_all" = sd(inc),
    "mean_delta_relevant" = mean(relevant_inc),
    "sd_delta_relevant" = sd(relevant_inc)
  )

  cv_fert_relevant <-
    c("cv_fert_relevant" =
        unname(stats["sd_delta_relevant"] / stats["mean_relevant"]))

  cv_fert_delta <-
    c("cv_fert_delta" =
        unname(stats["sd_delta_relevant"] / stats["mean_delta_relevant"]))

  c(n_highRisk, caused_by_fert, range_relevant,q_relevant, stats, cv_fert_relevant, cv_fert_delta)
}

input_path <-
  "C:/Users/mzamzo/Desktop/Masuren_Arbeit/fcr/input"
  # "Y:/WWT_Department/Projects/NextGen/Data-Work packages/WP2/QCRA/fcr/input"
dir(file.path(input_path, "fertilizers"))

siteName <- "germanMix"
fertilizerName <- "assBR_90"
pollutantNames <-
   #c("as", "cd", "cr", "cu", "hg", "ni", "pb", "zn", "benzo", "pcdd") # Sludge and Struvite
  # c("as", "cd", "cr", "cu", "hg", "ni", "pb", "zn", "benzo") # CAP
  # c("cd", "cr", "cu", "hg", "ni", "pb", "zn") # compost
  # c("as", "cd", "cr", "cu", "hg", "ni", "pb", "zn") # ASS
  c("cd", "cu", "hg", "ni", "pb", "zn") # ASS
# c("zn")


groundwater_assessment <- TRUE
nFields <- 1000

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
                                    nFields = nFields,
                                    use_mixing_factor = FALSE,
                                    PNECwater_c_i = groundwater_assessment,
                                    food_only = TRUE,
                                    growing_period = 180,
                                    t_res = 365 * 100,
                                    traceBackVariables = FALSE,
                                    keep_c_course = FALSE)
  fcr_out <- kwb.fcr::longterm_PEC(dat = dat_in$dat,
                                   info = dat_in$info,
                                   years = 100,
                                   nFields = nFields,
                                   use_mixing_factor = FALSE,
                                   PNECwater_c_i = groundwater_assessment,
                                   food_only = TRUE,
                                   growing_period = 180,
                                   t_res = 365 * 100,
                                   traceBackVariables = FALSE,
                                   keep_c_course = FALSE)

  output[[pollutantName]] <- if(groundwater_assessment){
    cbind(
      fcr_out$model_variables,
      get_risk(
        fertPEC = fcr_out$PEC[["porewater"]][100,],
        noFertPEC = fcr_out0$PEC[["porewater"]][100,],
        PNEC = fcr_out$model_variables[1,"PNEC_water"]
      )
    )
  } else {
    cbind(
      fcr_out$model_variables,
      get_risk(
        fertPEC = fcr_out$PEC[["soil"]][100,],
        noFertPEC = fcr_out0$PEC[["soil"]][100,],
        PNEC = fcr_out$model_variables[1,"PNEC_soil"]
      )
    )
  }
  rm(list = c("fcr_out0", "fcr_out"))
}

df_out <- as.data.frame(t(sapply(output, risk_aggregation)))
round(df_out$n_highRisk / 1000, 1)
cbind(round(df_out["mean_delta_relevant"], 3),
      round(df_out["cv_fert_relevant"] * 100, 1),
      round(df_out["cv_fert_delta"] * 100, 1),
      round(df_out["sd_delta_relevant"], 3),
      round(df_out["n_highRisk"] / nFields * 100, 1),
      deparse.level = 1)

cbind(round(df_out["mean_delta_relevant"], 3),
      round(df_out["mean_highest_5"], 1),
      round(df_out["n_highRisk"] / nFields * 100, 1),
      deparse.level = 1)

rm(list = c("get_risk", "risk_aggregation", "pollutantName", "pollutantNames",
            "nFields", "input_path"))


save.image(paste0(
  "C:/Users/mzamzo/Desktop/Masuren_Arbeit/fcr/output/", fertilizerName, "_",
  ifelse(groundwater_assessment, "water", "soil"), "_", siteName, ".RData"))


write.table(
  x = df_out,
  file = paste0("C:/Users/mzamzo/Desktop/Masuren_Arbeit/fcr/output",
                fertilizerName,
                format(Sys.time(), "%Y-%m-%d"),
                ".csv"),
  sep = ";",
  dec = ".",
  row.names = FALSE
)








#
# fcr_parameters <- function(RQ, RQno, RQ0){
#
#   high_risk <- which(RQ > 1 & RQ > RQ0)
#   if(length(high_risk) == 0L){
#     r95 <- risk_inc <- 0
#     inc_abs <- 0
#   } else {
#     delta <- RQ[high_risk]  -  RQno[high_risk]
#
#     r95 <- quantile(x = RQ[high_risk], probs = 0.95)
#     r95no <- quantile(x = RQno[high_risk], probs = 0.95)
#     # Absolute increase of risk (95th Quantile of all high-risk scenarios)
#     inc_abs <- quantile(x = delta, probs = 0.95)
#     # Increase of risk in % (95th Quantile of all high-risk scenarios)
#     risk_inc <- round(inc_abs  / r95no * 100, 2)
#   }
#   # Increase of high-risk situations in %
#   situation_inc <- if(length(high_risk) > 0L){
#     round((1 - sum(RQno > 1 & RQno > RQ0) / sum(RQ > 1 & RQ > RQ0)) * 100, 2)
#   } else {
#     0
#   }
#
#   round(c("tR" = signif(unname(r95),3),
#           "deltaR_abs" = signif(unname(inc_abs),3),
#           "deltaR_rel_percent" = unname(risk_inc),
#           "deltaRS_percent" = unname(situation_inc)
#   ), 3)
# }
#
# interprate_parameters <- function(
#     deltaRS_th = c(1, 10, 50),
#     tR_th = c(10, 100, 1000),
#     deltaR_rel_th = c(1, 10, 100),
#     df_in
# ){
#   mat <- matrix(data = c(1,1,1,2,2,2,2,3,2,2,3,4,2,3,4,5),
#                 nrow = 4, ncol = 4, byrow = T)
#
#   c_column <- as.numeric(
#     cut(
#       x = df_in[,"soil.deltaR_rel_percent"],
#       breaks = c(0, deltaR_rel_th, Inf),
#       include.lowest = T
#     )
#   )
#
#   c_row <- as.numeric(
#     cut(
#       x = df_in[,"soil.deltaRS_percent"],
#       breaks = c(0, deltaRS_th, Inf),
#       include.lowest = T
#     )
#   )
#
#   df_in$soil_class <- NA
#   for(i in seq_along(c_row)){
#     df_in$soil_class[i] <- mat[c_row[i], c_column[i]]
#   }
#
#   c_column <- as.numeric(
#     cut(
#       x = df_in[,"water.deltaR_rel_percent"],
#       breaks = c(0, deltaR_rel_th, Inf),
#       include.lowest = T
#     )
#   )
#
#   c_row <- as.numeric(
#     cut(
#       x = df_in[,"water.deltaRS_percent"],
#       breaks = c(0, deltaRS_th, Inf),
#       include.lowest = T
#     )
#   )
#
#   df_in$water_class <- NA
#   for(i in seq_along(c_row)){
#     df_in$water_class[i] <- mat[c_row[i], c_column[i]]
#   }
#
#   df_in$soil_class <- sapply(seq_along(df_in$soil_class), function(i){
#     add <- max(which(c(-Inf, tR_th) < df_in[i,"soil.tR"])) - 1
#     if(df_in$dep_impact[i] > 0.7){
#       add <- 0
#     }
#     df_in$soil_class[i] + add
#   })
#
#   df_in$water_class <- sapply(seq_along(df_in$water_class), function(i){
#     add <- max(which(c(-Inf, tR_th) < df_in[i,"water.tR"])) - 1
#     if(df_in$dep_impact[i] > 0.7){
#       add <- 0
#     }
#     df_in$water_class[i] + add
#   })
#
#   df_in$soil_class[df_in$soil_class > 5] <- 5
#   df_in$water_class[df_in$water_class > 5] <- 5
#   df_in
# }

