#' PECs for soil, porewater and human consumption over a long period of time
#'
#' Based on the input data provided in Excel sheets, 3 different PECs are
#' calculated. The number of years and number of agricultural fields with different
#' properties (Number of Monte Carlo cycles) can be chosen.
#'
#' @param dat List with all the input variables. This list is produced by
#' function [read_fcr_input()] from the Excel sheets.
#' @param info he table containing additional substance information loaded
#' with [additional_substanc_info()]
#' @param years Years of fertilizer application
#' @param nFields Number of Monte Carlo cycles
#' @param use_mixing_factor Not working yet! If TRUE, porewater is diluted by pollutant free
#' groundwater for a more realistic estimation of risks in groundwater.
#' However the TGD approach assumes porewater = groundwater for the assessment.
#' @param food_only If TRUE, the predicted human consumption via food is
#' multiplied by 2 to compensate the disregard of water consumption
#' @param growing_period Numeric value specifiyng the number of days with plant
#' growth
#' @param t_res Temporal resolution to be returned in the output matrix
#' (no effect on the calculation)
#' @param return_variables Should the distributed input variables be returned?
#' Note: this can lead to very large lists.
#'
#' @return
#' List with
#' 1) table for all PEC types per year,
#' 2) table for concentration course
#' in top soil in the predefined temporal resolution,
#' 3) optional: list of distributed input variables for each year
#'
#' @export
#' @importFrom grDevices dev.new dev.off
#' @importFrom graphics par text rect
#'
longterm_PEC <- function(
  dat, info, years, nFields, use_mixing_factor = FALSE, food_only = TRUE,
  growing_period = 180, t_res, return_variables = FALSE
){

  c_i <- rdist(value_1 = dat$c_i$value_1, value_2 = dat$c_i$value_2,
               n = nFields, dist_name = dat$c_i$distribution,
               shift = dat$c_i$shift, seed = 0)

  PEC <- list("soil" = list(), "porewater" = list(), "human" = list())
  c_course <- list(c_i)
  model_variables <- list()

  { # plot status bar
    dev.new(noRStudioGD = T, height = 2, width = 8)
    par(mar = c(0, 0, 0, 0))
    plot(x = 0, y = 0, type = "n", ylim = c(0,3), xlim = c(0,150))
    text(x = 75, y = 2, labels = "Status of long-term PEC calculation", pos = 3)
    rect(xleft = 25, xright = 125, ybottom = 1, ytop = 2, lwd = 2)
    status <- 0
    t <- Sys.time()
    delta_t <- c()
  }

  for(year in 1:years){

      p <- oneYear_matrix(dat = dat, c_i = c_i, nFields = nFields,
                          use_mixing_factor = use_mixing_factor)
      p <- add_variables(p = p,info = info)
      model_variables[[year]] <- p

      PEC$soil[[year]] <-
        get_PEC_soil(p = p, d = 30)
      PEC$human[[year]] <-
        get_PEC_human(p = p, d = growing_period, food_only = T)
      PEC$porewater[[year]] <-
        get_PEC_porewater(p = p, d = 30)

      c_course[[year + 1]] <-
        one_year(p = p, growing_period = growing_period, t_res = t_res)
      c_i <- c_course[[year + 1]][nrow(c_course[[year + 1]]),]


    { # Update status bar
      status_new <- floor(year/years * 100)
      t_new <- Sys.time()
      delta_t <- c(delta_t, difftime(t_new, t, units = "mins"))
      mean_delta <- mean(delta_t[1:4], na.rm = T)
      t_remain <- (100 - status_new) * mean_delta / (status_new - status)
      t_mins <- floor(t_remain)
      t_secs <- round((t_remain - t_mins) * 60, 0)

      status <- status_new
      t <- t_new

      rect(xleft = 25, xright = 25 + status, ybottom = 1, ytop = 2,
           border = NA, col = "steelblue")
      rect(xleft = c(0,130), xright = c(20,150), ybottom = 1, ytop = 2,
           border = NA, col = "white")
      text(x = 10, y = 1.5, labels = paste(status, "%"), pos = 4)
      text(x = 131, y = 1.5,
           labels =  paste(formatC(t_mins, width = 2, flag = 0),
                           formatC(t_secs, width = 2, flag = 0), sep = ":"),
           pos = 4)
    }
  }

  PEC$soil <- do.call(rbind, PEC$soil)
  PEC$human <- do.call(rbind, PEC$human)
  PEC$porewater <- do.call(rbind, PEC$porewater)
  c_course <- do.call(rbind, c_course)


  { # close status bar
    dev.off()
  }


  if(!return_variables){
    model_variables <- "Set 'return_variables' to TRUE if needed"
  }

  list("PEC" = PEC,"c_course" = c_course, "model_variables" = model_variables)
}



#' Calculation of the pollutant concentation course in top soil
#'
#' Estimation is based on the FCR prepared Monte Carlo table. The output
#' rate is split into one rate with plant uptake during plant growth and
#' one rate without plant uptake during winter time.
#'
#' A time period can be considered as growing period if 1) plants grow and 2)
#' plants will be harvested after growing. Otherwise the uptaken pollutants will
#' return to the soil.
#'
#' @param p Parameter table created with [oneYear_matrix()]
#' @param growing_period Numeric value specifiyng the number of days with plant
#' growth
#' @param t_res Temporal resolution to be returned in the output matrix
#' (no effect on the calculation)
#'
#' @return
#' Matrix with number of columns equal to the rows of p and number of rows
#' depending on the defined timesteps with t_res
#'
#' @export
#'
one_year <- function(p, growing_period, t_res){
  c_out <- temp_c_profile(
    conti_input = p[,"D_air"],
    output_rate = p[,"k1"],
    c_i = p[,"c_0"],
    t_max = growing_period, t_res = t_res, t_beg = 0)

  c_out2 <- temp_c_profile(
    conti_input = p[,"D_air"],
    output_rate = p[,"k2"],
    c_i = c_out[nrow(c_out),],
    t_max = 365 - growing_period, t_res = t_res, t_beg = growing_period)

  rbind(c_out, c_out2)
}

#' Predicted soil concentration
#'
#' Average predicted environmental concentration (PEC) over d days after
#' fertilizer application. The Calculation is based on the FCR prepared Monte
#' Carlo table
#'
#' @param p Parameter table created with [oneYear_matrix()]
#' @param d The number of days after fertilizer application. Somewhere between
#' 1 and the number of days of the growing period, since plant uptake is
#' considered
#'
#' @return Numeric vector of concentrations per field in mg/kg soil
#'
#' @export
#'
get_PEC_soil <- function(p, d){
  p[,"D_air"] / p[,"k1"] +
    1 / (p[,"k1"] * d) *
    (p[,"c_0"] - p[,"D_air"] / p[,"k1"]) *
    (1 - exp(- p[,"k1"] * d))
}

#' Predicted human consumption
#'
#' Predicted human consumption via crop consumption. The calculation is based
#' on the average predicted soil concentration within the growing period.
#'
#' @param p Parameter table created with [oneYear_matrix()] and extended with
#' [add_variables()]
#' @param d The number of days of the growing period.
#' @param food_only If TRUE, the predicted human consumption via food is
#' multiplied by 2 to compensate the disregard of water consumption
#'
#' @return Numeric vector of human consumption via crops in µg/d
#'
#' @export
#'
get_PEC_human <- function(p, d, food_only){
  PEC_soil <- get_PEC_soil(p = p, d = d)

  v_out <- PEC_soil * p[,"BCF"] * p[,"m_crop"] * p[,"f_resorbed"]
  if(food_only){
    v_out * 2
  } else {
    v_out
  }
}

#' Predicted pore water concentration
#'
#' Average predicted environmental concentration (PEC) over d days after
#' fertilizer application. The Calculation is based on the FCR prepared Monte
#' Carlo table
#'
#' @param p Parameter table created with [oneYear_matrix()] and extended with
#' [add_variables()]
#' @param d The number of days after fertilizer application. Somewhere between
#' 1 and the number of days of the growing period, since plant uptake is
#' considered
#'
#' @return Numeric vector of concentrations per field in µg/L
#'
#' @export
#'
get_PEC_porewater <- function(p, d){
  get_PEC_soil(p = p, d = d) * p[,"rho_soil"] / p[,"K_SoilWater"]
}






