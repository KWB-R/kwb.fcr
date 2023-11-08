input_path <- system.file(package = "kwb.fcr", file.path("extdata/data"))


siteName <- "germany"
fertilizerName <- "sludgePhorw_mean"
pollutantName <- "zn"

groundwater_assessment <- TRUE
nFields <- 100
years <- 100

# scenario without fertilizer --------------------------------------------------
dat_wo <- kwb.fcr::read_fcr_input(input_path = input_path,
                                  pollutantName = pollutantName,
                                  siteName = siteName,
                                  fertilizerName = "none")
# longterm calculation
fcr_wo <- kwb.fcr::longterm_PEC(dat = dat_wo$dat,
                                info = dat_wo$info,
                                years = years,
                                nFields = nFields,
                                use_mixing_factor = FALSE,
                                PNECwater_c_i = groundwater_assessment,
                                food_only = TRUE,
                                growing_period = 180,
                                t_res = 365 * 100,
                                traceBackVariables = FALSE,
                                keep_c_course = FALSE)

# scenario with fertilizer --------------------------------------------------
dat <- kwb.fcr::read_fcr_input(input_path = input_path,
                               pollutantName = pollutantName,
                               siteName = siteName,
                               fertilizerName = fertilizerName)


fcr <- kwb.fcr::longterm_PEC(dat = dat$dat,
                             info = dat$info,
                             years = years,
                             nFields = nFields,
                             use_mixing_factor = FALSE,
                             PNECwater_c_i = groundwater_assessment,
                             food_only = TRUE,
                             growing_period = 180,
                             t_res = 365 * 100,
                             traceBackVariables = FALSE,
                             keep_c_course = FALSE)

# Both simulations are based on the same environmental conditions in the
# same order (-> Example, pH distribution)
fcr$model_variables[,"pH"] == fcr_wo$model_variables[,"pH"]


# combine PEC of both scenarios after x years
compartment <- ifelse(groundwater_assessment, yes = "porewater", no = "soil")
PNEC <- ifelse(groundwater_assessment, yes = "PNEC_water", no = "PNEC_soil")

df_risk <- kwb.fcr::get_risk(
  fertPEC = fcr$PEC[[compartment]][years,],
  noFertPEC = fcr_wo$PEC[[compartment]][years,],
  PNEC = fcr$model_variables[1,PNEC]
)

# combine risk with input variables of field in order to check the underlying
# conditions
risk_simulation <- cbind(fcr$model_variables, df_risk)

# The impact of environmental variables can be checked
boxplot(pH ~ highRisk, data = risk_simulation)

kwb.fcr::risk_aggregation(df_risk = df_risk)



