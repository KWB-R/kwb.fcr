
# read data
input_path <- "Y:/WWT_Department/Projects/NextGen/Data-Work packages/WP2/QCRA/fcr/input"
dat_in <- kwb.fcr::read_fcr_input(input_path = input_path, pollutantName = "organics",
                                  siteName = "braun")
dat <- dat_in$dat
info <- dat_in$info

# for longterm application -----------------------------------------------------
fcr_out <- kwb.fcr::longterm_PEC(dat = dat,
                                 info = info,
                                 years = 100,
                                 nFields = 10000,
                                 use_mixing_factor = FALSE,
                                 food_only = TRUE,
                                 growing_period = 180,
                                 t_res = 10,
                                 traceBackVariables = FALSE)

dev.new(noRStudioGD = T, height = 5, width = 9)
kwb.fcr::shadingPlot(
  mat_xRow = fcr_out$PEC[["soil"]],
  ymin = 0, ymax = max(fcr_out$PEC[["soil"]]),
  resolution = 0.01, ylab = "Concentration [mg/kg]",
  xlab = "Years of Application", main = "PAH")
kwb.fcr::shadingPlot(
  mat_xRow = fcr_out$PEC[["porewater"]],
  ymin = 0, ymax = max(fcr_out$PEC[["porewater"]]),
  resolution = 0.01, ylab = "Concentration [µg/L]",
  xlab = "Years of Application", main = "PAH")
kwb.fcr::shadingPlot(
  mat_xRow = fcr_out$PEC[["human"]],
  ymin = 0, ymax = max(fcr_out$PEC[["human"]]),
  resolution = 0.01, ylab = "Concentration [mg/d]",
  xlab = "Years of Application", main = "PAH")

kwb.fcr::CumSumSoil(
  v0 = fcr_out$model_variables[,"c_i"] / fcr_out$model_variables[,"PNEC_soil"],
  v = fcr_out$PEC[["soil"]][100,] / fcr_out$model_variables[,"PNEC_soil"],
  year_x = 100, xmax = 1E08)
kwb.fcr::CumSumSoil(
  v0 = fcr_out$model_variables[,"c_water"] / fcr_out$model_variables[,"PNEC_water"],
  v = fcr_out$PEC[["porewater"]][100,] / fcr_out$model_variables[,"PNEC_water"],
  year_x = 100, xmin = 0.0001, xmax = 1E08)
kwb.fcr::CumSumSoil(
  v0 = fcr_out$model_variables[,"c_human"] / fcr_out$model_variables[,"PNEC_human"],
  v = fcr_out$PEC[["human"]][100,] / fcr_out$model_variables[,"PNEC_human"],
  year_x = 100, xmax = 1E08)

summary(fcr_out$model_variables[,"k_volat"])
summary(fcr_out$model_variables[,"k_bio"])
summary(fcr_out$model_variables[,"k_leach"])
summary(fcr_out$model_variables[,"k_plant"])

#
dat$K_oc$value_1
dat$K_oc$value_2
dat$K_oc$distribution <- "logderive"

# 1.
dat$PNEC_water$value_1 <- 0.05 # für PAK
dat$PNEC_soil$value_1 <- 0.71 # für PAK
dat$PNEC_human$value_1 <- 5.96 # für PAK

# 2.
summary(fcr_out$model_variables[,"DT50"])
hist(fcr_out$model_variables[,"DT50"])
dat$DT50$value_1 <- 100
dat$DT50$value_2 <- 4000

# 3.
dat$BCF$value_1 <- 0
dat$BCF$value_2 <- 1

# 4.
dat$K_oc$value_1 <- 1E05
dat$K_oc$value_2 <- 1E07

# 5.
dat$c_i$value_1 <- 0.2
dat$c_i$value_2 <- 0.1
dat$c_i$distribution <- "tnormal"


# 6. Deposition
dat$D_air_tot$value_1 <- 0.0015
dat$D_air_tot$value_2 <- 0.0005
dat$D_air_tot$distribution <- "tnormal"

# 7. Fertilizer
dat$c_fert$value_1 <- 41 / 12.9
dat$c_fert$value_2 <- 56 / 12.9
dat$c_fert$distribution <- "normal"

###############################################################################
vw <-fcr_out$PEC[["porewater"]][100,] / fcr_out$model_variables[,"PNEC_water"]
vb <-fcr_out$PEC[["soil"]][100,] / fcr_out$model_variables[,"PNEC_soil"]
vh <-fcr_out$PEC[["porewater"]][100,] / fcr_out$model_variables[,"PNEC_water"]


high <- which(vb > 1)

df <- fcr_out$model_variables
colnames(df)

p <- "c_fert"

p_groups <- cut(df[,p], breaks = quantile(x = df[,p], probs = seq(0,1,0.01)))

par_gl <- split(x = seq_along(p_groups), f = p_groups)

group_prop <- lapply(par_gl, function(x){
  sum(x %in% high) / length(x)
})


plot(density(df[high,p], from = min(df[,p]), to = max(df[,p])), col = "red")
lines(density(df[,p], from = min(df[,p]), to = max(df[,p])))

plot(x = 0, y = 0, xlim = c(0.5, length(group_prop) + 0.5),
     ylim = c(0,1), type = "n", xlab = "", ylab = "", las = 1, xaxt = "n", main = p)
axis(side = 1, at = seq_along(group_prop), labels = names(group_prop),
     las = 2, cex.axis = 0.8)
rect(xleft = seq(0.6, length(group_prop) - 0.4, 1),
     xright = seq(1.4, length(group_prop) + 0.4, 1),
     ybottom = 0, ytop = 1 - unlist(group_prop), col = "steelblue")
rect(xleft = seq(0.6, length(group_prop) - 0.4, 1),
     xright = seq(1.4, length(group_prop) + 0.4, 1),
     ybottom = 1 - unlist(group_prop), ytop = 1, col = "red")

# regression between concentration and critical exceedance probability
x <- quantile(x = df[,p], probs = seq(0,1,0.01))[-1]
y <- unlist(group_prop)

cor(x = x, y = y, method = "spearman")
cor(x = x, y = y, method = "pearson")
summary(lm(y ~ x))

# 2D Diagramm, mit x und y jeweils ein Parameter und Farbkodierung für das Risiko
# 0 -> grün, < 0.005 -> gelb, < 0.01 -> orange, alles andere Rot
# 3D Doagramm, alles gleich, aber z als Wahrscheinlichkeit von RQ > 1

