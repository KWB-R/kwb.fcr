# test functions

# read data
input_path <- "Y:/WWT_Department/Projects/NextGen/Data-Work packages/WP2/QCRA/fcr/input"
dat_0 <- kwb.fcr::read_fcr_input(input_path = input_path,
                                  pollutantName = "cd",
                                  siteName = "braun", fertilizerName = "none")
dat_in <- kwb.fcr::read_fcr_input(input_path = input_path,
                                  pollutantName = "cd",
                                  siteName = "braun", fertilizerName = "sampleFert")


# for longterm application -----------------------------------------------------
fcr_out0 <- kwb.fcr::longterm_PEC(dat = dat_0$dat,
                                 info = dat_0$info,
                                 years = 100,
                                 nFields = 10000,
                                 use_mixing_factor = FALSE,
                                 PNECwater_c_i = TRUE,
                                 food_only = TRUE,
                                 growing_period = 180,
                                 t_res = 10,
                                 traceBackVariables = FALSE)


fcr_out <- kwb.fcr::longterm_PEC(dat = dat_in$dat,
                    info = dat_in$info,
                    years = 100,
                    nFields = 10000,
                    use_mixing_factor = FALSE,
                    PNECwater_c_i = TRUE,
                    food_only = TRUE,
                    growing_period = 180,
                    t_res = 10,
                    traceBackVariables = FALSE)

dev.new()
kwb.fcr::shadingPlot(
  mat_xRow = fcr_out$PEC[["soil"]] / fcr_out$model_variables[,"PNEC_soil"],
  ymin = 0.6, ymax = 1.2, xlab = "Years", ylab = "Risk Quotient",
  resolution = 0.01)
abline(h = 1, lty = "dotted")
kwb.fcr::shadingPlot(
  mat_xRow = fcr_out0$PEC[["soil"]] / fcr_out0$model_variables[,"PNEC_soil"],
  ymin = 0.6, ymax = 1.2, xlab = "Years", ylab = "Risk Quotient",
  resolution = 0.01)
abline(h = 1, lty = "dotted")

kwb.fcr::shadingPlot(
  mat_xRow = fcr_out$PEC[["porewater"]] / fcr_out$model_variables[,"PNEC_water"],
  ymin = 0, ymax = 5, xlab = "Years", ylab = "Risk Quotient",
  resolution = 0.01)
abline(h = 1, lty = "dotted")

kwb.fcr::shadingPlot(
  mat_xRow = fcr_out0$PEC[["porewater"]] / fcr_out$model_variables[,"PNEC_water"],
  ymin = 0, ymax = 5, xlab = "Years", ylab = "Risk Quotient",
  resolution = 0.01)
abline(h = 1, lty = "dotted")


kwb.fcr::CumSumSoil(
  v0 = fcr_out0$PEC[["soil"]][100,] / fcr_out$model_variables[,"PNEC_soil"],
  v = fcr_out$PEC[["soil"]][100,] / fcr_out$model_variables[,"PNEC_soil"],
  year_x = 100, xmax = 1000)
kwb.fcr::CumSumSoil(
  v0 = fcr_out0$PEC[["porewater"]][100,] / fcr_out$model_variables[,"PNEC_water"],
  v = fcr_out$PEC[["porewater"]][100,] / fcr_out$model_variables[,"PNEC_water"],
  year_x = 100, xmax = 10000)

v_out <- result_aggregation(fcr = fcr, fcr0 = fcr0)
interprate_parameters(df_in = as.list(v_out),
                      high_risk_th = c(1, 10, 100),
                      rq_increase_th = c(1,10,100),
                      severity_th = 10)
# kwb.fcr::CumSumSoil(
#   v0 = fcr_out$model_variables[,"c_human"] / fcr_out$model_variables[,"PNEC_human"],
#   v = fcr_out$PEC[["human"]][100,] / fcr_out$model_variables[,"PNEC_human"],
#   year_x = 100, xmax = 100)

# plot(density(x = fcr_out$PEC[["soil"]][100,]), lwd = 2)

###############################################################################
# Increase of high-risk situations in %
RQ <- fcr_out$PEC[["soil"]][100,] / fcr_out$model_variables[,"PNEC_soil"]
RQ0 <- fcr_out0$PEC[["soil"]][100,] / fcr_out$model_variables[,"PNEC_soil"]
signif(sum(RQ > 1) / (sum(RQ0 > 1) + 1) * 100 - 100, 2)
# Increase of risk in % (95th Quantile)
delta <- (fcr_out$PEC[["soil"]][100,] - fcr_out0$PEC[["soil"]][100,]) /
  fcr_out$model_variables[,"PNEC_soil"]
signif(quantile(x = delta, probs = 0.95) * 100, 2)
# 95th quantile of all RQ
signif(quantile(x = RQ, probs = 0.95), 2)

# Deposition impact ------------------------------------------------------------
# Spearman between deposition and end concentration (of scenario without fertilizer)
round(cor(x = fcr_out$PEC[["soil"]][100,], y = fcr_out$model_variables[,"D_air"],
    method = "spearman"), 2)

# Porewater --------------------------------------------------------------------
# Increase of high-risk situations in %
RQ <- fcr_out$PEC[["porewater"]][100,] / fcr_out$model_variables[,"PNEC_water"]
RQ0 <- fcr_out0$PEC[["porewater"]][100,] / fcr_out$model_variables[,"PNEC_water"]
RQ <- RQ > 1 #& fcr_out$PEC[["porewater"]][100,] > fcr_out$PEC[["porewater"]][90,]
RQ0 <- RQ0 > 1 #& fcr_out0$PEC[["porewater"]][100,] > fcr_out0$PEC[["porewater"]][90,]
signif(sum(RQ) / (sum(RQ0) + 1) * 100 - 100, 2)
# Increase of risk in % (95th Quantile)
delta <- (fcr_out$PEC[["porewater"]][100,] - fcr_out0$PEC[["porewater"]][100,]) /
  fcr_out$model_variables[,"PNEC_water"]
signif(quantile(x = delta, probs = 0.95) * 100, 2)
# 95th quantile of all RQ
signif(quantile(x = fcr_out$PEC[["porewater"]][100,] / fcr_out$model_variables[,"PNEC_water"], probs = 0.95), 2)

#########################################

vb <-fcr_out$PEC[["soil"]][100,] / fcr_out$model_variables[,"PNEC_soil"]

high <- which(vb > 1)
length(high) / length(vb)
max(vb)
hist(vb, main = "", xlab = "Risk Quotient")

df <- fcr_out$model_variables

p <- "c_fert"
p_groups <- cut(df[,p], breaks = quantile(x = df[,p], probs = seq(0,1,0.02)))
par_gl <- split(x = 1:length(p_groups), f = p_groups)
group_prop <- lapply(par_gl, function(x){
  sum(x %in% high) / length(x)
})

plot(density(df[high,p], from = min(df[,p]), to = max(df[,p])), col = "red")
lines(density(df[,p], from = min(df[,p]), to = max(df[,p])))

plot(x = 0, y = 0, xlim = c(0.5, length(group_prop) + 0.5),
     ylim = c(0,1), type = "n", xlab = "", ylab = "", las = 1, xaxt = "n", main = p)
axis(side = 1, at = 1:length(group_prop), labels = names(group_prop),
     las = 2, cex.axis = 0.8)
rect(xleft = seq(0.6, length(group_prop) - 0.4, 1),
     xright = seq(1.4, length(group_prop) + 0.4, 1),
     ybottom = 0, ytop = 1 - unlist(group_prop), col = "steelblue")
rect(xleft = seq(0.6, length(group_prop) - 0.4, 1),
     xright = seq(1.4, length(group_prop) + 0.4, 1),
     ybottom = 1 - unlist(group_prop), ytop = 1, col = "red")

# regression between concentration and critical exceedance probability
x <- quantile(x = df[,p], probs = seq(0,1,0.02))
x <- x[-length(x)] + diff(x)
y <- unlist(group_prop)

cor(x = runif(n = 100000, min = 0, max = 0.1),
    y = runif(n = 100000, min = 0, max = 0.1), method = "spearman")
cor(x = x, y = y, method = "spearman")
cor(x = x, y = y, method = "pearson")
plot(x = x, y = y)
abline(lm(y~x))
summary(lm(y~x))
# if intercept > 0 --> Risk without application
# if intercept < 0 --> no Risk without application
# if spearman > 0.3 --> Impact of fertilization
# if spearman > 0.6 --> high impact of fertilization
# always referred to RQ = 1 (initial concentration)


# for one single year ----------------------------------------------------------
# prepare data
# define initial concentration (only for the first year)
c_i <- kwb.fcr::rdist(value_1 = dat$c_i$value_1,
                      value_2 = dat$c_i$value_2,
                      n = 100,
                      dist_name = dat$c_i$distribution,
                      shift = dat$c_i$shift,
                      seed = 0)

p <- kwb.fcr::oneYear_matrix(dat = dat,
                             c_i = c_i,
                             nFields = 100,
                             use_mixing_factor = FALSE)

p <- kwb.fcr::add_variables(p = p,
                            info = info)

# PECs in this year
PEC_soil <- kwb.fcr::get_PEC_soil(p = p, d = 30)
PEC_human <- kwb.fcr::get_PEC_human(p = p, d = 180, food_only = T)
PEC_porewater <- kwb.fcr::get_PEC_porewater(p = p, d = 30)

# temporal concentration course in year i
output <- kwb.fcr::one_year(p = p,
                            growing_period = 180,
                            t_res = 1)
# end concentration of year i
c_i <- output[nrow(output),]


