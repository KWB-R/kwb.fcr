RQ <- fcr_out$PEC[["soil"]][100, 1:100] / fcr_out$model_variables[,"PNEC_soil"][1]
RQ0 <- fcr_out0$PEC[["soil"]][100, 1:100] / fcr_out0$model_variables[,"PNEC_soil"][1]

plot(x = 0, y = 0, type = "n", ylim = c(0.3, 1.2), xlim = c(1,100),
     xlab = "Years", ylab = "Risk Quaotient", xaxs = "i", las = 1, main = "With Fertilization")

for(i in 1:100){
  col <-if(RQ[i] >= 1){
     rgb(0.7,0,0,0.3)
  } else {
    rgb(0,112,150,80, maxColorValue = 255)
  }
  lines(x = 1:100,
        y = fcr_out$PEC[["soil"]][1:100, i] /
          fcr_out$model_variables[,"PNEC_soil"][1],
        lwd = 2, col = col)
}

plot(x = 0, y = 0, type = "n", ylim = c(0.3, 1.2), xlim = c(1,100),
     xlab = "Years", ylab = "Risk Quaotient", xaxs = "i", las = 1,
     main = "Without Fertilization")

for(i in 1:100){
  col <-if(RQ0[i] >= 1){
    rgb(0.7,0,0,0.3)
  } else {
    rgb(0,112,150,80, maxColorValue = 255)
  }
  lines(x = 1:100,
        y = fcr_out0$PEC[["soil"]][1:100, i] /
          fcr_out0$model_variables[,"PNEC_soil"][1],
        lwd = 2, col = col)
}


## for Porewater
RQ <- fcr_out$PEC[["porewater"]][100, 1:100] / fcr_out$model_variables[,"PNEC_water"][1]
RQ0 <- fcr_out0$PEC[["porewater"]][100, 1:100] / fcr_out0$model_variables[,"PNEC_water"][1]

plot(x = 0, y = 0, type = "n", ylim = c(0, 100), xlim = c(1,100),
     xlab = "Years", ylab = "Risk Quaotient", xaxs = "i", las = 1, main = "With Fertilization")

for(i in 1:100){
  col <-if(RQ[i] >= 1){
    rgb(0.7,0,0,0.3)
  } else {
    rgb(0,112,150,80, maxColorValue = 255)
  }
  lines(x = 1:100,
        y = fcr_out$PEC[["porewater"]][1:100, i] /
          fcr_out$model_variables[,"PNEC_water"][1],
        lwd = 2, col = col)
}

plot(x = 0, y = 0, type = "n", ylim = c(0, 100), xlim = c(1,100),
     xlab = "Years", ylab = "Risk Quaotient", xaxs = "i", las = 1,
     main = "Without Fertilization")

for(i in 1:100){
  col <-if(RQ0[i] >= 1){
    rgb(0.7,0,0,0.3)
  } else {
    rgb(0,112,150,80, maxColorValue = 255)
  }
  lines(x = 1:100,
        y = fcr_out0$PEC[["porewater"]][1:100, i] /
          fcr_out0$model_variables[,"PNEC_water"][1],
        lwd = 2, col = col)
}

