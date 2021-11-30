# test functions

dat <- kwb.fcr::read_fcr_input(path = "inst/extdata/input", pollutantName = "cd")
df_in <- rbind(dat$Sub, dat$Env)[,-c(2,3)]
df_in$pID <- 1:nrow(df_in)

