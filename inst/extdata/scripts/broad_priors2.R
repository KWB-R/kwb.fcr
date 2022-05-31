# Absolute SD
true_dists <- list(
  "low" = c("m" = 50, "sd" = 10),
  "medium" = c("m" = 1000, "sd" = 10),
  "high" = c("m" = 50000, "sd" = 10)
)

set.seed(1)
sample_data <- unlist(lapply(true_dists, function(x){
  rnorm(n = 3, mean = x["m"], sd = x["sd"])
}))


est_dists <- list()
est_dists[["unif"]] <- c("min" = min(sample_data) / 10, "max" = max(sample_data) * 10)
est_dists[["norm"]] <- c("m" = mean(sample_data), "sd" = sd(sample_data))

n_draws <- 100000
est_data <- list(
  "unif" = runif(n = n_draws, min = est_dists$unif["min"], max = est_dists$unif["max"]),
  "log_unif" = 10^runif(n = n_draws, min = log10(est_dists$unif["min"]), max = log10(est_dists$unif["max"])),
  "norm" = rnorm(n = n_draws, mean = est_dists$norm["m"], sd = est_dists$norm["sd"]),
  "tnorm" = kwb.fcr::rtnorm(n = n_draws, mean = est_dists$norm["m"], sd = est_dists$norm["sd"], a = 0),
  "log_norm" = 10^rnorm(n = n_draws, mean = log10(est_dists$norm["m"]), sd = log10(est_dists$norm["sd"])))

hist(est_data$unif)
hist(est_data$log_unif)
hist(est_data$norm)
hist(est_data$tnorm)
hist(est_data$log_norm)

sapply(true_dists, function(x){
  interval <- qnorm(p = c(0.025, 0.975), mean = x["m"], sd = x["sd"])
  sapply(est_data, function(est){
    sum(est >= interval[1] &
          est<= interval[2]) / n_draws * 100
  })
})

#### Relative Sd
true_dists <- list(
  "low" = c("m" = 50, "sd" = 10),
  "medium" = c("m" = 1000, "sd" = 200),
  "high" = c("m" = 50000, "sd" = 10000)
)

set.seed(1)
sample_data <- unlist(lapply(true_dists, function(x){
  rnorm(n = 3, mean = x["m"], sd = x["sd"])
}))

est_dists <- list()
est_dists[["unif"]] <- c("min" = min(sample_data) / 10, "max" = max(sample_data) * 10)
est_dists[["norm"]] <- c("m" = mean(sample_data), "sd" = sd(sample_data))

n_draws <- 100000
est_data <- list(
  "unif" = runif(n = n_draws, min = est_dists$unif["min"], max = est_dists$unif["max"]),
  "log_unif" = 10^runif(n = n_draws, min = log10(est_dists$unif["min"]), max = log10(est_dists$unif["max"])),
  "norm" = rnorm(n = n_draws, mean = est_dists$norm["m"], sd = est_dists$norm["sd"]),
  "tnorm" = kwb.fcr::rtnorm(n = n_draws, mean = est_dists$norm["m"], sd = est_dists$norm["sd"], a = 0),
  "log_norm" = 10^rnorm(n = n_draws, mean = log10(est_dists$norm["m"]), sd = log10(est_dists$norm["sd"])))

hist(est_data$unif)
hist(est_data$log_unif)
hist(est_data$norm)
hist(est_data$tnorm)
hist(est_data$log_norm)

sapply(true_dists, function(x){
  interval <- qnorm(p = c(0.025, 0.975), mean = x["m"], sd = x["sd"])
  sapply(est_data, function(est){
    sum(est >= interval[1] &
          est<= interval[2]) / n_draws * 100
  })
})

#### Log fixed SD
true_dists <- list(
  "low" = c("m" = 50, "sd" = 10),
  "medium" = c("m" = 1000, "sd" = 200),
  "high" = c("m" = 50000, "sd" = 10000)
)

plot(x = seq(0,100, 0.1), dlnorm(x = seq(0,100, 0.1), meanlog = log(50), sdlog = log(10)))
plot(x = seq(0,1000, 1), dlnorm(x = seq(0,100, 0.1), meanlog = log(50), sdlog = log(10)))
plot(x = seq(0,1000, 0.1), dlnorm(x = seq(0,100, 0.1), meanlog = log(50), sdlog = log(10)))

set.seed(1)
sample_data <- unlist(lapply(true_dists, function(x){
  rnorm(n = 2, mean = x["m"], sd = x["sd"])
}))

est_dists <- list()
est_dists[["unif"]] <- c("min" = min(sample_data) / 10, "max" = max(sample_data) * 10)
est_dists[["norm"]] <- c("m" = mean(sample_data), "sd" = sd(sample_data))

n_draws <- 100000
est_data <- list(
  "unif" = runif(n = n_draws, min = est_dists$unif["min"], max = est_dists$unif["max"]),
  "log_unif" = 10^runif(n = n_draws, min = log10(est_dists$unif["min"]), max = log10(est_dists$unif["max"])),
  "norm" = rnorm(n = n_draws, mean = est_dists$norm["m"], sd = est_dists$norm["sd"]),
  "tnorm" = kwb.fcr::rtnorm(n = n_draws, mean = est_dists$norm["m"], sd = est_dists$norm["sd"], a = 0),
  "log_norm" = 10^rnorm(n = n_draws, mean = log10(est_dists$norm["m"]), sd = log10(est_dists$norm["sd"])))

hist(est_data$unif)
hist(est_data$log_unif)
hist(est_data$norm)
hist(est_data$tnorm)
hist(est_data$log_norm)

sapply(true_dists, function(x){
  interval <- qnorm(p = c(0.025, 0.975), mean = x["m"], sd = x["sd"])
  sapply(est_data, function(est){
    sum(est >= interval[1] &
          est<= interval[2]) / n_draws * 100
  })
})




