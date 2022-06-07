# update with either single values or mean values (in case of mean values the
# quotients can be 4 times as high
x <- seq(0,100,1)
min <- 50
max <- 500
sample_data <- kwb.fcr::rderived(n = 10000, min = min, max = max, a = 0)
prior <- density(sample_data)
x <- prior$x
prior <- prior$y
quant_95 <- quantile(x = sample_data, probs = c(0.025, 0.975))
ymax <- 0.04

experiment1 <- dnorm(x = x, mean = mean(sample_data), sd = diff(quant_95) / 0.5) # sehr schlecht
experiment2 <- dnorm(x = x, mean = mean(sample_data), sd = diff(quant_95) / 1) # schlecht
experiment3 <- dnorm(x = x, mean = mean(sample_data), sd = diff(quant_95) / 1.5) # mittel
experiment4 <- dnorm(x = x, mean = mean(sample_data), sd = diff(quant_95) / 2) # gut
experiment5 <- dnorm(x = x, mean = mean(sample_data), sd = diff(quant_95) / 2.5) # perfekt

plot(x = x, y = prior, type = "l")
plot(x = x, y = experiment1, type = "l")
plot(x = x, y = experiment2, type = "l")
plot(x = x, y = experiment3, type = "l")
plot(x = x, y = experiment4, type = "l")
plot(x = x, y = experiment5, type = "l")

posterior1 <- prior * experiment1 / sum(prior * experiment1)
posterior2 <- prior * experiment2 / sum(prior * experiment2)
posterior3 <- prior * experiment3 / sum(prior * experiment3)
posterior4 <- prior * experiment4 / sum(prior * experiment4)
posterior5 <- prior * experiment5 / sum(prior * experiment5)

plot(x = x, y = prior, type = "l", ylim = c(0,ymax), lwd = 2, lty = "dotted",
     main = "updated with terrible suiting data")
lines(x = x, y = posterior1, col = "steelblue", lwd = 2, lty = "solid")

plot(x = x, y = prior, type = "l", ylim = c(0,ymax), lwd = 2, lty = "dotted",
     main = "updated with poor suiting data")
lines(x = x, y = posterior2, col = "steelblue", lwd = 2, lty = "solid")

plot(x = x, y = prior, type = "l", ylim = c(0,ymax), lwd = 2, lty = "dotted",
     main = "updated with fair suiting data")
lines(x = x, y = posterior3, col = "steelblue", lwd = 2, lty = "solid")

plot(x = x, y = prior, type = "l", ylim = c(0,ymax), lwd = 2, lty = "dotted",
     main = "updated with good suiting data")
lines(x = x, y = posterior4, col = "steelblue", lwd = 2, lty = "solid")

plot(x = x, y = prior, type = "l", ylim = c(0,ymax), lwd = 2, lty = "dotted",
     main = "updated with perfect suiting data")
lines(x = x, y = posterior5, col = "steelblue", lwd = 2, lty = "solid")

# update with two different singel values
experiment1a <- dnorm(x = x, mean = quantile(sample_data, 0.35), sd = diff(quant_95) / 0.5) # sehr schlecht
experiment1b <- dnorm(x = x, mean = quantile(sample_data, 0.65), sd = diff(quant_95) / 0.5) # sehr schlecht

experiment2a <- dnorm(x = x, mean = quantile(sample_data, 0.35), sd = diff(quant_95) / 1) # schlecht
experiment2b <- dnorm(x = x, mean = quantile(sample_data, 0.65), sd = diff(quant_95) / 1) # schlecht

experiment3a <- dnorm(x = x, mean = quantile(sample_data, 0.35), sd = diff(quant_95) / 1.5) # mittel
experiment3b <- dnorm(x = x, mean = quantile(sample_data, 0.65), sd = diff(quant_95) / 1.5) # mittel

experiment4a <- dnorm(x = x, mean = quantile(sample_data, 0.35), sd = diff(quant_95) / 2) # gut
experiment4b <- dnorm(x = x, mean = quantile(sample_data, 0.65), sd = diff(quant_95) / 2) # gut

experiment5a <- dnorm(x = x, mean = quantile(sample_data, 0.35), sd = diff(quant_95) / 2.5) # perfekt
experiment5b <- dnorm(x = x, mean = quantile(sample_data, 0.65), sd = diff(quant_95) / 2.5) # perfekt


posterior1a <- prior * experiment1a / sum(prior * experiment1a)
posterior1b <- posterior1a * experiment1b / sum(posterior1a * experiment1b)

posterior2a <- prior * experiment2a / sum(prior * experiment2a)
posterior2b <- posterior2a * experiment2b / sum(posterior2a * experiment2b)

posterior3a <- prior * experiment3a / sum(prior * experiment3a)
posterior3b <- posterior3a * experiment3b / sum(posterior3a * experiment3b)

posterior4a <- prior * experiment4a / sum(prior * experiment4a)
posterior4b <- posterior4a * experiment4b / sum(posterior4a * experiment4b)

posterior5a <- prior * experiment5a / sum(prior * experiment5a)
posterior5b <- posterior5a * experiment5b / sum(posterior5a * experiment5b)


plot(x = x, y = prior, type = "l", ylim = c(0,ymax), lwd = 2, lty = "dotted",
     main = "updated with terrible suiting data")
lines(x = x, y = posterior1a, col = "steelblue", lwd = 2, lty = "solid")
lines(x = x, y = posterior1b, col = "blue", lwd = 2, lty = "solid")

plot(x = x, y = prior, type = "l", ylim = c(0,ymax), lwd = 2, lty = "dotted",
     main = "updated with poor suiting data")
lines(x = x, y = posterior2a, col = "steelblue", lwd = 2, lty = "solid")
lines(x = x, y = posterior2b, col = "blue", lwd = 2, lty = "solid")

plot(x = x, y = prior, type = "l", ylim = c(0,ymax), lwd = 2, lty = "dotted",
     main = "updated with fair suiting data")
lines(x = x, y = posterior3a, col = "steelblue", lwd = 2, lty = "solid")
lines(x = x, y = posterior3b, col = "blue", lwd = 2, lty = "solid")

plot(x = x, y = prior, type = "l", ylim = c(0,ymax), lwd = 2, lty = "dotted",
     main = "updated with good suiting data")
lines(x = x, y = posterior4a, col = "steelblue", lwd = 2, lty = "solid")
lines(x = x, y = posterior4b, col = "blue", lwd = 2, lty = "solid")

plot(x = x, y = prior, type = "l", ylim = c(0,ymax), lwd = 2, lty = "dotted",
     main = "updated with perfect suiting data")
lines(x = x, y = posterior5a, col = "steelblue", lwd = 2, lty = "solid")
lines(x = x, y = posterior5b, col = "blue", lwd = 2, lty = "solid")

# update with two equal singel values
experiment1a <- dnorm(x = x, mean = quantile(sample_data, 0.35), sd = diff(quant_95) / 0.5) # sehr schlecht
experiment1b <- dnorm(x = x, mean = quantile(sample_data, 0.35), sd = diff(quant_95) / 0.5) # sehr schlecht

experiment2a <- dnorm(x = x, mean = quantile(sample_data, 0.35), sd = diff(quant_95) / 1) # schlecht
experiment2b <- dnorm(x = x, mean = quantile(sample_data, 0.35), sd = diff(quant_95) / 1) # schlecht

experiment3a <- dnorm(x = x, mean = quantile(sample_data, 0.35), sd = diff(quant_95) / 1.5) # mittel
experiment3b <- dnorm(x = x, mean = quantile(sample_data, 0.35), sd = diff(quant_95) / 1.5) # mittel

experiment4a <- dnorm(x = x, mean = quantile(sample_data, 0.35), sd = diff(quant_95) / 2) # gut
experiment4b <- dnorm(x = x, mean = quantile(sample_data, 0.35), sd = diff(quant_95) / 2) # gut

experiment5a <- dnorm(x = x, mean = quantile(sample_data, 0.35), sd = diff(quant_95) / 2.5) # perfekt
experiment5b <- dnorm(x = x, mean = quantile(sample_data, 0.35), sd = diff(quant_95) / 2.5) # perfekt


posterior1a <- prior * experiment1a / sum(prior * experiment1a)
posterior1b <- posterior1a * experiment1b / sum(posterior1a * experiment1b)

posterior2a <- prior * experiment2a / sum(prior * experiment2a)
posterior2b <- posterior2a * experiment2b / sum(posterior2a * experiment2b)

posterior3a <- prior * experiment3a / sum(prior * experiment3a)
posterior3b <- posterior3a * experiment3b / sum(posterior3a * experiment3b)

posterior4a <- prior * experiment4a / sum(prior * experiment4a)
posterior4b <- posterior4a * experiment4b / sum(posterior4a * experiment4b)

posterior5a <- prior * experiment5a / sum(prior * experiment5a)
posterior5b <- posterior5a * experiment5b / sum(posterior5a * experiment5b)


plot(x = x, y = prior, type = "l", ylim = c(0,0.1), lwd = 2, lty = "dotted",
     main = "updated with terrible suiting data")
lines(x = x, y = posterior1a, col = "steelblue", lwd = 2, lty = "solid")
lines(x = x, y = posterior1b, col = "blue", lwd = 2, lty = "solid")

plot(x = x, y = prior, type = "l", ylim = c(0,0.1), lwd = 2, lty = "dotted",
     main = "updated with poor suiting data")
lines(x = x, y = posterior2a, col = "steelblue", lwd = 2, lty = "solid")
lines(x = x, y = posterior2b, col = "blue", lwd = 2, lty = "solid")

plot(x = x, y = prior, type = "l", ylim = c(0,0.1), lwd = 2, lty = "dotted",
     main = "updated with fair suiting data")
lines(x = x, y = posterior3a, col = "steelblue", lwd = 2, lty = "solid")
lines(x = x, y = posterior3b, col = "blue", lwd = 2, lty = "solid")

plot(x = x, y = prior, type = "l", ylim = c(0,0.1), lwd = 2, lty = "dotted",
     main = "updated with good suiting data")
lines(x = x, y = posterior4a, col = "steelblue", lwd = 2, lty = "solid")
lines(x = x, y = posterior4b, col = "blue", lwd = 2, lty = "solid")

plot(x = x, y = prior, type = "l", ylim = c(0,0.1), lwd = 2, lty = "dotted",
     main = "updated with perfect suiting data")
lines(x = x, y = posterior5a, col = "steelblue", lwd = 2, lty = "solid")
lines(x = x, y = posterior5b, col = "blue", lwd = 2, lty = "solid")


# only the best data update
experiment10_times <- dnorm(x = x, mean = quantile(sample_data, 0.35), sd = diff(quant_95) / 2.5)

post_list <- list()
post_list[[1]] <-  prior * experiment10_times/ sum(prior * experiment10_times)
for(i in 2:20){
  post_list[[i]] <- post_list[[i - 1]] * experiment10_times /
    sum(post_list[[i - 1]] * experiment10_times)
}
plot(x = x, y = prior, type = "l", ylim = c(0,0.1), lwd = 2, lty = "dotted",
     main = "updated with terrible suiting data")

lines(x = x, y = post_list[[20]], col = "steelblue", lwd = 2, lty = "solid")
lines(x = x, y = experiment10_times, col = "red", lwd = 1, lty = "solid")

# only the best data update
experiment10_times <- dnorm(x = x, mean = quantile(sample_data, 0.35), sd = diff(quant_95) / 2.5)

plot(x = x, y = prior, type = "l", ylim = c(0,0.1), lwd = 2, lty = "dotted",
     main = "updated with 20 perect and identical data points")
lines(x = x, y = experiment10_times, col = "red", lwd = 1, lty = "solid")

post_list <- list()
post_list[[1]] <-  prior * experiment10_times/ sum(prior * experiment10_times)
for(i in 2:20){
  post_list[[i]] <- post_list[[i - 1]] * experiment10_times /
    sum(post_list[[i - 1]] * experiment10_times)
  lines(x = x, y = post_list[[i]], col = "steelblue", lwd = 2, lty = "solid")
}
p_sum <- cumsum(post_list[[i]])
c(max(which(p_sum < 0.025)), min(which(p_sum > 0.975)))


# only the best data update  (random data points)
plot(x = x, y = prior, type = "l", ylim = c(0,0.1), lwd = 2, lty = "dotted",
     main = "updated with 20 perect and identical data points")

post_list <- list()
y <- quantile(sample_data, 0.35)

post_list[[1]] <-  prior * experiment10_times/ sum(prior * experiment10_times)
for(i in 2:20){
  set.seed(i)
  y <- c(y, sample(x = min:max, size = 1))
  experiment10_times <- dnorm(x = x, mean = y[i], sd = diff(quant_95) / 2.5)
  post_list[[i]] <- post_list[[i - 1]] * experiment10_times /
    sum(post_list[[i - 1]] * experiment10_times)
  lines(x = x, y = post_list[[i]], col = "steelblue", lwd = 2, lty = "solid")
}
p_sum <- cumsum(post_list[[i]])
c(max(which(p_sum < 0.025)), min(which(p_sum > 0.975)))

get_posterior <- function(prior_x, prior_prob, data, data_quality = NULL){
  data_dist <- dnorm(x = prior_x, mean = mean(data), sd = sd(data))
  prior * data_dist / sum(prior * data_dist)
}


post <- get_posterior(prior_x = seq(0,100,1),
              prior_prob = dunif(x = seq(0,100,1), min = 25, max = 75),
              data = rnorm(100, mean = 35, sd = 10))

plot(x = seq(0,100,1), y = post, type = "l")

# vorschlag:
# 5: passt nicht / qualit?t sehr schlecht: SD = 95quantile(prior)
# 4: passt schlecht / qualit?t schlecht: SD = 95quantile(prior) / 2
# 3: passt  / qualit?t Mittel  SD = 95quantile(prior) / 4
# 2: passt gut / qualit?t gut: SD = 95quantile(prior) / 6
# 1: passt perfekt / qualit?t ausgezeichnet: SD = 95quantile(prior) / 8


######################################
# Aproach 2 for fitting 2 parameters
# thrue values (not realy known) mean and sd of normal distribution
trueMu <- 5
trueSig <- 2

# assumptions for mean and standard deviation
# mean -> the mean is somewhere around 0
x1 <- seq(-10,10,0.1)
plot(x = x1, dnorm(x = x1, mean = 0, sd = 5))
# or in log form so that later on it can be added instead of multiplied
plot(x = x1, log(dnorm(x = x1, mean = 0, sd = 5)))

# sd -> darf nicht 0 sein, weil sonst die Wahrscheinlichkeit entweder -Inf oder Inf
x2 <- seq(0.1,5,0.1)
plot(x = x2, y = dexp(x = x2, rate = 1))
# or in log form so that later on it can be added instead of multiplied
plot(x = x2, y = log(dexp(x = x2, rate = 1)))

# some sample data
set.seed(100)
randomSample <- rnorm(100, trueMu, trueSig)

# Grid approximation, mu in [0, 10] and sigma in [1, 3]
grid <- expand.grid(mu = x1,
                    sigma = x2)

# Compute likelihood
lik <- sapply(1:nrow(grid), function(x){
  sum(dnorm(x = randomSample, mean = grid$mu[x],
            sd = grid$sigma[x], log = T))
})


grid["likelihood_log"] <- lik
grid["prior_log_mean"] <- dnorm(grid$mu, mean = 0, sd = 1, log = T)
grid["prior_log_sd"] <- dexp(grid$sigma, 1, log = T)

# Multiply (sum logs) likelihood and priors
prod <- grid["likelihood_log"] + grid["prior_log_mean"] + grid["prior_log_sd"]

grid["sum"] <- prod
# Standardize the lik x prior products to sum up to 1, recover unit

prob <- exp(prod - max(prod)) # --> maximal value is 1
prob <- prob/ sum(prob) # true probability

grid["probability"] <- prob

plot(grid$mu, y = grid$probability, type = "h", lwd = 2)
plot(grid$sigma, y = grid$probability, type = "h", lwd = 2)

# --------------------------------------------------------------------------------
# und jetzt das gleich Beispiel wie oben
# assumptions for mean and standard deviation
# mean
x1 <- seq(0,100,1)
plot(x = x1, dunif(x = x1, min = 25, 75), type = "l")
# or in log form so that later on it can be added instead of multiplied
plot(x = x1, log(dunif(x = x1, min = 25, 75)), type = "l")


quant95 <- diff(qunif(p = c(0.025, 0.975), min = 25, max = 75))
# sd
x2 <- seq(1,50,1)
plot(x = x2, y = dexp(x = x2, rate = 1/quant95))
# or in log form so that later on it can be added instead of multiplied
plot(x = x2, y = log(dexp(x = x2, rate =  1/quant95)))


# some sample data

set.seed(1)
randomSample <- 40

# Grid approximation, mu in [0, 10] and sigma in [1, 3]
grid <- expand.grid(mu = x1,
                    sigma = x2)

# Compute likelihood
lik <- sapply(1:nrow(grid), function(x){
  sum(dnorm(x = randomSample, mean = grid$mu[x],
            sd = grid$sigma[x], log = T))
})


grid["likelihood_log"] <- lik
grid["prior_log_mean"] <- log(dunif(x = grid$mu, min = 25, 75))
grid["prior_log_sd"] <- log(dexp(x = grid$sigma, rate = 1/quant95))

# Multiply (sum logs) likelihood and priors
prod <- grid["likelihood_log"] + grid["prior_log_mean"] + grid["prior_log_sd"]

grid["sum"] <- prod
# Standardize the lik x prior products to sum up to 1, recover unit

prob <- exp(prod - max(prod, na.rm = T)) # --> maximal value is 1
prob <- prob/ sum(prob, na.rm = T) # true probability

grid["probability"] <- prob

plot(x = grid$mu, y = grid$probability, type = "h", lwd = 2)
plot(grid$sigma, y = grid$probability, type = "h", lwd = 2)

mean_sample <- sample(x = grid$mu, size = 10000, replace = T, prob = grid$probability)
sd_sample <- sample(x = grid$sigma, size = 10000, replace = T, prob = grid$probability)

posterior <- rnorm(n = 10000, mean = mean_sample, sd = sd_sample)

plot(density(posterior))
mean(posterior)
sd(posterior)

mean(mean_sample)
sd(mean_sample)

mean(sd_sample)
sd(sd_sample)

# zweite Runde




