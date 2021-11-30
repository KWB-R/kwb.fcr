
true_rain <- rnorm(10000, mean = 500, sd = 30)

prior_mean <- rnorm(10000, 700, 200)

prior_sd <- runif(10000, 0, 50)

#prior_rain <- rexp(10000, rate = prior_rate)
#mean(prior_rate > 10 & prior_rate < 20)
#hist(prior_rain, breaks= 1000)

data <- sample(x = true_rain, size = 5, replace = T)


y <- data
N <- length(data)

model <- "data {
  int<lower=0> N;
  vector[N] y;
}
parameters {
  real mu;
  real <lower = 0> sigma;
  
}
model {
  mu ~ normal(700, 200);
  sigma ~ normal(0, 30);
  y ~ normal(mu, sigma);
}"


library(rstan)
res <- rstan::stan(model_code =  model, data = list("N" = N, "y" = y))

plot(res)

length(extract(res)$rate)

predictions <- rnorm(4000, 
                     extract(res)$mu, 
                     extract(res)$sigma)

predictions <- rnorm(4000, 
                     mean(data), 
                     sd(data))

quantile(predictions)

mean(rexp(4000, rate = extract(res)$rate) > 80)

# Updaten mit Gewichtung je nach Fehler     
df <- data.frame(obs = data)
df$error <- seq(0.1, 1, length.out= nrow(df))

library(brms)
library(rstanarm)

summary(stan_glm(obs ~  1, data =df))
res <- brms::brm(bf(obs|se(error) ~  1), data =df)
res
mean(df$obs)