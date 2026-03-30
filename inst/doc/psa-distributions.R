## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
mu <- 0.76
se <- 0.03

common <- (mu * (1 - mu)) / se^2 - 1
alpha <- mu * common
beta_param <- (1 - mu) * common

cat("Alpha:", round(alpha, 2), "\n")
cat("Beta:", round(beta_param, 2), "\n")

# Verify: sample and check
set.seed(42)
samples <- rbeta(10000, alpha, beta_param)
cat("\nVerification (10,000 samples):\n")
cat("Mean:", round(mean(samples), 4), "(target:", mu, ")\n")
cat("SE:", round(sd(samples), 4), "(target:", se, ")\n")
cat("Range:", round(range(samples), 4), "\n")

## -----------------------------------------------------------------------------
mu_cost <- 250000
se_cost <- 50000

k <- mu_cost^2 / se_cost^2
theta <- se_cost^2 / mu_cost

cat("Shape (k):", round(k, 2), "\n")
cat("Scale (theta):", round(theta, 2), "\n")

# Verify
set.seed(42)
samples_cost <- rgamma(10000, shape = k, scale = theta)
cat("\nVerification (10,000 samples):\n")
cat("Mean: INR", format(round(mean(samples_cost)), big.mark = ","), "(target:", format(mu_cost, big.mark = ","), ")\n")
cat("SE: INR", format(round(sd(samples_cost)), big.mark = ","), "(target:", format(se_cost, big.mark = ","), ")\n")
cat("Min: INR", format(round(min(samples_cost)), big.mark = ","), "(always positive)\n")

## -----------------------------------------------------------------------------
hr_mean <- 0.72
hr_low <- 0.58
hr_high <- 0.89

mu_log <- log(hr_mean)
se_log <- (log(hr_high) - log(hr_low)) / (2 * 1.96)
sigma2_log <- se_log^2

cat("mu_log:", round(mu_log, 4), "\n")
cat("sigma_log:", round(se_log, 4), "\n")

# Verify
set.seed(42)
samples_hr <- rlnorm(10000, meanlog = mu_log, sdlog = se_log)
cat("\nVerification (10,000 samples):\n")
cat("Median HR:", round(median(samples_hr), 3), "(target:", hr_mean, ")\n")
cat("2.5th percentile:", round(quantile(samples_hr, 0.025), 3), "(target:", hr_low, ")\n")
cat("97.5th percentile:", round(quantile(samples_hr, 0.975), 3), "(target:", hr_high, ")\n")

## -----------------------------------------------------------------------------
# Example: cost range INR 1,50,000 to INR 3,50,000
cost_low <- 150000
cost_high <- 350000
se_approx <- (cost_high - cost_low) / 4

cat("Estimated SE:", format(se_approx, big.mark = ","), "\n")

