## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
median_os <- 21.3  # months

# Hazard rate from median
lambda <- log(2) / median_os
cat("Hazard rate (lambda):", round(lambda, 5), "per month\n")

# Survival function: S(t) = exp(-lambda * t)
t_seq <- c(6, 12, 24, 36, 60)
surv <- exp(-lambda * t_seq)

data.frame(
  Month = t_seq,
  Survival = round(surv, 3),
  Event_Prob = round(1 - surv, 3)
)

## -----------------------------------------------------------------------------
# Two points from the KM curve
t1 <- 12; s1 <- 0.42
t2 <- 24; s2 <- 0.23

# Log-log transformation
y1 <- log(-log(s1)); x1 <- log(t1)
y2 <- log(-log(s2)); x2 <- log(t2)

# Solve for shape (gamma) and scale (lambda)
gamma <- (y2 - y1) / (x2 - x1)
lambda <- exp(y1 - gamma * x1)

cat("Weibull Shape (gamma):", round(gamma, 4), "\n")
cat("Weibull Scale (lambda):", format(lambda, scientific = TRUE, digits = 4), "\n")

# Generate survival table
t_seq <- seq(0, 60, by = 6)
surv <- exp(-lambda * t_seq^gamma)

# Transition probabilities (monthly)
t_monthly <- 0:60
s_monthly <- exp(-lambda * t_monthly^gamma)
tp <- c(1, s_monthly[-1] / s_monthly[-length(s_monthly)])

cat("\nSurvival projections:\n")
data.frame(
  Month = seq(0, 60, by = 6),
  Survival = round(exp(-lambda * seq(0, 60, by = 6)^gamma), 3)
)

## -----------------------------------------------------------------------------
# Generate annual transition probabilities from Weibull
cycles <- 0:10
s_t <- exp(-lambda * (cycles * 12)^gamma)  # Convert years to months for calculation
tp <- c(1, s_t[-1] / s_t[-length(s_t)])

trace <- data.frame(
  Year = cycles,
  Survival = round(s_t, 4),
  Annual_TP = round(tp, 4),
  Cumulative_Death = round(1 - s_t, 4)
)
print(trace)

