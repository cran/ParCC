## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
# RE-LY trial: Warfarin arm major bleeding
rate_per_100 <- 3.36
r <- rate_per_100 / 100   # Convert to per-person rate
t <- 1                     # 1-year model cycle

p <- 1 - exp(-r * t)
cat("Rate (per person-year):", r, "\n")
cat("Annual probability:", round(p, 5), "\n")
cat("Naive division would give:", r, "(overestimates by", round((r - p)/p * 100, 2), "%)\n")

## -----------------------------------------------------------------------------
p_10yr <- 0.20
t_old <- 10
t_new <- 1

p_1yr <- 1 - (1 - p_10yr)^(t_new / t_old)
cat("10-year probability:", p_10yr, "\n")
cat("Correct 1-year probability:", round(p_1yr, 5), "\n")
cat("Naive (divide by 10):", p_10yr / 10, "\n")
cat("Correct value is", round((p_1yr - 0.02)/0.02 * 100, 1), "% higher\n")

## -----------------------------------------------------------------------------
p_baseline <- 0.08
odds_baseline <- p_baseline / (1 - p_baseline)
odds_diabetes <- odds_baseline * 2.5
p_diabetes <- odds_diabetes / (1 + odds_diabetes)

cat("Baseline odds:", round(odds_baseline, 4), "\n")
cat("Diabetes odds (OR=2.5):", round(odds_diabetes, 4), "\n")
cat("Diabetes probability:", round(p_diabetes, 4), "\n")

