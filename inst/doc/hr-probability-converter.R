## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
# PLATO trial - CV Death
p_control <- 0.0525   # 5.25% at 12 months
hr <- 0.79             # Ticagrelor vs Aspirin
t <- 1                 # 1 year

# Step 1: Probability -> Rate
r_control <- -log(1 - p_control) / t
cat("Step 1 - Control rate:", round(r_control, 5), "per year\n")

# Step 2: Apply HR
r_intervention <- r_control * hr
cat("Step 2 - Intervention rate:", round(r_intervention, 5), "per year\n")

# Step 3: Rate -> Probability
p_intervention <- 1 - exp(-r_intervention * t)
cat("Step 3 - Intervention probability:", round(p_intervention, 5), "\n\n")

# Compare with naive method
p_naive <- p_control * hr
cat("Correct method:", round(p_intervention, 5), "\n")
cat("Naive (p x HR):", round(p_naive, 5), "\n")
cat("Difference:", round((p_intervention - p_naive) * 10000, 2), "per 10,000 patients\n")

## -----------------------------------------------------------------------------
# Apply CI bounds
hr_low <- 0.69
hr_high <- 0.91

p_low <- 1 - exp(-r_control * hr_low * t)
p_high <- 1 - exp(-r_control * hr_high * t)

cat("Intervention probability: ", round(p_intervention, 5),
    " (95% CI:", round(p_low, 5), "to", round(p_high, 5), ")\n")

# Clinical impact
arr <- p_control - p_intervention
nnt <- ceiling(1 / arr)
cat("ARR:", round(arr * 100, 2), "%\n")
cat("NNT:", nnt, "\n")

## -----------------------------------------------------------------------------
p_control_onc <- 0.40
hr_onc <- 0.75
t_onc <- 2

# Correct method
r_ctrl <- -log(1 - p_control_onc) / t_onc
r_int <- r_ctrl * hr_onc
p_int_correct <- 1 - exp(-r_int * t_onc)

# Naive method
p_int_naive <- p_control_onc * hr_onc

cat("Control 2-year mortality:", p_control_onc, "\n")
cat("HR:", hr_onc, "\n\n")
cat("Correct intervention probability:", round(p_int_correct, 4), "\n")
cat("Naive (p x HR):", round(p_int_naive, 4), "\n")
cat("Error:", round(abs(p_int_correct - p_int_naive) * 100, 2), "percentage points\n")
cat("This affects", round(abs(p_int_correct - p_int_naive) * 10000), "patients per 10,000\n")

## -----------------------------------------------------------------------------
# PLATO CV Death, but for a monthly model
p_annual_ctrl <- 0.0525
hr <- 0.79

# Convert annual probability to rate
r_ctrl <- -log(1 - p_annual_ctrl) / 1
r_int <- r_ctrl * hr

# Convert to monthly probabilities
t_month <- 1/12
p_monthly_ctrl <- 1 - exp(-r_ctrl * t_month)
p_monthly_int <- 1 - exp(-r_int * t_month)

cat("Monthly control probability:", round(p_monthly_ctrl, 6), "\n")
cat("Monthly intervention probability:", round(p_monthly_int, 6), "\n")

