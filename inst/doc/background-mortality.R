## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
r_pop <- 0.013   # General population rate, age 65
smr <- 2.5        # From published meta-analysis

r_disease <- r_pop * smr
p_disease <- 1 - exp(-r_disease)

cat("Population rate:", r_pop, "\n")
cat("SMR:", smr, "\n")
cat("Disease-adjusted rate:", round(r_disease, 5), "\n")
cat("Annual mortality probability:", round(p_disease, 5), "\n")

## -----------------------------------------------------------------------------
le_observed <- 25    # Disease-specific life expectancy
le_background <- 65  # General population

r_excess <- (1 / le_observed) - (1 / le_background)
cat("Excess mortality rate:", round(r_excess, 5), "\n")
cat("This means patients face an additional", round(r_excess * 1000, 2),
    "deaths per 1,000 person-years beyond background\n")

## -----------------------------------------------------------------------------
# Fit Gompertz from two life table points
# Age 60: rate = 0.008
# Age 80: rate = 0.065

r1 <- 0.008; age1 <- 60
r2 <- 0.065; age2 <- 80

beta <- log(r2 / r1) / (age2 - age1)
alpha <- r1 / exp(beta * age1)

cat("Gompertz alpha:", format(alpha, scientific = TRUE), "\n")
cat("Gompertz beta:", round(beta, 5), "\n\n")

# Generate age-specific rates
ages <- seq(50, 90, by = 5)
rates <- alpha * exp(beta * ages)
probs <- 1 - exp(-rates)

data.frame(
  Age = ages,
  Rate = round(rates, 5),
  Annual_Prob = round(probs, 5)
)

