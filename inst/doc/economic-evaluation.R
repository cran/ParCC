## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
c_new <- 142000; c_old <- 85000
e_new <- 4.15; e_old <- 3.82

delta_c <- c_new - c_old
delta_e <- e_new - e_old
icer <- delta_c / delta_e

cat("Incremental Cost: INR", format(delta_c, big.mark = ","), "\n")
cat("Incremental QALYs:", delta_e, "\n")
cat("ICER: INR", format(round(icer), big.mark = ","), "per QALY\n")

## -----------------------------------------------------------------------------
wtp <- 100000  # INR per QALY (HTAIn threshold)

inmb <- (delta_e * wtp) - delta_c
cat("iNMB at WTP INR 1,00,000/QALY: INR", format(round(inmb), big.mark = ","), "\n")

if (inmb > 0) {
  cat("Decision: Cost-effective (iNMB > 0)\n")
} else {
  cat("Decision: NOT cost-effective (iNMB < 0)\n")
}

## -----------------------------------------------------------------------------
delta_e_test <- 0.02
wtp_threshold <- 100000
c_comparator <- 500
c_associated <- 200
n_units <- 1

c_max <- (delta_e_test * wtp_threshold) + c_comparator
p_max <- (c_max - c_associated) / n_units

cat("Clinical Value: INR", format(delta_e_test * wtp_threshold, big.mark = ","), "\n")
cat("+ Savings from replacing old test: INR", format(c_comparator, big.mark = ","), "\n")
cat("= Total Headroom: INR", format(c_max, big.mark = ","), "\n")
cat("- Nurse time: INR", format(c_associated, big.mark = ","), "\n")
cat("= Maximum Justifiable Price: INR", format(p_max, big.mark = ","), "\n")

## -----------------------------------------------------------------------------
proposed_price <- 3000
overpriced_by <- ((proposed_price - p_max) / p_max) * 100

if (proposed_price > p_max) {
  cat("At INR", format(proposed_price, big.mark = ","),
      "the product is", round(overpriced_by, 1), "% above the maximum justifiable price.\n")
  cat("Recommendation: Reduce price by INR",
      format(proposed_price - p_max, big.mark = ","), "\n")
} else {
  cat("Price is within the cost-effective range.\n")
}

