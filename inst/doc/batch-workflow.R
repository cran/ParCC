## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
# Simulate what ParCC produces
df <- data.frame(
  Endpoint = c("CV Death", "MI", "Stroke", "Major Bleeding", "Dyspnea AE", "All-cause Mortality"),
  Control_Prob = c(0.0525, 0.0643, 0.0138, 0.1143, 0.0789, 0.0595),
  HR = c(0.79, 0.84, 1.01, 1.04, 1.37, 0.78),
  stringsAsFactors = FALSE
)

t <- 1  # 1-year horizon

df$Rate_Ctrl <- -log(1 - df$Control_Prob) / t
df$Rate_Int <- df$Rate_Ctrl * df$HR
df$Interv_Prob <- round(1 - exp(-df$Rate_Int * t), 5)
df$ARR <- round(df$Control_Prob - df$Interv_Prob, 5)
df$NNT <- ifelse(df$ARR > 0, ceiling(1 / df$ARR), NA)

# Clean display
result <- df[, c("Endpoint", "Control_Prob", "HR", "Interv_Prob", "ARR", "NNT")]
print(result)

