# 07_model_subset_ar1.R ------------------------------------------------------
library(dplyr)
library(tidyr)    # ← for drop_na()
library(car)

# ── 0. DATA PREP & SPLIT ────────────────────────────────────────────────────
# drop ONLY rows that are completely empty (same rule as 06_…_vif.R)
model_df <- model_df %>% 
  filter(if_any(everything(), ~ !is.na(.))) %>%   # keep rows with ≥1 non‑NA
  arrange(Date) %>% 
  mutate(USD_lag1 = lag(USD, 1))                  # add AR(1) term

n         <- nrow(model_df)
split_pt  <- floor(0.7 * n)
train_set <- model_df[1:split_pt, ]
test_set  <- model_df[(split_pt + 1):n, ]

# ── 1. SIGNIFICANCE + VIF (same logic as 06) ────────────────────────────────
full_fit <- lm(USD ~ m_diff + y_diff + rs_diff + pi_diff + TB_cum_arm + TB_cum_us,
               data = train_set)
pvals    <- summary(full_fit)$coefficients[-1, 4]
sig_vars <- names(pvals)[pvals < 0.05]

subset_vars <- sig_vars
repeat {
  fm    <- lm(reformulate(subset_vars, "USD"), data = train_set)
  vif_v <- vif(fm)
  if (max(vif_v) <= 5) break
  subset_vars <- setdiff(subset_vars, names(which.max(vif_v)))
}

# ── 2. FINAL TRAIN/TEST SETS (drop NA **only for the vars in the model**) ──
vars_used <- c("USD", "USD_lag1", subset_vars)

train_clean <- train_set %>% drop_na(all_of(vars_used))
test_clean  <- test_set  %>% drop_na(all_of(vars_used))

# ── 3. FIT + SUMMARY ────────────────────────────────────────────────────────
formula_ar1 <- reformulate(c("USD_lag1", subset_vars), response = "USD")
fit_ar1     <- lm(formula_ar1, data = train_clean)

cat("=== Summary: Subset + AR(1) ===\n")
print(summary(fit_ar1))

# ── 4. PREDICTIONS ──────────────────────────────────────────────────────────
pred_train <- predict(fit_ar1, newdata = train_clean)
pred_test  <- predict(fit_ar1, newdata = test_clean)

# ── 5. METRICS ─────────────────────────────────────────────────────────────
metrics <- function(actual, pred, p) {
  stopifnot(length(actual) == length(pred))
  n    <- length(actual)
  mse  <- mean((actual - pred)^2)
  rmse <- sqrt(mse)
  mae  <- mean(abs(actual - pred))
  r2   <- cor(actual, pred)^2
  adjr2 <- 1 - (1 - r2) * (n - 1) / (n - p - 1)
  da   <- mean(sign(diff(actual)) == sign(diff(pred)))
  list(MSE = mse, RMSE = rmse, MAE = mae, R2 = r2, AdjR2 = adjr2, DirAcc = da)
}

p_ar1   <- length(coef(fit_ar1)) - 1
m_train <- metrics(train_clean$USD, pred_train, p_ar1)
m_test  <- metrics(test_clean$USD,  pred_test,  p_ar1)

cat("\n=== Metrics: Subset + AR(1) ===\n")
cat("Selected vars:", paste(subset_vars, collapse = ", "), "\n")
cat("Train: ", sprintf(
  "MSE=%.4f  RMSE=%.4f  MAE=%.4f  R2=%.4f  AdjR2=%.4f  DirAcc=%.4f\n",
  m_train$MSE, m_train$RMSE, m_train$MAE, m_train$R2, m_train$AdjR2, m_train$DirAcc
))
cat("Test:  ", sprintf(
  "MSE=%.4f  RMSE=%.4f  MAE=%.4f  R2=%.4f  AdjR2=%.4f  DirAcc=%.4f\n",
  m_test$MSE, m_test$RMSE, m_test$MAE, m_test$R2, m_test$AdjR2, m_test$DirAcc
))
