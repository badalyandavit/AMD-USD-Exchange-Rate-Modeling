# 08_model_subset_time.R -----------------------------------------------------
library(dplyr)
library(car)

# 0. DATA PREP & SPLIT  ── keep rows unless *every* value is NA
model_df <- model_df %>% 
  filter(!if_all(everything(), is.na)) %>%   # drops only all‑NA rows
  arrange(Date) %>% 
  mutate(time_idx = as.numeric(Date))        # numeric time trend

n         <- nrow(model_df)
split_pt  <- floor(0.7 * n)
train_set <- model_df[1:split_pt, ]
test_set  <- model_df[(split_pt + 1):n, ]

# 1. SIGNIF + VIF (same as 06)
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

# 2. FIT + SUMMARY
formula_time <- as.formula(
  paste("USD ~ time_idx +", paste(subset_vars, collapse = " + "))
)
fit_time <- lm(formula_time, data = train_set)
cat("=== Summary: Subset + Time Trend ===\n")
print(summary(fit_time))

# 3. PREDICTIONS
pred_train <- fitted(fit_time)
pred_test  <- predict(fit_time, newdata = test_set)

# 4. METRICS (reuse helper)
metrics <- function(actual, pred, p) {
  n    <- length(actual)
  mse  <- mean((actual - pred)^2)
  rmse <- sqrt(mse)
  mae  <- mean(abs(actual - pred))
  r2   <- cor(actual, pred)^2
  adjr2 <- 1 - (1 - r2)*(n - 1)/(n - p - 1)
  da   <- mean(sign(diff(actual)) == sign(diff(pred)))
  list(MSE = mse, RMSE = rmse, MAE = mae, R2 = r2, AdjR2 = adjr2, DirAcc = da)
}

p_time  <- length(coef(fit_time)) - 1
m_train <- metrics(train_set$USD, pred_train, p_time)
m_test  <- metrics(test_set$USD,  pred_test,  p_time)

cat("\n=== Metrics: Subset + Time Trend ===\n")
cat("Train: ", sprintf(
  "MSE=%.4f  RMSE=%.4f  MAE=%.4f  R2=%.4f  AdjR2=%.4f  DirAcc=%.4f\n",
  m_train$MSE, m_train$RMSE, m_train$MAE, m_train$R2, m_train$AdjR2, m_train$DirAcc
))
cat("Test:  ", sprintf(
  "MSE=%.4f  RMSE=%.4f  MAE=%.4f  R2=%.4f  AdjR2=%.4f  DirAcc=%.4f\n",
  m_test$MSE, m_test$RMSE, m_test$MAE, m_test$R2, m_test$AdjR2, m_test$DirAcc
))
