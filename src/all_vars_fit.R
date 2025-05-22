# 05_model_all.R --------------------------------------------------------------
library(dplyr)

# 0. DATA PREP
model_df <- model_df %>%
  filter(if_any(everything(), ~ !is.na(.))) %>%
  arrange(Date)

# 1. TRAIN/TEST SPLIT (80/20)
n         <- nrow(model_df)
split_pt  <- floor(0.7 * n)
train_set <- model_df[1:split_pt, ]
test_set  <- model_df[(split_pt + 1):n, ]

# 2. FIT
fit_all <- lm(USD ~ m_diff + y_diff + rs_diff + pi_diff + TB_cum_arm + TB_cum_us,
              data = train_set)

# 3. SUMMARY
cat("=== Summary: All predictors ===\n")
print(summary(fit_all))

# 4. PREDICTIONS
pred_train <- fitted(fit_all)
pred_test  <- predict(fit_all, newdata = test_set)

# 5. METRICS HELPER
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

# 6. COMPUTE & PRINT
p_all   <- length(coef(fit_all)) - 1
m_train <- metrics(train_set$USD, pred_train, p_all)
m_test  <- metrics(test_set$USD,  pred_test,  p_all)

cat("\n=== Metrics: All predictors ===\n")
cat("Train: ", sprintf(
  "MSE=%.4f  RMSE=%.4f  MAE=%.4f  R2=%.4f  AdjR2=%.4f  DirAcc=%.4f\n",
  m_train$MSE, m_train$RMSE, m_train$MAE, m_train$R2, m_train$AdjR2, m_train$DirAcc
))
cat("Test:  ", sprintf(
  "MSE=%.4f  RMSE=%.4f  MAE=%.4f  R2=%.4f  AdjR2=%.4f  DirAcc=%.4f\n",
  m_test$MSE, m_test$RMSE, m_test$MAE, m_test$R2, m_test$AdjR2, m_test$DirAcc
))

