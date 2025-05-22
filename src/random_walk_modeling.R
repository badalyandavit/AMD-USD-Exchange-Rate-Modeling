library(tidyverse)
library(MLmetrics)

data <- read.csv("usd_exchange_rate.csv")
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
data <- data %>% arrange(Date)

set.seed(42)
data$random_walk <- data$USD + rnorm(n = nrow(data), mean = 0, sd = sd(data$USD))

# 1. Errors
data$error          <- data$USD - data$random_walk
data$squared_error  <- data$error^2
data$abs_error      <- abs(data$error)

# 2. Metrics
mse_value   <- MSE(data$USD, data$random_walk)
rmse_value  <- RMSE(data$USD, data$random_walk)
mae_value   <- MAE(data$USD, data$random_walk)
r2_value    <- R2_Score(data$USD, data$random_walk)

# 3. Directional accuracy
#    compare signs of day-to-day changes
actual_dir    <- sign(diff(data$USD))
predicted_dir <- sign(diff(data$random_walk))
directional_accuracy <- mean(actual_dir == predicted_dir, na.rm = TRUE) * 100

# 4. Print results
cat("MSE: ",  mse_value,   "\n")
cat("RMSE: ", rmse_value,  "\n")
cat("MAE: ",  mae_value,   "\n")
cat("R²: ",   r2_value,    "\n")
cat("Directional Accuracy: ", round(directional_accuracy, 2), "%\n")
