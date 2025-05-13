# Load required libraries
library(readr)
library(dplyr)
library(lubridate)
library(tseries)    # for adf.test()
library(forecast)   # for forecast()

# 1. Read in and parse your exchange‐rate data
rates <- read_csv("usd_exchange_rate.csv", col_types = cols(
  Date = col_character(),
  USD  = col_double()
)) %>%
  mutate(
    # try month-day-year first, then day-month-year
    Date = parse_date_time(Date, orders = c("mdy", "dmy")),
    Date = as.Date(Date)
  ) %>%
  arrange(Date)

# 2. Convert to a monthly ts object
start_year  <- year(rates$Date[1])
start_month <- month(rates$Date[1])
ts_rate <- ts(rates$USD,
              start     = c(start_year, start_month),
              frequency = 12)

# 3. Difference until stationary (ADF test p < 0.05)
d       <- 0
tmp_ts  <- ts_rate
repeat {
  adf <- adf.test(tmp_ts)
  cat(sprintf("Differences = %d → ADF p-value = %.4f\n", d, adf$p.value))
  if (adf$p.value < 0.05) break
  d      <- d + 1
  tmp_ts <- diff(ts_rate, differences = d)
}
diffed_ts <- tmp_ts

# 4. Compute critical value for significance bands
crit <- 1.96 / sqrt(length(diffed_ts))

# 5. Plot ACF and PACF for visual inspection
acf(diffed_ts, main = paste("ACF (d =", d, ")"))
pacf(diffed_ts, main = paste("PACF (d =", d, ")"))

# 6. Programmatically pick p and q
acf_vals   <- acf(diffed_ts, plot = FALSE)
pacf_vals  <- pacf(diffed_ts, plot = FALSE)

#    - For ACF: drop lag-0 then find first lag where |ACF| > crit → q
acf_acf      <- acf_vals$acf[-1]
q_cand       <- which(abs(acf_acf) > crit)[1]
q            <- if (!is.na(q_cand)) q_cand - 1 else 0

#    - For PACF: pacf()$acf corresponds to lags 1,2,… → p
pacf_pacf    <- pacf_vals$acf
p_cand       <- which(abs(pacf_pacf) > crit)[1]
p            <- if (!is.na(p_cand)) p_cand else 0

cat(sprintf("Selected ARIMA order: (p,d,q) = (%d,%d,%d)\n", p, d, q))

# 7. Fit the ARIMA(p,d,q) model on the original series
model <- arima(ts_rate, order = c(p, d, q))
print(summary(model))

# 8. Forecast the next 12 months and plot
fc <- forecast(model, h = 12)
print(fc)
plot(fc, main = paste0("ARIMA(", p, ",", d, ",", q, ") Forecast"))
