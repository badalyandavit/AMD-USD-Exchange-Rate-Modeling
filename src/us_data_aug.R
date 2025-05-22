library(fredr)
library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)

# Set API key for FRED
fredr_set_key("63607c8313d2159651503dcdac13f245")

# Define date range
start_date <- as.Date("2010-01-01")
end_date   <- as.Date("2025-03-01")

# Helper to fetch & truncate monthly series
get_fred_monthly <- function(id) {
  fredr(series_id = id, observation_start = start_date) |>
    select(Date = date, value) |>
    mutate(Date = floor_date(Date, "month")) |>
    group_by(Date) |>
    summarise(value = last(value), .groups = "drop") |>
    filter(Date >= start_date, Date <= end_date)
}

# Fetch series
m2_us   <- get_fred_monthly("M2SL")     |> rename(M2_us = value)          
rate_us <- get_fred_monthly("FEDFUNDS") |> rename(rate_us = value)        
infl_us <- get_fred_monthly("T10YIE")   |> rename(pi_exp_us = value)      
tb_us   <- get_fred_monthly("BOPGSTB")  |> rename(trade_balance_us = value)

# Quarterly real GDP → monthly with interpolation + forward‐fill
gdp_q <- fredr(series_id = "GDPC1", observation_start = start_date) |>
  select(Date = date, real_GDP_us = value) |>
  filter(Date >= start_date, Date <= end_date)

gdp_us <- gdp_q |>
  complete(Date = seq.Date(min(Date), max(Date), by = "month")) |>
  arrange(Date) |>
  mutate(
    real_GDP_us = na.approx(real_GDP_us, x = Date, rule = 2)
  ) |>
  fill(real_GDP_us, .direction = "down") |>
  filter(Date >= start_date, Date <= end_date)

# Combine all into one data frame
us_df <- m2_us |>
  left_join(rate_us, by = "Date") |>
  left_join(infl_us, by = "Date") |>
  left_join(tb_us,   by = "Date") |>
  left_join(gdp_us,  by = "Date") |>
  arrange(Date) |>
  fill(pi_exp_us, real_GDP_us, .direction = "down")

# Save and inspect
save(us_df, file = "us_df.RData")
tail(us_df)
