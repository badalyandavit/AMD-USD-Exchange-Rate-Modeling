library(readxl)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)

start_date <- as.Date("2010-01-01")
end_date   <- as.Date("2025-03-31")

read_monthly <- function(path, value_name) {
  read_excel(path, col_types = c("date", "numeric")) |>
    rename(!!value_name := 2) |>
    filter(Date >= start_date, Date <= end_date)
}

m2    <- read_monthly("data/armenia_M2.xlsx",                   "M2")               
bot   <- read_monthly("data/armenia_balance_of_trade.xlsx",     "balance_of_trade") 
infl  <- read_monthly("data/armenia_long_run_inflation_rate.xlsx",  "inflation_rate")
refi  <- read_monthly("data/armenia_short_term_interest_rate.xlsx","refinancing_rate")
unemp <- read_monthly("data/armenia_unemployment_rate.xlsx",    "unemployment_rate")

gdp <- read_excel("data/armenia_GDP.xlsx",
                  col_types = c("date","numeric","numeric","numeric")) |>
  rename(
    GDP         = 2,   # nominal USD bn
    Growth_Rate = 3,   # quarterly %
    CPI         = 4    # index (100 = base)
  ) |>
  filter(Date >= start_date, Date <= end_date)

fx_monthly <- read_csv("data/usd_exchange_rate.csv",
                       col_types = cols(
                         Date = col_date(format = "%d/%m/%Y"),
                         USD  = col_double()
                       )) |>
  mutate(Date = floor_date(Date, "month")) |>
  group_by(Date) |>
  summarise(USD = last(USD), .groups = "drop") |>
  filter(Date >= start_date, Date <= end_date)

arm_df <- fx_monthly |>
  left_join(m2,    by = "Date") |>
  left_join(bot,   by = "Date") |>
  left_join(infl,  by = "Date") |>
  left_join(refi,  by = "Date") |>
  left_join(unemp, by = "Date") |>
  left_join(gdp,   by = "Date") |>
  arrange(Date) |>
  
  mutate(
    GDP = case_when(
      !is.na(Growth_Rate) ~ lag(GDP, 12) * (1 + Growth_Rate/100),
      TRUE                ~ GDP
    )
  ) |>
  mutate(
    GDP = na.approx(GDP, x = Date, rule = 2),
    CPI = na.approx(CPI, x = Date, rule = 2)
  ) |>
  
  mutate(real_GDP = GDP / (CPI / 100)) |>
  
  fill(unemployment_rate, .direction = "up") |>
  
  mutate(M2_usd_bil = (M2 * 1e3) / USD) |>
  
  select(
    Date,
    USD,
    M2_usd_bil,
    real_GDP,
    everything()
  )

arm_df
save(arm_df, file = "arm_df.RData")
