# 03_merge_model_prep.R -----------------------------------------------------

assign("arm_df", arm_df, envir = .GlobalEnv)   # 01_armenia_data.R
assign("us_df",  us_df,  envir = .GlobalEnv)   # 02_us_data.R


library(dplyr)

model_df <- arm_df |>
  left_join(us_df, by = "Date") |>
  mutate(
    USD = log(USD),
    ## log‑differentials -----------------------------------------------------
    m_diff = log(M2_us) - log(M2_usd_bil),
    y_diff  = log(real_GDP_us) - log(real_GDP),
    rs_diff = rate_us - refinancing_rate,
    pi_diff = pi_exp_us - inflation_rate,
    TB_cum_arm = cumsum(balance_of_trade),        
    TB_cum_us  = cumsum(trade_balance_us)
  ) |>
  select(Date, USD, m_diff, y_diff, rs_diff, pi_diff,
         TB_cum_arm, TB_cum_us, everything())

print(head(model_df, 12))
