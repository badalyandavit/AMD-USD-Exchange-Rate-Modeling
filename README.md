# AMD–USD Exchange Rate Modeling

Developed multiple linear regression and time-series models to forecast the AMD–USD exchange rate, achieving a 95% out-of-sample adjusted R² and 68% directional accuracy.

## Repository Structure


```
main/
├── src/                       # Contains all code-related stuff, i.e. data augmentation, modeling, EDA, etc.
│   ├── AR1_fit.R                  # Subset + AR(1) regression with VIF-based variable selection
│   ├── all_vars_fit.R             # Multiple linear regression using all predictors
│   ├── arima_forecasting.R        # Automated ARIMA order selection and forecasting
│   ├── arm_data_aug.R             # Load and preprocess Armenian macro data
│   ├── combine_data.R             # Merge Armenian and US data; compute features
│   ├── eda.R                      # Exploratory data analysis and plots
│   ├── us_data_aug.R              # Load and preprocess US macro data
│   ├── random_walk_modeling.R     # Baseline random-walk model
│   ├── subset_vars_fit.R          # Regression with significance & VIF‐based subset
│   ├── time_fit.R                 # Time-trend regression model
│   
├── data/                      # Data Storage
│   ├── armenia_*.xlsx         # Raw Excel files for Armenian M2, GDP, inflation, etc.
│   └── usd_exchange_rate.csv      # Monthly AMD–USD exchange rate series
│   
├── plots/                     # EDA output figures (histograms, boxplots, time series)
│   ├── boxplot_by_year.png
│   ├── boxplot_overall.png
│   ├── histogram_density.png
│   ├── qqplot_usd.png
│   └── timeseries_usd.png
│   
├── us_df.RData                # Saved preprocessed US data frame
├── arm_df.RData               # Saved preprocessed Armenian data frame

```

## Prerequisites

* **R** (≥ 4.0)
* **R packages**:
  `readxl`, `readr`, `dplyr`, `tidyr`, `lubridate`, `zoo`, `ggplot2`, `moments`, `forecast`, `tseries`, `car`

```r
install.packages(c(
  "readxl", "readr", "dplyr", "tidyr", "lubridate",
  "zoo", "ggplot2", "moments", "forecast", "tseries", "car"
))
```

## Workflow

1. **Data Preparation**

   * Run `arm_data_aug.R` to read and augment Armenian time-series data.
   * Run `us_data_aug.R` to read and augment US time-series data.
   * Run `combine_data.R` to merge both datasets and compute log-differentials and cumulative trade balances.

2. **Exploratory Data Analysis**

   * Execute `eda.R` to generate summary statistics and save plots in `plots/`.

3. **Modeling**

   * **Baseline**: `random_walk_modeling.R`
   * **Time Trend**: `time_fit.R`
   * **Full MLR**: `all_vars_fit.R`
   * **Subset MLR**: `subset_vars_fit.R` (significance & VIF filtering)
   * **AR(1) Model**: `AR1_fit.R` (adds lag term)
   * **ARIMA**: `arima_forecasting.R` (automated (p,d,q) selection and 12-month forecast)

4. **Results**

   * Compare performance metrics printed in the console (MSE, RMSE, MAE, R², Adj R², directional accuracy).

## Key Findings

* **Adjusted R² (out-of-sample)**: 95%
* **Directional Accuracy**: 68%
* ARIMA and MLR with AR(1) components both delivered strong forecasting performance under different conditions.

## Data Sources

* **Exchange Rate**: Monthly AMD–USD close prices (`usd_exchange_rate.csv`)
* **Armenian Macros**: Central Bank and statistical service releases (M2, GDP, inflation, interest rates, unemployment)
* **US Macros**: Federal Reserve Economic Data (M2, GDP, trade balance, inflation expectations, refinancing rate)

---

**Contact**:
Davit Badalyan • GitHub: [@badalyandavit](https://github.com/badalyandavit)
Email: [davit_badalyan@edu.aua.am](mailto:davit_badalyan@edu.aua.am)
