library(dplyr)
library(ggplot2)
library(lubridate)
library(moments)   

plot_dir <- "plots"
if (!dir.exists(plot_dir)) dir.create(plot_dir)

usd_stats <- model_df %>% 
  summarise(
    count    = n(),
    mean     = mean(USD, na.rm = TRUE),
    median   = median(USD, na.rm = TRUE),
    sd       = sd(USD, na.rm = TRUE),
    min      = min(USD, na.rm = TRUE),
    q1       = quantile(USD, .25, na.rm = TRUE),
    q3       = quantile(USD, .75, na.rm = TRUE),
    max      = max(USD, na.rm = TRUE),
    skewness = skewness(USD, na.rm = TRUE),
    kurtosis = kurtosis(USD, na.rm = TRUE)
  )
print(usd_stats)

# 2. Histogram + density
p_hist <- ggplot(model_df, aes(x = USD)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey80", color = "black") +
  geom_density() +
  labs(title = "Distribution of AMD–USD Exchange Rate",
       x     = "AMD per USD", y = "Density")
ggsave(file.path(plot_dir, "histogram_density.png"), p_hist, width = 8, height = 6, dpi = 300)

p_qq <- ggplot(model_df, aes(sample = USD)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ-Plot of AMD–USD Exchange Rate",
       x     = "Theoretical Quantiles", y = "Sample Quantiles")
ggsave(file.path(plot_dir, "qqplot_usd.png"), p_qq, width = 8, height = 6, dpi = 300)

p_box_all <- ggplot(model_df, aes(y = USD)) +
  geom_boxplot() +
  labs(title = "Boxplot of AMD–USD Exchange Rate", y = "AMD per USD")
ggsave(file.path(plot_dir, "boxplot_overall.png"), p_box_all, width = 6, height = 6, dpi = 300)

model_df <- model_df %>% mutate(year = year(Date))
p_box_year <- ggplot(model_df, aes(x = factor(year), y = USD)) +
  geom_boxplot() +
  labs(title = "Year-by-Year Boxplot of AMD–USD",
       x     = "Year", y = "AMD per USD") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(plot_dir, "boxplot_by_year.png"), p_box_year, width = 10, height = 6, dpi = 300)

p_ts <- ggplot(model_df, aes(x = Date, y = USD)) +
  geom_line() +
  labs(title = "AMD–USD Exchange Rate Over Time",
       x     = "Date", y = "AMD per USD")
ggsave(file.path(plot_dir, "timeseries_usd.png"), p_ts, width = 10, height = 6, dpi = 300)
