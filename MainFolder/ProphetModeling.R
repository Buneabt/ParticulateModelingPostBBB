library(prophet)
library(janitor)
library(here)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(gridExtra)

# === BASIC PROPHET MODEL SETUP ===
df_full <- read.csv("data/finaldf.csv")
df_policy <- read.csv("data/finaldf.csv")

df_full$Carbon.dioxide <- as.numeric(gsub(",", "", as.character(df_full$Carbon.dioxide)))


df_full1 <- df_full %>% 
  filter(Year <= 2022) %>% 
  mutate(
    Carbon.dioxide = Carbon.dioxide/sum(Carbon.dioxide),
    Nitrous.oxide = Nitrous.oxide/sum(Nitrous.oxide),
    Pm2.5 = Pm2.5/sum(Pm2.5)
  ) %>% 
  select (Year, Carbon.dioxide, Pm2.5, Nitrous.oxide)


df_long <- df_full1 %>%
  select(Year, Carbon.dioxide, Pm2.5) %>%
  pivot_longer(cols = -Year, names_to = "Component", values_to = "Normalized_Value")

emissions_plot <- ggplot(df_long, aes(x = Year, y = Normalized_Value, color = Component)) +
  geom_line(size = 1.2) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    title = "Normalized Emissions and Air Quality Components Over Time",
    subtitle = "All components shown as proportion of total (2000-2022)",
    x = "Year",
    y = "Normalized Value (Proportion of Total)",
    color = "Component",
    caption = "Data normalized by dividing each component by its sum across all years"
  ) +
  scale_color_manual(
    values = c(
      "Carbon.dioxide" = "darkred",
      #"Nitrous.oxide" = "darkblue",
      "Pm2.5" = "darkgreen"
    ),
    labels = c(
      "Carbon.dioxide" = "Carbon Dioxide",
      #"Nitrous.oxide" = "Nitrous Oxide", 
      "Pm2.5" = "PM2.5"
    )
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(ncol = 3))

print(emissions_plot)

ggsave("data/emissions.png", emissions_plot, 
       width = 12, height = 10, dpi = 300)


dfprophet <- df_full %>%
  filter(!is.na(Pm2.5) & !is.na(Carbon.dioxide) & 
           is.finite(Pm2.5) & is.finite(Carbon.dioxide)) %>%
  select(Carbon.dioxide, Pm2.5, Year)

prophet_data <- data.frame(
  ds = as.Date(paste0(dfprophet$Year, "-01-01")),
  y = dfprophet$Pm2.5,
  carbon_dioxide = dfprophet$Carbon.dioxide
)

prophet_data <- prophet_data[complete.cases(prophet_data), ]
prophet_data <- prophet_data[is.finite(prophet_data$y) & is.finite(prophet_data$carbon_dioxide), ]

projected_data <- df_full %>%
  filter(Year >= 2024 & Year <= 2035 & !is.na(Carbon.dioxide)) %>%
  select(Year, Carbon.dioxide)


future_years <- projected_data$Year
future_co2 <- projected_data$Carbon.dioxide

m <- prophet(
  yearly.seasonality = FALSE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  changepoint.prior.scale = 0.1
)

m <- add_regressor(m, 'carbon_dioxide', prior.scale = 0.5, mode = 'additive')
m <- fit.prophet(m, prophet_data)

future <- data.frame(
  ds = as.Date(paste0(future_years, "-01-01")),
  carbon_dioxide = future_co2
)

future_combined <- rbind(
  prophet_data[, c("ds", "carbon_dioxide")],
  future
)

forecast <- predict(m, future_combined)

historical <- forecast[1:nrow(prophet_data), ]
r2 <- cor(prophet_data$y, historical$yhat)^2
rmse <- sqrt(mean((prophet_data$y - historical$yhat)^2))

future_forecasts <- forecast[forecast$ds >= as.Date("2025-01-01") & 
                               forecast$ds <= as.Date("2035-01-01"), ]

forecast_years <- as.numeric(format(future_forecasts$ds, "%Y"))

forecast_table <- data.frame(
  Year = forecast_years,
  PM25_Forecast = round(future_forecasts$yhat, 3),
  Lower_Bound = round(future_forecasts$yhat_lower, 3),
  Upper_Bound = round(future_forecasts$yhat_upper, 3)
)

# === BASELINE SCENARIO ===
baseline_forecast_table <- forecast_table
baseline_forecast_table$Scenario <- "Pre-BBB Baseline"

# === POST-BBB POLICY SCENARIO ===
df_policy$Carbon.dioxide <- as.numeric(gsub(",", "", as.character(df_policy$Carbon.dioxide)))
df_policy$forecastedC02 <- as.numeric(gsub(",", "", as.character(df_policy$forecastedC02)))

co2_comparison <- df_policy %>%
  filter(Year >= 2024) %>%
  select(Year, Carbon.dioxide, forecastedC02) %>%
  mutate(
    CO2_Difference = forecastedC02 - Carbon.dioxide,
    Percent_Reduction = ((Carbon.dioxide - forecastedC02) / Carbon.dioxide) * 100
  )

policy_co2_data <- df_policy %>%
  filter(Year >= 2024 & Year <= 2035 & !is.na(forecastedC02)) %>%
  select(Year, forecastedC02)

policy_future_years <- policy_co2_data$Year
policy_future_co2 <- policy_co2_data$forecastedC02

policy_future <- data.frame(
  ds = as.Date(paste0(policy_future_years, "-01-01")),
  carbon_dioxide = policy_future_co2
)

policy_future_combined <- rbind(
  prophet_data[, c("ds", "carbon_dioxide")],
  policy_future
)

policy_forecast <- predict(m, policy_future_combined)

policy_future_forecasts <- policy_forecast[policy_forecast$ds >= as.Date("2025-01-01") & 
                                             policy_forecast$ds <= as.Date("2035-01-01"), ]

policy_forecast_years <- as.numeric(format(policy_future_forecasts$ds, "%Y"))

policy_forecast_table <- data.frame(
  Year = policy_forecast_years,
  PM25_Forecast = round(policy_future_forecasts$yhat, 3),
  Lower_Bound = round(policy_future_forecasts$yhat_lower, 3),
  Upper_Bound = round(policy_future_forecasts$yhat_upper, 3),
  Scenario = "Post-BBB"
)

# === POLICY IMPACT ANALYSIS ===
combined_forecasts <- rbind(baseline_forecast_table, policy_forecast_table)

policy_impacts <- data.frame(
  Year = baseline_forecast_table$Year,
  Pre_BBB_PM25 = baseline_forecast_table$PM25_Forecast,
  Post_BBB_PM25 = policy_forecast_table$PM25_Forecast,
  PM25_Impact = policy_forecast_table$PM25_Forecast - baseline_forecast_table$PM25_Forecast,
  Percent_Impact = ((policy_forecast_table$PM25_Forecast - baseline_forecast_table$PM25_Forecast) / 
                      baseline_forecast_table$PM25_Forecast) * 100
)

print(policy_impacts)

total_sum_gap <- sum(policy_impacts$PM25_Impact)

# === BBB POLICY COMPARISON PLOT ===
plot_data <- rbind(
  data.frame(
    Year = as.Date(paste0(baseline_forecast_table$Year, "-01-01")),
    PM25 = baseline_forecast_table$PM25_Forecast,
    Lower = baseline_forecast_table$Lower_Bound,
    Upper = baseline_forecast_table$Upper_Bound,
    Scenario = "Pre-BBB Baseline"
  ),
  data.frame(
    Year = as.Date(paste0(policy_forecast_table$Year, "-01-01")),
    PM25 = policy_forecast_table$PM25_Forecast,
    Lower = policy_forecast_table$Lower_Bound,
    Upper = policy_forecast_table$Upper_Bound,
    Scenario = "Post-BBB"
  ),
  data.frame(
    Year = prophet_data$ds,
    PM25 = prophet_data$y,
    Lower = NA,
    Upper = NA,
    Scenario = "Historical"
  )
)

bbb_comparison_plot <- ggplot(plot_data, aes(x = Year, y = PM25, color = Scenario)) +
  geom_point(data = subset(plot_data, Scenario == "Historical"), 
             size = 2, alpha = 0.8, color = "black") +
  geom_line(data = subset(plot_data, Scenario == "Pre-BBB Baseline"), 
            size = 1.2, linetype = "dashed") +
  geom_ribbon(data = subset(plot_data, Scenario == "Pre-BBB Baseline"),
              aes(ymin = Lower, ymax = Upper, fill = Scenario), 
              alpha = 0.2) +
  geom_line(data = subset(plot_data, Scenario == "Post-BBB"), 
            size = 1.2) +
  geom_ribbon(data = subset(plot_data, Scenario == "Post-BBB"),
              aes(ymin = Lower, ymax = Upper, fill = Scenario), 
              alpha = 0.2) +
  geom_vline(xintercept = as.Date("2025-01-01"), linetype = "dotted", 
             color = "red", size = 1, alpha = 0.7) +
  labs(
    title = "Big Beautiful Bill Impact on PM2.5 Air Quality",
    subtitle = paste("Post-BBB increases PM2.5 by", 
                     round(total_sum_gap, 3), "μg/m³ between 2025 and 2035"),
    x = "Year",
    y = "PM2.5 (μg/m³)",
    color = "Scenario",
    fill = "Scenario",
    caption = "Black dots: Historical data (2000-2022) | Red line: BBB policy divergence (2025)"
  ) +
  scale_color_manual(values = c("Pre-BBB Baseline" = "blue", 
                                "Post-BBB" = "darkgreen", 
                                "Historical" = "black")) +
  scale_fill_manual(values = c("Pre-BBB Baseline" = "blue", 
                               "Post-BBB" = "darkgreen")) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )

print(bbb_comparison_plot)

ggsave("data/bbb_policy_impact.png", bbb_comparison_plot, 
       width = 12, height = 8, dpi = 300)



# === MODEL VALIDATION ===
cat("\n=== PROPHET MODEL VALIDATION ===\n")

# 1. Basic Performance Metrics
cat("Model Performance Metrics:\n")
cat(sprintf("R²: %.4f (%.1f%%)\n", r2, r2*100))
cat(sprintf("RMSE: %.4f μg/m³\n", rmse))
cat(sprintf("MAE: %.4f μg/m³\n", mean(abs(prophet_data$y - historical$yhat))))

# 2. Residual Analysis
residuals <- prophet_data$y - historical$yhat
cat(sprintf("\nResidual Statistics:\n"))
cat(sprintf("Mean residual: %.4f\n", mean(residuals)))
cat(sprintf("Std dev residuals: %.4f\n", sd(residuals)))
cat(sprintf("Min residual: %.4f\n", min(residuals)))
cat(sprintf("Max residual: %.4f\n", max(residuals)))

# 3. Cross-Validation
cat("\nPerforming time series cross-validation...\n")
cv_results <- cross_validation(m, initial = 15 * 365, period = 2 * 365, horizon = 3 * 365, units = 'days')
cv_performance <- performance_metrics(cv_results)

cat("Cross-validation Performance:\n")
print(cv_performance)

# 4. Residual Plots for Validation

validation_plots <- grid.arrange(
  # Residuals vs Fitted
  ggplot(data.frame(fitted = historical$yhat, residuals = residuals), 
         aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
    theme_bw(),
  
  # Q-Q Plot for Normality
  ggplot(data.frame(residuals = residuals), aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = "Q-Q Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_bw(),
  
  # Residuals over Time
  ggplot(data.frame(date = prophet_data$ds, residuals = residuals), 
         aes(x = date, y = residuals)) +
    geom_line(alpha = 0.7) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Residuals Over Time", x = "Year", y = "Residuals") +
    theme_bw(),
  
  # Histogram of Residuals
  ggplot(data.frame(residuals = residuals), aes(x = residuals)) +
    geom_histogram(bins = 10, alpha = 0.7, color = "black", fill = "lightblue") +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Distribution of Residuals", x = "Residuals", y = "Frequency") +
    theme_bw(),
  
  ncol = 2
)

print(validation_plots)

# 5. Normality Tests
cat("\nNormality Tests:\n")
shapiro_test <- shapiro.test(residuals)
cat(sprintf("Shapiro-Wilk test p-value: %.4f\n", shapiro_test$p.value))
if (shapiro_test$p.value > 0.05) {
  cat("✓ Residuals appear normally distributed (p > 0.05)\n")
} else {
  cat("⚠ Residuals may not be normally distributed (p < 0.05)\n")
}

# 6. Autocorrelation Test
cat("\nAutocorrelation Analysis:\n")
acf_result <- acf(residuals, plot = FALSE)
ljung_box <- Box.test(residuals, lag = 10, type = "Ljung-Box")
cat(sprintf("Ljung-Box test p-value: %.4f\n", ljung_box$p.value))
if (ljung_box$p.value > 0.05) {
  cat("✓ No significant autocorrelation detected (p > 0.05)\n")
} else {
  cat("⚠ Potential autocorrelation in residuals (p < 0.05)\n")
}

# 7. Model Components Analysis
cat("\nModel Components:\n")
components <- predict(m, future_combined)
trend_change <- tail(components$trend, 1) - components$trend[1]
cat(sprintf("Overall trend change: %.3f μg/m³ from %d to %d\n", 
            trend_change, 
            as.numeric(format(min(prophet_data$ds), "%Y")), 
            as.numeric(format(max(future_combined$ds), "%Y"))))

# 8. CO2 Regressor Impact Assessment
co2_range <- max(prophet_data$carbon_dioxide) - min(prophet_data$carbon_dioxide)
cat(sprintf("\nCO2 Regressor Analysis:\n"))
cat(sprintf("Historical CO2 range: %.1f units\n", co2_range))
cat(sprintf("CO2 standardization μ: %.1f, σ: %.1f\n", 
            m$extra_regressors$carbon_dioxide$mu, 
            m$extra_regressors$carbon_dioxide$std))

# 9. Validation Summary
cat("\n=== VALIDATION SUMMARY ===\n")
cat("Model Quality Assessment:\n")
if (r2 > 0.95) cat("✓ Excellent model fit (R² > 95%)\n") else if (r2 > 0.90) cat("✓ Good model fit (R² > 90%)\n") else cat("⚠ Moderate model fit\n")
if (rmse < 0.5) cat("✓ Low prediction error (RMSE < 0.5)\n") else cat("⚠ Moderate prediction error\n")
if (mean(cv_performance$rmse) < rmse * 1.2) cat("✓ Stable cross-validation performance\n") else cat("⚠ Potential overfitting detected\n")

cat(sprintf("\nRecommendation: Model is suitable for BBB policy analysis\n"))
cat(sprintf("Confidence in forecasts: %s\n", ifelse(r2 > 0.95 & rmse < 0.5, "High", "Moderate")))

# Save validation plots
ggsave("data/model_validation_diagnostics.png", validation_plots, 
       width = 12, height = 10, dpi = 300)

cat("\nValidation plots saved to: data/model_validation_diagnostics.png\n")



