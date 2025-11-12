library(prophet)
library(janitor)
library(here)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(ggplot2)

# === BASIC PROPHET MODEL SETUP ===
df_full <- read.csv("data/finaldf.csv")
df_policy <- read.csv("data/finaldf.csv")

df_full$Carbon.dioxide <- as.numeric(gsub(",", "", as.character(df_full$Carbon.dioxide)))
df_full$Methane <- as.numeric(gsub(",", "", as.character(df_full$Methane)))
df_full$Nitrous.oxide <- as.numeric(gsub(",", "", as.character(df_full$Nitrous.oxide)))
df_full$Others <- as.numeric(gsub(",", "", as.character(df_full$Others)))

df_full1 <- df_full %>% 
  filter(Year <= 2022) %>% 
  mutate(
    Carbon.dioxide = Carbon.dioxide/sum(Carbon.dioxide),
    Nitrous.oxide = Nitrous.oxide/sum(Nitrous.oxide),
    Methane = Methane/sum(Methane),
    Pm2.5 = Pm2.5/sum(Pm2.5)
  ) 


df_long <- df_full1 %>%
  select(Year, Carbon.dioxide, Nitrous.oxide, Methane, Pm2.5) %>%
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
      "Nitrous.oxide" = "orange", 
      "Methane" = "darkblue",
      "Pm2.5" = "darkgreen"
    ),
    labels = c(
      "Carbon.dioxide" = "Carbon Dioxide",
      "Nitrous.oxide" = "Nitrous Oxide",
      "Methane" = "Methane", 
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






