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
library(gridExtra)

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