library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(scales)

# Read and process the data (assuming your original data processing works)
df <- read_csv("data/EMSE 6220 - Sheet1.csv", skip = 5)
df <- df[c(2, 8), 3:33] 
df_pre <- df[1, 1:31]
df_tobind <- df_pre[,20]
df_pre <- pivot_longer(df_pre, cols = 1:31, names_to = 'years', values_to = 'C02')
df_post <- cbind(df_tobind, df[2, 21:31])
df_post <- pivot_longer(df_post, cols = 1:12, names_to = 'years', values_to = 'C02')

# Clean the data
df_pre <- df_pre %>% 
  mutate(C02 = gsub(",", "", C02)) %>% 
  mutate(years = as.numeric(years),
         C02 = as.numeric(C02)) %>%
  filter(!is.na(C02), !is.na(years))

df_post <- df_post %>% 
  mutate(C02 = gsub(",", "", C02)) %>% 
  mutate(years = as.numeric(years),
         C02 = as.numeric(C02)) %>%
  filter(!is.na(C02), !is.na(years))

# Add scenario labels
df_pre$scenario <- "Pre-BBB"
df_post$scenario <- "Big Beautiful Bill"

df_combined <- rbind(df_pre, df_post)


ribbon_years <- seq(2024, 2035, by = 1)
ribbon_data <- data.frame(
  years = ribbon_years,
  pre_bbb = approx(df_pre$years, df_pre$C02, xout = ribbon_years)$y,
  bbb = approx(df_post$years, df_post$C02, xout = ribbon_years)$y
)

ribbon_data <- ribbon_data[complete.cases(ribbon_data), ]

p <- ggplot() +
  geom_ribbon(data = ribbon_data, 
              aes(x = years, ymin = pre_bbb, ymax = bbb),
              fill = "#FF6B6B", alpha = 0.3,
              color = NA) +
  geom_line(data = df_pre, 
            aes(x = years, y = C02, color = "Pre-BBB"),
            size = 1.2, alpha = 0.9) +
  geom_line(data = df_post, 
            aes(x = years, y = C02, color = "Big Beautiful Bill"),
            size = 1.2, alpha = 0.9) +
  scale_color_manual(values = c("Pre-BBB" = "#2E8B57", 
                                "Big Beautiful Bill" = "#DC143C"),
                     name = "Scenario") +
  scale_x_continuous(breaks = seq(2005, 2035, 5),
                     minor_breaks = seq(2005, 2035, 1)) +
  scale_y_continuous(labels = comma_format(),
                     breaks = seq(3000, 6500, 500)) +
  labs(title = "U.S. CO2 Emissions Projections: Pre-BBB vs Big Beautiful Bill",
       subtitle = "Projected divergence in emissions trajectories from 2025-2035",
       x = "Year",
       y = "CO2 Emissions (Million Metric Tons)",
       caption = "Sources: NIH, Princeton REPEAT Project") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40", margin = margin(b = 20)),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_line(color = "gray95", size = 0.3),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  geom_hline(yintercept = 3064, linetype = "dashed", color = "gray20", alpha = 0.7) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = "gray50", alpha = 0.7) +
  annotate("text", x = 2025.5, y = 5800, 
           label = "Policy\nDivergence", 
           hjust = 0, size = 3.5, color = "gray40",
           fontface = "italic") + 
  annotate("text", x = 2015, y = 3200, 
           label = "United States Climate Goal", 
           hjust = 0, size = 3.5, color = "gray20",
           fontface = "italic") 

print(p)

# Save the plot
ggsave("data/co2_emissions_comparison.png", 
       plot = p, 
       width = 12, height = 8, 
       dpi = 300, 
       bg = "white")
