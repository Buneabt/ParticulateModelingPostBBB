library(janitor)
library(here)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)


df <- read_csv("data/EMSE 6220 - Sheet1.csv", 
                             skip = 5)
df <- df[c(10,16),2:28] 
df <- df[,-2]

dfLong <- df %>% 
        pivot_longer(cols = starts_with("20"),
                     names_to = "Pre/Post",
                     values_to = "Rate Change")

dfPre <- dfLong[1:25,]
dfPost <- dfLong[26:50,]


dfLong <- clean_names(dfLong) 

    

dfLong <- dfLong %>%
  mutate(rate_change = as.numeric(gsub("%", "", rate_change)),
         pre_post = as.numeric(pre_post)
         )

ggplot(dfLong, aes(x = pre_post, y = rate_change, color = pre_bbb)) +
  # Single line before 2025 (only plot Pre since they're identical)
  geom_line(data = dfLong %>% filter(pre_post <= 2025, pre_bbb == "Rate Change (Pre)"), 
            size = 1) +
  # Pre line from 2025 onwards (smoothed)
  geom_smooth(data = dfLong %>% filter(pre_post >= 2025, pre_bbb == "Rate Change (Pre)"), 
              se = FALSE, method = "loess", span = 0.75, size = 1) +
  # Post line from 2025 onwards (regular line)
  geom_line(data = dfLong %>% filter(pre_post >= 2025, pre_bbb == "Rate Change (Post)"), 
            size = 1) +
  geom_vline(xintercept = 2025, linetype = "dotted", color = "black", size = 1) +
  geom_hline(yintercept = 0, size = 1) +
  labs(
    title = "Rate Change Comparison: Pre vs Post Big Beautiful Bill",
    x = "Year",
    y = "Rate Change (%)",
    color = "Period"
  ) +
  theme_bw() +
  annotate("text", x = 2025, y = max(dfLong$rate_change, na.rm = TRUE), 
           label = "BBB Passage (2025)", hjust = -0.1, size = 3.5)


dfHealth <- read_csv("EMSE 6220 - Sheet1.csv", skip = 5)
dfHealth <- dfHealth[c(11,17), 2:28] 
dfHealth <- dfHealth[, -2]
dfHealth[1,1] <- "Premature Deaths (USA) Pre-BBB"
dfHealth[2,1] <- "Premature Deaths (USA) Post-BBB"

dfLongHealth <- dfHealth %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "Pre/Post",
               values_to = "Deaths")

dfLongHealth <- dfLongHealth %>%
  mutate(Deaths = as.numeric(Deaths),
         Year = as.numeric(`Pre/Post`)  # Use a clearer name
  )



# Get 2035 values for both Pre and Post BBB
deaths_2035 <- dfLongHealth %>% filter(Year == 2035)
pre_2035 <- deaths_2035 %>% filter(grepl("Pre-BBB", `Pre-BBB`)) %>% pull(Deaths)
post_2035 <- deaths_2035 %>% filter(grepl("Post-BBB", `Pre-BBB`)) %>% pull(Deaths)
diff_2035 <- pre_2035 - post_2035

# Create data for shading the area between lines (2025-2035)
shade_data <- dfLongHealth %>%
  filter(Year >= 2025, Year <= 2035) %>%
  select(Year, `Pre-BBB`, Deaths) %>%
  pivot_wider(names_from = `Pre-BBB`, values_from = Deaths)

# Create the plot
SecondGraph <- ggplot(dfLongHealth, aes(x = Year, y = Deaths, color = `Pre-BBB`)) +
  # Add shaded ribbon between the two lines
  geom_ribbon(data = shade_data, 
              aes(x = Year, 
                  ymin = `Premature Deaths (USA) Post-BBB`, 
                  ymax = `Premature Deaths (USA) Pre-BBB`),
              fill = "coral", alpha = 0.3, inherit.aes = FALSE) +
  geom_smooth(se = FALSE, method = "gam") +
  geom_vline(xintercept = 2025, linetype = "dotted", color = "black", size = 1) +
  # Add range marker for 2035
  annotate("segment", x = 2035, xend = 2035, y = post_2035 - 5000, yend = pre_2035,
           arrow = arrow(ends = "both", length = unit(0.2, "cm")),
           color = "black", size = 0.8) +
  # Add label for the TOTAL difference (150K total over 10 years)
  annotate("text", x = 2031, y = 260*10^3,
           label = "~150K Increase In \nPreventable Deaths (2025-2035)",
           hjust = 0, size = 3.5, fontface = "bold") +
  scale_x_continuous(
    breaks = seq(2010, 2035, 10),
    expand = expansion(add = c(1,10))
  ) +
  labs(
    title = "Premature Deaths in the US Comparison: Pre vs Post Big Beautiful Bill",
    x = "Year",
    y = "Number of Premature Deaths",
    color = "Period"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        legend.position = 'none') +
  annotate("text", x = 2025, y = max(dfLongHealth$Deaths, na.rm = TRUE), 
           label = "BBB Passage (2025)", hjust = -0.1, size = 3.5) +
  geom_text(data = data.frame(x = 2039, y = 218875.3, label = "Post-Big Beautiful Bill"),
            mapping = aes(x = x, y = y, label = label),
            size = 3.88, colour = "red", family = "", inherit.aes = FALSE) +
  geom_text(data = data.frame(x = 2039, y = 193500, label = "Pre-Big Beautiful Bill"),
            mapping = aes(x = x, y = y, label = label),
            size = 3.88, colour = "steelblue", family = "", inherit.aes = FALSE) 


SecondGraph


