# Crime Analysis Script for Puerto Rico (2000-2025)
# Analyzes crime trends with rates per 100,000 inhabitants

library(tidyverse)
library(ggplot2)
library(readr)

# Load data
crimes <- read_csv("delitos_tipo_1_pr_annual_2000_2025.csv")
population <- read_csv("pr_population_annual_2000_2025.csv")

# Clean and prepare data
crimes_clean <- crimes %>%
  select(year, tipo_delito, count) %>%
  rename(crime_type = tipo_delito)

population_clean <- population %>%
  select(year, population)

# Merge datasets
crime_data <- crimes_clean %>%
  left_join(population_clean, by = "year") %>%
  mutate(rate_per_100k = (count / population) * 100000)

# View first few rows
head(crime_data)

# Summary statistics
crime_data %>%
  group_by(crime_type) %>%
  summarise(
    mean_count = mean(count, na.rm = TRUE),
    mean_rate = mean(rate_per_100k, na.rm = TRUE),
    min_year = min(year),
    max_year = max(year),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_rate))

# --- VISUALIZATION 1: Crime Trends Over Time (Rates per 100,000) ---
p1 <- crime_data %>%
  ggplot(aes(x = year, y = rate_per_100k, color = crime_type, linetype = crime_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Crime Rates in Puerto Rico (2000-2025)",
    subtitle = "Annual rates per 100,000 inhabitants by crime type",
    x = "Year",
    y = "Rate per 100,000 inhabitants",
    color = "Crime Type",
    linetype = "Crime Type"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p1)
ggsave("crime_rates_trends.png", plot = p1, width = 12, height = 7, dpi = 300)

# --- VISUALIZATION 2: Faceted Plot by Crime Type ---
p2 <- crime_data %>%
  ggplot(aes(x = year, y = rate_per_100k, fill = crime_type)) +
  geom_area(alpha = 0.7) +
  facet_wrap(~ crime_type, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(
    title = "Crime Rates in Puerto Rico by Type (2000-2025)",
    subtitle = "Rates per 100,000 inhabitants",
    x = "Year",
    y = "Rate per 100,000 inhabitants",
    fill = "Crime Type"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(p2)
ggsave("crime_rates_faceted.png", plot = p2, width = 14, height = 10, dpi = 300)

# --- VISUALIZATION 3: Total Crime Rate Over Time ---
p3 <- crime_data %>%
  group_by(year, population) %>%
  summarise(total_count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
  mutate(total_rate = (total_count / population) * 100000) %>%
  ggplot(aes(x = year, y = total_rate)) +
  geom_line(color = "#d62728", linewidth = 1.5) +
  geom_point(color = "#d62728", size = 3) +
  theme_minimal() +
  labs(
    title = "Total Crime Rate in Puerto Rico (2000-2025)",
    subtitle = "All crime types combined, per 100,000 inhabitants",
    x = "Year",
    y = "Rate per 100,000 inhabitants"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p3)
ggsave("total_crime_rate.png", plot = p3, width = 12, height = 7, dpi = 300)

# --- VISUALIZATION 4: Comparison of Crime Rates (Start vs End) ---
start_end_comparison <- crime_data %>%
  filter(year %in% c(2000, 2025)) %>%
  select(year, crime_type, rate_per_100k) %>%
  pivot_wider(names_from = year, values_from = rate_per_100k) %>%
  rename(year_2000 = `2000`, year_2025 = `2025`) %>%
  mutate(change = year_2025 - year_2000,
         pct_change = ((year_2025 - year_2000) / year_2000) * 100) %>%
  arrange(desc(abs(change)))

p4 <- start_end_comparison %>%
  ggplot(aes(x = reorder(crime_type, change), y = change, fill = change > 0)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c("FALSE" = "#2ca02c", "TRUE" = "#d62728"),
                    labels = c("FALSE" = "Decreased", "TRUE" = "Increased")) +
  labs(
    title = "Crime Rate Changes (2000 vs 2025)",
    subtitle = "Difference in rates per 100,000 inhabitants",
    x = "Crime Type",
    y = "Change in Rate (per 100,000)",
    fill = "Direction"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

print(p4)
ggsave("crime_rate_changes.png", plot = p4, width = 11, height = 7, dpi = 300)

# Print comparison table
cat("\n=== CRIME RATE COMPARISON (2000 vs 2025) ===\n")
print(start_end_comparison, n = Inf)
