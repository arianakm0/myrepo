# Crime Analysis Script for Puerto Rico (2000-2025)
# Analyzes crime trends with rates per 100,000 inhabitants
# With professional styling (golden palette + clean typography)

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

# Define custom theme and color palette
theme_custom <- function() {
  theme_minimal() +
  theme(
    # Font styling
    text = element_text(family = "Arial", color = "#333333"),
    plot.title = element_text(size = 16, face = "bold", color = "#1a1a1a", margin = margin(b = 8)),
    plot.subtitle = element_text(size = 12, color = "#666666", margin = margin(b = 12)),
    axis.title = element_text(size = 11, color = "#333333"),
    axis.text = element_text(size = 10, color = "#666666"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    panel.grid.major = element_line(color = "#e8e8e8", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
}

# Color palette (golden/orange tones)
golden_palette <- c(
  "#FDB913",  # Bright golden
  "#F5A623",  # Golden orange
  "#E89B3C",  # Medium golden
  "#D68910",  # Dark golden
  "#C17817",  # Burnt orange
  "#A96B2F",  # Darker burnt
  "#8B5A2B",  # Saddle brown
  "#6B4423"   # Dark brown
)

# --- VISUALIZATION 1: Crime Trends Over Time (Rates per 100,000) ---
p1 <- crime_data %>%
  ggplot(aes(x = year, y = rate_per_100k, color = crime_type, linetype = crime_type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = golden_palette) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid", "solid", "solid")) +
  theme_custom() +
  labs(
    title = "Crime Rates in Puerto Rico (2000-2025)",
    subtitle = "Annual rates per 100,000 inhabitants by crime type",
    x = "Year",
    y = "Rate per 100,000 inhabitants",
    color = "Crime Type",
    linetype = "Crime Type"
  )

print(p1)
ggsave("crime_rates_trends.png", plot = p1, width = 14, height = 8, dpi = 300, bg = "white")

# --- VISUALIZATION 2: Faceted Plot by Crime Type ---
p2 <- crime_data %>%
  ggplot(aes(x = year, y = rate_per_100k, fill = crime_type)) +
  geom_area(alpha = 0.8, color = NA) +
  facet_wrap(~ crime_type, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = rep("#FDB913", 7)) +
  theme_custom() +
  theme(
    strip.text = element_text(size = 11, face = "bold", color = "#1a1a1a"),
    strip.background = element_rect(fill = "#f0f0f0", color = "#cccccc")
  ) +
  labs(
    title = "Crime Rates in Puerto Rico by Type (2000-2025)",
    subtitle = "Rates per 100,000 inhabitants",
    x = "Year",
    y = "Rate per 100,000 inhabitants",
    fill = "Crime Type"
  )

print(p2)
ggsave("crime_rates_faceted.png", plot = p2, width = 14, height = 10, dpi = 300, bg = "white")

# --- VISUALIZATION 3: Total Crime Rate Over Time ---
p3 <- crime_data %>%
  group_by(year, population) %>%
  summarise(total_count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
  mutate(total_rate = (total_count / population) * 100000) %>%
  ggplot(aes(x = year, y = total_rate)) +
  geom_col(fill = "#FDB913", color = NA, alpha = 0.9) +
  geom_text(aes(label = round(total_rate, 0)), vjust = -0.5, size = 4, color = "#1a1a1a", family = "Arial", fontface = "bold") +
  theme_custom() +
  theme(
    axis.line.x = element_line(color = "#cccccc", linewidth = 0.5),
    axis.line.y = element_line(color = "#cccccc", linewidth = 0.5)
  ) +
  labs(
    title = "Total Crime Rate in Puerto Rico (2000-2025)",
    subtitle = "All crime types combined, per 100,000 inhabitants",
    x = "Year",
    y = "Rate per 100,000 inhabitants"
  )

print(p3)
ggsave("total_crime_rate.png", plot = p3, width = 14, height = 8, dpi = 300, bg = "white")

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
  ggplot(aes(x = reorder(crime_type, change), y = change)) +
  geom_col(aes(fill = ifelse(change > 0, "#F5A623", "#FDB913")), color = NA, alpha = 0.9) +
  geom_text(aes(label = paste0(round(change, 1), "\n(", round(pct_change, 0), "%)")), 
            hjust = -0.1, size = 4, color = "#1a1a1a", family = "Arial", fontface = "bold") +
  coord_flip() +
  scale_fill_identity() +
  theme_custom() +
  theme(
    axis.text.y = element_text(color = "#1a1a1a"),
    axis.line.x = element_line(color = "#cccccc", linewidth = 0.5)
  ) +
  labs(
    title = "Crime Rate Changes (2000 vs 2025)",
    subtitle = "Difference in rates per 100,000 inhabitants (percentage change shown)",
    x = "Crime Type",
    y = "Change in Rate (per 100,000)"
  )

print(p4)
ggsave("crime_rate_changes.png", plot = p4, width = 12, height = 8, dpi = 300, bg = "white")

# Print comparison table
cat("\n=== CRIME RATE COMPARISON (2000 vs 2025) ===\n")
print(start_end_comparison, n = Inf)
