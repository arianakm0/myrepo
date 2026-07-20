# Crime Analysis Script: Puerto Rico vs United States (2000-2025)
# Compares crime trends with rates per 100,000 inhabitants
# Professional styling with golden palette + clean typography

library(tidyverse)
library(ggplot2)
library(readr)

# ===== LOAD DATA =====
crimes_pr <- read_csv("delitos_tipo_1_pr_annual_2000_2025.csv")
population_pr <- read_csv("pr_population_annual_2000_2025.csv")

crimes_us <- read_csv("delitos_tipo_1_us_annual_2000_2025.csv")
population_us <- read_csv("us_population_annual_2000_2025.csv")

# ===== CLEAN AND PREPARE DATA =====
crimes_pr_clean <- crimes_pr %>%
  select(year, tipo_delito, count) %>%
  rename(crime_type = tipo_delito) %>%
  mutate(region = "Puerto Rico")

crimes_us_clean <- crimes_us %>%
  select(year, tipo_delito, count) %>%
  rename(crime_type = tipo_delito) %>%
  mutate(region = "United States")

population_pr_clean <- population_pr %>%
  select(year, population) %>%
  mutate(region = "Puerto Rico")

population_us_clean <- population_us %>%
  select(year, population) %>%
  mutate(region = "United States")

# ===== MERGE DATASETS =====
crime_data_pr <- crimes_pr_clean %>%
  left_join(population_pr_clean, by = "year") %>%
  mutate(rate_per_100k = (count / population) * 100000)

crime_data_us <- crimes_us_clean %>%
  left_join(population_us_clean, by = "year") %>%
  mutate(rate_per_100k = (count / population) * 100000)

# Combine PR and US data
crime_data_combined <- bind_rows(crime_data_pr, crime_data_us)

# ===== SUMMARY STATISTICS =====
cat("\n=== CRIME RATE SUMMARY BY REGION ===\n")
crime_data_combined %>%
  group_by(region, crime_type) %>%
  summarise(
    mean_rate = mean(rate_per_100k, na.rm = TRUE),
    min_rate = min(rate_per_100k, na.rm = TRUE),
    max_rate = max(rate_per_100k, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(crime_type, desc(mean_rate)) %>%
  print(n = Inf)

# ===== CUSTOM THEME =====
theme_custom <- function() {
  theme_minimal() +
  theme(
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

# ===== COLOR PALETTE =====
golden_palette <- c(
  "#FDB913",  # Bright golden
  "#F5A623",  # Golden orange
  "#E89B3C",  # Medium golden
  "#D68910",  # Dark golden
  "#C17817",  # Burnt orange
  "#A96B2F",  # Darker burnt
  "#8B5A2B"   # Saddle brown
)

region_colors <- c("Puerto Rico" = "#FDB913", "United States" = "#E89B3C")

# ===== VIZ 1: TOTAL CRIME RATE COMPARISON (PR vs US) =====
p1 <- crime_data_combined %>%
  group_by(year, region, population) %>%
  summarise(total_count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
  mutate(total_rate = (total_count / population) * 100000) %>%
  ggplot(aes(x = year, y = total_rate, color = region, linetype = region)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.5) +
  scale_color_manual(values = region_colors) +
  scale_linetype_manual(values = c("Puerto Rico" = "solid", "United States" = "dashed")) +
  theme_custom() +
  labs(
    title = "Total Crime Rate Comparison: Puerto Rico vs United States",
    subtitle = "All crime types combined, per 100,000 inhabitants (2000-2025)",
    x = "Year",
    y = "Rate per 100,000 inhabitants",
    color = "Region",
    linetype = "Region"
  )

print(p1)
ggsave("01_total_crime_comparison.png", plot = p1, width = 14, height = 8, dpi = 300, bg = "white")

# ===== VIZ 2: CRIME TYPE COMPARISON (FACETED BY CRIME TYPE) =====
p2 <- crime_data_combined %>%
  ggplot(aes(x = year, y = rate_per_100k, color = region, linetype = region)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ crime_type, scales = "free_y", ncol = 2) +
  scale_color_manual(values = region_colors) +
  scale_linetype_manual(values = c("Puerto Rico" = "solid", "United States" = "dashed")) +
  theme_custom() +
  theme(
    strip.text = element_text(size = 11, face = "bold", color = "#1a1a1a"),
    strip.background = element_rect(fill = "#f0f0f0", color = "#cccccc")
  ) +
  labs(
    title = "Crime Rate Trends by Type: Puerto Rico vs United States",
    subtitle = "Per 100,000 inhabitants (2000-2025)",
    x = "Year",
    y = "Rate per 100,000 inhabitants",
    color = "Region",
    linetype = "Region"
  )

print(p2)
ggsave("02_crime_type_comparison_faceted.png", plot = p2, width = 16, height = 12, dpi = 300, bg = "white")

# ===== VIZ 3: SIDE-BY-SIDE BAR CHART (2000 vs 2025) =====
comparison_2000_2025 <- crime_data_combined %>%
  filter(year %in% c(2000, 2025)) %>%
  select(year, crime_type, rate_per_100k, region) %>%
  pivot_wider(names_from = year, values_from = rate_per_100k, values_fill = 0) %>%
  rename(rate_2000 = `2000`, rate_2025 = `2025`) %>%
  mutate(change = rate_2025 - rate_2000,
         pct_change = ((rate_2025 - rate_2000) / rate_2000) * 100)

p3 <- comparison_2000_2025 %>%
  pivot_longer(cols = c(rate_2000, rate_2025), names_to = "year_period", values_to = "rate") %>%
  mutate(year_period = ifelse(year_period == "rate_2000", "2000", "2025")) %>%
  ggplot(aes(x = crime_type, y = rate, fill = interaction(region, year_period))) +
  geom_col(position = "dodge", alpha = 0.9, color = NA) +
  scale_fill_manual(values = c(
    "Puerto Rico.2000" = "#FDB913",
    "Puerto Rico.2025" = "#D68910",
    "United States.2000" = "#E89B3C",
    "United States.2025" = "#8B5A2B"
  ), labels = c(
    "Puerto Rico.2000" = "PR 2000",
    "Puerto Rico.2025" = "PR 2025",
    "United States.2000" = "US 2000",
    "United States.2025" = "US 2025"
  )) +
  coord_flip() +
  theme_custom() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  labs(
    title = "Crime Rate Comparison: 2000 vs 2025",
    subtitle = "Puerto Rico vs United States, per 100,000 inhabitants",
    x = "Crime Type",
    y = "Rate per 100,000 inhabitants",
    fill = "Region & Year"
  )

print(p3)
ggsave("03_2000_vs_2025_comparison.png", plot = p3, width = 14, height = 9, dpi = 300, bg = "white")

# ===== VIZ 4: PERCENT CHANGE (2000-2025) BY REGION =====
p4 <- comparison_2000_2025 %>%
  ggplot(aes(x = reorder(crime_type, change), y = pct_change, fill = region)) +
  geom_col(position = "dodge", alpha = 0.9, color = NA) +
  scale_fill_manual(values = region_colors) +
  geom_hline(yintercept = 0, color = "#333333", linewidth = 0.5) +
  coord_flip() +
  theme_custom() +
  labs(
    title = "Percent Change in Crime Rates (2000-2025)",
    subtitle = "Puerto Rico vs United States",
    x = "Crime Type",
    y = "Percent Change (%)",
    fill = "Region"
  )

print(p4)
ggsave("04_percent_change_comparison.png", plot = p4, width = 14, height = 9, dpi = 300, bg = "white")

# ===== VIZ 5: RATE RATIO (PR to US) OVER TIME =====
rate_ratio <- crime_data_combined %>%
  select(year, crime_type, rate_per_100k, region) %>%
  pivot_wider(names_from = region, values_from = rate_per_100k) %>%
  mutate(pr_to_us_ratio = `Puerto Rico` / `United States`)

p5 <- rate_ratio %>%
  ggplot(aes(x = year, y = pr_to_us_ratio, color = crime_type)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(values = golden_palette) +
  geom_hline(yintercept = 1, color = "#ff0000", linetype = "dashed", linewidth = 0.8, alpha = 0.6) +
  theme_custom() +
  labs(
    title = "Crime Rate Ratio: Puerto Rico to United States",
    subtitle = "Red line = parity (ratio of 1.0). Above = PR rates higher than US",
    x = "Year",
    y = "PR Rate / US Rate",
    color = "Crime Type"
  ) +
  annotate("text", x = 2020, y = 1.05, label = "Parity", size = 3.5, color = "#ff0000")

print(p5)
ggsave("05_rate_ratio_pr_to_us.png", plot = p5, width = 14, height = 8, dpi = 300, bg = "white")

# ===== VIZ 6: MURDER/HOMICIDE DEEP DIVE =====
homicide_data <- crime_data_combined %>%
  filter(crime_type == "Asesinato/Homicidio")

p6 <- homicide_data %>%
  ggplot(aes(x = year, y = rate_per_100k, color = region, linetype = region)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = region_colors) +
  scale_linetype_manual(values = c("Puerto Rico" = "solid", "United States" = "dashed")) +
  theme_custom() +
  labs(
    title = "Murder/Homicide Rates: Puerto Rico vs United States",
    subtitle = "Per 100,000 inhabitants (2000-2025)",
    x = "Year",
    y = "Rate per 100,000 inhabitants",
    color = "Region",
    linetype = "Region"
  )

print(p6)
ggsave("06_homicide_deep_dive.png", plot = p6, width = 14, height = 8, dpi = 300, bg = "white")

# ===== DETAILED COMPARISON TABLE =====
cat("\n=== 2000 vs 2025 CRIME RATE COMPARISON ===\n")
print(comparison_2000_2025, n = Inf)

cat("\n=== CRIME RATE RATIO (PR / US) ===\n")
cat("Values > 1.0 indicate PR rates are higher than US rates\n\n")
print(rate_ratio %>% select(year, crime_type, pr_to_us_ratio), n = Inf)
