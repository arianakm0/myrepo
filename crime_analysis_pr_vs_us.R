# Crime Analysis Script: Puerto Rico vs United States (2000-2025)
# Analyzes crime trends with rates per 100,000 inhabitants
# With professional styling (golden palette + clean typography)

library(tidyverse)
library(ggplot2)
library(readr)

# Load data
crimes_pr <- read_csv("delitos_tipo_1_pr_annual_2000_2025.csv")
population_pr <- read_csv("pr_population_annual_2000_2025.csv")

crimes_us <- read_csv("delitos_tipo_1_us_annual_2000_2025.csv")
population_us <- read_csv("us_population_annual_2000_2025.csv")

# Clean and prepare PR data
crimes_pr_clean <- crimes_pr %>%
  select(year, tipo_delito, count) %>%
  rename(crime_type = tipo_delito) %>%
  mutate(geo = "Puerto Rico")

population_pr_clean <- population_pr %>%
  select(year, population) %>%
  mutate(geo = "Puerto Rico")

# Clean and prepare US data
crimes_us_clean <- crimes_us %>%
  select(year, tipo_delito, count) %>%
  rename(crime_type = tipo_delito) %>%
  mutate(geo = "United States")

population_us_clean <- population_us %>%
  select(year, population) %>%
  mutate(geo = "United States")

# Combine datasets
crimes_all <- bind_rows(crimes_pr_clean, crimes_us_clean)
population_all <- bind_rows(population_pr_clean, population_us_clean)

# Merge and calculate rates
crime_data <- crimes_all %>%
  left_join(population_all, by = c("year", "geo")) %>%
  mutate(rate_per_100k = (count / population) * 100000)

# View first few rows
head(crime_data, 20)

# Summary statistics by jurisdiction
crime_data %>%
  group_by(geo, crime_type) %>%
  summarise(
    mean_count = mean(count, na.rm = TRUE),
    mean_rate = mean(rate_per_100k, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(geo, desc(mean_rate))

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

# Color palette (golden/orange tones for PR, muted grays for US)
color_pr <- "#FDB913"
color_us <- "#999999"

# --- VISUALIZATION 1: Total Crime Rates - PR vs US ---
p1 <- crime_data %>%
  group_by(year, geo, population) %>%
  summarise(total_count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
  mutate(total_rate = (total_count / population) * 100000) %>%
  ggplot(aes(x = year, y = total_rate, color = geo, linetype = geo)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Puerto Rico" = color_pr, "United States" = color_us)) +
  scale_linetype_manual(values = c("Puerto Rico" = "solid", "United States" = "dashed")) +
  theme_custom() +
  labs(
    title = "Total Crime Rate Comparison: Puerto Rico vs United States (2000-2025)",
    subtitle = "All crime types combined, per 100,000 inhabitants",
    x = "Year",
    y = "Rate per 100,000 inhabitants",
    color = "Jurisdiction",
    linetype = "Jurisdiction"
  )

print(p1)
ggsave("01_total_crime_comparison.png", plot = p1, width = 14, height = 8, dpi = 300, bg = "white")

# --- VISUALIZATION 2: Crime Types Comparison (Faceted) ---
p2 <- crime_data %>%
  ggplot(aes(x = year, y = rate_per_100k, color = geo, linetype = geo)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ crime_type, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("Puerto Rico" = color_pr, "United States" = color_us)) +
  scale_linetype_manual(values = c("Puerto Rico" = "solid", "United States" = "dashed")) +
  theme_custom() +
  theme(
    strip.text = element_text(size = 11, face = "bold", color = "#1a1a1a"),
    strip.background = element_rect(fill = "#f0f0f0", color = "#cccccc")
  ) +
  labs(
    title = "Crime Rates by Type: Puerto Rico vs United States (2000-2025)",
    subtitle = "Rates per 100,000 inhabitants",
    x = "Year",
    y = "Rate per 100,000 inhabitants",
    color = "Jurisdiction",
    linetype = "Jurisdiction"
  )

print(p2)
ggsave("02_crime_types_comparison.png", plot = p2, width = 16, height = 12, dpi = 300, bg = "white")

# --- VISUALIZATION 3: Rate Ratio (PR/US) Over Time ---
ratio_data <- crime_data %>%
  select(year, crime_type, rate_per_100k, geo) %>%
  pivot_wider(names_from = geo, values_from = rate_per_100k) %>%
  mutate(ratio = `Puerto Rico` / `United States`)

p3 <- ratio_data %>%
  ggplot(aes(x = year, y = ratio, color = crime_type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "red", linewidth = 1) +
  scale_color_manual(values = c(
    "#FDB913", "#F5A623", "#E89B3C", "#D68910", 
    "#C17817", "#A96B2F", "#8B5A2B"
  )) +
  theme_custom() +
  labs(
    title = "Crime Rate Ratio: Puerto Rico vs United States (2000-2025)",
    subtitle = "Values > 1.0 indicate PR has higher rates than US; Red line = parity",
    x = "Year",
    y = "Ratio (PR Rate / US Rate)",
    color = "Crime Type"
  )

print(p3)
ggsave("03_rate_ratio_comparison.png", plot = p3, width = 14, height = 8, dpi = 300, bg = "white")

# --- VISUALIZATION 4: 2000 vs 2025 Comparison by Crime Type ---
comparison_data <- crime_data %>%
  filter(year %in% c(2000, 2025)) %>%
  select(year, geo, crime_type, rate_per_100k) %>%
  pivot_wider(names_from = year, values_from = rate_per_100k) %>%
  rename(year_2000 = `2000`, year_2025 = `2025`) %>%
  mutate(
    change = year_2025 - year_2000,
    pct_change = ((year_2025 - year_2000) / year_2000) * 100
  )

p4 <- comparison_data %>%
  ggplot(aes(x = reorder(crime_type, change), y = change, fill = geo)) +
  geom_col(position = "dodge", alpha = 0.85) +
  coord_flip() +
  scale_fill_manual(values = c("Puerto Rico" = color_pr, "United States" = color_us)) +
  theme_custom() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    title = "Crime Rate Changes (2000 vs 2025): Puerto Rico vs United States",
    subtitle = "Difference in rates per 100,000 inhabitants",
    x = "Crime Type",
    y = "Change in Rate (per 100,000)",
    fill = "Jurisdiction"
  )

print(p4)
ggsave("04_rate_changes_comparison.png", plot = p4, width = 12, height = 8, dpi = 300, bg = "white")

# --- VISUALIZATION 5: Puerto Rico Trends Only (Original Analysis) ---
crime_data_pr <- crime_data %>% filter(geo == "Puerto Rico")

p5 <- crime_data_pr %>%
  ggplot(aes(x = year, y = rate_per_100k, color = crime_type, linetype = crime_type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    "#FDB913", "#F5A623", "#E89B3C", "#D68910", 
    "#C17817", "#A96B2F", "#8B5A2B"
  )) +
  scale_linetype_manual(values = rep("solid", 7)) +
  theme_custom() +
  labs(
    title = "Puerto Rico Crime Rates (2000-2025)",
    subtitle = "Annual rates per 100,000 inhabitants by crime type",
    x = "Year",
    y = "Rate per 100,000 inhabitants",
    color = "Crime Type",
    linetype = "Crime Type"
  )

print(p5)
ggsave("05_pr_crime_trends.png", plot = p5, width = 14, height = 8, dpi = 300, bg = "white")

# --- VISUALIZATION 6: United States Trends Only ---
crime_data_us <- crime_data %>% filter(geo == "United States")

p6 <- crime_data_us %>%
  ggplot(aes(x = year, y = rate_per_100k, color = crime_type, linetype = crime_type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    "#999999", "#888888", "#777777", "#666666", 
    "#555555", "#444444", "#333333"
  )) +
  scale_linetype_manual(values = rep("solid", 7)) +
  theme_custom() +
  labs(
    title = "United States Crime Rates (2000-2025)",
    subtitle = "Annual rates per 100,000 inhabitants by crime type",
    x = "Year",
    y = "Rate per 100,000 inhabitants",
    color = "Crime Type",
    linetype = "Crime Type"
  )

print(p6)
ggsave("06_us_crime_trends.png", plot = p6, width = 14, height = 8, dpi = 300, bg = "white")

# --- VISUALIZATION 7: Homicide Rates Deep Dive ---
p7 <- crime_data %>%
  filter(crime_type == "Asesinato/Homicidio") %>%
  ggplot(aes(x = year, y = rate_per_100k, fill = geo)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_text(aes(label = round(rate_per_100k, 1)), position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3, color = "#1a1a1a", family = "Arial", fontface = "bold") +
  scale_fill_manual(values = c("Puerto Rico" = color_pr, "United States" = color_us)) +
  theme_custom() +
  labs(
    title = "Homicide Rates: Puerto Rico vs United States (2000-2025)",
    subtitle = "Per 100,000 inhabitants",
    x = "Year",
    y = "Rate per 100,000 inhabitants",
    fill = "Jurisdiction"
  )

print(p7)
ggsave("07_homicide_comparison.png", plot = p7, width = 14, height = 8, dpi = 300, bg = "white")

# --- Print Summary Statistics ---
cat("\n========== SUMMARY STATISTICS ==========\n")
cat("\n--- PUERTO RICO (Mean Rates by Crime Type) ---\n")
crime_data %>%
  filter(geo == "Puerto Rico") %>%
  group_by(crime_type) %>%
  summarise(
    mean_rate = mean(rate_per_100k, na.rm = TRUE),
    min_rate = min(rate_per_100k, na.rm = TRUE),
    max_rate = max(rate_per_100k, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_rate)) %>%
  print(n = Inf)

cat("\n--- UNITED STATES (Mean Rates by Crime Type) ---\n")
crime_data %>%
  filter(geo == "United States") %>%
  group_by(crime_type) %>%
  summarise(
    mean_rate = mean(rate_per_100k, na.rm = TRUE),
    min_rate = min(rate_per_100k, na.rm = TRUE),
    max_rate = max(rate_per_100k, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_rate)) %>%
  print(n = Inf)

cat("\n--- RATE RATIOS: Puerto Rico / US (2000 vs 2025) ---\n")
print(comparison_data, n = Inf)
