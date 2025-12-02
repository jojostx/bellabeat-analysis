# visualization_full.R
# ============================================================
# PURPOSE: Load analysis.R outputs and produce simple plots
# AUTHOR:  Generated for Onyedikachi Ikuru
# DATE:    2025-11-17
# ============================================================

library(dplyr)
library(ggplot2)
library(forcats)
library(here)
library(readr)
library(tidyr)
library(scales)

# ensure output folder
dir.create(here("plots"), showWarnings = FALSE)

# factor levels
duration_levels <- c("<5 min", "5–10 min", "10–15 min", "15–30 min", "30–60 min", "1–2 hrs", ">2 hrs")
weekday_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
member_levels <- c("member", "casual")
month_levels <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# ---------------------------
# 1. bike_behavior_by_member.csv
# ---------------------------
bike_behavior <- read_csv(here("data", "output", "bike_behavior_by_member.csv"), show_col_types = FALSE) %>%
  mutate(member_casual = factor(member_casual, levels = member_levels))

# Plot: grouped bar (bike type usage) faceted by member type
p_bike <- ggplot(bike_behavior, aes(x = rideable_type, y = total_rides, fill = rideable_type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~member_casual, scales = "free_y") +
  labs(title = "Bike type usage by member type", x = "Bike type", y = "Total rides") +
  theme_minimal()

ggsave(here("plots/bike_behavior_by_member.png"), p_bike, width = 9, height = 5, dpi = 320)

# ---------------------------
# 2. daily_by_member.csv
# ---------------------------
daily_by_member <- read_csv(here("data", "output", "daily_by_member.csv"), show_col_types = FALSE) %>%
  mutate(
    member_casual = factor(member_casual, levels = member_levels),
    start_day_of_week = factor(start_day_of_week, levels = weekday_levels)
  )

# Plot: simple bar (total rides by weekday) faceted by member
p_daily <- ggplot(daily_by_member, aes(x = start_day_of_week, y = total_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Rides by weekday (member vs casual)", x = "Weekday", y = "Total rides", fill = "Member type") +
  theme_minimal()

ggsave(here("plots/daily_rides_by_member.png"), p_daily, width = 9, height = 5, dpi = 320)

# ---------------------------
# 3. duration_summary_by_member.csv
# ---------------------------
duration_summary <- read_csv(here("data", "output", "duration_summary_by_member.csv"), show_col_types = FALSE) %>%
  mutate(
    duration_bucket = factor(duration_bucket, levels = duration_levels, ordered = TRUE),
    member_casual = factor(member_casual, levels = member_levels)
  )

# Plot: stacked bar of counts by duration bucket, grouped by member
p_duration <- ggplot(duration_summary, aes(x = duration_bucket, y = total_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Ride duration distribution by member type", x = "Duration bucket", y = "Total rides", fill = "Member type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("plots/duration_summary_by_member.png"), p_duration, width = 10, height = 5, dpi = 320)

# ---------------------------
# 4. duration_time_by_member.csv
# ---------------------------
duration_time_member <- read_csv(here("data", "output", "duration_time_by_member.csv"), show_col_types = FALSE) %>%
  mutate(
    duration_bucket = factor(duration_bucket, levels = duration_levels, ordered = TRUE),
    member_casual = factor(member_casual, levels = member_levels),
    start_hour = as.integer(start_hour),
    start_day_of_week = factor(start_day_of_week, levels = weekday_levels)
  )

# Summarize per hour & member (sum across duration buckets) for simple hour plot
hourly_agg <- duration_time_member %>%
  group_by(member_casual, start_hour) %>%
  summarise(total_rides = sum(total_rides), .groups = "drop")

p_duration_hour <- ggplot(hourly_agg, aes(x = start_hour, y = total_rides, color = member_casual)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1) +
  labs(title = "Hourly rides (all durations) by member type", x = "Hour", y = "Total rides", color = "Member type") +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  theme_minimal()

ggsave(here("plots/hourly_rides_all_durations_by_member.png"), p_duration_hour, width = 9, height = 5, dpi = 320)

# ---------------------------
# 5. hourly_by_member.csv
# ---------------------------
hourly_by_member <- read_csv(here("data", "output", "hourly_by_member.csv"), show_col_types = FALSE) %>%
  mutate(member_casual = factor(member_casual, levels = member_levels), start_hour = as.integer(start_hour))

# Plot: line chart (hourly total rides) - similar to above but uses produced CSV
p_hourly <- ggplot(hourly_by_member, aes(x = start_hour, y = total_rides, color = member_casual)) +
  geom_line() +
  labs(title = "Hourly ride trends by member type", x = "Hour", y = "Total rides", color = "Member type") +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  theme_minimal()

ggsave(here("plots/hourly_by_member.png"), p_hourly, width = 9, height = 5, dpi = 320)

# ---------------------------
# 6. member_stats.csv (avg + 95% CI)
# ---------------------------
member_stats <- read_csv(here("data", "output", "member_stats.csv"), show_col_types = FALSE) %>%
  mutate(member_casual = factor(member_casual, levels = member_levels))

# If ci columns exist use errorbars, else just bar
if (all(c("ci_low", "ci_high") %in% colnames(member_stats))) {
  p_member_stats <- ggplot(member_stats, aes(x = member_casual, y = avg_duration, fill = member_casual)) +
    geom_col(width = 0.5, show.legend = FALSE) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
    labs(title = "Average ride duration with 95% CI", x = "Member type", y = "Average duration (min)") +
    theme_minimal()
} else {
  p_member_stats <- ggplot(member_stats, aes(x = member_casual, y = avg_duration, fill = member_casual)) +
    geom_col(width = 0.5, show.legend = FALSE) +
    labs(title = "Average ride duration by member type", x = "Member type", y = "Average duration (min)") +
    theme_minimal()
}
ggsave(here("plots/member_stats_avg_duration.png"), p_member_stats, width = 7, height = 5, dpi = 320)

# ---------------------------
# 7. monthly_by_member.csv
# ---------------------------
monthly_by_member <- read_csv(here("data", "output", "monthly_by_member.csv"), show_col_types = FALSE) %>%
  mutate(
    member_casual = factor(member_casual, levels = member_levels),
    start_month = factor(start_month, levels = month_levels, ordered = TRUE)
  )

# ensure months with zero are present (complete)
monthly_complete <- monthly_by_member %>%
  complete(member_casual, start_month = month_levels, fill = list(total_rides = 0, avg_duration = NA))

p_monthly <- ggplot(monthly_complete, aes(x = start_month, y = total_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Monthly rides by member type", x = "Month", y = "Total rides", fill = "Member type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("plots/monthly_rides_by_member.png"), p_monthly, width = 10, height = 6, dpi = 320)

# ---------------------------
# 8. period_by_member.csv (AM/PM/Off-peak)
# ---------------------------
period_by_member <- read_csv(here("data", "output", "period_by_member.csv"), show_col_types = FALSE) %>%
  mutate(
    member_casual = factor(member_casual, levels = member_levels),
    period = factor(period, levels = c("AM peak", "PM peak", "Off-peak"))
  )

p_period <- ggplot(period_by_member, aes(x = period, y = share, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = label_percent(accuracy = 1L)) +
  labs(title = "Share of rides by period and member type", x = "Period", y = "Share", fill = "Member type") +
  theme_minimal()

ggsave(here("plots/period_share_by_member.png"), p_period, width = 8, height = 5, dpi = 320)

# ---------------------------
# 9. top_start_with_category.csv and top_end_with_category.csv
# ---------------------------
top_start_with_category <- read_csv(here("data", "output", "top_start_with_category.csv"), show_col_types = FALSE) %>%
  mutate(
    member_casual = factor(member_casual, levels = member_levels),
    category = ifelse(is.na(category) | category == "", "Unknown", category)
  ) %>%
  group_by(member_casual, station_name, category) %>%
  summarise(total_rides = sum(total_rides), .groups = "drop") %>%
  arrange(member_casual, desc(total_rides))

p_top_start <- ggplot(
  top_start_with_category %>%
    filter(station_name != "Not Specified") %>%
    group_by(member_casual) %>%
    slice_max(total_rides, n = 10) %>%
    ungroup() %>%
    mutate(station_name = fct_reorder(station_name, total_rides)),
  aes(x = station_name, y = total_rides, fill = category)
) +
  geom_col() +
  coord_flip() +
  facet_wrap(~member_casual, scales = "free_y") +
  labs(
    title = "Top start stations by member type (top 10 each)",
    x = "Station",
    y = "Total rides",
    fill = "Category"
  ) +
  theme_minimal(base_size = 12)

ggsave(here("plots/top_start_stations_by_member_category_simple.png"), p_top_start, width = 12, height = 6, dpi = 320)

top_end_with_category <- read_csv(here("data", "output", "top_end_with_category.csv"), show_col_types = FALSE) %>%
  mutate(
    member_casual = factor(member_casual, levels = member_levels),
    category = ifelse(is.na(category) | category == "", "Unknown", category)
  ) %>%
  group_by(member_casual, station_name, category) %>%
  summarise(total_rides = sum(total_rides), .groups = "drop") %>%
  arrange(member_casual, desc(total_rides))

p_top_end <- ggplot(
  top_end_with_category %>%
    filter(station_name != "Not Specified") %>%
    group_by(member_casual) %>%
    slice_max(total_rides, n = 10) %>%
    ungroup() %>%
    mutate(station_name = fct_reorder(station_name, total_rides)),
  aes(x = station_name, y = total_rides, fill = category)
) +
  geom_col() +
  coord_flip() +
  facet_wrap(~member_casual, scales = "free_y") +
  labs(
    title = "Top end stations by member type (top 10 each)",
    x = "Station",
    y = "Total rides",
    fill = "Category"
  ) +
  theme_minimal(base_size = 12)

ggsave(here("plots/top_end_stations_by_member_category_simple.png"), p_top_end, width = 12, height = 6, dpi = 320)

# ---------------------------
# 10. weekday_month_index.csv -> simplified stacked bars (share by weekday per month)
# ---------------------------
weekday_month_index <- read_csv(here("data", "output", "weekday_month_index.csv"), show_col_types = FALSE) %>%
  mutate(
    member_casual = factor(member_casual, levels = member_levels),
    start_month = factor(start_month, levels = month_levels, ordered = TRUE),
    start_day_of_week = factor(start_day_of_week, levels = weekday_levels)
  )

# For readability plot only top months present in data (or all months)
weekday_month_complete <- weekday_month_index %>%
  group_by(member_casual, start_month, start_day_of_week) %>%
  summarise(share = sum(share), .groups = "drop")

# Stacked bar: for each month show share by weekday, faceted by member
p_weekday_month <- ggplot(weekday_month_complete, aes(x = start_month, y = share, fill = start_day_of_week)) +
  geom_col(position = "stack") +
  facet_wrap(~member_casual, nrow = 2) +
  labs(title = "Weekday composition of rides within each month (stacked)", x = "Month", y = "Share", fill = "Weekday") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("plots/weekday_month_composition_by_member.png"), p_weekday_month, width = 12, height = 7, dpi = 320)
