# ============================================================
# PURPOSE: Perform exploratory and statistical analysis on the
#          cleaned Bellabeat 2016 dataset
# AUTHOR:  Onyedikachi Ikuru
# DATE:    12-01-2025
# ============================================================

# Load libraries
library(vroom)
library(dplyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(here)
library(tidyr)
library(readr)

# create output directory
dir.create(here("data", "output"), recursive = TRUE, showWarnings = FALSE)

file_paths <- list.files(
  here("data", "cleaned"),
  pattern = "\\.csv$",
  full.names = TRUE
)

cleaned_bellabeat_files <- map(file_paths, ~ vroom(.x, progress = FALSE))

# Assign names to list
names(cleaned_bellabeat_files) <- basename(file_paths)

# -------------------------------------------------------------------------
# Helper function to find dataset by name pattern
# -------------------------------------------------------------------------
find_dataset <- function(pattern) {
  idx <- grep(pattern, names(cleaned_bellabeat_files), ignore.case = TRUE)
  if (length(idx) == 0) {
    return(NULL)
  }
  cleaned_bellabeat_files[[idx[1]]]
}

# -------------------------------------------------------------------------
# Load Cleaned Data
# -------------------------------------------------------------------------
daily_activities <- find_dataset("daily_activities_cleaned")
daily_calories <- find_dataset("daily_calories_cleaned")
daily_intensities <- find_dataset("daily_intensities_cleaned")
daily_steps <- find_dataset("daily_steps_cleaned")
sleep_day <- find_dataset("sleep_day_cleaned")
weight_log <- find_dataset("weight_log_cleaned")
hourly_calories <- find_dataset("hourly_calories_cleaned")
hourly_intensities <- find_dataset("hourly_intensities_cleaned")
hourly_steps <- find_dataset("hourly_steps_cleaned")
heartrate_seconds <- find_dataset("heartrate_seconds_cleaned")

# Purpose: Data Coverage & Quality Analysis for FitBit Dataset ------------

# -------------------------------------------------------------------------
# A. USERS PER DATASET
# -------------------------------------------------------------------------
message("\n=== USERS PER DATASET ===\n")

users_per_dataset <- data.frame(
  Dataset = c(
    "Daily Activities",
    "Daily Calories",
    "Daily Intensities",
    "Daily Steps",
    "Sleep Day",
    "Weight Log",
    "Hourly Calories",
    "Hourly Intensities",
    "Hourly Steps",
    "Heartrate Seconds"
  ),
  Users = c(
    n_distinct(daily_activities$id),
    n_distinct(daily_calories$id),
    n_distinct(daily_intensities$id),
    n_distinct(daily_steps$id),
    n_distinct(sleep_day$id),
    n_distinct(weight_log$id),
    n_distinct(hourly_calories$id),
    n_distinct(hourly_intensities$id),
    n_distinct(hourly_steps$id),
    n_distinct(heartrate_seconds$id)
  ),
  Records = c(
    nrow(daily_activities),
    nrow(daily_calories),
    nrow(daily_intensities),
    nrow(daily_steps),
    nrow(sleep_day),
    nrow(weight_log),
    nrow(hourly_calories),
    nrow(hourly_intensities),
    nrow(hourly_steps),
    nrow(heartrate_seconds)
  )
)

print(users_per_dataset)

# -------------------------------------------------------------------------
# B. DAYS TRACKED PER USER
# -------------------------------------------------------------------------
message("\n=== DAYS TRACKED PER USER ===\n")

user_tracking_days <- daily_activities %>%
  group_by(id) %>%
  summarize(
    days_tracked = n(),
    first_day = min(activity_day),
    last_day = max(activity_day),
    date_range_days = as.numeric(
      difftime(last_day, first_day, units = "days")
    ) + 1,
    .groups = "drop"
  ) %>%
  arrange(desc(days_tracked))

# Summary statistics
tracking_summary <- user_tracking_days %>%
  summarize(
    min_days = min(days_tracked),
    max_days = max(days_tracked),
    mean_days = mean(days_tracked),
    median_days = median(days_tracked)
  )

message("Tracking Days Summary:")
print(tracking_summary)

# User engagement classification
user_engagement <- user_tracking_days %>%
  mutate(
    engagement_level = case_when(
      days_tracked >= 25 ~ "Everyday User (25-31 days)",
      days_tracked >= 21 ~ "Heavy User (21-24 days)",
      days_tracked >= 11 ~ "Moderate User (11-20 days)",
      TRUE ~ "Light User (1-10 days)"
    )
  )

engagement_distribution <- user_engagement %>%
  count(engagement_level) %>%
  arrange(desc(n))

message("\nUser Engagement Distribution:")
print(engagement_distribution)

message("\nDetailed User Tracking:")
print(user_engagement, n = Inf)

# -------------------------------------------------------------------------
# C. FEATURE UTILIZATION - WHO HAS WHAT DATA?
# -------------------------------------------------------------------------
message("\n=== FEATURE UTILIZATION ===\n")

# Get all unique users from daily activities (baseline)
all_users <- unique(daily_activities$id)

# Create feature utilization matrix
feature_matrix <- data.frame(id = all_users) %>%
  mutate(
    has_activity = id %in% daily_activities$id,
    has_sleep = id %in% sleep_day$id,
    has_weight = id %in% weight_log$id,
    has_heartrate = id %in% heartrate_seconds$id
  )

# Summary of feature adoption
feature_summary <- feature_matrix %>%
  summarize(
    total_users = n(),
    activity_users = sum(has_activity),
    sleep_users = sum(has_sleep),
    weight_users = sum(has_weight),
    heartrate_users = sum(has_heartrate),
    activity_pct = round(100 * activity_users / total_users, 1),
    sleep_pct = round(100 * sleep_users / total_users, 1),
    weight_pct = round(100 * weight_users / total_users, 1),
    heartrate_pct = round(100 * heartrate_users / total_users, 1)
  )

message("Feature Adoption Summary:")
print(feature_summary)

# List users by feature
message("\n--- Users WITH Sleep Data ---")
sleep_users <- feature_matrix %>%
  filter(has_sleep) %>%
  pull(id)
message(paste("Count:", length(sleep_users)))
print(sleep_users)

message("\n--- Users WITH Heart Rate Data ---")
hr_users <- feature_matrix %>%
  filter(has_heartrate) %>%
  pull(id)
message(paste("Count:", length(hr_users)))
print(hr_users)

message("\n--- Users WITH Weight Logs ---")
weight_users <- feature_matrix %>%
  filter(has_weight) %>%
  pull(id)
message(paste("Count:", length(weight_users)))
print(weight_users)

message("\n--- Users WITHOUT Sleep Data ---")
no_sleep_users <- feature_matrix %>%
  filter(!has_sleep) %>%
  pull(id)
message(paste("Count:", length(no_sleep_users)))
print(no_sleep_users)

# -------------------------------------------------------------------------
# D. TRACKING FREQUENCY FOR EACH FEATURE
# -------------------------------------------------------------------------
message("\n=== TRACKING FREQUENCY ===\n")

# Sleep tracking frequency
if (nrow(sleep_day) > 0) {
  sleep_frequency <- sleep_day %>%
    group_by(id) %>%
    summarize(
      sleep_records = n(),
      first_sleep = min(sleep_day),
      last_sleep = max(sleep_day),
      date_range = as.numeric(
        difftime(last_sleep, first_sleep, units = "days")
      ) + 1,
      .groups = "drop"
    ) %>%
    mutate(sleep_tracking_rate = round(100 * sleep_records / date_range, 1)) %>%
    arrange(desc(sleep_records))

  message("Sleep Tracking Frequency:")
  print(sleep_frequency, n = Inf)
}

# Weight logging frequency
if (nrow(weight_log) > 0) {
  weight_frequency <- weight_log %>%
    group_by(id) %>%
    summarize(
      weight_logs = n(),
      manual_logs = sum(is_manual_report, na.rm = TRUE),
      auto_logs = sum(!is_manual_report, na.rm = TRUE),
      first_log = min(date),
      last_log = max(date),
      .groups = "drop"
    ) %>%
    arrange(desc(weight_logs))

  message("\nWeight Logging Frequency:")
  print(weight_frequency, n = Inf)
}

# Heart rate tracking days
if (nrow(heartrate_seconds) > 0) {
  hr_frequency <- heartrate_seconds %>%
    mutate(date = as.Date(time)) %>%
    group_by(id) %>%
    summarize(
      hr_records = n(),
      days_with_hr = n_distinct(date),
      first_hr = min(date),
      last_hr = max(date),
      .groups = "drop"
    ) %>%
    arrange(desc(days_with_hr))

  message("\nHeart Rate Tracking Frequency:")
  print(hr_frequency, n = Inf)
}

# -------------------------------------------------------------------------
# E. DATA QUALITY ISSUES
# -------------------------------------------------------------------------
message("\n=== DATA QUALITY CHECKS ===\n")

# Non-wear days in daily activities
non_wear_summary <- daily_activities %>%
  group_by(id) %>%
  summarize(
    total_days = n(),
    non_wear_days = sum(non_wear),
    non_wear_pct = round(100 * non_wear_days / total_days, 1),
    .groups = "drop"
  ) %>%
  filter(non_wear_days > 0) %>%
  arrange(desc(non_wear_pct))

if (nrow(non_wear_summary) > 0) {
  message("Users with Non-Wear Days:")
  print(non_wear_summary, n = Inf)
} else {
  message("No non-wear days detected (all users wore device every tracked day)")
}

# -------------------------------------------------------------------------
# F. USERS TO EXCLUDE (INSUFFICIENT DATA)
# -------------------------------------------------------------------------
message("\n=== RECOMMENDED EXCLUSIONS ===\n")

# Users with <7 days of tracking
insufficient_users <- user_engagement %>%
  filter(days_tracked < 7)

if (nrow(insufficient_users) > 0) {
  message("Users with <7 days of tracking (consider excluding):")
  print(insufficient_users)
} else {
  message("All users have ≥7 days of tracking")
}

# -------------------------------------------------------------------------
# G. SAVE OUTPUTS
# -------------------------------------------------------------------------
dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)

write_csv(
  users_per_dataset,
  here("data", "processed", "users_per_dataset.csv")
)
write_csv(
  user_engagement,
  here("data", "processed", "user_engagement.csv")
)
write_csv(
  feature_matrix,
  here("data", "processed", "feature_utilization_matrix.csv")
)
write_csv(
  feature_summary,
  here("data", "processed", "feature_summary.csv")
)

if (exists("sleep_frequency")) {
  write_csv(
    sleep_frequency,
    here("data", "processed", "sleep_tracking_frequency.csv")
  )
}

if (exists("weight_frequency")) {
  write_csv(
    weight_frequency,
    here("data", "processed", "weight_logging_frequency.csv")
  )
}

if (exists("hr_frequency")) {
  write_csv(
    hr_frequency,
    here("data", "processed", "heartrate_tracking_frequency.csv")
  )
}

message("\n=== DATA COVERAGE ANALYSIS COMPLETE ===")
message("Processed files saved to: data/processed/")
