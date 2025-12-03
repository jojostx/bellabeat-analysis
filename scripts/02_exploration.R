# exploration.R
# ============================================================
# purpose: exploratory analysis - bellabeat 2016
# author:  Onyedikachi Ikuru
# date:    12-01-2025
# ============================================================

# libraries ---------------------------------------------------
library(vroom)
library(dplyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(here)
library(tidyr)
library(readr)
library(purrr)

# output directory --------------------------------------------
dir.create(
  here("data", "processed"),
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# load cleaned datasets
# ============================================================

file_paths <- list.files(
  here("data", "cleaned"),
  pattern = "\\.csv$",
  full.names = TRUE
)

cleaned_bellabeat_files <- map(
  file_paths,
  ~ vroom(.x, progress = FALSE)
)
names(cleaned_bellabeat_files) <- basename(file_paths)

# helper finder
find_dataset <- function(pattern) {
  match <- grep(
    pattern,
    names(cleaned_bellabeat_files),
    ignore.case = TRUE
  )
  if (length(match) == 0) return(NULL)
  cleaned_bellabeat_files[[match[1]]]
}

# load specific cleaned datasets ------------------------------
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

# ============================================================
# a. users per dataset
# ============================================================

message("\n=== users per dataset ===\n")

users_per_dataset <- tibble(
  Dataset = c(
    "Daily Activities", "Daily Calories", "Daily Intensities",
    "Daily Steps", "Sleep Day", "Weight Log",
    "Hourly Calories", "Hourly Intensities", "Hourly Steps",
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
    nrow(daily_activities), nrow(daily_calories),
    nrow(daily_intensities), nrow(daily_steps),
    nrow(sleep_day), nrow(weight_log),
    nrow(hourly_calories), nrow(hourly_intensities),
    nrow(hourly_steps), nrow(heartrate_seconds)
  )
)

print(users_per_dataset)

# ============================================================
# b. days tracked per user
# ============================================================

message("\n=== days tracked per user ===\n")

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

tracking_summary <- user_tracking_days %>%
  summarize(
    min_days = min(days_tracked),
    max_days = max(days_tracked),
    mean_days = mean(days_tracked),
    median_days = median(days_tracked)
  )

message("tracking days summary:")
print(tracking_summary)

# engagement categories
user_engagement <- user_tracking_days %>%
  mutate(
    engagement_level = case_when(
      days_tracked >= 25 ~ "Everyday User (25-31 days)",
      days_tracked >= 21 ~ "Heavy User (21-24 days)",
      days_tracked >= 11 ~ "Moderate User (11-20 days)",
      TRUE ~ "Light User (1-10 days)"
    )
  )

engagement_distribution <- count(user_engagement, engagement_level) %>%
  arrange(desc(n))

message("\nuser engagement distribution:")
print(engagement_distribution)

message("\ndetailed user tracking:")
print(user_engagement, n = Inf)

# ============================================================
# c. basic distributions
# ============================================================

message("\n=== basic distributions ===\n")

# daily aggregates --------------------------------------------
activity_distributions <- daily_activities %>%
  summarize(
    steps_mean = mean(total_steps, na.rm = TRUE),
    steps_median = median(total_steps, na.rm = TRUE),
    steps_sd = sd(total_steps, na.rm = TRUE),
    steps_min = min(total_steps, na.rm = TRUE),
    steps_max = max(total_steps, na.rm = TRUE),
    calories_mean = mean(calories, na.rm = TRUE),
    calories_median = median(calories, na.rm = TRUE),
    calories_sd = sd(calories, na.rm = TRUE),
    calories_min = min(calories, na.rm = TRUE),
    calories_max = max(calories, na.rm = TRUE),
    very_active_mean = mean(very_active_minutes, na.rm = TRUE),
    fairly_active_mean = mean(fairly_active_minutes, na.rm = TRUE),
    lightly_active_mean = mean(lightly_active_minutes, na.rm = TRUE),
    sedentary_mean = mean(sedentary_minutes, na.rm = TRUE)
  )

message("activity distributions summary:")
print(activity_distributions)

# sleep distributions -----------------------------------------
if (nrow(sleep_day) > 0) {
  sleep_distributions <- sleep_day %>%
    summarize(
      sleep_hours_mean = mean(
        total_minutes_asleep / 60,
        na.rm = TRUE
      ),
      sleep_hours_median = median(
        total_minutes_asleep / 60,
        na.rm = TRUE
      ),
      sleep_hours_sd = sd(
        total_minutes_asleep / 60,
        na.rm = TRUE
      ),
      sleep_hours_min = min(
        total_minutes_asleep / 60,
        na.rm = TRUE
      ),
      sleep_hours_max = max(
        total_minutes_asleep / 60,
        na.rm = TRUE
      ),
      sleep_efficiency_mean = mean(sleep_efficiency, na.rm = TRUE),
      sleep_efficiency_median = median(
        sleep_efficiency,
        na.rm = TRUE
      )
    )

  message("\nsleep distributions summary:")
  print(sleep_distributions)
}

# identify outliers -------------------------------------------
message("\nidentifying outliers:")

# zero-step days
zero_step_days <- daily_activities %>%
  filter(total_steps == 0) %>%
  select(id, activity_day, total_steps, calories, non_wear)

message("\nzero-step days:")
if (nrow(zero_step_days) > 0) {
  print(zero_step_days, n = Inf)
} else {
  message("no zero-step days found")
}

# extreme calorie burns (>5000 or <1000)
extreme_calories <- daily_activities %>%
  filter(calories > 5000 | calories < 1000) %>%
  select(id, activity_day, calories, total_steps) %>%
  arrange(desc(calories))

message("\nextreme calorie burns (>5000 or <1000):")
if (nrow(extreme_calories) > 0) {
  print(extreme_calories, n = Inf)
} else {
  message("no extreme calorie values found")
}

# abnormal sleep durations (<3 hours or >12 hours)
if (nrow(sleep_day) > 0) {
  abnormal_sleep <- sleep_day %>%
    mutate(sleep_hours = total_minutes_asleep / 60) %>%
    filter(sleep_hours < 3 | sleep_hours > 12) %>%
    select(
      id,
      sleep_day,
      sleep_hours,
      total_time_in_bed,
      sleep_efficiency
    ) %>%
    arrange(sleep_hours)

  message("\nabnormal sleep durations (<3 or >12 hours):")
  if (nrow(abnormal_sleep) > 0) {
    print(abnormal_sleep, n = Inf)
  } else {
    message("no abnormal sleep durations found")
  }
}

# ============================================================
# d. feature utilization
# ============================================================

message("\n=== feature utilization ===\n")

all_users <- unique(daily_activities$id)

feature_matrix <- tibble(id = all_users) %>%
  mutate(
    has_activity = id %in% daily_activities$id,
    has_sleep = id %in% sleep_day$id,
    has_weight = id %in% weight_log$id,
    has_heartrate = id %in% heartrate_seconds$id
  )

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

message("feature adoption summary:")
print(feature_summary)

# display users per feature (cleaner)
show_ids <- function(title, vector) {
  message("\n--- ", title, " ---")
  message("count: ", length(vector))
  print(vector)
}

show_ids(
  "users with sleep data",
  feature_matrix$id[feature_matrix$has_sleep]
)
show_ids(
  "users with heart rate data",
  feature_matrix$id[feature_matrix$has_heartrate]
)
show_ids(
  "users with weight logs",
  feature_matrix$id[feature_matrix$has_weight]
)
show_ids(
  "users without sleep data",
  feature_matrix$id[!feature_matrix$has_sleep]
)

# ============================================================
# e. tracking frequency
# ============================================================

message("\n=== tracking frequency ===\n")

# sleep -------------------------------------------------------
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
      sleep_tracking_rate = round(
        100 * sleep_records / date_range,
        1
      ),
      .groups = "drop"
    ) %>%
    arrange(desc(sleep_records))

  message("sleep tracking frequency:")
  print(sleep_frequency, n = Inf)
}

# weight ------------------------------------------------------
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

  message("\nweight logging frequency:")
  print(weight_frequency, n = Inf)
}

# heart rate --------------------------------------------------
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

  message("\nheart rate tracking frequency:")
  print(hr_frequency, n = Inf)
}

# ============================================================
# f. data quality
# ============================================================

message("\n=== data quality checks ===\n")

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
  message("users with non-wear days:")
  print(non_wear_summary, n = Inf)
} else {
  message("no non-wear days detected.")
}

# ============================================================
# g. recommended exclusions
# ============================================================

message("\n=== recommended exclusions ===\n")

insufficient_users <- user_engagement %>%
  filter(days_tracked < 7)

if (nrow(insufficient_users) > 0) {
  message("users with <7 days of tracking:")
  print(insufficient_users)
} else {
  message("all users have ≥7 days of tracking.")
}

# ============================================================
# h. save outputs
# ============================================================

dir.create(
  here("data", "processed"),
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  users_per_dataset,
  here("data/processed/users_per_dataset.csv")
)
write_csv(
  user_engagement,
  here("data/processed/user_engagement.csv")
)
write_csv(
  feature_matrix,
  here("data/processed/feature_utilization_matrix.csv")
)
write_csv(
  feature_summary,
  here("data/processed/feature_summary.csv")
)
write_csv(
  activity_distributions,
  here("data/processed/activity_distributions.csv")
)

if (exists("sleep_distributions")) {
  write_csv(
    sleep_distributions,
    here("data/processed/sleep_distributions.csv")
  )
}

if (exists("sleep_frequency")) {
  write_csv(
    sleep_frequency,
    here("data/processed/sleep_tracking_frequency.csv")
  )
}

if (exists("weight_frequency")) {
  write_csv(
    weight_frequency,
    here("data/processed/weight_logging_frequency.csv")
  )
}

if (exists("hr_frequency")) {
  write_csv(
    hr_frequency,
    here("data/processed/heartrate_tracking_frequency.csv")
  )
}

message("\n=== data coverage analysis complete ===")
message("processed files saved to: data/processed/")