# analysis.R
# ============================================================
# purpose: statistical analysis - bellabeat 2016
# author:  Onyedikachi Ikuru
# date:    12-01-2025
# ============================================================

# libraries ---------------------------------------------------
library(dplyr)
library(readr)
library(here)
library(lubridate)
library(tidyr)
library(broom)

# create output directory -------------------------------------
dir.create(
  here("data", "processed"),
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# load cleaned data
# ============================================================

daily_activities <- read_csv(
  here("data", "cleaned", "daily_activities_cleaned.csv"),
  show_col_types = FALSE
)
sleep_day <- read_csv(
  here("data", "cleaned", "sleep_day_cleaned.csv"),
  show_col_types = FALSE
)
hourly_steps <- read_csv(
  here("data", "cleaned", "hourly_steps_cleaned.csv"),
  show_col_types = FALSE
)
hourly_calories <- read_csv(
  here("data", "cleaned", "hourly_calories_cleaned.csv"),
  show_col_types = FALSE
)
hourly_intensities <- read_csv(
  here("data", "cleaned", "hourly_intensities_cleaned.csv"),
  show_col_types = FALSE
)

# load processed exploration outputs
user_engagement <- read_csv(
  here("data", "processed", "user_engagement.csv"),
  show_col_types = FALSE
)

# ============================================================
# a. user segmentation
# ============================================================

message("\n=== user segmentation ===\n")

# activity level classification -------------------------------
user_activity_segments <- daily_activities %>%
  group_by(id) %>%
  summarize(
    avg_daily_steps = mean(total_steps, na.rm = TRUE),
    avg_calories = mean(calories, na.rm = TRUE),
    avg_distance = mean(total_distance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    activity_level = case_when(
      avg_daily_steps < 5000 ~ "Sedentary",
      avg_daily_steps < 7500 ~ "Low Active",
      avg_daily_steps < 10000 ~ "Somewhat Active",
      avg_daily_steps < 12500 ~ "Active",
      TRUE ~ "Highly Active"
    )
  )

# sleep behavior classification -------------------------------
if (nrow(sleep_day) > 0) {
  user_sleep_segments <- sleep_day %>%
    mutate(sleep_hours = total_minutes_asleep / 60) %>%
    group_by(id) %>%
    summarize(
      avg_sleep_hours = mean(sleep_hours, na.rm = TRUE),
      avg_sleep_efficiency = mean(sleep_efficiency, na.rm = TRUE),
      sleep_consistency = sd(sleep_hours, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      sleep_pattern = case_when(
        avg_sleep_hours < 6 ~ "Under-sleeper",
        avg_sleep_hours <= 8 ~ "Normal sleeper",
        TRUE ~ "Over-sleeper"
      )
    )
} else {
  user_sleep_segments <- tibble(
    id = character(),
    avg_sleep_hours = numeric(),
    sleep_pattern = character()
  )
}

# combine all segments ----------------------------------------
user_segments <- user_engagement %>%
  select(id, days_tracked, engagement_level) %>%
  left_join(user_activity_segments, by = "id") %>%
  left_join(
    user_sleep_segments %>% select(id, avg_sleep_hours, sleep_pattern),
    by = "id"
  )

message("user segmentation complete:")
message("  activity levels: ", n_distinct(user_segments$activity_level))
message(
  "  engagement levels: ",
  n_distinct(user_segments$engagement_level)
)
message(
  "  sleep patterns: ",
  n_distinct(user_segments$sleep_pattern, na.rm = TRUE)
)

# ============================================================
# b. correlation analysis
# ============================================================

message("\n=== correlation analysis ===\n")

# daily-level correlations ------------------------------------
daily_cor_data <- daily_activities %>%
  select(
    total_steps,
    total_distance,
    calories,
    very_active_minutes,
    fairly_active_minutes,
    lightly_active_minutes,
    sedentary_minutes,
    total_active_minutes
  )

correlation_matrix <- cor(
  daily_cor_data,
  use = "complete.obs",
  method = "pearson"
)

# extract key correlations
key_correlations <- tibble(
  relationship = c(
    "Steps → Calories",
    "Very Active Minutes → Calories",
    "Total Active Minutes → Calories",
    "Sedentary Minutes → Calories",
    "Distance → Calories",
    "Steps → Distance"
  ),
  correlation = c(
    cor(
      daily_activities$total_steps,
      daily_activities$calories,
      use = "complete.obs"
    ),
    cor(
      daily_activities$very_active_minutes,
      daily_activities$calories,
      use = "complete.obs"
    ),
    cor(
      daily_activities$total_active_minutes,
      daily_activities$calories,
      use = "complete.obs"
    ),
    cor(
      daily_activities$sedentary_minutes,
      daily_activities$calories,
      use = "complete.obs"
    ),
    cor(
      daily_activities$total_distance,
      daily_activities$calories,
      use = "complete.obs"
    ),
    cor(
      daily_activities$total_steps,
      daily_activities$total_distance,
      use = "complete.obs"
    )
  ),
  interpretation = case_when(
    abs(correlation) >= 0.7 ~ "Strong",
    abs(correlation) >= 0.4 ~ "Moderate",
    abs(correlation) >= 0.2 ~ "Weak",
    TRUE ~ "Very Weak"
  )
)

message("key correlations:")
print(key_correlations)

# sleep-activity correlations ---------------------------------
if (nrow(sleep_day) > 0) {
  sleep_activity_data <- sleep_day %>%
    mutate(sleep_date = as.Date(sleep_day)) %>%
    left_join(
      daily_activities %>%
        mutate(activity_date = as.Date(activity_day)) %>%
        select(
          id,
          activity_date,
          total_steps,
          very_active_minutes,
          calories
        ),
      by = c("id" = "id", "sleep_date" = "activity_date")
    ) %>%
    filter(!is.na(total_steps))

  if (nrow(sleep_activity_data) > 0) {
    sleep_correlations <- tibble(
      relationship = c(
        "Steps → Sleep Duration",
        "Very Active Minutes → Sleep Duration",
        "Steps → Sleep Efficiency",
        "Very Active Minutes → Sleep Efficiency"
      ),
      correlation = c(
        cor(
          sleep_activity_data$total_steps,
          sleep_activity_data$total_minutes_asleep,
          use = "complete.obs"
        ),
        cor(
          sleep_activity_data$very_active_minutes,
          sleep_activity_data$total_minutes_asleep,
          use = "complete.obs"
        ),
        cor(
          sleep_activity_data$total_steps,
          sleep_activity_data$sleep_efficiency,
          use = "complete.obs"
        ),
        cor(
          sleep_activity_data$very_active_minutes,
          sleep_activity_data$sleep_efficiency,
          use = "complete.obs"
        )
      ),
      interpretation = case_when(
        abs(correlation) >= 0.7 ~ "Strong",
        abs(correlation) >= 0.4 ~ "Moderate",
        abs(correlation) >= 0.2 ~ "Weak",
        TRUE ~ "Very Weak"
      )
    )

    message("\nsleep-activity correlations:")
    print(sleep_correlations)

    key_correlations <- bind_rows(key_correlations, sleep_correlations)
  }

  # lag analysis: sleep → next day steps
  lag_data <- sleep_day %>%
    arrange(id, sleep_day) %>%
    group_by(id) %>%
    mutate(next_day = sleep_day + days(1)) %>%
    ungroup() %>%
    left_join(
      daily_activities %>%
        mutate(activity_date = as.Date(activity_day)) %>%
        select(id, activity_date, total_steps),
      by = c("id" = "id", "next_day" = "activity_date")
    ) %>%
    filter(!is.na(total_steps))

  if (nrow(lag_data) > 0) {
    sleep_next_day_cor <- cor(
      lag_data$total_minutes_asleep,
      lag_data$total_steps,
      use = "complete.obs"
    )

    lag_correlation <- tibble(
      relationship = "Sleep Duration → Next Day Steps",
      correlation = sleep_next_day_cor,
      interpretation = case_when(
        abs(sleep_next_day_cor) >= 0.7 ~ "Strong",
        abs(sleep_next_day_cor) >= 0.4 ~ "Moderate",
        abs(sleep_next_day_cor) >= 0.2 ~ "Weak",
        TRUE ~ "Very Weak"
      )
    )

    message("\nlag analysis (sleep → next day):")
    print(lag_correlation)

    key_correlations <- bind_rows(key_correlations, lag_correlation)
  }
}

# ============================================================
# c. temporal patterns
# ============================================================

message("\n=== temporal patterns ===\n")

# time-of-day analysis (hourly) -------------------------------
hourly_activity_summary <- hourly_steps %>%
  mutate(hour = hour(activity_hour)) %>%
  group_by(hour) %>%
  summarize(
    avg_steps = mean(step_total, na.rm = TRUE),
    median_steps = median(step_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(hour)

# add hourly calories
hourly_calories_summary <- hourly_calories %>%
  mutate(hour = hour(activity_hour)) %>%
  group_by(hour) %>%
  summarize(
    avg_calories = mean(calories, na.rm = TRUE),
    .groups = "drop"
  )

hourly_activity_summary <- hourly_activity_summary %>%
  left_join(hourly_calories_summary, by = "hour")

message("peak activity hours:")
peak_hours <- hourly_activity_summary %>%
  arrange(desc(avg_steps)) %>%
  head(5)
print(peak_hours)

# day-of-week analysis ----------------------------------------
daily_averages_by_dow <- daily_activities %>%
  mutate(
    day_of_week = wday(activity_day, label = TRUE, abbr = FALSE),
    is_weekend = day_of_week %in% c("Saturday", "Sunday")
  ) %>%
  group_by(day_of_week, is_weekend) %>%
  summarize(
    avg_steps = mean(total_steps, na.rm = TRUE),
    avg_calories = mean(calories, na.rm = TRUE),
    avg_sedentary_minutes = mean(sedentary_minutes, na.rm = TRUE),
    avg_active_minutes = mean(total_active_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(day_of_week)

message("\nday of week patterns:")
print(daily_averages_by_dow)

# weekday vs weekend comparison
weekday_weekend <- daily_activities %>%
  mutate(
    day_of_week = wday(activity_day, label = TRUE),
    is_weekend = day_of_week %in% c("Sat", "Sun")
  ) %>%
  group_by(is_weekend) %>%
  summarize(
    avg_steps = mean(total_steps, na.rm = TRUE),
    avg_calories = mean(calories, na.rm = TRUE),
    avg_active_minutes = mean(total_active_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(period = ifelse(is_weekend, "Weekend", "Weekday"))

message("\nweekday vs weekend:")
print(weekday_weekend)

# sleep by day of week
if (nrow(sleep_day) > 0) {
  sleep_by_dow <- sleep_day %>%
    mutate(
      day_of_week = wday(sleep_day, label = TRUE, abbr = FALSE),
      sleep_hours = total_minutes_asleep / 60
    ) %>%
    group_by(day_of_week) %>%
    summarize(
      avg_sleep_hours = mean(sleep_hours, na.rm = TRUE),
      avg_sleep_efficiency = mean(sleep_efficiency, na.rm = TRUE),
      .groups = "drop"
    )

  message("\nsleep patterns by day of week:")
  print(sleep_by_dow)
}

# trend over time (weekly) ------------------------------------
weekly_trends <- daily_activities %>%
  mutate(week_number = floor(
    as.numeric(
      difftime(activity_day, min(activity_day), units = "weeks")
    )
  ) + 1) %>%
  group_by(week_number) %>%
  summarize(
    avg_steps = mean(total_steps, na.rm = TRUE),
    avg_calories = mean(calories, na.rm = TRUE),
    avg_active_minutes = mean(total_active_minutes, na.rm = TRUE),
    unique_users = n_distinct(id),
    .groups = "drop"
  ) %>%
  arrange(week_number)

message("\nweekly trends (retention):")
print(weekly_trends)

# week 1 vs week 8 comparison
if (max(weekly_trends$week_number) >= 8) {
  week_comparison <- weekly_trends %>%
    filter(week_number %in% c(1, 8)) %>%
    mutate(
      pct_change_steps = (avg_steps - lag(avg_steps)) /
        lag(avg_steps) * 100,
      pct_change_users = (unique_users - lag(unique_users)) /
        lag(unique_users) * 100
    ) %>%
    filter(week_number == 8)

  message("\nweek 1 vs week 8 comparison:")
  print(week_comparison)
}

# ============================================================
# d. sleep analysis
# ============================================================

if (nrow(sleep_day) > 0) {
  message("\n=== sleep analysis ===\n")

  sleep_analysis <- sleep_day %>%
    mutate(
      sleep_hours = total_minutes_asleep / 60,
      time_awake_in_bed = total_time_in_bed - total_minutes_asleep
    ) %>%
    summarize(
      avg_sleep_hours = mean(sleep_hours, na.rm = TRUE),
      median_sleep_hours = median(sleep_hours, na.rm = TRUE),
      sd_sleep_hours = sd(sleep_hours, na.rm = TRUE),
      avg_time_in_bed = mean(total_time_in_bed / 60, na.rm = TRUE),
      avg_sleep_efficiency = mean(sleep_efficiency, na.rm = TRUE),
      avg_time_awake = mean(time_awake_in_bed, na.rm = TRUE)
    )

  message("sleep analysis summary:")
  print(sleep_analysis)

  # user-level sleep consistency
  user_sleep_consistency <- sleep_day %>%
    mutate(sleep_hours = total_minutes_asleep / 60) %>%
    group_by(id) %>%
    summarize(
      avg_sleep = mean(sleep_hours, na.rm = TRUE),
      sleep_sd = sd(sleep_hours, na.rm = TRUE),
      avg_efficiency = mean(sleep_efficiency, na.rm = TRUE),
      sleep_records = n(),
      .groups = "drop"
    ) %>%
    mutate(
      consistency_rating = case_when(
        sleep_sd < 1 ~ "Very Consistent",
        sleep_sd < 1.5 ~ "Consistent",
        sleep_sd < 2 ~ "Moderate",
        TRUE ~ "Inconsistent"
      )
    )
}

# ============================================================
# e. business questions
# ============================================================

message("\n=== business questions ===\n")

# when are users most active?
peak_activity_times <- hourly_activity_summary %>%
  arrange(desc(avg_steps)) %>%
  head(5) %>%
  mutate(recommendation = "Optimal notification window")

message("peak activity times (for notifications):")
print(peak_activity_times %>% select(hour, avg_steps, recommendation))

# percentage meeting 10k step goal
pct_meeting_goal <- daily_activities %>%
  group_by(id) %>%
  summarize(
    days_meeting_goal = sum(total_steps >= 10000),
    total_days = n(),
    pct_days_meeting_goal = 100 * days_meeting_goal / total_days,
    .groups = "drop"
  ) %>%
  summarize(
    users_meeting_goal_50pct = sum(pct_days_meeting_goal >= 50),
    total_users = n(),
    pct_users = 100 * users_meeting_goal_50pct / total_users
  )

message("\nusers meeting 10k step goal (50%+ of days):")
print(pct_meeting_goal)

# percentage getting adequate sleep
if (nrow(sleep_day) > 0) {
  pct_adequate_sleep <- sleep_day %>%
    mutate(
      sleep_hours = total_minutes_asleep / 60,
      adequate_sleep = sleep_hours >= 7 & sleep_hours <= 9
    ) %>%
    group_by(id) %>%
    summarize(
      days_adequate = sum(adequate_sleep),
      total_days = n(),
      pct_adequate = 100 * days_adequate / total_days,
      .groups = "drop"
    ) %>%
    summarize(
      users_adequate_50pct = sum(pct_adequate >= 50),
      total_users = n(),
      pct_users = 100 * users_adequate_50pct / total_users
    )

  message("\nusers getting adequate sleep 7-9h (50%+ of days):")
  print(pct_adequate_sleep)
}

# feature adoption by segment
feature_by_segment <- user_segments %>%
  mutate(
    has_sleep = !is.na(sleep_pattern),
    adequate_tracking = days_tracked >= 21
  ) %>%
  group_by(activity_level) %>%
  summarize(
    users = n(),
    pct_with_sleep = 100 * sum(has_sleep) / n(),
    pct_adequate_tracking = 100 * sum(adequate_tracking) / n(),
    .groups = "drop"
  )

message("\nfeature adoption by activity segment:")
print(feature_by_segment)

# drop-off rate by week
dropout_analysis <- weekly_trends %>%
  mutate(
    retention_rate = 100 * unique_users / first(unique_users),
    dropout_rate = 100 - retention_rate
  )

message("\nuser retention by week:")
print(
  dropout_analysis %>% 
    select(week_number, unique_users, retention_rate)
)

# compile business insights
business_insights <- bind_rows(
  tibble(
    metric = "Peak Activity Hour",
    value = as.character(peak_activity_times$hour[1]),
    interpretation = "Best time for activity notifications"
  ),
  tibble(
    metric = "Users Meeting 10k Steps (50%+ days)",
    value = paste0(round(pct_meeting_goal$pct_users, 1), "%"),
    interpretation = "Percentage of engaged, active users"
  ),
  tibble(
    metric = "Week 8 Retention Rate",
    value = paste0(
      round(
        dropout_analysis$retention_rate[nrow(dropout_analysis)],
        1
      ),
      "%"
    ),
    interpretation = "Users still active after 8 weeks"
  )
)

if (nrow(sleep_day) > 0 && exists("pct_adequate_sleep")) {
  business_insights <- bind_rows(
    business_insights,
    tibble(
      metric = "Users with Adequate Sleep (50%+ days)",
      value = paste0(round(pct_adequate_sleep$pct_users, 1), "%"),
      interpretation = "Users getting healthy 7-9h sleep"
    )
  )
}

message("\nbusiness insights:")
print(business_insights)

# ============================================================
# save all results
# ============================================================

message("\n=== saving all results ===\n")

# user segmentation
write_csv(
  user_segments,
  here("data", "processed", "user_segments.csv")
)
message("saved: user_segments.csv")

# correlation analysis
write_csv(
  as_tibble(correlation_matrix, rownames = "variable"),
  here("data", "processed", "correlation_matrix.csv")
)
write_csv(
  key_correlations,
  here("data", "processed", "key_correlations.csv")
)
message("saved: correlation_matrix.csv, key_correlations.csv")

# temporal patterns
write_csv(
  hourly_activity_summary,
  here("data", "processed", "hourly_activity_summary.csv")
)
write_csv(
  daily_averages_by_dow,
  here("data", "processed", "daily_averages_by_dow.csv")
)
write_csv(
  weekly_trends,
  here("data", "processed", "weekly_trends.csv")
)
message(
  "saved: hourly_activity_summary.csv, ",
  "daily_averages_by_dow.csv, weekly_trends.csv"
)

# sleep analysis
if (exists("sleep_analysis") && exists("user_sleep_consistency")) {
  write_csv(
    bind_rows(
      sleep_analysis %>% mutate(metric_type = "overall"),
      user_sleep_consistency %>%
        summarize(
          avg_consistency = mean(sleep_sd, na.rm = TRUE)
        ) %>%
        mutate(metric_type = "consistency")
    ),
    here("data", "processed", "sleep_analysis.csv")
  )

  write_csv(
    user_sleep_consistency,
    here("data", "processed", "user_sleep_consistency.csv")
  )
  message("saved: sleep_analysis.csv, user_sleep_consistency.csv")
}

# business insights
write_csv(
  business_insights,
  here("data", "processed", "business_insights.csv")
)
message("saved: business_insights.csv")

message("\n=== analysis complete ===")
message("all processed files saved to: data/processed/")