# data_cleaning.R
# ============================================================
# purpose: data cleaning - bellabeat 2016
# author:  Onyedikachi Ikuru
# date:    12-01-2025
# ============================================================

# -------------------------------------------------------------------------
# Libraries
# -------------------------------------------------------------------------
library(vroom)
library(purrr)
library(dplyr)
library(janitor)
library(lubridate)
library(stringr)
library(here)

# -------------------------------------------------------------------------
# Safe datetime parser
# -------------------------------------------------------------------------
.datetime_orders <- c(
  "Ymd HMS", "Ymd HM", "Ymd",
  "Y-m-d HMS", "Y-m-d HM",
  "Y/m/d HMS", "Y/m/d HM",
  "mdY HMS", "mdY HM", "mdY"
)

parse_datetime_safe <- function(x) {
  x <- str_squish(x)
  parse_date_time(
    x,
    orders = .datetime_orders,
    tz = "UTC",
    quiet = TRUE,
    locale = "C"
  )
}

# -------------------------------------------------------------------------
# Load all Bellabeat CSVs
# -------------------------------------------------------------------------
file_paths <- list.files(
  here("data", "raw"),
  pattern = "\\.csv$",
  full.names = TRUE
)

bellabeat_file_data_list <- map(file_paths, ~ vroom(.x, progress = FALSE))

# Assign names to list
names(bellabeat_file_data_list) <- basename(file_paths)

# Clean column names
bellabeat_file_data_list <- map(bellabeat_file_data_list, clean_names)

# -------------------------------------------------------------------------
# Helper function to find dataset by name pattern
# -------------------------------------------------------------------------
find_dataset <- function(pattern) {
  idx <- grep(pattern, names(bellabeat_file_data_list), ignore.case = TRUE)
  if (length(idx) == 0) {
    return(NULL)
  }
  bellabeat_file_data_list[[idx[1]]]
}

# -------------------------------------------------------------------------
# Clean daily_activities
# -------------------------------------------------------------------------
daily_activities_raw <- find_dataset("dailyActivity")
if (!is.null(daily_activities_raw)) {
  daily_activities <- daily_activities_raw %>%
    mutate(
      id = as.character(id),
      across(
        where(is.character),
        ~ str_squish(str_replace_all(.x, "[\r\n]", ""))
      )
    ) %>%
    mutate(activity_date = parse_datetime_safe(activity_date)) %>%
    filter(!is.na(activity_date)) %>%
    rename(activity_day = activity_date) %>%
    mutate(across(contains("distance"), ~ round(.x, 2))) %>%
    mutate(
      total_minutes = very_active_minutes + fairly_active_minutes + lightly_active_minutes + sedentary_minutes,
      total_active_minutes = very_active_minutes + fairly_active_minutes + lightly_active_minutes,
      active_ratio = total_active_minutes / total_minutes,
      non_wear = (very_active_minutes == 0 & fairly_active_minutes == 0 & lightly_active_minutes == 0 & total_steps == 0)
    )

  invalid_days <- daily_activities %>% filter(total_minutes > 1440)
  if (nrow(invalid_days) > 0) {
    message(
      sprintf(
        "Removing %d rows from daily_activities where total_minutes > 1440",
        nrow(invalid_days)
      )
    )
  }

  daily_activities <- daily_activities %>%
    filter(total_minutes <= 1440) %>%
    distinct()
}

# -------------------------------------------------------------------------
# Clean daily_calories
# -------------------------------------------------------------------------
daily_calories_raw <- find_dataset("dailyCalories")
if (!is.null(daily_calories_raw)) {
  daily_calories <- daily_calories_raw %>%
    mutate(
      id = as.character(id),
      across(
        where(is.character),
        ~ str_squish(str_replace_all(.x, "[\r\n]", ""))
      )
    ) %>%
    mutate(
      activity_day = parse_datetime_safe(activity_day),
      calories = as.numeric(calories),
      non_wear = calories == 0 | is.na(calories)
    ) %>%
    filter(!is.na(activity_day)) %>%
    distinct()
}

# -------------------------------------------------------------------------
# Clean daily_intensities
# -------------------------------------------------------------------------
daily_intensities_raw <- find_dataset("dailyIntensities")
if (!is.null(daily_intensities_raw)) {
  daily_intensities <- daily_intensities_raw %>%
    mutate(
      id = as.character(id),
      across(
        where(is.character),
        ~ str_squish(str_replace_all(.x, "[\r\n]", ""))
      )
    ) %>%
    mutate(activity_day = parse_datetime_safe(activity_day)) %>%
    filter(!is.na(activity_day)) %>%
    mutate(across(contains("distance"), ~ round(.x, 2))) %>%
    mutate(
      total_minutes = very_active_minutes + fairly_active_minutes + lightly_active_minutes + sedentary_minutes,
      total_active_minutes = very_active_minutes + fairly_active_minutes + lightly_active_minutes,
      active_ratio = total_active_minutes / total_minutes,
      non_wear = (very_active_minutes == 0 & fairly_active_minutes == 0 & lightly_active_minutes == 0)
    )

  invalid_intensities <- daily_intensities %>% filter(total_minutes > 1440)
  if (nrow(invalid_intensities) > 0) {
    message(
      sprintf(
        "Removing %d rows from daily_intensities where total_minutes > 1440",
        nrow(invalid_intensities)
      )
    )
  }

  daily_intensities <- daily_intensities %>%
    filter(total_minutes <= 1440) %>%
    distinct()
}

# -------------------------------------------------------------------------
# Clean daily_steps
# -------------------------------------------------------------------------
daily_steps_raw <- find_dataset("dailySteps")
if (!is.null(daily_steps_raw)) {
  daily_steps <- daily_steps_raw %>%
    mutate(
      id = as.character(id),
      across(
        where(is.character),
        ~ str_squish(str_replace_all(.x, "[\r\n]", ""))
      )
    ) %>%
    mutate(
      activity_day = parse_datetime_safe(activity_day),
      step_total = as.numeric(step_total),
      non_wear = step_total == 0 | is.na(step_total)
    ) %>%
    filter(!is.na(activity_day)) %>%
    distinct()
}

# -------------------------------------------------------------------------
# Clean sleep_day
# -------------------------------------------------------------------------
sleep_day_raw <- find_dataset("sleepDay")
if (!is.null(sleep_day_raw)) {
  sleep_day <- sleep_day_raw %>%
    mutate(
      id = as.character(id),
      across(
        where(is.character),
        ~ str_squish(str_replace_all(.x, "[\r\n]", ""))
      )
    ) %>%
    mutate(sleep_day = parse_datetime_safe(sleep_day)) %>%
    filter(!is.na(sleep_day)) %>%
    mutate(
      total_minutes_asleep = as.numeric(total_minutes_asleep),
      total_time_in_bed = as.numeric(total_time_in_bed),
      sleep_efficiency = total_minutes_asleep / total_time_in_bed,
      time_awake_in_bed = total_time_in_bed - total_minutes_asleep
    ) %>%
    filter(total_minutes_asleep > 0 & total_time_in_bed > 0) %>%
    distinct()
}

# -------------------------------------------------------------------------
# Clean weight_log
# -------------------------------------------------------------------------
weight_log_raw <- find_dataset("weightLogInfo")
if (!is.null(weight_log_raw)) {
  weight_log <- weight_log_raw %>%
    mutate(
      id = as.character(id),
      across(
        where(is.character),
        ~ str_squish(str_replace_all(.x, "[\r\n]", ""))
      )
    ) %>%
    mutate(date = parse_datetime_safe(date)) %>%
    filter(!is.na(date)) %>%
    mutate(
      weight_kg = as.numeric(weight_kg),
      weight_pounds = as.numeric(weight_pounds),
      bmi = as.numeric(bmi),
      is_manual_report = as.logical(is_manual_report)
    ) %>%
    filter(weight_kg > 0) %>%
    distinct()
}

# -------------------------------------------------------------------------
# Clean hourly_calories
# -------------------------------------------------------------------------
hourly_calories_raw <- find_dataset("hourlyCalories")
if (!is.null(hourly_calories_raw)) {
  hourly_calories <- hourly_calories_raw %>%
    mutate(
      id = as.character(id),
      across(
        where(is.character),
        ~ str_squish(str_replace_all(.x, "[\r\n]", ""))
      )
    ) %>%
    mutate(
      activity_hour = parse_datetime_safe(activity_hour),
      calories = as.numeric(calories)
    ) %>%
    filter(!is.na(activity_hour) & !is.na(calories)) %>%
    distinct()
}

# -------------------------------------------------------------------------
# Clean hourly_intensities
# -------------------------------------------------------------------------
hourly_intensities_raw <- find_dataset("hourlyIntensities")
if (!is.null(hourly_intensities_raw)) {
  hourly_intensities <- hourly_intensities_raw %>%
    mutate(
      id = as.character(id),
      across(
        where(is.character),
        ~ str_squish(str_replace_all(.x, "[\r\n]", ""))
      )
    ) %>%
    mutate(
      activity_hour = parse_datetime_safe(activity_hour),
      total_intensity = as.numeric(total_intensity),
      average_intensity = as.numeric(average_intensity)
    ) %>%
    filter(!is.na(activity_hour)) %>%
    distinct()
}

# -------------------------------------------------------------------------
# Clean hourly_steps
# -------------------------------------------------------------------------
hourly_steps_raw <- find_dataset("hourlySteps")
if (!is.null(hourly_steps_raw)) {
  hourly_steps <- hourly_steps_raw %>%
    mutate(
      id = as.character(id),
      across(
        where(is.character),
        ~ str_squish(str_replace_all(.x, "[\r\n]", ""))
      )
    ) %>%
    mutate(
      activity_hour = parse_datetime_safe(activity_hour),
      step_total = as.numeric(step_total)
    ) %>%
    filter(!is.na(activity_hour)) %>%
    distinct()
}

# -------------------------------------------------------------------------
# Clean heartrate_seconds
# -------------------------------------------------------------------------
heartrate_seconds_raw <- find_dataset("heartrate_seconds")
if (!is.null(heartrate_seconds_raw)) {
  heartrate_seconds <- heartrate_seconds_raw %>%
    mutate(
      id = as.character(id),
      across(
        where(is.character),
        ~ str_squish(str_replace_all(.x, "[\r\n]", ""))
      )
    ) %>%
    mutate(
      time = parse_datetime_safe(time),
      value = as.numeric(value)
    ) %>%
    filter(!is.na(time) & value > 0 & value < 220) %>%
    distinct()
}

# -------------------------------------------------------------------------
# Clean minute-level data (narrow format)
# -------------------------------------------------------------------------
minute_calories_narrow_raw <- find_dataset("minuteCaloriesNarrow")
if (!is.null(minute_calories_narrow_raw)) {
  minute_calories_narrow <- minute_calories_narrow_raw %>%
    mutate(
      id = as.character(id),
      across(
        where(is.character),
        ~ str_squish(str_replace_all(.x, "[\r\n]", ""))
      )
    ) %>%
    mutate(
      activity_minute = parse_datetime_safe(activity_minute),
      calories = as.numeric(calories)
    ) %>%
    filter(!is.na(activity_minute)) %>%
    distinct()
}

minute_intensities_narrow_raw <- find_dataset("minuteIntensitiesNarrow")
if (!is.null(minute_intensities_narrow_raw)) {
  minute_intensities_narrow <- minute_intensities_narrow_raw %>%
    mutate(
      id = as.character(id),
      across(
        where(is.character),
        ~ str_squish(str_replace_all(.x, "[\r\n]", ""))
      )
    ) %>%
    mutate(
      activity_minute = parse_datetime_safe(activity_minute),
      intensity = as.numeric(intensity)
    ) %>%
    filter(!is.na(activity_minute)) %>%
    distinct()
}

minute_steps_narrow_raw <- find_dataset("minuteStepsNarrow")
if (!is.null(minute_steps_narrow_raw)) {
  minute_steps_narrow <- minute_steps_narrow_raw %>%
    mutate(
      id = as.character(id),
      across(
        where(is.character),
        ~ str_squish(str_replace_all(.x, "[\r\n]", ""))
      )
    ) %>%
    mutate(
      activity_minute = parse_datetime_safe(activity_minute),
      steps = as.numeric(steps)
    ) %>%
    filter(!is.na(activity_minute)) %>%
    distinct()
}

minute_sleep_raw <- find_dataset("minuteSleep")
if (!is.null(minute_sleep_raw)) {
  minute_sleep <- minute_sleep_raw %>%
    mutate(
      id = as.character(id),
      across(
        where(is.character),
        ~ str_squish(str_replace_all(.x, "[\r\n]", ""))
      )
    ) %>%
    mutate(
      date = parse_datetime_safe(date),
      value = as.numeric(value),
      log_id = as.character(log_id)
    ) %>%
    filter(!is.na(date)) %>%
    distinct()
}

minute_mets_narrow_raw <- find_dataset("minuteMETsNarrow")
if (!is.null(minute_mets_narrow_raw)) {
  minute_mets_narrow <- minute_mets_narrow_raw %>%
    mutate(
      id = as.character(id),
      across(
        where(is.character),
        ~ str_squish(str_replace_all(.x, "[\r\n]", ""))
      )
    ) %>%
    mutate(
      activity_minute = parse_datetime_safe(activity_minute),
      me_ts = as.numeric(me_ts)
    ) %>%
    rename(mets = me_ts) %>%
    filter(!is.na(activity_minute)) %>%
    distinct()
}

# -------------------------------------------------------------------------
# Clean minute-level data (wide format)
# -------------------------------------------------------------------------
minute_calories_wide_raw <- find_dataset("minuteCaloriesWide")
if (!is.null(minute_calories_wide_raw)) {
  minute_calories_wide <- minute_calories_wide_raw %>%
    mutate(
      id = as.character(id),
      across(
        where(is.character),
        ~ str_squish(str_replace_all(.x, "[\r\n]", ""))
      )
    ) %>%
    mutate(activity_hour = parse_datetime_safe(activity_hour)) %>%
    filter(!is.na(activity_hour)) %>%
    mutate(across(starts_with("calories"), as.numeric)) %>%
    distinct()
}

minute_intensities_wide_raw <- find_dataset("minuteIntensitiesWide")
if (!is.null(minute_intensities_wide_raw)) {
  minute_intensities_wide <- minute_intensities_wide_raw %>%
    mutate(
      id = as.character(id),
      across(
        where(is.character),
        ~ str_squish(str_replace_all(.x, "[\r\n]", ""))
      )
    ) %>%
    mutate(activity_hour = parse_datetime_safe(activity_hour)) %>%
    filter(!is.na(activity_hour)) %>%
    mutate(across(starts_with("intensity"), as.numeric)) %>%
    distinct()
}

minute_steps_wide_raw <- find_dataset("minuteStepsWide")
if (!is.null(minute_steps_wide_raw)) {
  minute_steps_wide <- minute_steps_wide_raw %>%
    mutate(
      id = as.character(id),
      across(
        where(is.character),
        ~ str_squish(str_replace_all(.x, "[\r\n]", ""))
      )
    ) %>%
    mutate(activity_hour = parse_datetime_safe(activity_hour)) %>%
    filter(!is.na(activity_hour)) %>%
    mutate(across(starts_with("steps"), as.numeric)) %>%
    distinct()
}

# -------------------------------------------------------------------------
# Preview
# -------------------------------------------------------------------------
if (interactive()) {
  datasets <- list(
    "Daily Activities" = if (exists("daily_activities")) daily_activities else NULL,
    "Daily Calories" = if (exists("daily_calories")) daily_calories else NULL,
    "Daily Intensities" = if (exists("daily_intensities")) daily_intensities else NULL,
    "Daily Steps" = if (exists("daily_steps")) daily_steps else NULL,
    "Sleep Day" = if (exists("sleep_day")) sleep_day else NULL,
    "Weight Log" = if (exists("weight_log")) weight_log else NULL,
    "Hourly Calories" = if (exists("hourly_calories")) hourly_calories else NULL,
    "Hourly Intensities" = if (exists("hourly_intensities")) hourly_intensities else NULL,
    "Hourly Steps" = if (exists("hourly_steps")) hourly_steps else NULL,
    "Heartrate Seconds" = if (exists("heartrate_seconds")) heartrate_seconds else NULL,
    "Minute Calories (Narrow)" = if (exists("minute_calories_narrow")) minute_calories_narrow else NULL,
    "Minute Intensities (Narrow)" = if (exists("minute_intensities_narrow")) minute_intensities_narrow else NULL,
    "Minute Steps (Narrow)" = if (exists("minute_steps_narrow")) minute_steps_narrow else NULL,
    "Minute Sleep" = if (exists("minute_sleep")) minute_sleep else NULL,
    "Minute METs" = if (exists("minute_mets_narrow")) minute_mets_narrow else NULL,
    "Minute Calories (Wide)" = if (exists("minute_calories_wide")) minute_calories_wide else NULL,
    "Minute Intensities (Wide)" = if (exists("minute_intensities_wide")) minute_intensities_wide else NULL,
    "Minute Steps (Wide)" = if (exists("minute_steps_wide")) minute_steps_wide else NULL
  )

  message("\n=== CLEANED DATASETS SUMMARY ===\n")
  for (name in names(datasets)) {
    if (!is.null(datasets[[name]])) {
      message(sprintf(
        "%s: %d rows, %d unique IDs",
        name,
        nrow(datasets[[name]]),
        n_distinct(datasets[[name]]$id)
      ))
    }
  }
}

# -------------------------------------------------------------------------
# Save cleaned datasets
# -------------------------------------------------------------------------
dir.create(here("data", "cleaned"), recursive = TRUE, showWarnings = FALSE)

# Save individual cleaned datasets
if (exists("daily_activities")) {
  write.csv(
    daily_activities,
    here("data", "cleaned", "daily_activities_cleaned.csv"),
    row.names = FALSE
  )
}
if (exists("daily_calories")) {
  write.csv(
    daily_calories,
    here("data", "cleaned", "daily_calories_cleaned.csv"),
    row.names = FALSE
  )
}
if (exists("daily_intensities")) {
  write.csv(
    daily_intensities,
    here("data", "cleaned", "daily_intensities_cleaned.csv"),
    row.names = FALSE
  )
}
if (exists("daily_steps")) {
  write.csv(
    daily_steps,
    here("data", "cleaned", "daily_steps_cleaned.csv"),
    row.names = FALSE
  )
}
if (exists("sleep_day")) {
  write.csv(
    sleep_day,
    here("data", "cleaned", "sleep_day_cleaned.csv"),
    row.names = FALSE
  )
}
if (exists("weight_log")) {
  write.csv(
    weight_log,
    here("data", "cleaned", "weight_log_cleaned.csv"),
    row.names = FALSE
  )
}
if (exists("hourly_calories")) {
  write.csv(
    hourly_calories,
    here("data", "cleaned", "hourly_calories_cleaned.csv"),
    row.names = FALSE
  )
}
if (exists("hourly_intensities")) {
  write.csv(
    hourly_intensities,
    here("data", "cleaned", "hourly_intensities_cleaned.csv"),
    row.names = FALSE
  )
}
if (exists("hourly_steps")) {
  write.csv(
    hourly_steps,
    here("data", "cleaned", "hourly_steps_cleaned.csv"),
    row.names = FALSE
  )
}
if (exists("heartrate_seconds")) {
  write.csv(
    heartrate_seconds,
    here("data", "cleaned", "heartrate_seconds_cleaned.csv"),
    row.names = FALSE
  )
}
if (exists("minute_calories_narrow")) {
  write.csv(
    minute_calories_narrow,
    here("data", "cleaned", "minute_calories_narrow_cleaned.csv"),
    row.names = FALSE
  )
}
if (exists("minute_intensities_narrow")) {
  write.csv(
    minute_intensities_narrow,
    here("data", "cleaned", "minute_intensities_narrow_cleaned.csv"),
    row.names = FALSE
  )
}
if (exists("minute_steps_narrow")) {
  write.csv(
    minute_steps_narrow,
    here("data", "cleaned", "minute_steps_narrow_cleaned.csv"),
    row.names = FALSE
  )
}
if (exists("minute_sleep")) {
  write.csv(
    minute_sleep, here("data", "cleaned", "minute_sleep_cleaned.csv"),
    row.names = FALSE
  )
}
if (exists("minute_mets_narrow")) {
  write.csv(
    minute_mets_narrow,
    here("data", "cleaned", "minute_mets_narrow_cleaned.csv"),
    row.names = FALSE
  )
}
if (exists("minute_calories_wide")) {
  write.csv(
    minute_calories_wide,
    here("data", "cleaned", "minute_calories_wide_cleaned.csv"),
    row.names = FALSE
  )
}
if (exists("minute_intensities_wide")) {
  write.csv(
    minute_intensities_wide,
    here("data", "cleaned", "minute_intensities_wide_cleaned.csv"),
    row.names = FALSE
  )
}
if (exists("minute_steps_wide")) {
  write.csv(
    minute_steps_wide,
    here("data", "cleaned", "minute_steps_wide_cleaned.csv"),
    row.names = FALSE
  )
}

message("\n=== CLEANING COMPLETE ===")
message(sprintf("Cleaned datasets saved to: %s", here("data", "cleaned")))
