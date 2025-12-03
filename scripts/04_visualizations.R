# visualization.R
# ============================================================
# purpose: statistical visualization - bellabeat 2016
# author:  Onyedikachi Ikuru
# date:    2025-12-02
# ============================================================

# libraries ---------------------------------------------------
library(dplyr)
library(ggplot2)
library(forcats)
library(here)
library(readr)
library(tidyr)
library(scales)
library(lubridate)

# ensure output folder ----------------------------------------
dir.create(
  here("plots"),
  showWarnings = FALSE,
  recursive = TRUE
)

# custom theme ------------------------------------------------
theme_bellabeat <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(
        size = 14,
        face = "bold",
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 10,
        hjust = 0.5,
        color = "gray40"
      ),
      axis.title = element_text(size = 11),
      legend.position = "bottom"
    )
}

# color palette -----------------------------------------------
colors_activity <- c(
  "Sedentary" = "#E53935",
  "Low Active" = "#FB8C00",
  "Somewhat Active" = "#FDD835",
  "Active" = "#7CB342",
  "Highly Active" = "#43A047"
)

colors_engagement <- c(
  "Light User (1-10 days)" = "#FFCDD2",
  "Moderate User (11-20 days)" = "#FFB74D",
  "Heavy User (21-24 days)" = "#81C784",
  "Everyday User (25-31 days)" = "#66BB6A"
)

# ============================================================
# load processed data
# ============================================================

message("\n=== loading processed data ===\n")

# exploration outputs
users_per_dataset <- read_csv(
  here("data/processed/users_per_dataset.csv"),
  show_col_types = FALSE
)
user_engagement <- read_csv(
  here("data/processed/user_engagement.csv"),
  show_col_types = FALSE
)
feature_summary <- read_csv(
  here("data/processed/feature_summary.csv"),
  show_col_types = FALSE
)

# analysis outputs
user_segments <- read_csv(
  here("data/processed/user_segments.csv"),
  show_col_types = FALSE
)
key_correlations <- read_csv(
  here("data/processed/key_correlations.csv"),
  show_col_types = FALSE
)
correlation_matrix <- read_csv(
  here("data/processed/correlation_matrix.csv"),
  show_col_types = FALSE
)
hourly_activity_summary <- read_csv(
  here("data/processed/hourly_activity_summary.csv"),
  show_col_types = FALSE
)
daily_averages_by_dow <- read_csv(
  here("data/processed/daily_averages_by_dow.csv"),
  show_col_types = FALSE
)
weekly_trends <- read_csv(
  here("data/processed/weekly_trends.csv"),
  show_col_types = FALSE
)
business_insights <- read_csv(
  here("data/processed/business_insights.csv"),
  show_col_types = FALSE
)

# load cleaned data for distributions
daily_activities <- read_csv(
  here("data/cleaned/daily_activities_cleaned.csv"),
  show_col_types = FALSE
)
sleep_day <- read_csv(
  here("data/cleaned/sleep_day_cleaned.csv"),
  show_col_types = FALSE
)

message("data loaded successfully\n")

# ============================================================
# a. user overview dashboard
# ============================================================

message("creating user overview visualizations...\n")

# users per dataset (bar chart) ------------------------------
p1 <- ggplot(
  users_per_dataset,
  aes(x = reorder(Dataset, Users), y = Users)
) +
  geom_col(fill = "#1976D2", alpha = 0.8) +
  geom_text(aes(label = Users), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(
    title = "users per dataset",
    subtitle = "fitbit tracking coverage across data types",
    x = NULL,
    y = "number of users"
  ) +
  theme_bellabeat() +
  scale_y_continuous(limits = c(0, 35))

ggsave(
  here("plots/01_users_per_dataset.png"),
  p1,
  width = 10,
  height = 6,
  dpi = 300
)

# user engagement distribution (pie chart) -------------------
engagement_counts <- user_engagement %>%
  count(engagement_level) %>%
  mutate(
    pct = n / sum(n) * 100,
    label = paste0(
      engagement_level,
      "\n",
      n,
      " (",
      round(pct, 1),
      "%)"
    )
  )

p2 <- ggplot(
  engagement_counts,
  aes(x = "", y = n, fill = engagement_level)
) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  scale_fill_manual(values = colors_engagement) +
  labs(
    title = "user engagement distribution",
    subtitle = "based on tracking consistency (days tracked)",
    fill = "engagement level"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 14,
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "bottom"
  ) +
  geom_text(
    aes(label = n),
    position = position_stack(vjust = 0.5),
    size = 4,
    fontface = "bold"
  )

ggsave(
  here("plots/02_engagement_distribution.png"),
  p2,
  width = 8,
  height = 8,
  dpi = 300
)

# activity level distribution (bar chart) --------------------
activity_counts <- user_segments %>%
  count(activity_level) %>%
  mutate(
    activity_level = factor(
      activity_level,
      levels = c(
        "Sedentary",
        "Low Active",
        "Somewhat Active",
        "Active",
        "Highly Active"
      )
    )
  )

p3 <- ggplot(
  activity_counts,
  aes(x = activity_level, y = n, fill = activity_level)
) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = n), vjust = -0.5, size = 4) +
  scale_fill_manual(values = colors_activity) +
  labs(
    title = "activity level distribution",
    subtitle = "based on average daily steps",
    x = "activity level",
    y = "number of users"
  ) +
  theme_bellabeat() +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, max(activity_counts$n) + 2))

ggsave(
  here("plots/03_activity_level_distribution.png"),
  p3,
  width = 10,
  height = 6,
  dpi = 300
)

# feature adoption (stacked bar) ------------------------------
feature_adoption_long <- tibble(
  Feature = c("Activity", "Sleep", "Heart Rate", "Weight"),
  Users = c(
    feature_summary$activity_users,
    feature_summary$sleep_users,
    feature_summary$heartrate_users,
    feature_summary$weight_users
  ),
  Total = feature_summary$total_users,
  Percentage = c(
    feature_summary$activity_pct,
    feature_summary$sleep_pct,
    feature_summary$heartrate_pct,
    feature_summary$weight_pct
  )
) %>%
  mutate(
    Not_Using = Total - Users,
    Feature = factor(
      Feature,
      levels = c("Activity", "Sleep", "Heart Rate", "Weight")
    )
  ) %>%
  pivot_longer(
    cols = c(Users, Not_Using),
    names_to = "Status",
    values_to = "Count"
  )

p4 <- ggplot(
  feature_adoption_long,
  aes(x = Feature, y = Count, fill = Status)
) +
  geom_col(position = "stack", alpha = 0.8) +
  scale_fill_manual(
    values = c("Users" = "#43A047", "Not_Using" = "#BDBDBD"),
    labels = c("Not_Using" = "not using", "Users" = "using")
  ) +
  labs(
    title = "feature utilization across users",
    subtitle = "percentage of users tracking each feature",
    x = "feature",
    y = "number of users",
    fill = "status"
  ) +
  theme_bellabeat() +
  geom_text(
    data = feature_adoption_long %>% filter(Status == "Users"),
    aes(label = paste0(Count, "\n(", Percentage, "%)")),
    position = position_stack(vjust = 0.5),
    size = 3.5
  )

ggsave(
  here("plots/04_feature_utilization.png"),
  p4,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# b. activity patterns
# ============================================================

message("creating activity pattern visualizations...\n")

# hourly activity heatmap -------------------------------------
hourly_steps_data <- read_csv(
  here("data/cleaned/hourly_steps_cleaned.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    day_of_week = wday(activity_hour, label = TRUE, abbr = FALSE),
    hour = hour(activity_hour)
  ) %>%
  group_by(day_of_week, hour) %>%
  summarize(
    avg_steps = mean(step_total, na.rm = TRUE),
    .groups = "drop"
  )

p5 <- ggplot(
  hourly_steps_data,
  aes(x = hour, y = day_of_week, fill = avg_steps)
) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#FFF9C4",
    high = "#F57C00",
    name = "avg steps"
  ) +
  labs(
    title = "activity heatmap: steps by hour and day of week",
    subtitle = "darker colors indicate higher activity",
    x = "hour of day",
    y = NULL
  ) +
  theme_bellabeat() +
  scale_x_continuous(breaks = seq(0, 23, 3))

ggsave(
  here("plots/05_activity_heatmap.png"),
  p5,
  width = 12,
  height = 6,
  dpi = 300
)

# peak activity hours (line chart) ---------------------------
p6 <- ggplot(
  hourly_activity_summary,
  aes(x = hour, y = avg_steps)
) +
  geom_line(color = "#1976D2", linewidth = 1.2) +
  geom_point(color = "#1976D2", size = 2) +
  geom_area(alpha = 0.2, fill = "#1976D2") +
  labs(
    title = "average steps by hour of day",
    subtitle = "peak activity hours for notification timing",
    x = "hour of day",
    y = "average steps"
  ) +
  theme_bellabeat() +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  scale_y_continuous(labels = comma)

ggsave(
  here("plots/06_peak_activity_hours.png"),
  p6,
  width = 12,
  height = 6,
  dpi = 300
)

# steps distribution by activity segment (boxplot) -----------
activity_distribution_data <- daily_activities %>%
  left_join(
    user_segments %>% select(id, activity_level),
    by = "id"
  ) %>%
  filter(!is.na(activity_level)) %>%
  mutate(
    activity_level = factor(
      activity_level,
      levels = c(
        "Sedentary",
        "Low Active",
        "Somewhat Active",
        "Active",
        "Highly Active"
      )
    )
  )

p7 <- ggplot(
  activity_distribution_data,
  aes(x = activity_level, y = total_steps, fill = activity_level)
) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = colors_activity) +
  labs(
    title = "steps distribution by activity segment",
    subtitle = "daily step patterns across user activity levels",
    x = "activity level",
    y = "daily steps"
  ) +
  theme_bellabeat() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

ggsave(
  here("plots/07_steps_by_activity_segment.png"),
  p7,
  width = 10,
  height = 6,
  dpi = 300
)

# weekday vs weekend comparison (side-by-side bars) ----------
weekday_weekend_data <- daily_averages_by_dow %>%
  mutate(period = ifelse(is_weekend, "Weekend", "Weekday")) %>%
  group_by(period) %>%
  summarize(
    avg_steps = mean(avg_steps),
    avg_calories = mean(avg_calories),
    avg_active_minutes = mean(avg_active_minutes),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("avg_"),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = case_when(
      metric == "avg_steps" ~ "steps",
      metric == "avg_calories" ~ "calories",
      metric == "avg_active_minutes" ~ "active minutes"
    )
  )

p8 <- ggplot(
  weekday_weekend_data,
  aes(x = metric, y = value, fill = period)
) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c(
    "Weekday" = "#1976D2",
    "Weekend" = "#FBC02D"
  )) +
  labs(
    title = "weekday vs weekend activity comparison",
    subtitle = "average metrics across all users",
    x = NULL,
    y = "average value",
    fill = "period"
  ) +
  theme_bellabeat() +
  scale_y_continuous(labels = comma)

ggsave(
  here("plots/08_weekday_vs_weekend.png"),
  p8,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# c. correlations
# ============================================================

message("creating correlation visualizations...\n")

# correlation matrix heatmap ----------------------------------
cor_matrix_long <- correlation_matrix %>%
  pivot_longer(
    cols = -variable,
    names_to = "variable2",
    values_to = "correlation"
  )

p9 <- ggplot(
  cor_matrix_long,
  aes(x = variable, y = variable2, fill = correlation)
) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#E53935",
    mid = "white",
    high = "#43A047",
    midpoint = 0,
    limit = c(-1, 1),
    name = "correlation"
  ) +
  labs(
    title = "correlation matrix: activity metrics",
    subtitle = "relationships between daily activity variables",
    x = NULL,
    y = NULL
  ) +
  theme_bellabeat() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8)
  ) +
  geom_text(aes(label = round(correlation, 2)), size = 2.5)

ggsave(
  here("plots/09_correlation_matrix.png"),
  p9,
  width = 10,
  height = 10,
  dpi = 300
)

# scatter plot: steps vs calories -----------------------------
p10 <- ggplot(
  daily_activities,
  aes(x = total_steps, y = calories)
) +
  geom_point(alpha = 0.3, color = "#1976D2") +
  geom_smooth(method = "lm", color = "#E53935", se = TRUE) +
  labs(
    title = "steps vs calories burned",
    subtitle = paste0(
      "correlation: ",
      round(cor(
        daily_activities$total_steps,
        daily_activities$calories
      ), 3)
    ),
    x = "total steps",
    y = "calories burned"
  ) +
  theme_bellabeat() +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)

ggsave(
  here("plots/10_steps_vs_calories.png"),
  p10,
  width = 10,
  height = 6,
  dpi = 300
)

# scatter plot: very active minutes vs calories ---------------
p11 <- ggplot(
  daily_activities,
  aes(x = very_active_minutes, y = calories)
) +
  geom_point(alpha = 0.3, color = "#F57C00") +
  geom_smooth(method = "lm", color = "#E53935", se = TRUE) +
  labs(
    title = "very active minutes vs calories burned",
    subtitle = paste0(
      "correlation: ",
      round(cor(
        daily_activities$very_active_minutes,
        daily_activities$calories
      ), 3)
    ),
    x = "very active minutes",
    y = "calories burned"
  ) +
  theme_bellabeat() +
  scale_y_continuous(labels = comma)

ggsave(
  here("plots/11_active_minutes_vs_calories.png"),
  p11,
  width = 10,
  height = 6,
  dpi = 300
)

# key correlations bar chart ----------------------------------
p12 <- ggplot(
  key_correlations %>%
    mutate(
      relationship = fct_reorder(relationship, abs(correlation))
    ),
  aes(x = relationship, y = correlation, fill = correlation > 0)
) +
  geom_col(alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(
    values = c("TRUE" = "#43A047", "FALSE" = "#E53935"),
    guide = "none"
  ) +
  labs(
    title = "key correlations summary",
    subtitle = "strength of relationships between variables",
    x = NULL,
    y = "correlation coefficient"
  ) +
  theme_bellabeat() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray50"
  )

ggsave(
  here("plots/12_key_correlations.png"),
  p12,
  width = 10,
  height = 8,
  dpi = 300
)

# ============================================================
# d. temporal trends
# ============================================================

message("creating temporal trend visualizations...\n")

# weekly activity trends (line chart) ------------------------
p13 <- ggplot(
  weekly_trends,
  aes(x = week_number, y = avg_steps)
) +
  geom_line(color = "#1976D2", linewidth = 1.2) +
  geom_point(color = "#1976D2", size = 3) +
  labs(
    title = "weekly activity trends",
    subtitle = "average steps per week over 2-month period",
    x = "week number",
    y = "average daily steps"
  ) +
  theme_bellabeat() +
  scale_x_continuous(
    breaks = seq(1, max(weekly_trends$week_number), 1)
  ) +
  scale_y_continuous(labels = comma)

ggsave(
  here("plots/13_weekly_trends.png"),
  p13,
  width = 10,
  height = 6,
  dpi = 300
)

# user retention funnel (bar chart) --------------------------
p14 <- ggplot(
  weekly_trends,
  aes(x = as.factor(week_number), y = unique_users)
) +
  geom_col(fill = "#43A047", alpha = 0.8) +
  geom_text(
    aes(label = unique_users),
    vjust = -0.5,
    size = 3.5
  ) +
  labs(
    title = "user retention by week",
    subtitle = "number of active users per week",
    x = "week number",
    y = "active users"
  ) +
  theme_bellabeat() +
  scale_y_continuous(
    limits = c(0, max(weekly_trends$unique_users) + 2)
  )

ggsave(
  here("plots/14_user_retention.png"),
  p14,
  width = 10,
  height = 6,
  dpi = 300
)

# day of week patterns (line chart) --------------------------
dow_order <- c(
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday",
  "Friday",
  "Saturday",
  "Sunday"
)

p15 <- ggplot(
  daily_averages_by_dow %>%
    mutate(
      day_of_week = factor(day_of_week, levels = dow_order)
    ),
  aes(x = day_of_week, y = avg_steps, group = 1)
) +
  geom_line(color = "#1976D2", linewidth = 1.2) +
  geom_point(aes(color = is_weekend), size = 4) +
  scale_color_manual(
    values = c("TRUE" = "#FBC02D", "FALSE" = "#1976D2"),
    labels = c("weekday", "weekend")
  ) +
  labs(
    title = "activity by day of week",
    subtitle = "average steps across all users",
    x = NULL,
    y = "average steps",
    color = NULL
  ) +
  theme_bellabeat() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma)

ggsave(
  here("plots/15_day_of_week_patterns.png"),
  p15,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# e. sleep visualizations
# ============================================================

if (nrow(sleep_day) > 0) {
  message("creating sleep visualizations...\n")

  # sleep duration distribution (histogram) ------------------
  sleep_data_plot <- sleep_day %>%
    mutate(sleep_hours = total_minutes_asleep / 60)

  p16 <- ggplot(sleep_data_plot, aes(x = sleep_hours)) +
    geom_histogram(
      bins = 30,
      fill = "#6A1B9A",
      alpha = 0.7,
      color = "white"
    ) +
    geom_vline(
      xintercept = 7,
      linetype = "dashed",
      color = "#E53935",
      linewidth = 1
    ) +
    geom_vline(
      xintercept = 9,
      linetype = "dashed",
      color = "#E53935",
      linewidth = 1
    ) +
    annotate(
      "rect",
      xmin = 7,
      xmax = 9,
      ymin = 0,
      ymax = Inf,
      alpha = 0.1,
      fill = "#43A047"
    ) +
    labs(
      title = "sleep duration distribution",
      subtitle = "shaded area shows recommended 7-9 hours",
      x = "sleep hours",
      y = "frequency"
    ) +
    theme_bellabeat()

  ggsave(
    here("plots/16_sleep_duration_histogram.png"),
    p16,
    width = 10,
    height = 6,
    dpi = 300
  )

  # sleep efficiency by user segment (boxplot) ---------------
  sleep_by_segment <- sleep_day %>%
    left_join(
      user_segments %>% select(id, activity_level),
      by = "id"
    ) %>%
    filter(!is.na(activity_level)) %>%
    mutate(
      activity_level = factor(
        activity_level,
        levels = c(
          "Sedentary",
          "Low Active",
          "Somewhat Active",
          "Active",
          "Highly Active"
        )
      )
    )

  p17 <- ggplot(
    sleep_by_segment,
    aes(
      x = activity_level,
      y = sleep_efficiency,
      fill = activity_level
    )
  ) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = colors_activity) +
    labs(
      title = "sleep efficiency by activity level",
      subtitle = "relationship between activity and sleep quality",
      x = "activity level",
      y = "sleep efficiency"
    ) +
    theme_bellabeat() +
    theme(legend.position = "none")

  ggsave(
    here("plots/17_sleep_efficiency_by_segment.png"),
    p17,
    width = 10,
    height = 6,
    dpi = 300
  )

  # time in bed vs time asleep (scatter) ----------------------
  p18 <- ggplot(
    sleep_day,
    aes(
      x = total_time_in_bed / 60,
      y = total_minutes_asleep / 60
    )
  ) +
    geom_point(alpha = 0.4, color = "#6A1B9A") +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = "#E53935"
    ) +
    labs(
      title = "time in bed vs time asleep",
      subtitle = "dashed line shows perfect sleep efficiency (100%)",
      x = "time in bed (hours)",
      y = "time asleep (hours)"
    ) +
    theme_bellabeat()

  ggsave(
    here("plots/18_time_in_bed_vs_asleep.png"),
    p18,
    width = 10,
    height = 6,
    dpi = 300
  )
}

# ============================================================
# f. business insights dashboard
# ============================================================

message("creating business insights visualizations...\n")

# steps distribution with 10k goal line ----------------------
p19 <- ggplot(daily_activities, aes(x = total_steps)) +
  geom_histogram(
    bins = 30,
    fill = "#1976D2",
    alpha = 0.7,
    color = "white"
  ) +
  geom_vline(
    xintercept = 10000,
    linetype = "dashed",
    color = "#E53935",
    linewidth = 1.2
  ) +
  annotate(
    "text",
    x = 10000,
    y = Inf,
    label = "10k goal",
    vjust = 2,
    hjust = -0.1,
    color = "#E53935",
    fontface = "bold"
  ) +
  labs(
    title = "daily steps distribution",
    subtitle = "red line shows 10,000 step goal",
    x = "total steps",
    y = "frequency"
  ) +
  theme_bellabeat() +
  scale_x_continuous(labels = comma)

ggsave(
  here("plots/19_steps_distribution_with_goal.png"),
  p19,
  width = 10,
  height = 6,
  dpi = 300
)

# business insights summary (text visual) --------------------
p20 <- ggplot(
  business_insights,
  aes(x = 1, y = seq_along(metric), label = metric)
) +
  geom_text(
    aes(label = paste0(metric, ": ", value)),
    hjust = 0,
    size = 5,
    fontface = "bold"
  ) +
  geom_text(
    aes(label = interpretation, y = seq_along(metric) - 0.3),
    hjust = 0,
    size = 3.5,
    color = "gray40"
  ) +
  xlim(0.5, 3) +
  labs(title = "key business insights") +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 20)
    )
  )

ggsave(
  here("plots/20_business_insights_summary.png"),
  p20,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# complete
# ============================================================

message("\n=== visualization complete ===")
message("all plots saved to: plots/")
message(sprintf("total plots created: 20"))