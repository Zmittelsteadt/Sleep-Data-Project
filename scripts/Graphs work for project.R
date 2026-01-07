# ---------------------------------------------------------
# Sleep Duration and Blood Pressure Analysis
# Project: sleep-data-project
# Purpose: Explore the association between sleep duration
#          and systolic blood pressure
# Data: Sleep.data.xlsx (synthetic health dataset)
# ---------------------------------------------------------

# Load required libraries
library(tidyverse)
library(readxl)

# ---------------------------------------------------------
# Load the dataset
# ---------------------------------------------------------

df <- read_excel("data/Sleep.data.xlsx")

# Inspect structure and column names
glimpse(df)
names(df)

# ---------------------------------------------------------
# Clean blood pressure into numeric variables
# ---------------------------------------------------------
# Blood Pressure is stored as "systolic/diastolic" (e.g., "120/80")
# We split this into two numeric columns

df2 <- df %>%
  separate(
    `Blood Pressure`,
    into = c("sys_bp", "dia_bp"),
    sep = "/",
    convert = TRUE
  ) %>%
  mutate(
    sys_bp = as.numeric(sys_bp),
    dia_bp = as.numeric(dia_bp),
    
    # Create a hypertension indicator (1 = hypertensive)
    hypertensive = if_else(sys_bp >= 130 | dia_bp >= 80, 1L, 0L)
  )

# Quick summary checks
summary(df2$sys_bp)
summary(df2$dia_bp)
table(df2$hypertensive)

# ---------------------------------------------------------
# FIGURE 1: Sleep Duration vs Systolic Blood Pressure
# ---------------------------------------------------------
# Scatter plot with linear trend to visualize association

p1 <- ggplot(df2, aes(x = `Sleep Duration`, y = sys_bp)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Sleep Duration vs Systolic Blood Pressure",
    subtitle = "Longer sleep duration is associated with lower systolic BP",
    x = "Sleep Duration (hours)",
    y = "Systolic Blood Pressure (mmHg)"
  ) +
  theme_minimal()

p1

# Save Figure 1
ggsave(
  "figures/sleep_vs_systolic_bp.png",
  plot = p1,
  width = 6,
  height = 4,
  dpi = 300
)

# ---------------------------------------------------------
# FIGURE 2: Average Systolic BP by Sleep Duration Group
# ---------------------------------------------------------
# Group sleep duration into meaningful categories

df2 <- df2 %>%
  mutate(
    sleep_group = cut(
      `Sleep Duration`,
      breaks = c(5, 6, 7, 8, Inf),
      labels = c("5–6", "6–7", "7–8", "8+"),
      right = FALSE
    )
  )

p2 <- df2 %>%
  group_by(sleep_group) %>%
  summarise(
    mean_sys_bp = mean(sys_bp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = sleep_group, y = mean_sys_bp)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Systolic Blood Pressure by Sleep Duration Group",
    x = "Sleep Duration Group (hours)",
    y = "Mean Systolic Blood Pressure (mmHg)"
  ) +
  theme_minimal()

p2

# Save Figure 2
ggsave(
  "figures/mean_sys_bp_by_sleep_group.png",
  plot = p2,
  width = 6,
  height = 4,
  dpi = 300
)
  
# ---------------------------------------------------------
# FIGURE 3: Hypertension Rate by Sleep Duration Group
# ---------------------------------------------------------
# Shows the % classified as hypertensive within each sleep group

p3 <- df2 %>%
  filter(!is.na(sleep_group)) %>%
  group_by(sleep_group) %>%
  summarise(
    n = n(),
    hypertension_rate = mean(hypertensive, na.rm = TRUE),  # proportion 0–1
    .groups = "drop"
  ) %>%
  mutate(hypertension_pct = 100 * hypertension_rate) %>%
  ggplot(aes(x = sleep_group, y = hypertension_pct)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", hypertension_pct)),
            vjust = -0.4, size = 4) +
  labs(
    title = "Hypertension Rate by Sleep Duration Group",
    subtitle = "Hypertension defined as systolic ≥ 130 or diastolic ≥ 80",
    x = "Sleep Duration Group (hours)",
    y = "Percent Hypertensive (%)"
  ) +
  ylim(0, max(df2$hypertensive, na.rm = TRUE) * 100 + 5) +  # gives label room
  theme_minimal()

p3

# Save Figure 3
ggsave(
  "figures/hypertension_rate_by_sleep_group.png",
  plot = p3,
  width = 6,
  height = 4,
  dpi = 300
)

# ---------------------------------------------------------
# FIGURE 4: Stage 2 Hypertension Rate by Sleep Duration Group
# ---------------------------------------------------------
# Rationale:
# Using the standard ≥130/80 definition classifies nearly the entire
# sample as hypertensive. To improve interpretability, we define
# Stage 2 hypertension using a stricter clinical cutoff:
#   - Systolic ≥ 140 OR Diastolic ≥ 90
# ---------------------------------------------------------

# Create Stage 2 hypertension indicator
df2 <- df2 %>%
  mutate(
    stage2_htn = if_else(sys_bp >= 140 | dia_bp >= 90, 1L, 0L)
  )

# Sanity check: confirm variable exists and distribution looks reasonable
table(df2$stage2_htn)

# ---------------------------------------------------------
# Aggregate Stage 2 hypertension rate by sleep group
# ---------------------------------------------------------

p4_stage2 <- df2 %>%
  filter(!is.na(sleep_group)) %>%
  group_by(sleep_group) %>%
  summarise(
    n = n(),
    stage2_rate = mean(stage2_htn, na.rm = TRUE),  # proportion (0–1)
    .groups = "drop"
  ) %>%
  mutate(stage2_pct = 100 * stage2_rate) %>%
  ggplot(aes(x = sleep_group, y = stage2_pct)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = sprintf("%.1f%%", stage2_pct)),
    vjust = -0.4,
    size = 4
  ) +
  labs(
    title = "Stage 2 Hypertension Rate by Sleep Duration Group",
    subtitle = "Stage 2 defined as systolic ≥ 140 or diastolic ≥ 90",
    x = "Sleep Duration Group (hours)",
    y = "Percent with Stage 2 Hypertension (%)"
  ) +
  theme_minimal()

# Display the plot
p4_stage2

# ---------------------------------------------------------
# Save Figure 4 for use in the paper
# ---------------------------------------------------------

ggsave(
  filename = "figures/stage2_hypertension_rate_by_sleep_group.png",
  plot = p3_stage2,
  width = 6,
  height = 4,
  dpi = 300
)

# ---------------------------------------------------------
# FIGURE 5A: Quadratic relationship between sleep duration
#            and systolic blood pressure (raw data + fit)
# ---------------------------------------------------------

p5a <- ggplot(df2, aes(x = `Sleep Duration`, y = sys_bp)) +
  geom_point(alpha = 0.15) +
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, 2, raw = TRUE),
    se = TRUE
  ) +
  labs(
    title = "Non-linear Relationship Between Sleep Duration and Systolic BP",
    subtitle = "Quadratic fit indicates lowest BP at moderate sleep durations",
    x = "Sleep Duration (hours)",
    y = "Systolic Blood Pressure (mmHg)"
  ) +
  theme_minimal()

p5a

ggsave(
  "figures/figure5_quadratic_sleep_vs_systolic_bp_raw.png",
  plot = p5a,
  width = 6,
  height = 4,
  dpi = 300
)

# ---------------------------------------------------------
# FIGURE 5B: Predicted systolic BP from quadratic model
#            with 95% confidence interval
# ---------------------------------------------------------

# Fit quadratic model
m_quad <- lm(sys_bp ~ poly(`Sleep Duration`, 2, raw = TRUE), data = df2)

# Create prediction grid
sleep_grid <- tibble(
  `Sleep Duration` = seq(
    min(df2$`Sleep Duration`, na.rm = TRUE),
    max(df2$`Sleep Duration`, na.rm = TRUE),
    length.out = 200
  )
)

# Generate predictions
pred <- predict(m_quad, newdata = sleep_grid, se.fit = TRUE)

sleep_grid <- sleep_grid %>%
  mutate(
    fit   = pred$fit,
    se    = pred$se.fit,
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )

# Plot predicted curve
p5b <- ggplot(sleep_grid, aes(x = `Sleep Duration`, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
  geom_line(linewidth = 1) +
  labs(
    title = "Predicted Systolic Blood Pressure by Sleep Duration",
    subtitle = "Quadratic model with 95% confidence interval",
    x = "Sleep Duration (hours)",
    y = "Predicted Systolic Blood Pressure (mmHg)"
  ) +
  theme_minimal()

p5b

ggsave(
  "figures/figure5_quadratic_sleep_vs_systolic_bp_predicted.png",
  plot = p5b,
  width = 6,
  height = 4,
  dpi = 300
)

# ---------------------------------------------------------
# Model comparison: Linear vs Quadratic relationship
# ---------------------------------------------------------
# Purpose:
# Test whether a quadratic model better captures the relationship
# between sleep duration and systolic blood pressure than a linear model.
# ---------------------------------------------------------

# Linear model
m_lin <- lm(sys_bp ~ `Sleep Duration`, data = df2)

# Quadratic model
m_quad <- lm(sys_bp ~ poly(`Sleep Duration`, 2, raw = TRUE), data = df2)

# View model summaries
summary(m_lin)
summary(m_quad)

# Compare linear and quadratic models
anova(m_lin, m_quad)

# Compare model fit statistics
summary(m_lin)$adj.r.squared
summary(m_quad)$adj.r.squared

AIC(m_lin)
AIC(m_quad)

