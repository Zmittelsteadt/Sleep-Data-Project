# ============================================================
# Sleep Duration & Blood Pressure Project
# Purpose: Build + save all BP figures
# ============================================================

# ---------------------------
# 0) Packages
# ---------------------------
library(tidyverse)
library(readxl)
library(rpart)
library(rpart.plot)


out_dir <- file.path("figures", "blood_pressure")

# Create folder if it doesn't exist
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---------------------------
# 2) Load + clean data (self-contained)
# ---------------------------
# # NOTE: This assumes your project root contains /data/Sleep.data.xlsx
df_raw <- read_excel("data/Sleep.data.xlsx")

# # NOTE: Blood Pressure is stored like "120/80" -> split into sys/dia
df2 <- df_raw %>%
  separate(`Blood Pressure`, into = c("sys_bp", "dia_bp"), sep = "/", remove = FALSE) %>%
  mutate(
    sys_bp = as.numeric(sys_bp),
    dia_bp = as.numeric(dia_bp),
    
    # # NOTE: Standard HTN threshold (used earlier)
    hypertensive = if_else(sys_bp >= 130 | dia_bp >= 80, 1L, 0L),
    
    # # NOTE: Stage 2 threshold used for clearer separation
    stage2_htn = if_else(sys_bp >= 140 | dia_bp >= 90, 1L, 0L),
    
    # # NOTE: Sleep groups for bar charts
    sleep_group = case_when(
      `Sleep Duration` >= 5 & `Sleep Duration` < 6 ~ "5–6",
      `Sleep Duration` >= 6 & `Sleep Duration` < 7 ~ "6–7",
      `Sleep Duration` >= 7 & `Sleep Duration` < 8 ~ "7–8",
      `Sleep Duration` >= 8 ~ "8+",
      TRUE ~ NA_character_
    ),
    sleep_group = factor(sleep_group, levels = c("5–6", "6–7", "7–8", "8+"))
  ) %>%
  drop_na(sys_bp, dia_bp, `Sleep Duration`, sleep_group)

# ---------------------------
# Helper: save ggplot with consistent settings
# ---------------------------
save_plot <- function(p, filename, width = 9, height = 6, dpi = 300) {
  ggsave(
    filename = file.path(out_dir, filename),
    plot = p,
    width = width,
    height = height,
    dpi = dpi
  )
}

# ============================================================
# FIGURE 1: Sleep Duration vs Systolic BP (Linear smooth)
# ============================================================
fig1 <- ggplot(df2, aes(x = `Sleep Duration`, y = sys_bp)) +
  geom_jitter(alpha = 0.3, width = 0.05, height = 0) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Sleep Duration (hours)",
    y = "Systolic Blood Pressure (mmHg)"
  ) +
  theme_minimal() +
  ggtitle("Figure 1: Sleep Duration vs Systolic Blood Pressure")

save_plot(fig1, "Figure1_sleep_vs_systolic_bp_linear.png")

# ============================================================
# FIGURE 2: Mean Systolic BP by Sleep Group
# ============================================================
mean_df <- df2 %>%
  group_by(sleep_group) %>%
  summarise(mean_sys_bp = mean(sys_bp, na.rm = TRUE), .groups = "drop")

fig2 <- ggplot(mean_df, aes(x = sleep_group, y = mean_sys_bp)) +
  geom_col() +
  labs(
    x = "Sleep Duration Group (hours)",
    y = "Mean Systolic Blood Pressure (mmHg)"
  ) +
  theme_minimal() +
  ggtitle("Figure 2: Average Systolic BP by Sleep Duration Group")

save_plot(fig2, "Figure2_mean_systolic_bp_by_sleep_group.png")

# ============================================================
# FIGURE 3: Stage 2 Hypertension Rate by Sleep Group (≥140/90)
# ============================================================
stage2_df <- df2 %>%
  group_by(sleep_group) %>%
  summarise(stage2_rate = mean(stage2_htn, na.rm = TRUE), .groups = "drop") %>%
  mutate(label = paste0(round(stage2_rate * 100, 1), "%"))

fig3 <- ggplot(stage2_df, aes(x = sleep_group, y = stage2_rate * 100)) +
  geom_col() +
  geom_text(aes(label = label), vjust = -0.4) +
  labs(
    x = "Sleep Duration Group (hours)",
    y = "Percent with Stage 2 Hypertension (%)"
  ) +
  ylim(0, max(stage2_df$stage2_rate * 100) + 10) +
  theme_minimal() +
  ggtitle("Figure 3: Stage 2 Hypertension Rate by Sleep Duration Group")

save_plot(fig3, "Figure3_stage2_hypertension_rate_by_sleep_group.png")

# ============================================================
# FIGURE 4: Quadratic model predicted systolic BP with 95% CI
# ============================================================
# # NOTE: quadratic regression
m_quad <- lm(sys_bp ~ poly(`Sleep Duration`, 2, raw = TRUE), data = df2)

pred_grid <- tibble(
  sleep = seq(
    min(df2$`Sleep Duration`, na.rm = TRUE),
    max(df2$`Sleep Duration`, na.rm = TRUE),
    length.out = 200
  )
)

pred <- predict(
  m_quad,
  newdata = tibble(`Sleep Duration` = pred_grid$sleep),
  interval = "confidence",
  level = 0.95
)

pred_df <- bind_cols(
  pred_grid,
  as_tibble(pred) %>% rename(fit = fit, lwr = lwr, upr = upr)
)

fig4 <- ggplot(pred_df, aes(x = sleep, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
  geom_line(linewidth = 1) +
  labs(
    x = "Sleep Duration (hours)",
    y = "Predicted Systolic Blood Pressure (mmHg)"
  ) +
  theme_minimal() +
  ggtitle("Figure 4: Predicted Systolic BP by Sleep Duration (Quadratic + 95% CI)")

save_plot(fig4, "Figure4_quadratic_predicted_systolic_bp_with_CI.png")


# ============================================================
# Figure 5: Decision Tree (Stage 2 Hypertension)
# ============================================================

# Packages
library(tidyverse)
library(readxl)
library(rpart)
library(rpart.plot)

# 1) Load data
df_raw <- read_excel("data/Sleep.data.xlsx")

# 2) Clean BP: split "120/80" into sys/dia
df <- df_raw %>%
  separate(
    col = `Blood Pressure`,
    into = c("sys_bp", "dia_bp"),
    sep = "/",
    convert = TRUE
  )

# 3) Outcome: Stage 2 hypertension (≥140 systolic OR ≥90 diastolic)
df <- df %>%
  mutate(
    stage2_htn = if_else(sys_bp >= 140 | dia_bp >= 90, 1, 0),
    stage2_htn = factor(stage2_htn, levels = c(0, 1), labels = c("No", "Yes"))
  )

# 4) Fit decision tree 
# NOTE: cp controls how complex the tree is (bigger = simpler)
tree_model <- rpart(
  stage2_htn ~ `Sleep Duration` +
    `Quality of Sleep` +
    `Physical Activity Level` +
    `Stress Level` +
    `BMI Category` +
    Age +
    `Daily Steps` +
    Gender +
    `Sleep Disorder`,
  data = df,
  method = "class",
  control = rpart.control(cp = 0.01)
)

# 5) Plot tree 
rpart.plot(
  tree_model,
  type = 3,                 # clean split labels
  extra = 104,              # show probs + % + n
  fallen.leaves = TRUE,
  main = "Figure 5: Decision Tree Predicting Stage 2 Hypertension"
)

# 6) Save PNG 
dir.create("figures/blood_pressure", showWarnings = FALSE, recursive = TRUE)

png("figures/blood_pressure/fig5_decision_tree_stage2_htn.png",
    width = 1600, height = 1000, res = 200)

rpart.plot(
  tree_model,
  type = 3,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Figure 5: Decision Tree Predicting Stage 2 Hypertension"
)

