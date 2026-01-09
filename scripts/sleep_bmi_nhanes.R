# =====================================================
# Sleep Duration and Body Mass Index
# NHANES 2017–2018 (Cycle J)
#
# NOTE:
# This script assumes all NHANES .XPT files are located
# in the same directory as this R script.
# =====================================================

# -------------------------------
# Load required packages
# -------------------------------
library(tidyverse)
library(haven)
library(janitor)
library(skimr)

# -------------------------------
# Load NHANES data files
# -------------------------------
demo  <- read_xpt("DEMO_J.XPT")
sleep <- read_xpt("SLQ_J.XPT")
bmx   <- read_xpt("BMX_J.XPT")

# Clean variable names
demo  <- clean_names(demo)
sleep <- clean_names(sleep)
bmx   <- clean_names(bmx)

# -------------------------------
# Merge datasets by respondent ID
# -------------------------------
full <- demo %>%
  left_join(sleep, by = "seqn") %>%
  left_join(bmx, by = "seqn")

# -------------------------------
# Create analysis variables
# -------------------------------
full2 <- full %>%
  mutate(
    # Sleep duration (hours)
    sleep_hours = na_if(sld012, 77),
    sleep_hours = na_if(sleep_hours, 99),
    sleep_hours = ifelse(sleep_hours < 1 | sleep_hours > 24, NA, sleep_hours),
    
    # BMI
    bmi = bmxbmi,
    
    # Obesity indicator
    obese = ifelse(!is.na(bmi) & bmi >= 30, 1, 0),
    
    # Covariates
    sex = factor(riagendr, levels = c(1, 2),
                 labels = c("Male", "Female")),
    age  = ridageyr,
    race = factor(ridreth1),
    pir  = indfmpir
  )

# -------------------------------
# Sample size check
# -------------------------------
nrow(full2)
glimpse(full2)

# -------------------------------
# Descriptive statistics
# -------------------------------
skim(full2 %>% select(sleep_hours, bmi, age, sex, race, pir))

# -------------------------------
# Linear regression model
# -------------------------------
m_bmi <- lm(
  bmi ~ sleep_hours + age + sex + race + pir,
  data = full2
)

summary(m_bmi)

# Exact sleep coefficient for reporting
coef(summary(m_bmi))["sleep_hours", ]

# -------------------------------
# Create sleep duration categories
# -------------------------------
full2 <- full2 %>%
  mutate(
    sleep_group = case_when(
      sleep_hours < 6 ~ "<6",
      sleep_hours >= 6 & sleep_hours < 7 ~ "6–7",
      sleep_hours >= 7 & sleep_hours < 8 ~ "7–8",
      sleep_hours >= 8 ~ "8+",
      TRUE ~ NA_character_
    )
  )

# -------------------------------
# FIGURE 1: Scatter + LOESS trend
# -------------------------------
fig1 <- full2 %>%
  filter(!is.na(sleep_hours), !is.na(bmi)) %>%
  ggplot(aes(x = sleep_hours, y = bmi)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Sleep Duration and Body Mass Index",
    x = "Sleep duration (hours)",
    y = "BMI"
  ) +
  theme_minimal()

fig1

ggsave(
  "Figure1_Sleep_BMI_LOESS.png",
  plot = fig1,
  width = 7,
  height = 5,
  dpi = 300
)

# -------------------------------
# FIGURE 2: Mean BMI by sleep group
# -------------------------------
fig2 <- full2 %>%
  filter(!is.na(sleep_group), !is.na(bmi)) %>%
  group_by(sleep_group) %>%
  summarise(mean_bmi = mean(bmi, na.rm = TRUE)) %>%
  ggplot(aes(x = sleep_group, y = mean_bmi)) +
  geom_col(fill = "gray40") +
  labs(
    title = "Average BMI by Sleep Duration Category",
    x = "Sleep duration (hours)",
    y = "Average BMI"
  ) +
  theme_minimal()

fig2

ggsave(
  "Figure2_Mean_BMI_by_Sleep_Group.png",
  plot = fig2,
  width = 7,
  height = 5,
  dpi = 300
)

# -------------------------------
# FIGURE 3: Obesity prevalence
# -------------------------------
fig3 <- full2 %>%
  filter(!is.na(sleep_group), !is.na(obese)) %>%
  group_by(sleep_group) %>%
  summarise(obesity_rate = mean(obese, na.rm = TRUE) * 100) %>%
  ggplot(aes(x = sleep_group, y = obesity_rate)) +
  geom_col(fill = "gray40") +
  labs(
    title = "Obesity Prevalence by Sleep Duration Category",
    x = "Sleep duration (hours)",
    y = "Obesity prevalence (%)"
  ) +
  theme_minimal()

fig3

ggsave(
  "Figure3_Obesity_by_Sleep_Group.png",
  plot = fig3,
  width = 7,
  height = 5,
  dpi = 300
)

