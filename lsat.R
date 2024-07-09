library(groundhog)
groundhog.library("
  tidymodels
  fs
  tidyverse
", "2024-07-04")

################################################################################
# Setup the environment.

source("setup.R")

DATASET <- "lsat"

################################################################################
# Load the data

df <- read_csv(
    path("data", "lsat", "lsac.csv"),
    skip = 1,
    col_types = cols(.default = "c"),
    col_names = c(
      "row_number",
      "decile1b",
      "decile3",
      "ID",
      "decile1",
      "sex",
      "race",
      "cluster",
      "lsat",
      "ugpa",
      "zfygpa",
      "DOB_yr",
      "grad",
      "zgpa",
      "bar1",
      "bar1_yr",
      "bar2",
      "bar2_yr",
      "fulltime",
      "fam_inc",
      "age",
      "gender",
      "parttime",
      "male",
      "race1",
      "race2",
      "Dropout",
      "other",
      "asian",
      "black",
      "hisp",
      "pass_bar",
      "bar",
      "tier",
      "index6040",
      "indxgrp",
      "indxgrp2",
      "dnn_bar_pass_prediction",
      "gpa"
    )
  ) %>%
  transmute(
    id = as.integer(ID),
    race = fct_collapse(
      race1,
      b           = "black",
      h           = "hisp",
      w           = "white",
      other_level = "Other"
    ),
    gender = fct_recode(gender, f = "female", m = "male"),
    income = as_factor(fam_inc),
    lsat = as.double(lsat),
    gpa = as.double(ugpa),
    outcome = factor(bar2 == "P", levels = c(TRUE, FALSE))
  ) %>%
  select(outcome, race, gender, income, lsat, gpa) %>%
  filter(race %in% c("b", "h", "w")) %>%
  drop_na()

################################################################################
# Model risk.

# Fit a logistic regression model to predict bar passage.
m_risk <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(outcome ~ race + gender + income + lsat + gpa, data = df)

# Add predictions to the data.
df <- predict(m_risk, df, type = "prob") %>%
  bind_cols(df) %>%
  select(
    outcome,
    risk = .pred_TRUE,
    race
  )

rm(m_risk)

################################################################################
# Convenience function for calculating the density ratio
density_ratio <- function(df, n_buckets) {
  df %>%
    group_by(bucket = ntile(risk, {{ n_buckets }})) %>%
    summarize(
      p_b = mean(race == "b"),
      p_h = mean(race == "h"),
      p_w = mean(race == "w"),
      r_b = p_b / (p_b + p_w),
      r_h = p_h / (p_h + p_w)
    )
}

source("monotonicity.R")

################################################################################

source("simulation.R")

################################################################################

source("model_checks.R")
