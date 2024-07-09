library(groundhog)
groundhog.library("
  tidymodels
  fs
  RSQLite
  dbplyr
  tidyverse
", "2024-07-04")

################################################################################
# Setup the environment.

source("setup.R")

DATASET <- "compas"

################################################################################
# Load and clean the COMPAS dataset.

conn <- dbConnect(RSQLite::SQLite(), path("data", "compas", "compas.db"))

df <- tbl(conn, "compas") %>%
  filter(type_of_assessment == "Risk of Recidivism") %>%
  left_join(
    tbl(conn, "people"),
    by = c("person_id" = "id"),
  ) %>%
  select(
    race,
    score = raw_score,
    outcome = is_recid
  ) %>%
  # NOTE: The original dataset has a few missing compas scores.
  filter(outcome != -1) %>%
  collect() %>%
  mutate(
    race = factor(case_when(
      race == "African-American" ~ "b",
      race == "Hispanic"         ~ "h",
      race == "Caucasian"        ~ "w"
    ), levels = c("b", "h", "w")),
    outcome = factor(as.logical(outcome), levels = c(TRUE, FALSE))
  ) %>%
  drop_na(race)

################################################################################
# Model risk.

# Recalibrate the COMPAS scores using a logistic regression model.
m_risk <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(outcome ~ race * score, data = df)

df <- predict(m_risk, df, type = "prob") %>%
  bind_cols(df) %>%
  select(
    outcome,
    risk = .pred_TRUE,
    race
  )

rm(m_risk)

################################################################################
# Generate the data needed for the monotonicity plots.

# Convenience function for calculating the proportion of people belonging to
# each group at a given risk level.
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
# Run the simulation.

source("simulation.R")

################################################################################
# Perform model checks.

source("model_checks.R")
