# NOTE: Not to be run directly, but sourced from the individual data processing
#       scripts. The `groundhog` package should be laoded, and the scripts
#       should define:
#       - DATASET: A global variable that contains the name of the dataset
#       - df: The original data. The data should have the following columns:
#           - risk: The risk score
#           - race: The race of the individuals involved
#       - n_tiles: The number of tiles to use for the robust outcome test

groundhog.library("
  brio
  decor
  cpp11
  glue
", "2024-07-04")
cpp_source("wtd_quantiles.cpp")

################################################################################
# Define the paths for saving results.
simulation_path <- path("data", "clean", str_c(DATASET, "-sim.rds"))
histogram_path  <- path("data", "clean", str_c(DATASET, "-hist.rds"))
quantiles_path  <- path("data", "clean", str_c(DATASET, "-quant.rds"))

################################################################################
# Calculate the quantiles for the threshold policy.
thresholds <- df %>%
  group_by(race) %>%
  mutate(w = 1 / n()) %>%
  ungroup() %>%
  reframe(
    p = seq(1, n_tiles - 1) / n_tiles,
    q = wtd_quantile(risk, w, p)
  )

################################################################################
# Convenience function for testing the hybrid test on a threshold policy.
test_t_policy <- function(t) {
  df %>%
    mutate(d_g = as.numeric(risk >= t)) %>%
    group_by(race) %>%
    summarize(
      decision_rate = mean(d_g),
      outcome_rate = weighted.mean(risk, d_g),
      .groups = "drop"
    )
}

# Compute the decision and outcome rates for the threshold policy.
t_policy <- thresholds %>%
  rowwise() %>%
  mutate(sim = list(test_t_policy(q))) %>%
  unnest(sim) %>%
  select(race, t = p, decision_rate, outcome_rate) %>%
  mutate(policy = factor("threshold", levels = c("threshold", "logit-normal")))

################################################################################
# Convenience function for testing the hybrid test on a logit-normal policy.
test_ln_policy <- function(t, sigma) {
  # If the parameters are invalid, return NaN.
  if (!is.finite(t) || sigma <= 0) {
    return(tibble(
      race = unique(df$race),
      decision_rate = NaN,
      outcome_rate = NaN
    ))
  }

  # Compute the decision and outcome rates.
  df %>%
    mutate(d_g = pnorm(log(risk / (1 - risk)), log(t / (1 - t)), sigma)) %>%
    group_by(race) %>%
    summarize(
      decision_rate = mean(d_g),
      outcome_rate = weighted.mean(risk, d_g),
      .groups = "drop"
    )
}

# Lay out a grid of thresholds at each of the buckets, and then compute
# what the hybrid test would say about the policy.
sigma <- with(df, sd(log(risk / (1 - risk))) / 2)
print(glue("Scale parameter for the logit normal policy: {sigma}"))

ln_policy <- thresholds %>%
  rowwise() %>%
  mutate(sim = list(test_ln_policy(q, sigma))) %>%
  unnest(sim) %>%
  select(race, t = p, decision_rate, outcome_rate) %>%
  mutate(
    policy = factor("logit-normal", levels = c("threshold", "logit-normal"))
  )

################################################################################
# Save the results.
rbind(t_policy, ln_policy) %>%
  write_rds(simulation_path)

################################################################################
# Store data necesasry to plot a histogram of different policies.
df %>%
  group_by(ecdf = ceiling(1000 * risk) / 1000) %>%
  summarize(
    count = n(),
    .groups = "drop"
  ) %>%
  mutate(p = count / sum(count)) %>%
  select(-count) %>%
  write_rds(histogram_path)

################################################################################
# Store the quantiles and the standard deviation for the logit-normal policy.
tibble(
    quantile = c(1/3, 1/2, 2/3),
    t        = quantile(df$risk, quantile),
    sigma    = sigma
  ) %>%
  mutate(quantile = factor(
    quantile,
    levels = c(1/3, 1/2, 2/3),
    labels = c("1/3", "1/2", "2/3"))
  ) %>%
  write_rds(quantiles_path)
