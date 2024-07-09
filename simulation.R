# NOTE: Not to be run directly, but sourced from the individual data processing
#       scripts. The `groundhog` package should be laoded, and the scripts
#       should define:
#       - DATASET: A global variable that contains the name of the dataset
#       - df: The original data. The data should have the following columns:
#           - risk: The risk score
#           - race: The race of the individuals involved
#       - n_tiles: The number of tiles to use for the hybrid test

groundhog.library("glue", "2024-07-04")

################################################################################
# Define the paths for saving results.
simulation_path <- path("data", "clean", str_c(DATASET, "-sim.rds"))
histogram_path  <- path("data", "clean", str_c(DATASET, "-hist.rds"))
quantiles_path  <- path("data", "clean", str_c(DATASET, "-quant.rds"))

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

# Lay out a grid of thresholds at each of the buckets, and then compute
# what the hybrid test would say about the policy.
t_policy <- tibble(t = seq(1, n_tiles - 1) / n_tiles) %>%
  rowwise() %>%
  mutate(sim = list(test_t_policy(quantile(df$risk, t)))) %>%
  unnest(sim) %>%
  select(race, t, decision_rate, outcome_rate) %>%
  mutate(policy = factor("threshold", levels = c("threshold", "beta")))

################################################################################
# Convenience function for calculating beta parameters from a mean and variance.
beta_params <- function(mean, var) {
  alpha <- mean * (mean * (1 - mean) / var - 1)
  beta <- alpha * (1 - mean) / mean
  tibble(alpha = alpha, beta = beta)
}
# Convenience function for testing the hybrid test on a beta policy.
test_beta_policy <- function(t, sigma) {
  # Compute the alpha and beta parameters for the beta distribution.
  alpha <- beta_params(t, sigma)$alpha
  beta  <- beta_params(t, sigma)$beta

  # If the parameters are invalid, return NaN.
  if (alpha <= 0 || beta <= 0) {
    return(tibble(
      race = unique(df$race),
      decision_rate = NaN,
      outcome_rate = NaN
    ))
  }

  # Compute the decision and outcome rates.
  df %>%
    mutate(d_g = pbeta(risk, alpha, beta)) %>%
    group_by(race) %>%
    summarize(
      decision_rate = mean(d_g),
      outcome_rate = weighted.mean(risk, d_g),
      .groups = "drop"
    )
}

# Lay out a grid of thresholds at each of the buckets, and then compute
# what the hybrid test would say about the policy.
sigma <- with(df, var(risk) / 4)
print(glue("Standard deviation for the beta policy: {sqrt(sigma)}"))

beta_policy <- tibble(t = seq(1, n_tiles - 1) / n_tiles) %>%
  rowwise() %>%
  mutate(sim = list(test_beta_policy(quantile(df$risk, t), sigma))) %>%
  unnest(sim) %>%
  select(race, t, decision_rate, outcome_rate) %>%
  mutate(policy = factor("beta", levels = c("threshold", "beta")))

################################################################################
# Save the results.
rbind(t_policy, beta_policy) %>%
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
# Store the quantiles and the standard deviation for the beta policy.
tibble(
    quantile = c(1/3, 1/2, 2/3),
    t        = quantile(df$risk, quantile)
  ) %>%
  rowwise() %>%
  mutate(params = list(beta_params(t, sigma))) %>%
  unnest(params) %>%
  mutate(quantile = factor(
    quantile,
    levels = c(1/3, 1/2, 2/3),
    labels = c("1/3", "1/2", "2/3"))
  ) %>%
  write_rds(quantiles_path)
