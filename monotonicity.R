# NOTE: Not to be run directly, but sourced from the individual data processing
#       scripts. The scripts should define:
#       - DATASET: A global variable that contains the name of the dataset
#       - df: The original data. The data should have the following columns:
#           - risk: The risk score
#           - race: The race of the individuals involved
#       - density_ratio: The density ratio function
#       - n_buckets: The number of buckets to use
#       - n_bootstrap: The number of bootstrap iterations to use
#       - alpha0: The alpha level for the inner confidence band
#       - alpha1: The alpha level for the outer confidence band

################################################################################
# Define the paths for saving results.
monotonicity_path <- path("data", "clean", str_c(DATASET, "-mono.rds"))

################################################################################
# Calculate the density ratio on the original data
dr <- density_ratio(df, n_buckets)

# Use a pivotal bootstrap to estimate the variance of the density ratio
bootstrap <- function(i, df, n_buckets) {
  df %>%
    slice_sample(prop = 1, replace = TRUE) %>%
    density_ratio({{ n_buckets }}) %>%
    mutate(bootstrap = {{ i }})
}

map(seq(n_bootstrap), bootstrap, df, n_buckets) %>%
  list_rbind() %>%
  group_by(bucket) %>%
  summarize(
    # NOTE: Initially inverted before pivoting
    p_lwr0_b = quantile(p_b, 1 - alpha0 / 2),
    p_lwr1_b = quantile(p_b, 1 - alpha1 / 2),
    p_upr0_b = quantile(p_b, alpha0 / 2),
    p_upr1_b = quantile(p_b, alpha1 / 2),
    p_lwr0_h = quantile(p_h, 1 - alpha0 / 2),
    p_lwr1_h = quantile(p_h, 1 - alpha1 / 2),
    p_upr0_h = quantile(p_h, alpha0 / 2),
    p_upr1_h = quantile(p_h, alpha1 / 2),
    p_lwr0_w = quantile(p_w, 1 - alpha0 / 2),
    p_lwr1_w = quantile(p_w, 1 - alpha1 / 2),
    p_upr0_w = quantile(p_w, alpha0 / 2),
    p_upr1_w = quantile(p_w, alpha1 / 2),
    r_lwr0_b = quantile(r_b, 1 - alpha0 / 2),
    r_lwr1_b = quantile(r_b, 1 - alpha1 / 2),
    r_upr0_b = quantile(r_b, alpha0 / 2),
    r_upr1_b = quantile(r_b, alpha1 / 2),
    r_lwr0_h = quantile(r_h, 1 - alpha0 / 2),
    r_lwr1_h = quantile(r_h, 1 - alpha1 / 2),
    r_upr0_h = quantile(r_h, alpha0 / 2),
    r_upr1_h = quantile(r_h, alpha1 / 2)
  ) %>%
  left_join(dr, by = "bucket") %>%
  mutate(
    p_lwr0_b = 2 * p_b - p_lwr0_b,
    p_lwr1_b = 2 * p_b - p_lwr1_b,
    p_upr0_b = 2 * p_b - p_upr0_b,
    p_upr1_b = 2 * p_b - p_upr1_b,
    p_lwr0_h = 2 * p_h - p_lwr0_h,
    p_lwr1_h = 2 * p_h - p_lwr1_h,
    p_upr0_h = 2 * p_h - p_upr0_h,
    p_upr1_h = 2 * p_h - p_upr1_h,
    p_lwr0_w = 2 * p_w - p_lwr0_w,
    p_lwr1_w = 2 * p_w - p_lwr1_w,
    p_upr0_w = 2 * p_w - p_upr0_w,
    p_upr1_w = 2 * p_w - p_upr1_w,
    r_lwr0_b = 2 * r_b - r_lwr0_b,
    r_lwr1_b = 2 * r_b - r_lwr1_b,
    r_upr0_b = 2 * r_b - r_upr0_b,
    r_upr1_b = 2 * r_b - r_upr1_b,
    r_lwr0_h = 2 * r_h - r_lwr0_h,
    r_lwr1_h = 2 * r_h - r_lwr1_h,
    r_upr0_h = 2 * r_h - r_upr0_h,
    r_upr1_h = 2 * r_h - r_upr1_h
  ) %>%
  pivot_longer(
    cols = c(starts_with("p_"), starts_with("r_")),
    names_pattern = "(.+)_(b|h|w)",
    names_to = c("statistic", "race"),
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = "statistic",
    values_from = "value"
  ) %>%
  write_rds(monotonicity_path)
