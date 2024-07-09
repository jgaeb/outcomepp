# NOTE: Not to be run directly, but sourced from the individual data processing
#       scripts. The `groundhog` package should be loaded and the scripts should
#       define:
#       - DATASET: A global variable that contains the name of the dataset
#       - df: The original data. The data should have the following columns:
#           - outcome: The predicted outcome
#           - risk: The risk score
#           - race: The race of the individuals involved

groundhog.library(c("mgcv", "glue"), "2024-07-04")

################################################################################

# Print the AUC
df %>%
  roc_auc(outcome, risk) %>%
  glue_data("AUC: {.estimate}") %>%
  print()

# Covenience function for fitting a GAM
fit_gam <- function(race) {
  bounds <- df %>%
    filter(race == {{ race }}) %>%
    summarize(
      min = min(risk),
      max = max(risk)
    )
    m <- gam(
      outcome == "TRUE" ~ s(risk),
      family = binomial,
      data = filter(df, race == {{ race }})
    )
    tibble(
        risk  = seq(bounds$min, bounds$max, by = 1e-2)
      ) %>%
      predict(m, newdata = ., type = "response", se.fit = TRUE) %>%
      as_tibble() %>%
      mutate(
        risk   = seq(bounds$min, bounds$max, by = 1e-2),
        fit    = as.double(fit),
        se.fit = as.double(se.fit),
        ymin   = pmax(0, pmin(1, fit - 1.96 * se.fit)),
        ymax   = pmax(0, pmin(1, fit + 1.96 * se.fit))
      ) %>%
      select(risk, fit, ymin, ymax)
}
tibble(race = c("b", "h", "w")) %>%
  mutate(
    fit  = map(race, fit_gam),
    race = as_factor(race)
  ) %>%
  unnest(fit) %>%
  write_rds(path("data", "clean", str_c(DATASET, "-calib.rds")))
