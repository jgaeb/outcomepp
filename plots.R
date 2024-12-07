library(groundhog)
groundhog.library("
  ggh4x
  argparse
  fs
  scales
  glue
  tidyverse
", "2024-07-04")

parser <- ArgumentParser()
parser$add_argument("--n_buckets", type = "integer")
parser$add_argument("--n_tiles", type = "integer")
args <- parser$parse_args()

n_buckets <- args$n_buckets
n_tiles <- args$n_tiles

theme_set(
  theme_bw() +
    theme(
      title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text  = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      strip.text = element_text(size = 8),
    )
)

################################################################################
############################### INFRAMARGINALITY ###############################
################################################################################

glue(str_c(rep("=", 80), collapse = ""))
glue("Inframarginality")

# Plot the inframarginality plots
rejection_rate <- 2/3

alpha_1 <- 2
beta_1  <- 2.87
alpha_2 <- 2.87
beta_2  <- 2

n_points <- 1001

threshold <- qbeta(rejection_rate, alpha_1, beta_1)

# Print the acceptance rates for the two distributions
glue("Monotonic Example")
glue("Threshold: {threshold}")
glue("Parameter for distribution 1: {alpha_1}, {beta_1}")
glue("Parameter for distribution 2: {alpha_2}, {beta_2}")
glue("Lending rate for distribution 1: {1 - pbeta(threshold, alpha_1, beta_1)}")
glue("Lending rate for distribution 2: {1 - pbeta(threshold, alpha_2, beta_2)}")

# Print out hit rates
hit_rate <- function(alpha, beta, threshold) {
  alpha / (alpha + beta) * (1 - pbeta(threshold, alpha + 1, beta)) /
  (1 - pbeta(threshold, alpha, beta))
}
glue("Repayment rate for distribution 1: {hit_rate(alpha_1, beta_1, threshold)}")
glue("Repayment rate for distribution 2: {hit_rate(alpha_2, beta_2, threshold)}")

# Plot the two distributions
df <- tibble(t = c(seq(0, 1, by = 1 / (n_points - 1))))

# Plot the distribution conditional on loan approval
p_main_infra <- ggplot() +
  geom_vline(
    xintercept = threshold,
    linetype   = "dashed",
    color      = "black"
  ) +
  geom_function(
    aes(x = t),
    fun   = dbeta,
    args  = list(shape1 = alpha_1, shape2 = beta_1),
    color = "#B51700",
    n     = n_points,
    data  = df
  ) +
  geom_area(
    aes(
      x = t,
      y = dbeta(t, alpha_1, beta_1)
    ),
    fill  = "#B51700",
    data  = df %>% filter(t >= threshold),
    alpha = 1/3
  ) +
  geom_function(
    aes(x = t),
    fun   = dbeta,
    args  = list(shape1 = alpha_2, shape2 = beta_2),
    color = "#004D80",
    n     = n_points,
    data  = df
  ) +
  geom_area(
    aes(
      x = t,
      y = dbeta(t, alpha_2, beta_2)
    ),
    fill = "#004D80",
    data  = df %>% filter(t >= threshold),
    alpha = 1/3
  ) +
  geom_vline(
    xintercept = hit_rate(alpha_1, beta_1, threshold),
    color     = "#B51700",
    linetype  = "dotted"
  ) +
  geom_vline(
    xintercept = hit_rate(alpha_2, beta_2, threshold),
    color     = "#004D80",
    linetype  = "dotted"
  ) +
  scale_x_continuous(
    name   = "Probability of Repayment",
    labels = label_percent(),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Density",
    limits = c(0, 2.5),
    labels = NULL,
    expand = c(0, 0)
  ) +
  theme(
    axis.ticks.y = element_blank(),
    aspect.ratio = 1,
    plot.margin = margin(5.5, 11, 5.5, 5.5)
  )

ggsave(
  filename = path("plots", "inframarginality-main.pdf"),
  plot     = p_main_infra,
  width    = 2.5,
  height   = 2.5,
  units    = "in"
)

# Plot the exception to the hybrid test
alpha_3 <- 5
beta_3  <- 5
alpha_4 <- 1.5
beta_4  <- 2

threshold <- 1/2

# Print out the lending rates and hit rates
glue("Non-Monotonic Example")
glue("Threshold: {threshold}")
glue("Parameter for distribution 1: {alpha_3}, {beta_3}")
glue("Parameter for distribution 2: {alpha_4}, {beta_4}")
glue("Lending rate for distribution 1: {1 - pbeta(threshold, alpha_3, beta_3)}")
glue("Lending rate for distribution 2: {1 - pbeta(threshold, alpha_4, beta_4)}")
glue("Repayment rate for distribution 1: {hit_rate(alpha_3, beta_3, threshold)}")
glue("Repayment rate for distribution 2: {hit_rate(alpha_4, beta_4, threshold)}")

# Plot the distribution conditional on loan approval
p_app_infra <- ggplot() +
  geom_vline(
    xintercept = threshold,
    linetype   = "dashed",
    color      = "black"
  ) +
  geom_function(
    aes(x = t),
    fun   = dbeta,
    args  = list(shape1 = alpha_3, shape2 = beta_3),
    color = "#B51700",
    n     = n_points,
    data  = df
  ) +
  geom_area(
    aes(
      x = t,
      y = dbeta(t, alpha_3, beta_3)
    ),
    fill  = "#B51700",
    data  = df %>% filter(t >= threshold),
    alpha = 1/3
  ) +
  geom_function(
    aes(x = t),
    fun   = dbeta,
    args  = list(shape1 = alpha_4, shape2 = beta_4),
    color = "#004D80",
    n     = n_points,
    data  = df
  ) +
  geom_area(
    aes(
      x = t,
      y = dbeta(t, alpha_4, beta_4)
    ),
    fill = "#004D80",
    data  = df %>% filter(t >= threshold),
    alpha = 1/3
  ) +
  geom_vline(
    xintercept = hit_rate(alpha_3, beta_3, threshold),
    color     = "#B51700",
    linetype  = "dotted"
  ) +
  geom_vline(
    xintercept = hit_rate(alpha_4, beta_4, threshold),
    color     = "#004D80",
    linetype  = "dotted"
  ) +
  scale_x_continuous(
    name   = "Probability of Repayment",
    labels = label_percent(),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Density",
    limits = c(0, 3),
    labels = NULL,
    expand = c(0, 0)
  ) +
  theme(
    axis.ticks.y = element_blank(),
    aspect.ratio = 1,
    plot.margin = margin(5.5, 11, 5.5, 5.5)
  )

ggsave(
  filename = path("plots", "inframarginality-appendix.pdf"),
  plot     = p_app_infra,
  width    = 2.5,
  height   = 2.5,
  units    = "in"
)

################################################################################
#################################### RIPA ######################################
################################################################################

# Load the ripa data
ripa <- read_rds(path("data", "clean", "ripa.rds")) %>%
  mutate(
    # Recode race to be easier to read
    race = fct_recode(race, Black = "b", Hispanic = "h"),
    # Calculate whether we found discrimination or not
    discrimination = factor(
      Delta_outcome_rate * Delta_decision_rate < 0,
      levels = c(TRUE, FALSE),
      labels = c("Yes", "No")
    )
  )

# Print out the number of stops and agencies in the data
glue(str_c(rep("=", 80), collapse = ""))
glue("RIPA")
glue("Number of stops: {sum(ripa$n_agency) / 2}")
glue("Number of agencies: {n_distinct(ripa$agency)}")
ripa %>%
  group_by(race) %>%
  summarize(n_discrim = sum(discrimination == "Yes")) %>%
  glue_data(
    "Number of agencies discriminating against ",
    "{ race } individuals: {n_discrim}"
  )
ripa %>%
  group_by(race) %>%
  summarize(n_w_discrim_outcome = sum(Delta_outcome_rate > 0)) %>%
  glue_data(
    "Number of agencies for which the outcome test detects discrimination ",
    "against White individuals relative to { race } individuals: ",
    "{ n_w_discrim_outcome }"
  )

# Plot the ripa data
p_ripa <- ripa %>%
  # Filter one point that cannot be seen on the plot
  filter(agency != "CITRUS HEIGHTS PD" | race != "Black") %>%
  ggplot(aes(
    x = Delta_outcome_rate,
    y = Delta_decision_rate,
    color = discrimination
  )) +
  geom_point(aes(size = n_agency), alpha = 1/2) +
  guides(size = "none", color = "none") +
  # Annotate with a rectangle in the second quadrant
  annotate(
    "rect",
    xmin  = -Inf,
    xmax  = 0,
    ymin  = 0,
    ymax  = Inf,
    fill  = "red",
    alpha = 0.1
  ) +
  # Annotate with a rectangle in the fourth quadrant
  annotate(
    "rect",
    xmin  = 0,
    xmax  = Inf,
    ymin  = -Inf,
    ymax  = 0,
    fill  = "red",
    alpha = 0.1
  ) +
  # Annotate with the x-axis
  geom_hline(yintercept = 0, linetype = "dashed") +
  # Annotate with the y-axis
  geom_vline(xintercept = 0, linetype = "dashed") +
  # Fix the aspect ratio
  scale_x_continuous(labels = scales::percent, limits = c(-1/4, 1/4)) +
  scale_y_continuous(labels = scales::percent, limits = c(-1/6, 1/6)) +
  coord_fixed() +
  # Match the colors to the backgrounds
  scale_color_manual(values = c("Yes" = "red", "No" = "black")) +
  # Set the maximum alpha level to 1/2
  scale_alpha_continuous(range = c(0, 2/3)) +
  # Scale the size of the points
  scale_size_area() +
  labs(
    title = "Difference in decision and outcome rates",
    color = "Race",
    x     = expr(paste(
      phantom() %<-% phantom(),
      " Lower minority outcome rate      Lower White outcome rate",
      phantom() %->% phantom(),
      "    ",
    )),
    y     = expr(paste(
      "   ",
      atop(
        phantom(),
        atop(
          phantom(),
          textstyle(phantom() %<-% phantom())
        )
      ),
      atop(
        phantom(),
        atop(textstyle(" Higher White"), textstyle("decision rate"))
      ),
      "      ",
      atop(
        phantom(),
        atop(textstyle("Higher minority"), textstyle("decision rate    "))
      ),
      atop(
        phantom(),
        atop(
          phantom(),
          textstyle(phantom() %->% phantom())
        )
      ),
    ))
  ) +
  facet_wrap(vars(race)) +
  theme(aspect.ratio = 2/3)

ggsave(
  path("plots", "ripa.pdf"),
  p_ripa,
  width  = 6,
  height = 2.5,
  device = cairo_pdf
)

################################################################################
################################ MONOTONICITY ##################################
################################################################################

# Convenience function for loading monotonicity data
load_monotonicity <- function(name) {
  read_rds(path("data", "clean", str_c(name, "-mono.rds"))) %>%
    mutate(dataset = {{ name }})
}
monotonicity <- c("lending", "compas", "lsat", "sqf") %>%
  map(load_monotonicity) %>%
  list_rbind() %>%
  mutate(
    dataset = factor(
      dataset,
      levels = c("lending", "compas", "sqf", "lsat"),
      labels = c(
        "Lending\n(loan approvals)",
        "Recidivism\n(bail decisions)",
        "Contraband\n(police searches)",
        "Bar passage\n(law school admissions)"
      )
    )
  )

# Plot the monotonicity data
p_monotonicity <- monotonicity %>%
  filter(race != "w") %>%
  mutate(race = fct_recode(race, "Black" = "b", "Hispanic" = "h")) %>%
  ggplot(aes(x = bucket, y = r, color = race, fill = race)) +
  geom_line() +
  geom_point(size = 1/3) +
  geom_ribbon(
    aes(ymin = r_lwr0, ymax = r_upr0),
    color = "transparent",
    alpha = 1/2
  ) +
  geom_ribbon(
    aes(ymin = r_lwr1, ymax = r_upr1),
    color = "transparent",
    alpha = 1/5
  ) +
  scale_x_continuous(
    breaks = seq(n_buckets),
  ) +
  scale_y_continuous(
    labels = label_percent(),
    expand = c(0, 0),
    limits = c(0, 1)
  ) +
  coord_fixed(10) +
  facet_wrap(vars(dataset), nrow = 1) +
  guides(
    fill = "none",
    color = guide_legend(position = "inside")
  ) +
  labs(
    color = NULL,
    fill  = NULL,
    x     = "Risk decile",
    y     = expr(Pr({{G == 1} * " | " * {R == r}}))
  ) +
  theme(
    # Get rid of minor grid lines
    panel.grid.minor = element_blank(),
    legend.position.inside  = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    # Get rid of key background
    legend.key = element_blank(),
    # Make facet labels smaller
    strip.text = element_text(size = 5),
    # Space the facets out
    panel.spacing = unit(1, "lines")
  )

ggsave(
  path("plots", "monotonicity.pdf"),
  p_monotonicity,
  width  = 6,
  height = 1.75,
)

################################################################################
############################ REAL-DATA SIMULATION ##############################
################################################################################

# Convenience function for loading the simulation data
load_simulation <- function(name) {
  # Load the data
  df <- read_rds(path("data", "clean", str_c(name, "-sim.rds"))) %>%
    # Ensure that t == 0 is accounted for
    complete(t = 0, race, policy)

  # Check whether receiving more decisions is good or bad
  prefer_more <- case_when(
    name == "lending" ~ TRUE,
    name == "compas"  ~ FALSE,
    name == "sqf"     ~ FALSE,
    name == "lsat"    ~ TRUE
  )

  # Left join minority and White results
  df %>%
    filter(race == "w") %>%
    left_join(
      filter(df, race %in% c("b", "h")),
      suffix = c("_0", "_1"),
      by = "policy",
      relationship = "many-to-many"
    ) %>%
    rename(race = race_1) %>%
    select(-race_0) %>%
    mutate(
      Delta_decision_rate = decision_rate_0 - decision_rate_1,
      Delta_outcome_rate  = outcome_rate_0 - outcome_rate_1,
      # Calculate the result of the hybrid test
      hybrid_result       = case_when(
        Delta_decision_rate > 0 & Delta_outcome_rate < 0 ~ if_else(
          prefer_more,
          "Discriminatory",
          "Favorable"
        ),
        Delta_decision_rate < 0 & Delta_outcome_rate > 0 ~ if_else(
          prefer_more,
          "Favorable",
          "Discriminatory"
        ),
        is.na(Delta_decision_rate) | is.na(Delta_outcome_rate) ~ "Infeasible",
        TRUE ~ "Inconclusive"
      ),
      hybrid_result       = factor(
        hybrid_result,
        levels = c("Discriminatory", "Inconclusive", "Favorable", "Infeasible")
      ),
      hybrid_result       = fct_relevel(
        hybrid_result,
        "Discriminatory",
        "Favorable",
        "Inconclusive",
        "Infeasible"
      ),
      # Calculate the result of the outcome test
      outcome_result      = case_when(
        Delta_outcome_rate < 0 ~ if_else(
          prefer_more,
          "Discriminatory",
          "Favorable"
        ),
        Delta_outcome_rate > 0 ~ if_else(
          prefer_more,
          "Favorable",
          "Discriminatory"
        ),
        is.na(Delta_outcome_rate) ~ "Infeasible",
      ),
      outcome_result      = factor(
        outcome_result,
        levels = c("Discriminatory", "Favorable", "Infeasible")
      ),
      outcome_result = fct_relevel(
        outcome_result,
        "Discriminatory",
        "Favorable",
        "Infeasible"
      ),
      dataset = {{ name }}
    ) %>%
    select(
      race,
      t_0,
      t_1,
      Delta_decision_rate,
      Delta_outcome_rate,
      hybrid_result,
      outcome_result,
      policy,
      dataset
    )
}
real_simulation <- c("lending", "compas", "sqf", "lsat") %>%
  map(load_simulation) %>%
  list_rbind() %>%
  mutate(
    dataset = factor(
      dataset,
      levels = c("lending", "compas", "sqf", "lsat"),
      labels = c(
        "Lending\n(loan approvals)",
        "Recidivism\n(bail decisions)",
        "Contraband\n(police searches)",
        "Bar passage\n(law school admissions)"
      )
    )
  )

# Plot the simulation data
for (policy_ in levels(real_simulation$policy)) {
  for (race_ in c("Black", "Hispanic")) {
    # Prepare the hybrid simulation data
    sim_data <- real_simulation %>%
      mutate(race = fct_recode(race, "Black" = "b", "Hispanic" = "h")) %>%
      filter(policy == policy_, race == race_) %>%
      mutate(
        hybrid_result = fct_recode(
          hybrid_result,
          "Discrimination against\n{ race_ } individuals" := "Discriminatory",
          "Discrimination against\nWhite individuals"     := "Favorable"
        ),
        outcome_result = fct_recode(
          outcome_result,
          "Discrimination against\n{ race_ } individuals" := "Discriminatory",
          "Discrimination against\nWhite individuals"     := "Favorable"
        ),
        shape         = case_when(
          t_0 > t_1 & str_detect(dataset, "Recid|Contra") ~
            glue("Discrimination against\n{ race_ } individuals"),
          t_0 > t_1 & str_detect(dataset, "Lend|Bar") ~
            "Discrimination against\nWhite individuals",
          t_0 < t_1 & str_detect(dataset, "Recid|Contra") ~
            "Discrimination against\nWhite individuals",
          t_0 < t_1 & str_detect(dataset, "Lend|Bar") ~
            glue("Discrimination against\n{ race_ } individuals"),
          TRUE ~ " "
        )
      ) %>%
      pivot_longer(
        cols = ends_with("result"),
        names_to = "test",
        values_to = "result"
      ) %>%
      mutate(test = factor(
        test,
        levels = c("hybrid_result", "outcome_result"),
        labels = c("Robust", "Standard")
      ))

    # Plot the simulation data
    p_simulation <- sim_data %>%
      ggplot() +
      geom_raster(
        aes(
          x = t_0,
          y = t_1,
          fill = result
        ),
        alpha = 0.5
      ) +
      geom_point(
        aes(x = t_0, y = t_1, shape = shape, color = result),
        size = 2,
        alpha = 1,
        # Only print at 10% intervals
        data = filter(
          sim_data,
          abs(((10 * t_0) %% 1) - 0.5) < 1e-6,
          abs(((10 * t_1) %% 1) - 0.5) < 1e-6
        )
      ) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      scale_x_continuous(
        labels = scales::percent,
        limits = c(0, 1),
        expand = c(0, 0),
        oob = oob_keep
      ) +
      scale_y_continuous(
        labels = scales::percent,
        limits = c(0, 1),
        expand = c(0, 0),
        oob = oob_keep
      ) +
      scale_fill_manual(
        values = set_names(
          c("red", "blue", "yellow", "grey"),
          c(
            glue("Discrimination against\n{ race_ } individuals"),
            "Discrimination against\nWhite individuals",
            "Inconclusive",
            "Infeasible"
          )
        ),
        breaks = c(
          glue("Discrimination against\n{ race_ } individuals"),
          "Discrimination against\nWhite individuals",
          "Inconclusive"
        )
      ) +
      scale_color_manual(
        values = set_names(
          c("red", "blue", "#8B8000", "grey"),
          c(
            glue("Discrimination against\n{ race_ } individuals"),
            "Discrimination against\nWhite individuals",
            "Inconclusive",
            "Infeasible"
          )
        )
      ) +
      scale_shape_manual(
        values = set_names(
          c("\u00D7", "\u00B7", " "),
          c(
            glue("Discrimination against\n{ race_ } individuals"),
            "Discrimination against\nWhite individuals",
            " "
          )
        ),
        breaks = c(
          glue("Discrimination against\n{ race_ } individuals"),
          "Discrimination against\nWhite individuals"
        )
      ) +
      coord_fixed() +
      labs(
        x     = "White decision threshold (percentile)",
        y     = glue("{ race_ } decision threshold (percentile)"),
        fill  = "Test result:",
        shape = "Ground truth:"
      ) +
      guides(
        alpha = guide_none(),
        color = guide_none(),
        fill = guide_legend(
          order = 1,
          nrow = 3,
          byrow = TRUE,
          override.aes = list(alpha = 1)
        ),
        shape = guide_legend(
          order = 2,
          nrow = 3,
          byrow = TRUE,
          override.aes = list(alpha = 1, color = "black", size = 6)
        )
      ) +
      facet_grid(rows = vars(test), cols = vars(dataset)) +
      theme(
        legend.position = "right",
        legend.text = element_text(size = 7),
        legend.spacing.y = unit(0, "lines"),
        axis.text = element_text(size = 6),
        strip.text = element_text(size = 5),
        panel.spacing = unit(1, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )

    ggsave(
      path("plots", glue("simulation-{ policy_ }-{ str_to_lower(race_) }.pdf")),
      p_simulation,
      width  = 6.5,
      height = 2.8,
      dpi    = 300
    )
  }
}

################################################################################
# Convenience function for simulating discrimination for fixed parameters
disc_sim <- function(nu, mu_0, mu_1) {
# Lay out a grid of parameters
  expand_grid(t_0 = seq(1, n_tiles - 1) / n_tiles, t_1 = seq(1, n_tiles - 1) / n_tiles) %>%
    mutate(
      # Calculate decision and outcome rates
      dr_0 = pbeta(t_0, mu_0 * nu, nu - mu_0 * nu, lower.tail = FALSE),
      or_0 = mu_0 * pbeta(t_0, mu_0 * nu + 1, nu - mu_0 * nu, lower.tail = FALSE) / dr_0,
      dr_1 = pbeta(t_1, mu_1 * nu, nu - mu_1 * nu, lower.tail = FALSE),
      or_1 = mu_1 * pbeta(t_1, mu_1 * nu + 1, nu - mu_1 * nu, lower.tail = FALSE) / dr_1,
      # Add robust outcome test results
      result = case_when(
        or_0 >  or_1 & dr_0 <  dr_1 | or_0 <  or_1 & dr_0 >  dr_1 ~ "conclusive",
        or_0 >= or_1 & dr_0 >= dr_1 | or_0 <= or_1 & dr_0 <= dr_1 ~ "inconclusive",
        TRUE ~ "infeasible"
      ),
      result = factor(result, levels = c("conclusive", "inconclusive", "infeasible"))
    ) %>%
    count(result, .drop = FALSE) %>%
    mutate(p = n / sum(n)) %>%
    select(-n) %>%
    pivot_wider(names_from = result, values_from = p) %>%
    select(-inconclusive)
}

p_baserates <- expand_grid( nu   = c(1, 2, 4, 8, 16, 32, 64),
    mu_0 = seq(0.1, 0.9, by = 0.1),
    mu_1 = seq(0.01, 0.99, by = 0.01)
  ) %>%
  rowwise() %>%
  mutate(simulation = list(disc_sim(nu, mu_0, mu_1))) %>%
  unnest(simulation) %>%
  filter(infeasible < 0.01) %>%
  mutate(nu = factor(nu, levels = c(1, 2, 4, 8, 16, 32, 64))) %>%
  ggplot(aes(x = mu_1, y = conclusive, color = nu)) +
  geom_line() +
  geom_vline(
    aes(xintercept = mu_0),
    linetype = "dashed",
    data     = tibble(mu_0 = seq(0.1, 0.9, by = 0.1))
  ) +
  scale_x_continuous(
    labels = scales::percent,
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_color_discrete() +
  coord_fixed() +
  labs(
    x = expr(mu[1]),
    y = expr("Proportion of conclusive tests"),
    color = expression(nu)
  ) +
  facet_wrap(
    vars(mu_0),
    labeller = label_bquote(mu[0] == .(scales::percent(mu_0)))
  ) +
  theme(panel.spacing = unit(1.5, "lines"))

ggsave(
  path("plots", "simulation-baserates.pdf"),
  plot   = p_baserates,
  width  = 6.5,
  height = 6.5
)

################################################################################
######################### EXAMPLE SIMULATION POLICIES ##########################
################################################################################

# Convenience functions for loading the example data
X_LIMITS <- list(
  "lending" = c(0, 1),
  "compas"  = c(0, 1),
  "sqf"     = c(0, 0.05),
  "lsat"    = c(0.9, 1)
)

load_hist <- function(dataset) {
  rescaling <- list(
    "lending" = \(p) floor(50 * p + 1e-6) / 50,
    "compas"  = \(p) floor(50 * p + 1e-6) / 50,
    "sqf"     = identity,
    "lsat"    = \(p) floor(500 * p + 1e-6) / 500
  )
  width <- c(
    "lending" = 1 / 50,
    "compas"  = 1 / 50,
    "sqf"     = 0.001,
    "lsat"    = 1 / 500
  )
  read_rds(path("data", "clean", str_c(dataset, "-hist.rds"))) %>%
    filter(ecdf >= X_LIMITS[[dataset]][[1]], ecdf <= X_LIMITS[[dataset]][[2]]) %>%
    group_by(ecdf = rescaling[[dataset]](ecdf)) %>%
    summarize(ecdf = min(ecdf), p = sum(p), .groups = "drop") %>%
    mutate(dataset = {{ dataset }}, width = width[dataset])
}

load_quant <- function(dataset) {
  read_rds(path("data", "clean", str_c(dataset, "-quant.rds"))) %>%
    mutate(dataset = {{ dataset }})
}

hist <- c("lending", "compas", "sqf", "lsat") %>%
  map(load_hist) %>%
  list_rbind() %>%
  mutate(
    dataset = factor(
      dataset,
      levels = c("lending", "compas", "sqf", "lsat"),
      labels = c(
        "Lending\n(loan approvals)",
        "Recidivism\n(bail decisions)",
        "Contraband\n(police searches)",
        "Bar passage\n(law school admissions)"
      )
    )
  ) %>%
  group_by(dataset) %>%
  mutate(p = p / max(p), .groups = "drop")

quant <- c("lending", "compas", "sqf", "lsat") %>%
  map(load_quant) %>%
  list_rbind() %>%
  mutate(
    dataset = factor(
      dataset,
      levels = c("lending", "compas", "sqf", "lsat"),
      labels = c(
        "Lending\n(loan approvals)",
        "Recidivism\n(bail decisions)",
        "Contraband\n(police searches)",
        "Bar passage\n(law school admissions)"
      )
    )
  )

fun_data <- expand_grid(
    x        = seq(0, 1, length.out = 10000),
    quantile = factor(c("1/3", "1/2", "2/3"), levels = c("1/3", "1/2", "2/3")),
  ) %>%
  left_join(quant, by = "quantile", relationship = "many-to-many") %>%
  rowwise() %>%
  filter(x >= X_LIMITS[[dataset]][[1]], x <= X_LIMITS[[dataset]][[2]]) %>%
  mutate(y = pnorm(log(x / (1 - x)), log(t / (1 - t)), sigma))

# Plot the example quasi-rational decision policies
p_policy <- hist %>%
  ggplot(aes(x = ecdf, y = p, width = width)) +
  geom_col(just = 0, fill = "gray") +
  geom_line(
    aes(x = x, y = y, color = quantile),
    data = fun_data,
    inherit.aes = FALSE
  ) +
  geom_vline(
    aes(xintercept = t, color = quantile),
    data = fun_data,
    linetype = "dashed",
    show.legend = FALSE
  ) +
  scale_x_continuous(
    name = "Risk",
    labels = label_percent(),
    breaks = trans_breaks(identity, identity, n = 5),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = NULL,
    labels = NULL,
    limits = c(0, 1.05),
    expand = expansion(c(0, 0))
  ) +
  scale_color_discrete(
    name = "Quantile",
    labels = list(
      "1/3" = expr(over(1, 3)),
      "1/2" = expr(over(1, 2)),
      "2/3" = expr(over(2, 3))
    )
  ) +
  facet_wrap(vars(dataset), scales = "free_x") +
  theme(
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    plot.margin = margin(5.5, 16.5, 5.5, 5.5),
    panel.spacing = unit(2, "lines")
  )

ggsave(
  path("plots", "example-policies.pdf"),
  p_policy,
  width  = 6,
  height = 6
)

################################################################################
################################# CALIBRATION ##################################
################################################################################

# Convenience functio for loading calibration data
load_calibration <- function(name) {
  read_rds(path("data", "clean", str_c(name, "-calib.rds"))) %>%
    mutate(dataset = {{ name }})
}
calibration <- c("lending", "compas", "sqf", "lsat") %>%
  map(load_calibration) %>%
  list_rbind() %>%
  mutate(
    dataset = factor(
      dataset,
      levels = c("lending", "compas", "sqf", "lsat"),
      labels = c(
        "Lending\n(loan approvals)",
        "Recidivism\n(bail decisions)",
        "Contraband\n(police searches)",
        "Bar passage\n(law school admissions)"
      )
    )
  )

# Generate the elements for manually scaling the axes
x_scales <- list(
  # Lending
  scale_x_continuous(
    labels = label_percent(),
    breaks = seq(0.2, 1, by = 0.2),
    limits = c(0.2, 1),
    expand = c(0, 0)
  ),
  scale_x_continuous(
    labels = label_percent(),
    breaks = seq(0.2, 1, by = 0.2),
    limits = c(0.2, 1),
    expand = c(0, 0)
  ),
  scale_x_continuous(
    labels = label_percent(),
    breaks = seq(0.2, 1, by = 0.2),
    limits = c(0.2, 1),
    expand = c(0, 0)
  ),
  # Compas
  scale_x_continuous(
    labels = label_percent(),
    breaks = seq(0, 0.8, by = 0.2),
    limits = c(0, 0.8),
    expand = c(0, 0)
  ),
  scale_x_continuous(
    labels = label_percent(),
    breaks = seq(0, 0.8, by = 0.2),
    limits = c(0, 0.8),
    expand = c(0, 0)
  ),
  scale_x_continuous(
    labels = label_percent(),
    breaks = seq(0, 0.8, by = 0.2),
    limits = c(0, 0.8),
    expand = c(0, 0)
  ),
  # SQF
  scale_x_continuous(
    labels = label_percent(),
    breaks = seq(0, 0.1, by = 0.025),
    limits = c(0, 0.1),
    expand = c(0, 0)
  ),
  scale_x_continuous(
    labels = label_percent(),
    breaks = seq(0, 0.1, by = 0.025),
    limits = c(0, 0.1),
    expand = c(0, 0)
  ),
  scale_x_continuous(
    labels = label_percent(),
    breaks = seq(0, 0.1, by = 0.025),
    limits = c(0, 0.1),
    expand = c(0, 0)
  ),
  # LSAT
  scale_x_continuous(
    labels = label_percent(),
    breaks = seq(0.6, 1, by = 0.1),
    limits = c(0.6, 1),
    expand = c(0, 0)
  ),
  scale_x_continuous(
    labels = label_percent(),
    breaks = seq(0.6, 1, by = 0.1),
    limits = c(0.6, 1),
    expand = c(0, 0)
  ),
  scale_x_continuous(
    labels = label_percent(),
    breaks = seq(0.6, 1, by = 0.1),
    limits = c(0.6, 1),
    expand = c(0, 0)
  )
)
y_scales <- list(
  # Lending
  scale_y_continuous(
    labels = label_percent(),
    breaks = seq(0.2, 1, by = 0.2),
    limits = c(0.2, 1),
    expand = c(0, 0)
  ),
  scale_y_continuous(
    labels = label_percent(),
    breaks = seq(0.2, 1, by = 0.2),
    limits = c(0.2, 1),
    expand = c(0, 0)
  ),
  scale_y_continuous(
    labels = label_percent(),
    breaks = seq(0.2, 1, by = 0.2),
    limits = c(0.2, 1),
    expand = c(0, 0)
  ),
  # Compas
  scale_y_continuous(
    labels = label_percent(),
    breaks = seq(0, 0.8, by = 0.2),
    limits = c(0, 0.8),
    expand = c(0, 0)
  ),
  scale_y_continuous(
    labels = label_percent(),
    breaks = seq(0, 0.8, by = 0.2),
    limits = c(0, 0.8),
    expand = c(0, 0)
  ),
  scale_y_continuous(
    labels = label_percent(),
    breaks = seq(0, 0.8, by = 0.2),
    limits = c(0, 0.8),
    expand = c(0, 0)
  ),
  # SQF
  scale_y_continuous(
    labels = label_percent(),
    breaks = seq(0, 0.1, by = 0.025),
    limits = c(0, 0.1),
    expand = c(0, 0)
  ),
  scale_y_continuous(
    labels = label_percent(),
    breaks = seq(0, 0.1, by = 0.025),
    limits = c(0, 0.1),
    expand = c(0, 0)
  ),
  scale_y_continuous(
    labels = label_percent(),
    breaks = seq(0, 0.1, by = 0.025),
    limits = c(0, 0.1),
    expand = c(0, 0)
  ),
  # LSAT
  scale_y_continuous(
    labels = label_percent(),
    breaks = seq(0.6, 1, by = 0.1),
    limits = c(0.6, 1),
    expand = c(0, 0)
  ),
  scale_y_continuous(
    labels = label_percent(),
    breaks = seq(0.6, 1, by = 0.1),
    limits = c(0.6, 1),
    expand = c(0, 0)
  ),
  scale_y_continuous(
    labels = label_percent(),
    breaks = seq(0.6, 1, by = 0.1),
    limits = c(0.6, 1),
    expand = c(0, 0)
  )
)

p_calibration <- calibration %>%
  mutate(race = fct_recode(
    race,
    "Black" = "b",
    "Hispanic" = "h",
    "White" = "w"
  )) %>%
  ggplot(aes(x = risk, y = fit, ymin = ymin, ymax = ymax, color = race, fill = race)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_ribbon(alpha = 1/5, color = "transparent") +
  facet_grid2(vars(dataset), vars(race), scales = "free", independent = TRUE) +
  facetted_pos_scales(x = x_scales, y = y_scales) +
  labs(
    x     = "Estimated frequency",
    y     = "Observed frequency",
    color = "Race",
    fill  = "Race"
  ) +
  theme(
    legend.position = "bottom",
    panel.spacing = unit(0.75, "lines"),
    strip.text = element_text(size = 6),
    aspect.ratio = 1
  )

ggsave(
  path("plots", "calibration.pdf"),
  p_calibration,
  width  = 6,
  height = 7.5,
)
