
################################################################################
# SENSITIVITY AND ROBUSTNESS: HURRICANE IAN REMITTANCE SHOCK                   #
#                                                                              #
#   0. Setup        libraries, constants, paths, helper functions              #
#   1. Data prep    Florida-only panel + netted panel (CA, TX) over -8/+8 qtrs #
#   2. Estimation   OLS (state-time FE) and PPML (state-time FE)               #
#   3. Robustness   Ramsey RESET (both models) + Pearson residual diagnostic   #
#   4. Why PPML     mean-variance relationship across municipality-corridors   #
#   5. Sensitivity  HonestDiD relative magnitudes (baseline, netted, spatial)  #
################################################################################

# ---- 0. Setup ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(fixest)
library(HonestDiD)

# Set global plot theme
source("figures-tables/theme.R")

ref_q        <- as.Date("2022-07-01")        # reference quarter (one before landfall)
shock_date   <- as.Date("2022-10-01")        # Ian landfall (first post-period)
window_start <- shock_date %m-% months(24)   # -8 quarters
window_end   <- shock_date %m+% months(24)   # +8 quarters

# relative paths to figures
fig_dir       <- "figures-tables/ppml"

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# "2022Q3" -> 2022-07-01 UTC
quarter_to_date <- function(year_quarter) {
  yr <- as.integer(str_sub(year_quarter, 1, 4))
  q  <- as.integer(str_extract(year_quarter, "(?<=Q)[1-4]"))
  as.Date(sprintf("%04d-%02d-01", yr, (q - 1) * 3 + 1))
}

# Pull HonestDiD inputs from a fixest event study.
# Every regressor except the i() interactions is absorbed as a fixed effect, so
# matching coefficient names on `keep` isolates the event-study path. In the
# netted model this keeps Florida terms only and drops the CA/TX controls.
honest_inputs <- function(model,
                          shock_date = as.Date("2022-10-01"),
                          keep       = "florida_pct",
                          vcov_mat   = NULL) {
  b <- coef(model)
  V <- if (is.null(vcov_mat)) vcov(model) else vcov_mat
  
  sel <- grepl(keep, names(b))
  b <- b[sel]
  V <- V[sel, sel, drop = FALSE]
  
  dates <- as.Date(str_extract(names(b), "\\d{4}-\\d{2}-\\d{2}"))
  ord   <- order(dates)
  b     <- unname(b[ord])
  V     <- unname(V[ord, ord, drop = FALSE])
  dates <- dates[ord]
  
  list(
    betahat        = b,
    sigma          = V,
    numPrePeriods  = sum(dates <  shock_date),
    numPostPeriods = sum(dates >= shock_date),
    dates          = dates
  )
}

# Smallest Mbar whose relative-magnitudes robust CI first contains zero.
# Returns NA if the covariance is too near-singular for the optimizer.
breakdown_mbar <- function(hd, Mbarvec = seq(0, 0.5, by = 0.01)) {
  rm <- tryCatch(
    createSensitivityResults_relativeMagnitudes(
      betahat        = hd$betahat,
      sigma          = hd$sigma,
      numPrePeriods  = hd$numPrePeriods,
      numPostPeriods = hd$numPostPeriods,
      Mbarvec        = Mbarvec
    ),
    error = function(e) NULL
  )
  if (is.null(rm)) return(NA_real_)
  hit <- filter(rm, lb <= 0 & ub >= 0)
  if (nrow(hit) == 0) return(NA_real_)
  min(hit$Mbar)
}

# store expensive honestDiD calculations
cache_dir <- "data/cache/honestdid"
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

cache_rds <- function(name, expr, refresh = FALSE) {
  path <- file.path(cache_dir, paste0(name, ".rds"))
  if (!refresh && file.exists(path)) return(readRDS(path))
  result <- expr               # evaluated only when not already cached
  saveRDS(result, path)
  result
}

################################################################################
# ---- 1. Data preparation ----------------------------------------------------#
################################################################################

migration <- read.csv("data/migration_matrix_rows.csv")
remit      <- read.csv("data/mx_muni_inflows.csv")

remit_clean <- remit %>%
  mutate(
    remittances = remittances_musd * 1e6,
    period_date = as.Date(period_date)
  ) %>%
  select(-remittances_musd, -year, -quarter)

# Florida-only panel (baseline specification).
analysis_data <- remit_clean %>%
  inner_join(
    migration %>% select(mx_state, mx_municipality, Florida),
    by = c("mx_state", "mx_municipality")
  ) %>%
  filter(period_date >= window_start, period_date <= window_end) %>%
  mutate(florida_pct = Florida * 100)

# Netted panel (Florida plus California and Texas controls).
analysis_data_net <- remit_clean %>%
  inner_join(
    migration %>% select(mx_state, mx_municipality, Florida, California, Texas),
    by = c("mx_state", "mx_municipality")
  ) %>%
  filter(period_date >= window_start, period_date <= window_end) %>%
  mutate(
    florida_pct    = Florida    * 100,
    california_pct = California * 100,
    texas_pct      = Texas      * 100
  )

################################################################################
# ---- 2. Estimation ----------------------------------------------------------#
################################################################################

ols_with_state_fe <- feols(
  log(remittances + 1) ~ i(period_date, florida_pct, ref = ref_q) |
    cvegeo + mx_state^period_date,
  data = analysis_data, cluster = ~cvegeo
)

ppml_with_state_fe <- fepois(
  remittances ~ i(period_date, florida_pct, ref = ref_q) |
    cvegeo + mx_state^period_date,
  data = analysis_data, cluster = ~cvegeo
)

etable(ols_with_state_fe, ppml_with_state_fe,
       headers = c("OLS: state-time FE", "PPML: state-time FE"))

################################################################################
# ---- 3. Conditional-mean: robustness ----------------------------------------#
################################################################################

# Response-scale fitted values and Pearson residuals, aligned to the panel rows.
analysis_data$mu_ppml       <- predict(ppml_with_state_fe, newdata = analysis_data, type = "response")
analysis_data$pearson_resid <- (analysis_data$remittances - analysis_data$mu_ppml) / sqrt(analysis_data$mu_ppml)

# 3.1 Ramsey RESET: are powers of the fitted index jointly zero?
analysis_data$mu_sq    <- analysis_data$mu_ppml^2
analysis_data$mu_cu    <- analysis_data$mu_ppml^3
analysis_data$yhat_ols <- predict(ols_with_state_fe, newdata = analysis_data)
analysis_data$yhat_sq  <- analysis_data$yhat_ols^2
analysis_data$yhat_cu  <- analysis_data$yhat_ols^3

ppml_reset <- fepois(
  remittances ~ i(period_date, florida_pct, ref = ref_q) + mu_sq + mu_cu |
    cvegeo + mx_state^period_date,
  data = analysis_data, cluster = ~cvegeo
)
wald(ppml_reset, c("mu_sq", "mu_cu"))

ols_reset <- feols(
  log(remittances + 1) ~ i(period_date, florida_pct, ref = ref_q) + yhat_sq + yhat_cu |
    cvegeo + mx_state^period_date,
  data = analysis_data, cluster = ~cvegeo
)
wald(ols_reset, c("yhat_sq", "yhat_cu"))

# 3.2 Pearson residuals vs fitted values: largest residuals + the cloud.
analysis_data %>%
  arrange(desc(pearson_resid)) %>%
  select(cvegeo, period_date, florida_pct, remittances, mu_ppml, pearson_resid) %>%
  head(12)

resid_plot <- ggplot(analysis_data, aes(x = mu_ppml, y = pearson_resid)) +
  geom_point(alpha = 0.2, size = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_log10() +
  labs(
    x = "Fitted values (log scale)",
    y = "Pearson residuals"
  ) 

ggsave(file.path(fig_dir, "pearson-resids-ppml.pdf"),
       plot = resid_plot, height = 4, width = 5, dpi = 300, device = cairo_pdf)

################################################################################
# ---- 4. Why PPML: mean-variance relationship --------------------------------#
################################################################################

# PPML models municipality-quarter total inflows (the `remittances` outcome
# estimated in section 2), so the diagnostic is computed on that same outcome at
# the same unit: for each municipality, the mean and variance of its quarterly
# inflow across the pre-shock history. A log-log slope of 1 is the Poisson
# benchmark (variance = mean); a slope above 1 is overdispersion, which PPML
# accommodates and OLS-on-logs **does not**. The full pre-shock history is used
# (not the -8/+8 window) for stable per-municipality variance estimates.

diag_check <- remit_clean %>%
  semi_join(migration, by = c("mx_state", "mx_municipality")) %>%  # estimation municipalities
  filter(period_date < shock_date) %>%
  group_by(cvegeo) %>%
  summarise(
    mean_remit = mean(remittances, na.rm = TRUE),
    var_remit  = var(remittances, na.rm = TRUE),
    n_obs      = n(),
    .groups = "drop"
  ) %>%
  filter(n_obs >= 4, mean_remit > 0, var_remit > 0)

mv_plot <- ggplot(diag_check, aes(x = mean_remit, y = var_remit)) +
  geom_point(alpha = 0.3, size = 1.5, color = "#08306b") +
  geom_smooth(method = "lm", linewidth = 0.75, color= "red3") +
  geom_abline(slope = 1, intercept = 0, color = "grey40", linetype = "dashed") +
  scale_x_log10(labels = scales::label_scientific()) +
  scale_y_log10(labels = scales::label_scientific()) +
  labs(
    x = "Mean remittance inflow (log)",
    y = "Variance of remittance inflow (log)"
  ) 

ggsave(file.path(fig_dir, "muni-inflows-mean-var.pdf"),
       plot = mv_plot, width = 6, height = 4, dpi = 300, device = cairo_pdf)

mv_fit <- lm(log(var_remit) ~ log(mean_remit), data = diag_check)
summary(mv_fit)
coef(mv_fit)[["log(mean_remit)"]]   # slope; > 1 indicates overdispersion

################################################################################
# ---- 5. HonestDiD sensitivity (relative magnitudes) -------------------------#
################################################################################

# 5.1 Baseline PPML, Florida only. Breakdown Mbar ~ 0.15.
hd_base <- honest_inputs(ppml_with_state_fe)

orig_base <- constructOriginalCS(
  betahat        = hd_base$betahat,
  sigma          = hd_base$sigma,
  numPrePeriods  = hd_base$numPrePeriods,
  numPostPeriods = hd_base$numPostPeriods
)
rm_base <- createSensitivityResults_relativeMagnitudes(
  betahat        = hd_base$betahat,
  sigma          = hd_base$sigma,
  numPrePeriods  = hd_base$numPrePeriods,
  numPostPeriods = hd_base$numPostPeriods,
  Mbarvec        = seq(0, 1, by = 0.05)   # coarse grid for the plot
)
createSensitivityPlot_relativeMagnitudes(rm_base, orig_base)

breakdown_base <- cache_rds("breakdown_base", breakdown_mbar(hd_base))
breakdown_base # MBar = 0.15

# 5.2 Netted PPML: Florida conditional on California and Texas. ~ 0.04 (bonus)
ppml_netted <- fepois(
  remittances ~ i(period_date, florida_pct,    ref = ref_q) +
    i(period_date, california_pct, ref = ref_q) +
    i(period_date, texas_pct,      ref = ref_q) |
    cvegeo + mx_state^period_date,
  data = analysis_data_net, cluster = ~cvegeo
)


etable(ppml_netted, keep = "florida")  # Florida e=0 result robust to conditioning on TX/CA migration
etable(ppml_netted, drop = "florida")  # CA, TX post-shock effects insignificant

hd_net <- honest_inputs(ppml_netted)   # keep = "florida_pct" isolates Florida terms

breakdown_net <- cache_rds("breakdown_net", breakdown_mbar(hd_net))
breakdown_net    # ~ 0.04

# 5.3 Breakdown across spatial-clustering choices.
# Re-fit without cluster in the formula so the vcov can be swapped per spec.
ppml_base <- fepois(
  remittances ~ i(period_date, florida_pct, ref = ref_q) |
    cvegeo + mx_state^period_date,
  data = analysis_data
)

spatial_vcov <- list(
  "Cluster (muni)" = ~cvegeo,
  "Conley 10km"    = conley(cutoff = 10),
  "Conley 25km"    = conley(cutoff = 25),
  "Conley 50km"    = conley(cutoff = 50)
)

spatial_breakdowns <- cache_rds("spatial_breakdowns",
                                imap_dfr(spatial_vcov, function(v, label) {
                                  V  <- vcov(ppml_base, vcov = v)
                                  hd <- honest_inputs(ppml_base, vcov_mat = V)
                                  tibble(spec = label, breakdown = breakdown_mbar(hd))
                                })
)
spatial_breakdowns

