################################################################################
# DIFF-IN-DIFF ESTIMATION OF MUNICIPALITY REMITTANCE INFLOWS
################################################################################

# Load libraries
library(tidyverse)
library(fixest)
library(ggplot2)
library(patchwork)
library(zoo)

# Set global plot theme
theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title        = element_text(face = "bold", size = 14, hjust = 0),
      plot.subtitle     = element_text(size = 11, hjust = 0),
      axis.text.x       = element_text(angle = 45, hjust = 1),
      panel.grid.major  = element_line(color = "grey95", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      legend.position   = "bottom",
      legend.background = element_blank()
    )
)

################################################################################
# DATA PREPARATION
################################################################################

# Loading the data
migration <- read.csv("data/migration_matrix_rows.csv")
remit     <- read.csv("data/mx_muni_inflows.csv")

# Converting remittance data to unit dollars and formatting date variable
remit_clean <- remit %>%
  mutate(
    remittances = remittances_musd * 1000000,
    period_date = as.Date(period_date)
  ) %>%
  select(-remittances_musd, -c(year, quarter))

# Extracting the Florida exposure variable and merging into a final panel
florida_weights <- migration %>%
  select(mx_state, mx_municipality, Florida)
master_panel <- remit_clean %>%
  inner_join(florida_weights, by = c("mx_state", "mx_municipality"))

# Restrict timeline to -8/+8 quarters
shock_date <- as.Date("2022-10-01")
unique_quarters <- sort(unique(master_panel$period_date))
shock_index <- which(unique_quarters == shock_date)
master_panel <- master_panel %>%
  mutate(
    rel_quarter = match(period_date, unique_quarters) - shock_index
  ) %>%
  filter(rel_quarter >= -8 & rel_quarter <= 8)

# Convert florida weights to percentage points
analysis_data <- master_panel %>%
  mutate(
    florida_pct = Florida * 100
  )

################################################################################
# ESTIMATION
################################################################################ 

# OLS DiD without Mexican state-time fixed effects
ols_no_state_fe <- feols(
  log(remittances + 1) ~ i(period_date, florida_pct, ref = as.Date("2022-07-01")) | 
    cvegeo + period_date,
  data = analysis_data,
  cluster = ~cvegeo
)

# OLS DiD with Mexican state-time fixed effects
ols_with_state_fe <- feols(
  log(remittances + 1) ~ i(period_date, florida_pct, ref = as.Date("2022-07-01")) | 
    cvegeo + mx_state^period_date,
  data = analysis_data,
  cluster = ~cvegeo
)

# PPML DiD with Mexican state-time fixed effects
ppml_with_state_fe <- fepois(
  remittances ~ i(period_date, florida_pct, ref = as.Date("2022-07-01")) | 
    cvegeo + mx_state^period_date,
  data = analysis_data,
  cluster = ~cvegeo
)

# Table to show results
etable(
  ols_no_state_fe, ols_with_state_fe, ppml_with_state_fe,
  headers = c("OLS: No State-Time FE", "OLS: With State-Time FE", "PPML: With State-Time FE")
)

################################################################################
# SPATIAL CLUSTERING
################################################################################

# Base model
ppml_base <- fepois(
  remittances ~ i(period_date, florida_pct, ref = as.Date("2022-07-01")) | 
    cvegeo + mx_state^period_date,
  data = analysis_data
)

# Compute different spatial covariance structures using different cutoffs
ppml_conley_10  <- summary(ppml_base, vcov = conley(cutoff = 10))
ppml_conley_20  <- summary(ppml_base, vcov = conley(cutoff = 20))
ppml_conley_30  <- summary(ppml_base, vcov = conley(cutoff = 30))
ppml_conley_50  <- summary(ppml_base, vcov = conley(cutoff = 50))
ppml_conley_100 <- summary(ppml_base, vcov = conley(cutoff = 100))

################################################################################
# VISUALIZATION PREP
################################################################################

# Helper function to extract fixest event study results
event_study <- function(model_object, model_label) {
  coef_mat <- as.data.frame(fixest::coeftable(model_object))
  names(coef_mat) <- c("estimate", "std_error", "t_stat", "p_value")
  coef_mat$term <- rownames(coef_mat)
  
  # Filter only event study interaction terms
  coef_mat <- coef_mat[grepl("period_date", coef_mat$term), ]
  
  # Extract dates
  coef_mat$date_str <- stringr::str_extract(coef_mat$term, "\\d{4}-\\d{2}-\\d{2}")
  coef_mat$period_date <- as.Date(coef_mat$date_str)
  
  # Calculate 95% CI
  coef_mat$ci_low  <- coef_mat$estimate - (1.96 * coef_mat$std_error)
  coef_mat$ci_high <- coef_mat$estimate + (1.96 * coef_mat$std_error)
  coef_mat$model_label <- model_label
  
  # Add the omitted reference period back in
  ref_row <- data.frame(
    estimate = 0, std_error = 0, t_stat = NA, p_value = NA, term = "reference",
    date_str = "2022-07-01", period_date = as.Date("2022-07-01"),
    ci_low = 0, ci_high = 0, model_label = model_label
  )
  
  clean_df <- rbind(coef_mat, ref_row)
  clean_df <- clean_df[order(clean_df$period_date), ]
  
  # Convert regular dates to continuous quarterly variables for plotting
  clean_df$period_quarter <- zoo::as.yearqtr(clean_df$period_date)
  
  return(clean_df)
}

# Generic plotting function for single event studies
plot_event_study <- function(data, title, y_limits) {
  ggplot(data, aes(x = period_quarter, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.5) +
    geom_vline(xintercept = zoo::as.yearqtr(as.Date("2022-07-01")), 
               linetype = "dotted", color = "grey30", linewidth = 0.7) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), 
                  width = 0.05, linewidth = 0.6, color = "black") +
    geom_point(size = 1, color = "black") +
    scale_x_yearqtr(format = "%Y Q%q", breaks = seq(min(data$period_quarter), max(data$period_quarter), by = 0.25)) +
    scale_y_continuous(limits = y_limits) +
    labs(title = title, x = "Quarter", y = "Coefficient Effect")
}

################################################################################
# VISUALIZATION 1: THREE PLOTS SIDE-BY-SIDE
################################################################################

# Extract data
data_ols_no_fe   <- event_study(ols_no_state_fe, "OLS: No State-Time FE")
data_ols_with_fe <- event_study(ols_with_state_fe, "OLS: With State-Time FE")
data_ppml        <- event_study(ppml_with_state_fe, "PPML: State-Time FE")

# Construct plots
p1 <- plot_event_study(data_ols_no_fe, "OLS: No State-Time FE", c(-0.15, 0.15))
p2 <- plot_event_study(data_ols_with_fe, "OLS: With State-Time FE", c(-0.15, 0.15))
p3 <- plot_event_study(data_ppml, "PPML: State-Time FE", c(-0.15, 0.15))

# Combine using patchwork
combined_plot <- p1 + p2 + p3 + plot_layout(ncol = 3)

# Save plot
ggsave(
  filename = "figures-tables/municipality-inflows/muni_dids.png",
  plot = combined_plot,
  width = 15, height = 5, dpi = 300
)

################################################################################
# VISUALIZATION 2: SINGLE PPML MAIN SPECIFICATION
################################################################################

# Construct plot
ppml_single_plot <- plot_event_study(data_ppml, "PPML: State-Time FE", c(-0.02, 0.02))

# Save
ggsave(
  filename = "figures-tables/municipality-inflows/muni_did_ppml.png",
  plot = ppml_single_plot,
  width = 10, height = 5, dpi = 300
)

################################################################################
# VISUALIZATION 3: SPATIAL CLUSTERING SENSITIVITY
################################################################################

# Combine all spatial cutoffs into one dataframe
spatial_plot_data <- bind_rows(
  event_study(ppml_conley_10,  "10 km"),
  event_study(ppml_conley_20,  "20 km"),
  event_study(ppml_conley_30,  "30 km"),
  event_study(ppml_conley_50,  "50 km"),
  event_study(ppml_conley_100, "100 km")
)

# Lock in factor levels for legend ordering
spatial_plot_data$model_label <- factor(
  spatial_plot_data$model_label, 
  levels = c("10 km", "20 km", "30 km", "50 km", "100 km")
)

# Construct plot
spatial_gg <- ggplot(spatial_plot_data, aes(x = period_quarter, y = estimate, 
                                            color = model_label, shape = model_label)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_vline(xintercept = zoo::as.yearqtr(as.Date("2022-07-01")), 
             linetype = "dotted", color = "firebrick", linewidth = 0.7) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), 
                width = 0.04, position = position_dodge(width = 0.08), linewidth = 0.6) +
  geom_point(position = position_dodge(width = 0.08), size = 2.5) +
  scale_x_yearqtr(format = "%Y Q%q", 
                  breaks = seq(min(spatial_plot_data$period_quarter), 
                               max(spatial_plot_data$period_quarter), by = 0.25)) +
  scale_y_continuous(limits = c(-0.03, 0.03), breaks = seq(-0.03, 0.03, 0.01)) +
  scale_color_manual(values = c("blue", "purple", "darkgreen", "darkorange", "firebrick3")) +
  scale_shape_manual(values = c(16, 17, 15, 18, 19)) +
  labs(
    title = "PPML Event Study: Sensitivity to Spatial Cutoffs",
    subtitle = "Comparing Conley standard error adjustments across distance thresholds",
    x = "Quarter", y = "Coefficient Effect",
    color = "Spatial Cutoff", shape = "Spatial Cutoff"
  )

ggsave(
  filename = "figures-tables/municipality-inflows/muni_spatial_clustering.png",
  plot = spatial_gg,
  width = 11, height = 6, dpi = 300
)