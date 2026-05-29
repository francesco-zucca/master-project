################################################################################
# DIFF-IN-DIFF ESTIMATION OF MUNICIPALITY REMITTANCE INFLOWS (WITH SPILLOVERS)
################################################################################

# Load libraries
library(tidyverse)
library(fixest)
library(ggplot2)

################################################################################
# DATA PREPARATION
################################################################################

# Set global plot theme
source("figures-tables/theme.R")

# Loading the data
migration <- read.csv("data/migration_matrix_rows.csv")
remit     <- read.csv("data/mx_muni_inflows.csv")                                                                                                                                                                                                                                                                                                  

# Converting remittance data to unit dollars and formatting date variable
# Note: Keeping cvegeo, lon, and lat in the select statement
remit_clean <- remit %>%
  mutate(
    remittances = remittances_musd * 1000000,
    period_date = as.Date(period_date)
  ) %>%
  select(-remittances_musd, -c(year, quarter))

# Select relevant network exposure variables from migration (Florida, Texas, California)
spillover_weights <- migration %>%
  select(mx_state, mx_municipality, Florida, Texas, California) 

# Merge using spatial names to pull migration weights into the panel with cvegeo/coordinates
master_panel <- remit_clean %>%
  inner_join(spillover_weights, by = c("mx_state", "mx_municipality"))

# Restrict timeline 
shock_date <- as.Date("2022-10-01")
unique_quarters <- sort(unique(master_panel$period_date))
shock_index <- which(unique_quarters == shock_date)

# Restrict timeline to -8/+8 quarters
master_panel <- master_panel %>%
  mutate(
    rel_quarter = match(period_date, unique_quarters) - shock_index
  ) %>%
  filter(rel_quarter >= -8 & rel_quarter <= 8)

# Convert weights to percentage points
analysis_data <- master_panel %>%
  mutate(
    florida_pct = Florida * 100,
    texas_pct   = Texas * 100,
    cali_pct    = California * 100
  )

# Calculate percentiles for Texas network density
tx_p10 <- quantile(analysis_data$texas_pct, probs = 0.10, na.rm = TRUE)
tx_p25 <- quantile(analysis_data$texas_pct, probs = 0.25, na.rm = TRUE)
tx_p50 <- quantile(analysis_data$texas_pct, probs = 0.50, na.rm = TRUE)
tx_p75 <- quantile(analysis_data$texas_pct, probs = 0.75, na.rm = TRUE)
tx_p90 <- quantile(analysis_data$texas_pct, probs = 0.90, na.rm = TRUE)

# Create recentered variables and interaction terms
analysis_data <- analysis_data %>%
  mutate(
    # 1. Base interactions for Texas and California spillovers
    fl_tx_interaction = florida_pct * texas_pct,
    fl_ca_interaction = florida_pct * cali_pct,
    
    # 2. Recentered Texas variables for the percentile table
    tx_pct_p10 = texas_pct - tx_p10,
    tx_pct_p25 = texas_pct - tx_p25,
    tx_pct_p50 = texas_pct - tx_p50,
    tx_pct_p75 = texas_pct - tx_p75,
    tx_pct_p90 = texas_pct - tx_p90,
    
    # 3. Recentered interactions
    fl_tx_int_p10 = florida_pct * tx_pct_p10,
    fl_tx_int_p25 = florida_pct * tx_pct_p25,
    fl_tx_int_p50 = florida_pct * tx_pct_p50,
    fl_tx_int_p75 = florida_pct * tx_pct_p75,
    fl_tx_int_p90 = florida_pct * tx_pct_p90
  )

################################################################################
# ESTIMATION 1: THE TEXAS PERCENTILE TABLE (Municipality Clustered SEs)
################################################################################ 

# Note on SEs: Clustering standard errors at the municipality level (cvegeo).
# Fixed effects explicitly utilize the bulletproof 'cvegeo' variable.

# Texas Network at 10th Percentile
ppml_tx_p10 <- fepois(
  remittances ~ i(period_date, florida_pct, ref = as.Date("2022-07-01")) +
    i(period_date, tx_pct_p10, ref = as.Date("2022-07-01")) +
    i(period_date, fl_tx_int_p10, ref = as.Date("2022-07-01")) | 
    cvegeo + mx_state^period_date,
  data = analysis_data,
  vcov = ~cvegeo
)

# Texas Network at 25th Percentile
ppml_tx_p25 <- fepois(
  remittances ~ i(period_date, florida_pct, ref = as.Date("2022-07-01")) +
    i(period_date, tx_pct_p25, ref = as.Date("2022-07-01")) +
    i(period_date, fl_tx_int_p25, ref = as.Date("2022-07-01")) | 
    cvegeo + mx_state^period_date,
  data = analysis_data,
  vcov = ~cvegeo
)

# Texas Network at 50th Percentile (Median)
ppml_tx_p50 <- fepois(
  remittances ~ i(period_date, florida_pct, ref = as.Date("2022-07-01")) +
    i(period_date, tx_pct_p50, ref = as.Date("2022-07-01")) +
    i(period_date, fl_tx_int_p50, ref = as.Date("2022-07-01")) | 
    cvegeo + mx_state^period_date,
  data = analysis_data,
  vcov = ~cvegeo
)

# Texas Network at 75th Percentile
ppml_tx_p75 <- fepois(
  remittances ~ i(period_date, florida_pct, ref = as.Date("2022-07-01")) +
    i(period_date, tx_pct_p75, ref = as.Date("2022-07-01")) +
    i(period_date, fl_tx_int_p75, ref = as.Date("2022-07-01")) | 
    cvegeo + mx_state^period_date,
  data = analysis_data,
  vcov = ~cvegeo
)

# Texas Network at 90th Percentile
ppml_tx_p90 <- fepois(
  remittances ~ i(period_date, florida_pct, ref = as.Date("2022-07-01")) +
    i(period_date, tx_pct_p90, ref = as.Date("2022-07-01")) +
    i(period_date, fl_tx_int_p90, ref = as.Date("2022-07-01")) | 
    cvegeo + mx_state^period_date,
  data = analysis_data,
  vcov = ~cvegeo
)

# Output Table 1: Net Florida Shock evaluated at different levels of Texas network density
etable(
  ppml_tx_p10, ppml_tx_p25, ppml_tx_p50, ppml_tx_p75, ppml_tx_p90,
  headers = c("10th Pct", "25th Pct", "50th Pct", "75th Pct", "90th Pct"),
  keep = "florida_pct",
  file = "figures-tables/spillovers/spillovers_texas_percentiles.tex",
  replace = TRUE,
  title = "Net Florida Shock Evaluated at Texas Network Percentiles",
  label = "tab:texas_percentiles"
)

################################################################################
# ESTIMATION 2: TEXAS VS CALIFORNIA SPILLOVERS (Municipality Clustered SEs)
################################################################################

# 1. Base Texas Spillover Effect
ppml_tx_base <- fepois(
  remittances ~ i(period_date, florida_pct, ref = as.Date("2022-07-01")) +
    i(period_date, texas_pct, ref = as.Date("2022-07-01")) +
    i(period_date, fl_tx_interaction, ref = as.Date("2022-07-01")) | 
    cvegeo + mx_state^period_date,
  data = analysis_data,
  vcov = ~cvegeo
)

# 2. Base California Spillover Effect
ppml_ca_base <- fepois(
  remittances ~ i(period_date, florida_pct, ref = as.Date("2022-07-01")) +
    i(period_date, cali_pct, ref = as.Date("2022-07-01")) +
    i(period_date, fl_ca_interaction, ref = as.Date("2022-07-01")) | 
    cvegeo + mx_state^period_date,
  data = analysis_data,
  vcov = ~cvegeo
)

# Output Table 2: Comparing Network Spillovers between Texas and California
etable(
  ppml_tx_base, ppml_ca_base,
  headers = c("Texas Network", "California Network"),
  keep = "interaction", # Filters down strictly to the spillover interaction terms
  file = "figures-tables/spillovers/spillovers_tx_ca.tex",
  replace = TRUE,
  title = "Network Spillovers: Texas vs. California",
  label = "tab:spillovers_tx_ca"
)

################################################################################
# VISUALIZATION: THE NETWORK DIVERGENCE (10th vs 90th Percentile)
################################################################################

event_study <- function(model_object, model_label) {
  coef_mat <- as.data.frame(fixest::coeftable(model_object))
  names(coef_mat) <- c("estimate", "std_error", "t_stat", "p_value")
  coef_mat$term <- rownames(coef_mat)
  
  # Filter strictly to the main Florida terms
  coef_mat <- coef_mat[
    grepl("florida_pct", coef_mat$term) & !grepl("texas|interaction|tx_pct_p", tolower(coef_mat$term)), 
  ]
  
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

# 1. Extract data using helper function
data_p10 <- event_study(ppml_tx_p10, "10th Pct: Low Texas Network")
data_p90 <- event_study(ppml_tx_p90, "90th Pct: High Texas Network")

# 2. Combine into one plotting dataframe
percentile_plot_data <- bind_rows(data_p10, data_p90)

# Lock in factor levels so they appear in a logical order in the legend
percentile_plot_data$model_label <- factor(
  percentile_plot_data$model_label, 
  levels = c("10th Pct: Low Texas Network", "90th Pct: High Texas Network")
)

# 3. Construct the Divergence Plot
percentile_gg <- ggplot(
  percentile_plot_data, aes(x = period_quarter, 
                            y = estimate, color = model_label)) +
  
  # Baseline zero and shock vertical lines
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.5) +
  geom_vline(xintercept = zoo::as.yearqtr(as.Date("2022-10-01")), 
             linetype = "dotted", color = "grey20", linewidth = 0.7) +
  
  # Confidence intervals (dodged so they don't overlap)
  geom_errorbar(data = subset(percentile_plot_data, 
                              period_quarter != zoo::as.yearqtr(as.Date("2022-07-01"))),
                aes(ymin = ci_low, ymax = ci_high), 
                width = 0.06, position = position_dodge(width = 0.1), 
                linewidth = 0.6, lineend = "square") +
  
  # Point estimates (dodged to match the error bars perfectly)
  geom_point(aes(x = period_quarter, y = estimate), 
             position = position_dodge(width = 0.1), size = 1.5) +
  
  # X-axis formatting matching your script
  scale_x_yearqtr(format = "%Y Q%q", 
                  breaks = seq(min(percentile_plot_data$period_quarter), 
                               max(percentile_plot_data$period_quarter), by = 0.25)) +
  
  # High-contrast, colorblind-friendly grayscale colors from your latest update
  scale_color_manual(values = c("grey40", "grey20")) +
  
  labs(
    title = "Divergent Remittance Responses by Network Density",
    subtitle = "Evaluating the Florida Shock at High vs. Low Texas Network Exposure",
    x = "Quarter", y = "Coefficient Effect",
    color = "Network Exposure:"
  ) + 
  
  theme(legend.position = "bottom")

# 4. Save the plot to your figures folder
ggsave(
  filename = "figures-tables/spillovers/muni_network_divergence_muni.png",
  plot = percentile_gg,
  width = 11, height = 6, dpi = 300
)