################################################################################
# DIFF-IN-DIFF ESTIMATION OF MUNICIPALITY REMITTANCE INFLOWS (WITH SPILLOVERS)
################################################################################

# Load libraries
library(tidyverse)
library(fixest)
library(ggplot2)
library(knitr)

################################################################################
# DATA PREPARATION
################################################################################

# Set global plot theme
source("figures-tables/theme.R")

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

# Select relevant network exposure variables from migration
spillover_weights <- migration %>%
  select(mx_state, mx_municipality, Florida, Texas, California) 

# Merge using spatial names to attach geographic coordinates
master_panel <- remit_clean %>%
  inner_join(spillover_weights, by = c("mx_state", "mx_municipality"))

# Restrict timeline to -8/+8 quarters
shock_date      <- as.Date("2022-10-01")
unique_quarters <- sort(unique(master_panel$period_date))
shock_index     <- which(unique_quarters == shock_date)
master_panel    <- master_panel %>%
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
    # Baseline interactions for Texas and California spillovers
    fl_tx_interaction = florida_pct * texas_pct,
    fl_ca_interaction = florida_pct * cali_pct,
    
    # Recentered Texas variables for the percentile table
    tx_pct_p10 = texas_pct - tx_p10,
    tx_pct_p25 = texas_pct - tx_p25,
    tx_pct_p50 = texas_pct - tx_p50,
    tx_pct_p75 = texas_pct - tx_p75,
    tx_pct_p90 = texas_pct - tx_p90,
    
    # Recentered interactions
    fl_tx_int_p10 = florida_pct * tx_pct_p10,
    fl_tx_int_p25 = florida_pct * tx_pct_p25,
    fl_tx_int_p50 = florida_pct * tx_pct_p50,
    fl_tx_int_p75 = florida_pct * tx_pct_p75,
    fl_tx_int_p90 = florida_pct * tx_pct_p90
  )

################################################################################
# ESTIMATION 1: TEXAS VS CALIFORNIA SPILLOVERS
################################################################################

# Texas spillover
ppml_tx_base <- fepois(
  remittances ~ i(period_date, florida_pct, ref = as.Date("2022-07-01")) +
    i(period_date, texas_pct, ref = as.Date("2022-07-01")) +
    i(period_date, fl_tx_interaction, ref = as.Date("2022-07-01")) | 
    cvegeo + mx_state^period_date,
  data = analysis_data,
  vcov = ~cvegeo
)

# California spillover
ppml_ca_base <- fepois(
  remittances ~ i(period_date, florida_pct, ref = as.Date("2022-07-01")) +
    i(period_date, cali_pct, ref = as.Date("2022-07-01")) +
    i(period_date, fl_ca_interaction, ref = as.Date("2022-07-01")) | 
    cvegeo + mx_state^period_date,
  data = analysis_data,
  vcov = ~cvegeo
)

################################################################################
# TABLE 1: TEXAS VS CALIFORNIA SPILLOVERS (POST-SHOCK ONLY)
################################################################################

# Extract and format the Texas results into a dataframe
tx_df <- as.data.frame(coeftable(ppml_tx_base)) %>%
  rownames_to_column("term") %>%
  filter(grepl("fl_tx_interaction", term)) %>%
  mutate(
    raw_date = str_extract(term, "\\d{4}-\\d{2}-\\d{2}")
  ) %>%
  # Keep only the shock quarter (2022-10-01) and subsequent quarters
  filter(as.Date(raw_date) >= as.Date("2022-10-01")) %>%
  mutate(
    Date = format(zoo::as.yearqtr(as.Date(raw_date)), "%Y Q%q"),
    stars = case_when(
      `Pr(>|z|)` < 0.01 ~ "***",
      `Pr(>|z|)` < 0.05 ~ "**",
      `Pr(>|z|)` < 0.1  ~ "*",
      TRUE              ~ ""
    ),
    # Multiply by 100 for percentage terms, keep 2 decimal places
    Estimate_str = paste0(sprintf("%.2f", Estimate * 100), stars),
    SE_str       = paste0("(", sprintf("%.2f", `Std. Error` * 100), ")")
  ) %>%
  select(Date, Estimate_str, SE_str) %>%
  pivot_longer(cols = c(Estimate_str, SE_str), names_to = "type", values_to = "Texas")

# Extract and format the California results into a dataframe
ca_df <- as.data.frame(coeftable(ppml_ca_base)) %>%
  rownames_to_column("term") %>%
  filter(grepl("fl_ca_interaction", term)) %>%
  mutate(
    raw_date = str_extract(term, "\\d{4}-\\d{2}-\\d{2}")
  ) %>%
  # Keep only the shock quarter (2022-10-01) and subsequent quarters
  filter(as.Date(raw_date) >= as.Date("2022-10-01")) %>%
  mutate(
    Date = format(zoo::as.yearqtr(as.Date(raw_date)), "%Y Q%q"),
    stars = case_when(
      `Pr(>|z|)` < 0.01 ~ "***",
      `Pr(>|z|)` < 0.05 ~ "**",
      `Pr(>|z|)` < 0.1  ~ "*",
      TRUE              ~ ""
    ),
    Estimate_str = paste0(sprintf("%.2f", Estimate * 100), stars),
    SE_str       = paste0("(", sprintf("%.2f", `Std. Error` * 100), ")")
  ) %>%
  select(Date, Estimate_str, SE_str) %>%
  pivot_longer(cols = c(Estimate_str, SE_str), names_to = "type", values_to = "California")

# Merge and pivot columns
final_table_data <- tx_df %>%
  left_join(ca_df, by = c("Date", "type")) %>%
  
  # Stack into rows
  pivot_longer(cols = c(Texas, California), names_to = "Network", values_to = "Coefficient") %>%
  
  # Push Dates into columns
  pivot_wider(names_from = Date, values_from = Coefficient) %>%
  
  # Sort
  arrange(desc(Network), type) %>%
  
  # Blank out standard error row labels
  mutate(Network = ifelse(type == "SE_str", "", Network)) %>%
  select(-type)

# Wrap column names
rotated_colnames <- colnames(final_table_data)
rotated_colnames[-1] <- paste0("\\rotatebox{45}{", rotated_colnames[-1], "}")

# Setup dynamic column alignment
align_string <- paste0("l", strrep("c", ncol(final_table_data) - 1))

# Export dataset into latex
final_table_data %>%
  kable(
    format = "latex", 
    booktabs = TRUE, 
    align = align_string,
    col.names = rotated_colnames, 
    caption = "Network Spillovers: Texas vs. California (Post-Shock Periods). \\textit{Note: Coefficients and standard errors are multiplied by 100 to represent percentage effects.} \\label{tab:spillovers_tx_ca}",
    escape = FALSE 
  ) %>%
  cat(file = "figures-tables/spillovers/spillovers_tx_ca.tex")

################################################################################
# ESTIMATION 2: BETA1 FOR DIFFERENT RECENTERINGS OF TEXAS
################################################################################ 

# 10th percentile Texas exposure
ppml_tx_p10 <- fepois(
  remittances ~ i(period_date, florida_pct, ref = as.Date("2022-07-01")) +
    i(period_date, tx_pct_p10, ref = as.Date("2022-07-01")) +
    i(period_date, fl_tx_int_p10, ref = as.Date("2022-07-01")) | 
    cvegeo + mx_state^period_date,
  data = analysis_data,
  vcov = ~cvegeo
)

# 25th percentile Texas exposure
ppml_tx_p25 <- fepois(
  remittances ~ i(period_date, florida_pct, ref = as.Date("2022-07-01")) +
    i(period_date, tx_pct_p25, ref = as.Date("2022-07-01")) +
    i(period_date, fl_tx_int_p25, ref = as.Date("2022-07-01")) | 
    cvegeo + mx_state^period_date,
  data = analysis_data,
  vcov = ~cvegeo
)

# 50th percentile Texas exposure
ppml_tx_p50 <- fepois(
  remittances ~ i(period_date, florida_pct, ref = as.Date("2022-07-01")) +
    i(period_date, tx_pct_p50, ref = as.Date("2022-07-01")) +
    i(period_date, fl_tx_int_p50, ref = as.Date("2022-07-01")) | 
    cvegeo + mx_state^period_date,
  data = analysis_data,
  vcov = ~cvegeo
)

# 75th percentile Texas exposure
ppml_tx_p75 <- fepois(
  remittances ~ i(period_date, florida_pct, ref = as.Date("2022-07-01")) +
    i(period_date, tx_pct_p75, ref = as.Date("2022-07-01")) +
    i(period_date, fl_tx_int_p75, ref = as.Date("2022-07-01")) | 
    cvegeo + mx_state^period_date,
  data = analysis_data,
  vcov = ~cvegeo
)

# 90th percentile Texas exposure
ppml_tx_p90 <- fepois(
  remittances ~ i(period_date, florida_pct, ref = as.Date("2022-07-01")) +
    i(period_date, tx_pct_p90, ref = as.Date("2022-07-01")) +
    i(period_date, fl_tx_int_p90, ref = as.Date("2022-07-01")) | 
    cvegeo + mx_state^period_date,
  data = analysis_data,
  vcov = ~cvegeo
)

# Output table 2: net Florida shock at different levels of Texas exposure
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
# VISUALIZATION: 10th vs 90th exposure percentile
################################################################################

# Helper function to extract event study data
event_study <- function(model_object, model_label) {
  coef_mat <- as.data.frame(fixest::coeftable(model_object))
  names(coef_mat) <- c("estimate", "std_error", "t_stat", "p_value")
  coef_mat$term <- rownames(coef_mat)
  
  # Filter strictly to the main Florida terms
  coef_mat <- coef_mat[
    grepl("florida_pct", coef_mat$term) & !grepl("texas|interaction|tx_pct_p", 
                                                 tolower(coef_mat$term)), 
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

# Extract data using helper function
data_p10 <- event_study(ppml_tx_p10, "10th Pct: Low Texas Network")
data_p90 <- event_study(ppml_tx_p90, "90th Pct: High Texas Network")

# Combine into one plotting dataframe
percentile_plot_data <- bind_rows(data_p10, data_p90)

# Lock in factor levels so they appear in a logical order in the legend
percentile_plot_data$model_label <- factor(
  percentile_plot_data$model_label, 
  levels = c("10th Pct: Low Texas Network", "90th Pct: High Texas Network")
)

# Construct the divergence plot
percentile_gg <- ggplot(
  percentile_plot_data, 
  aes(x = period_quarter, y = estimate, 
      color = model_label, 
      shape = model_label,
      group = model_label)
) +
  
  # Baseline zero and shock vertical lines
  geom_hline(yintercept = 0, color = "grey20", linewidth = 0.5, linetype = "dotted") +
  geom_vline(xintercept = zoo::as.yearqtr(as.Date("2022-10-01")), 
             linetype = "dotted", color = "grey20", linewidth = 0.7) +
  
  # Confidence intervals
  geom_errorbar(data = subset(percentile_plot_data, 
                              period_quarter != zoo::as.yearqtr(as.Date("2022-07-01"))),
                aes(ymin = ci_low, ymax = ci_high), 
                width = 0.06, position = position_dodge(width = 0.1), 
                linewidth = 0.6, lineend = "square",
                color = "grey70") + 
  
  # Point estimates
  geom_point(aes(x = period_quarter, y = estimate), 
             position = position_dodge(width = 0.1), size = 2) +
  
  # X-axis formatting
  scale_x_yearqtr(format = "%Y Q%q", 
                  breaks = seq(min(percentile_plot_data$period_quarter), 
                               max(percentile_plot_data$period_quarter), by = 0.25)) +
  
  # Colors
  scale_color_manual(values = c("grey20", "grey20")) +
  
  # Shapes
  scale_shape_manual(values = c(16, 17)) +
  
  # Labels
  labs(
    title = "Divergent Remittance Responses by Network Density",
    subtitle = "Evaluating the Florida Shock at High vs. Low Texas Network Exposure",
    x = NULL, 
    y = "Coefficient",
    color = "Network exposure:",
    shape = "Network exposure:"
  ) + 
  theme(legend.position = "bottom")

# Save the plot
ggsave(
  filename = "figures-tables/spillovers/total_effect_texas_exposure.pdf",
  plot = percentile_gg,
  width = 11, height = 6, dpi = 300,
  device = cairo_pdf
)