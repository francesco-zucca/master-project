################################################################################
# DIFF-IN-DIFF ESTIMATION OF MUNICIPALITY REMITTANCE INFLOWS
################################################################################

# Load libraries
library(tidyverse)
library(fixest)

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

# Extracting the Florida, Texas, and California exposure variables and merging into a final panel
spillover_weights <- migration %>%
  select(mx_state, mx_municipality, Florida, Texas, California)
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

# Create IDs, convert weights to percentage points, recenter Texas, and generate cross-state interaction terms
analysis_data <- master_panel %>%
  mutate(
    unit_id = paste(mx_state, mx_municipality, sep = "_"),
    florida_pct = Florida * 100,
    texas_pct = Texas * 100,
    cali_pct = California * 100,
    
    # Recenter Texas exposure around the mean
    texas_pct_centered = texas_pct - mean(texas_pct, na.rm = TRUE),
    
    # Recenter California exposure around the mean
    cali_pct_centered = cali_pct - mean(cali_pct, na.rm = TRUE),
    
    # Rebuild the interaction term using the centered variable
    fl_tx_interaction_cent = florida_pct * texas_pct_centered,
    
    # Leave California as is unless you want to recenter it too)
    fl_ca_interaction_cent = florida_pct * cali_pct_centered
  )

################################################################################
# ESTIMATION
################################################################################ 

# PPML DiD with Texas network spillover effects
ppml_spillover_tx <- fepois(
  remittances ~ i(period_date, florida_pct, ref = as.Date("2022-07-01")) +
    i(period_date, texas_pct, ref = as.Date("2022-07-01")) +
    i(period_date, fl_tx_interaction_cent, ref = as.Date("2022-07-01")) | 
    unit_id + mx_state^period_date,
  data = analysis_data,
  cluster = ~unit_id
)

# PPML DiD with California network spillover effects
ppml_spillover_ca <- fepois(
  remittances ~ i(period_date, florida_pct, ref = as.Date("2022-07-01")) +
    i(period_date, cali_pct, ref = as.Date("2022-07-01")) +
    i(period_date, fl_ca_interaction_cent, ref = as.Date("2022-07-01")) | 
    unit_id + mx_state^period_date,
  data = analysis_data,
  cluster = ~unit_id
)

# Table to show results
etable(
  ppml_spillover_tx, ppml_spillover_ca,
  headers = c("PPML: Texas Spillover", "PPML: California Spillover")
)

################################################################################
# VISUALIZATION
################################################################################

# Displaying 2 plots side-by-side
par(mfrow = c(1, 2))

# Start png for saving
png("figures-tables/spillovers/spillovers_effect.png", 
    width = 12, height = 5, units = "in", res = 300)

# PPML Texas Spillover Interaction Effect
iplot(
  ppml_spillover_tx,
  i.select = 3,
  main = "Texas Spillover Effect",
  xlab = "Quarter", 
  ylab = "Coefficient Effect",
  ylim = c(-0.005, 0.005),
  ref.line = 9,
  ref.line.par = list(col = "firebrick3", lty = 2) 
)

# PPML California Spillover Interaction Effect
iplot(
  ppml_spillover_ca,
  i.select = 3,
  main = "California Spillover Effect",
  xlab = "Quarter", 
  ylab = "Coefficient Effect",
  ylim = c(-0.005, 0.005),
  ref.line = 9,
  ref.line.par = list(col = "firebrick3", lty = 2)
)

# Saving file
dev.off()

# Start png for saving
png("figures-tables/spillovers/hurricane_effect.png", 
    width = 12, height = 5, units = "in", res = 300)

# Shock effect accounting for spillovers (Texas)
iplot(
  ppml_spillover_tx,
  i.select = 1,
  main = "Hurricane effect on remittance inflows (average Texas exposure)",
  xlab = "Quarter", 
  ylab = "Coefficient Effect",
  ylim = c(-0.1, 0.1),
  ref.line = 9,
  ref.line.par = list(col = "firebrick3", lty = 2) 
)

# Shock effect accounting for spillovers (California)
iplot(
  ppml_spillover_ca,
  i.select = 1,
  main = "Hurricane effect on remittance inflows (average California exposure)",
  xlab = "Quarter", 
  ylab = "Coefficient Effect",
  ylim = c(-0.1, 0.1),
  ref.line = 9,
  ref.line.par = list(col = "firebrick3", lty = 2)
)

# Saving file
dev.off()

# Resetting the plotting layout back to standard
par(mfrow = c(1, 1))