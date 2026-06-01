################################################################################
# TWO RAW DATA DESCRIPTIVE PLOTS
################################################################################

# Load libraries
library(tidyverse)
library(ggplot2)
library(patchwork)
library(zoo)
library(ggrepel)

# Set global plot theme
source("figures-tables/theme.R")

################################################################################
# DATA PREPARATION PLOT 1: 
################################################################################

# Loading the data
migration <- read.csv("data/migration_matrix_rows.csv")
remit     <- read.csv("data/mx_muni_inflows.csv")
outflows  <- read.csv("data/us_state_outflows.csv")

# Converting remittance data outflows to unit dollars and formatting date variable
outflows_clean <- outflows %>%
  mutate(
    remittances = remittances_musd * 1000000,
    period_date = as.Date(period_date),
    group = ifelse(us_state == "Florida", "Florida", "Other States")
  ) %>%
  group_by(period_date, group) %>%
  summarise(
    remittances = mean(remittances),
    .groups = "drop"
  )

################################################################################
# DATA PREPARATION PLOT 2:
################################################################################

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
  )

# CREATE EXPOSURE QUARTILES

# Create quartiles
municipality_exposure <- master_panel %>%
  distinct(cvegeo, Florida) %>%
  mutate(
    exposure_rank = percent_rank(Florida),
    
    exposure_group = case_when(
      exposure_rank <= 0.10 ~ "Bottom 10%",
      exposure_rank <= 0.40 ~ "Low-middle 30%",
      exposure_rank <= 0.90 ~ "High-middle 50%",
      exposure_rank >  0.90 ~ "Top 10%"
    ),
    
    exposure_group = factor(
      exposure_group,
      levels = c(
        "Bottom 10%",
        "Low-middle 30%",
        "High-middle 50%",
        "Top 10%"
      )
    )
  )

master_panel_exposure_groups <- master_panel %>%
  left_join(
    municipality_exposure %>%
      select(cvegeo, exposure_group),
    by = "cvegeo"
  )

plot_data <- master_panel_exposure_groups %>%
  group_by(period_date, exposure_group) %>%
  summarise(
    remittances = sum(remittances, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(exposure_group) %>%
  mutate(
    shock_value = remittances[period_date == shock_date],
    index = remittances / shock_value * 100
  ) %>%
  ungroup()

################################################################################
# PLOT 1
################################################################################

# Total remittances outflows from Florida vs other states

ggplot(outflows_clean, aes(x = period_date, y = remittances, color = group)) +
  geom_line(linewidth = 1) +
  
  scale_y_continuous(
  labels = scales::label_number(scale = 1e-6, comma = TRUE)
) +
  geom_vline(
    xintercept = shock_date,
    linetype = "dashed"
  ) +
  
  labs(
    title = "Total Remittance Outflows from Florida vs Other States",
    x = "Date",
    y = "Remittances (Million USD)"
  ) +
  
  scale_color_manual(
    values = c(
      "Florida" = "blue",
      "Other States" = "grey50"
    )
  ) +
  
  theme(legend.title = element_blank())

ggsave("figures-tables/state-outflows/total_remittance_outflows.png", width = 8, height = 5)

#################################################################################
# PLOT 2
#################################################################################

# Total remittances inflows from Florida depending on exposure

ggplot(plot_data,
       aes(x = period_date,
           y = index,
           color = exposure_group)) +
  
  geom_line(linewidth = 1) +
  
  geom_vline(
    xintercept = shock_date,
    linetype = "dashed"
  ) +
  
  labs(
  title = "Indexed Remittances by Florida Exposure Group",
  subtitle = "Series indexed to Hurricane Ian quarter (=100)",
  x = "Date",
  y = "Index (Hurricane Ian Quarter = 100)",
  color = "Exposure Group"
  )

ggsave("figures-tables/municipality-inflows/indexed_remittances_by_florida_exposure_quartile.png", width = 8, height = 5)

################################################################################
# NETWORK PERSISTENCE DECAY: ALL BASE YEARS
################################################################################

# Folder containing yearly migration matrices
files <- list.files(
  path = "data/yearly-migration-matrices",
  pattern = "\\.csv$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop("No yearly migration matrices found in data/yearly-migration-matrices")
}

# Read and stack all yearly migration matrices
migration_long <- map_dfr(files, function(file) {
  
  year_file <- str_extract(basename(file), "\\d{4}") %>% as.integer()
  
  read.csv(file, check.names = FALSE) %>%
    mutate(year = year_file) %>%
    pivot_longer(
      cols = -c(mx_state, mx_municipality, year),
      names_to = "us_state",
      values_to = "weight"
    )
})

# Compute correlations using every available year as the base year
base_years <- sort(unique(migration_long$year))

persistence_data <- map_dfr(base_years, function(base_year) {
  
  baseline <- migration_long %>%
    filter(year == base_year) %>%
    select(mx_state, mx_municipality, us_state, base_weight = weight)
  
  migration_long %>%
    left_join(
      baseline,
      by = c("mx_state", "mx_municipality", "us_state")
    ) %>%
    group_by(year) %>%
    summarise(
      correlation = cor(base_weight, weight, use = "complete.obs"),
      .groups = "drop"
    ) %>%
    mutate(
      base_year = base_year,
      years_from_base = year - base_year
    )
})

# Create custom color groups
persistence_data <- persistence_data %>%
  mutate(
    highlight_group = case_when(
      base_year == 2011 ~ "2011",
      base_year == 2016 ~ "2016",
      TRUE ~ "Other Years"
    )
  )

# Data for labels
label_data <- persistence_data %>%
  filter(
    base_year %in% c(2011, 2016),
    year == max(year)
  )

ggplot(
  persistence_data,
  aes(
    x = year,
    y = correlation,
    group = factor(base_year),
    color = highlight_group
  )
) +
  
  geom_line(linewidth = 0.8, alpha = 0.75) +
  geom_point(size = 1.5, alpha = 0.8) +
  
  geom_label_repel(
    data = label_data,
    aes(label = base_year),
    show.legend = FALSE,
    nudge_x = 0.6,
    size = 4
  ) +
  
  scale_color_manual(
    values = c(
      "Other Years" = "grey50",
      "2011" = "blue",
      "2016" = "blue"
    ),
    breaks = c("2011", "2016"),
    labels = c("2011 Base Year", "2016 Base Year")
  ) +
  
  scale_x_continuous(breaks = base_years) +
  
  scale_y_continuous(
    limits = c(0.70, 1),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  
  labs(
    title = "Migration Network Persistence Across Base Years",
    subtitle = "Correlation of yearly migration matrices using each year as the base",
    x = "Comparison Year",
    y = "Correlation with Base-Year Matrix",
    color = NULL
  ) +
  
  theme(
    legend.position = "bottom"
  )

ggsave("figures-tables/municipality-inflows/network_persistence_all_base_years.png", width = 8, height = 5)

##### ###########################################################################

# percetages needed for section 5.1


