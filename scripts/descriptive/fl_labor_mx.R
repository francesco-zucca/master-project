################################################################################
# DESCRIPTIVE EVIDENCE: MEXICAN-BORN FLORIDA WORKFORCE AND THE IAN SHOCK       #
#                                                                              #
#   0. Setup     libraries, theme, paths, shared B/W encodings, Ian marker     #
#   1. Mexican-born Florida panel (4 sectors)            [usa_00002]           #
#   2. Workforce by sector                                                     #
#   3. Construction workforce by citizenship status                            #
#   4. Construction: naturalized vs non-citizen (survey CI)                    #
#   5. Median wage income by sector                                            #
#   6. Non-citizen construction wage distribution (p25/median/p75)             #
#   7. All-birthplace Florida construction panel         [usa_00003]           #
#   8. Construction employment indexed by origin                               #
################################################################################

# ---- 0. Setup ----------------------------------------------------------------
library(ipumsr)
library(tidyverse)
library(srvyr)        # as_survey_design / survey_total; pulls in survey

# Global plot theme
source("figures-tables/theme.R")

# data and figures pathways
ddi2_path <- "data/IPUMS/usa_00002.xml"  # MX-born
ddi3_path <- "data/IPUMS/usa_00003.xml"  # all birthplaces
fig_dir   <- "figures-tables/descriptive"

ian_year <- 2022.75   # Ian landfall, on the continuous year axis

# Shared black-and-white encodings. Color + shape are mapped to the same series
# so the lines stay distinguishable in grayscale print.
bw_scale <- list(
  scale_color_manual(values = c("grey10", "grey35", "grey55", "grey75")),
  scale_shape_manual(values = c(16, 17, 15, 18))
)

# Reusable Ian marker (grey dotted line)
ian_marker <- geom_vline(xintercept = ian_year, linetype = "dotted",
                         color = "grey40", linewidth = 0.6)

# Year axis labels: flat 
flat_x <- theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

################################################################################
# ---- 1. Mexican-born Florida panel (4 sectors) ------------------------------#
################################################################################

ddi  <- read_ipums_ddi(ddi2_path)
data <- read_ipums_micro(ddi)

# documentation for relevant IND IPUMS codes: https://usa.ipums.org/usa/volii/ind2022.shtml

# Filter for only the four most affected sectors
florida_data <- data %>%
  filter(
    EMPSTAT == 1,                                   # employed
    IND %in% c(170, 180, 190,                       # agriculture
               770,                                 # construction
               8660, 8670, 8680, 8690,              # hospitality
               7770)                                # landscaping
  ) %>%
  mutate(
    sector = case_when(
      IND %in% 170:190                   ~ "Agriculture",
      IND == 770                         ~ "Construction",
      IND %in% c(8660, 8670, 8680, 8690) ~ "Hospitality",
      IND == 7770                        ~ "Landscaping"
    ),
    sector = factor(sector, levels = c("Agriculture", "Construction", "Hospitality",
                                       "Landscaping"))
  )

# TABLE 1: MX-Born Sector Shares in Florida, relative to within the four most affected sectors
florida_data_table <- florida_data %>%
  group_by(sector) %>%
  summarise(workers = sum(PERWT), .groups = "drop") %>%
  mutate(pct = workers / sum(workers) * 100) %>% 
  arrange(desc(pct))

florida_data_table

# save as .tex table
florida_data_table %>%
  transmute(
    Industry         = sector,
    `Workers (000s)` = round(workers / 1000, 1),
    `Share (\\%)`    = round(pct, 1)
  ) %>%
  knitr::kable(format = "latex", booktabs = TRUE, linesep = "", escape = FALSE) %>%
  writeLines(file.path(fig_dir, "tab_relative_industry_composition.tex"))

# TABLE 2: Overall MX-Born Sector Shares in Florida
industry_composition <- data %>%
  filter(EMPSTAT == 1) %>%
  mutate(division = case_when(
    IND == 7770        ~ "Landscaping services",
    IND == 7690        ~ "Cleaning services",
    IND %in% 170:290   ~ "Agriculture",
    IND %in% 370:490   ~ "Mining",
    IND %in% 570:690   ~ "Utilities",
    IND == 770         ~ "Construction",
    IND %in% 1070:3990 ~ "Manufacturing",
    IND %in% 4070:4590 ~ "Wholesale trade",
    IND %in% 4670:5790 ~ "Retail trade",
    IND %in% 6070:6390 ~ "Transportation and warehousing",
    IND %in% 6470:6780 ~ "Information",
    IND %in% 6870:7190 ~ "Finance, insurance, real estate",
    IND %in% 7270:7790 ~ "Professional, admin, waste",
    IND %in% 7860:8470 ~ "Education, health, social services",
    IND %in% 8560:8650 ~ "Arts, entertainment, accommodation, food",
    IND %in% 8660:8690 ~ "Hospitality",
    IND %in% 8770:9290 ~ "Other services",
    IND %in% 9370:9590 ~ "Public administration",
    IND %in% 9670:9870 ~ "Military",
    TRUE               ~ "Other / unclassified"
  )) %>%
  count(division, wt = PERWT, name = "workers") %>%
  mutate(share = workers / sum(workers) * 100,
         share = round(share, 1)) %>%
  arrange(desc(share))

industry_composition

# save as .tex table
industry_composition %>%
  transmute(
    Industry        = division,
    `Workers (000s)` = round(workers / 1000, 1),
    `Share (\\%)`    = round(share, 1)
  ) %>%
  knitr::kable(format = "latex", booktabs = TRUE, linesep = "", escape = FALSE) %>%
  writeLines(file.path(fig_dir, "tab_industry_composition.tex"))

################################################################################
# ---- 2. Workforce by sector -------------------------------------------------#
################################################################################

sector_ts <- florida_data %>%
  as_survey_design(weights = PERWT) %>%
  group_by(YEAR, sector) %>%
  summarise(workers = survey_total(vartype = "ci"), .groups = "drop")

p_sector <- ggplot(sector_ts, aes(YEAR, workers / 1000, color = sector, shape = sector)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  ian_marker +
  scale_x_continuous(breaks = 2021:2024) +
  bw_scale +
  labs(
    x = NULL, y = "Employed workers (thousands)", color = NULL, shape = NULL
  ) +
  flat_x

ggsave(file.path(fig_dir, "mexican_workers_FL.png"),
       p_sector, width = 5.5, height = 3.5, dpi = 300, bg = "white")

################################################################################
# ---- 3. Construction workforce by citizenship status ------------------------# 
################################################################################

construction_status <- florida_data %>%
  filter(sector == "Construction") %>%
  mutate(status = factor(
    case_when(
      CITIZEN == 1 ~ "Born abroad of US parents",
      CITIZEN == 2 ~ "Naturalized",
      CITIZEN == 3 ~ "Non-citizen"
    ),
    levels = c("Born abroad of US parents", "Naturalized", "Non-citizen")
  )) %>%
  group_by(YEAR, status) %>%
  summarise(workers = sum(PERWT), .groups = "drop")

p_status <- ggplot(construction_status, aes(YEAR, workers / 1000, color = status, shape = status)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  ian_marker +
  scale_x_continuous(breaks = 2021:2024) +
  bw_scale +
  labs(
    x = NULL, y = "Workers (thousands)", color = NULL, shape = NULL
  ) +
  flat_x

ggsave(file.path(fig_dir, "mexican_construction_citizenship.png"),
       p_status, width = 5.5, height = 3.5, dpi = 300, bg = "white")

################################################################################
# ---- 4. Construction: naturalized vs non-citizen (survey CI) ----------------#
################################################################################

construction_natnon <- florida_data %>%
  filter(sector == "Construction", CITIZEN %in% c(2, 3)) %>%
  mutate(status = factor(ifelse(CITIZEN == 2, "Naturalized", "Non-citizen"),
                         levels = c("Naturalized", "Non-citizen"))) %>%
  as_survey_design(weights = PERWT) %>%
  group_by(YEAR, status) %>%
  summarise(workers = survey_total(vartype = "ci"), .groups = "drop")

p_natnon <- ggplot(construction_natnon, aes(YEAR, workers / 1000, 
                                            color = status, shape = status)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  ian_marker +
  scale_x_continuous(breaks = 2021:2024) +
  bw_scale +
  labs(
    x = NULL, y = "Workers (thousands)", color = NULL, shape = NULL
  ) +
  flat_x

ggsave(file.path(fig_dir, "mexican_construction_natnon.png"),
       p_natnon, width = 5.5, height = 3.5, dpi = 300, bg = "white")

################################################################################
# ---- 5. Median wage income by sector ----------------------------------------#
################################################################################

wages_sector <- florida_data %>%
  filter(INCWAGE > 0, INCWAGE < 999999) %>%   # drop missing / topcoded
  group_by(YEAR, sector) %>%
  summarise(
    median_wage = matrixStats::weightedMedian(INCWAGE, PERWT, na.rm = TRUE),
    .groups = "drop"
  )

p_wages <- ggplot(wages_sector, aes(YEAR, median_wage, color = sector, shape = sector)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  ian_marker +
  scale_x_continuous(breaks = 2021:2024) +
  scale_y_continuous(labels = scales::dollar) +
  bw_scale +
  labs(
    x = NULL, y = "Median wage income (USD)", color = NULL, shape = NULL
  ) +
  flat_x

ggsave(file.path(fig_dir, "median_wages_FL_by_sector.png"),
       p_wages, width = 5.5, height = 3.5, dpi = 300, bg = "white")

################################################################################
# ---- 6. Non-citizen construction wage distribution --------------------------# 
################################################################################

wage_dist <- florida_data %>%
  filter(sector == "Construction", CITIZEN == 3, INCWAGE > 0, INCWAGE < 999999) %>%
  group_by(YEAR) %>%
  summarise(
    p25    = Hmisc::wtd.quantile(INCWAGE, weights = PERWT, probs = 0.25),
    median = Hmisc::wtd.quantile(INCWAGE, weights = PERWT, probs = 0.50),
    p75    = Hmisc::wtd.quantile(INCWAGE, weights = PERWT, probs = 0.75),
    .groups = "drop"
  ) %>%
  pivot_longer(c(p25, median, p75), names_to = "quantile", values_to = "wage") %>%
  mutate(quantile = factor(quantile,
                           levels = c("p75", "median", "p25"),
                           labels = c("75th percentile", "Median", "25th percentile")))

p_wagedist <- ggplot(wage_dist, aes(YEAR, wage, color = quantile, shape = quantile)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  ian_marker +
  scale_x_continuous(breaks = 2021:2024) +
  scale_y_continuous(labels = scales::dollar) +
  bw_scale +
  labs(
    x = NULL, y = "Wage income (USD)", color = NULL, shape = NULL
  ) +
  flat_x

ggsave(file.path(fig_dir, "non_citizen_wages_FL_construction.png"),
       p_wagedist, width = 5.5, height = 3.5, dpi = 300, bg = "white")

################################################################################
# ---- 7. All-birthplace Florida construction panel ---------------------------# 
################################################################################

ddi3  <- read_ipums_ddi(ddi3_path)
data3 <- read_ipums_micro(ddi3)

construction_origin <- data3 %>%
  filter(EMPSTAT == 1, IND == 770) %>%   # construction only
  mutate(origin = factor(
    case_when(
      BPL == 200            ~ "Mexican-born",
      BPL <= 99             ~ "US-born",
      BPL > 99 & BPL != 200 ~ "Other foreign-born"
    ),
    levels = c("Mexican-born", "Other foreign-born", "US-born")
  )) %>%
  group_by(YEAR, origin) %>%
  summarise(workers = sum(PERWT), .groups = "drop") %>%
  group_by(origin) %>%
  mutate(index = workers / workers[YEAR == 2021] * 100) %>%
  ungroup()

################################################################################
# ---- 8. Construction employment indexed by origin ---------------------------#
################################################################################

p_index <- ggplot(construction_origin, aes(YEAR, index, color = origin, shape = origin)) +
  geom_hline(yintercept = 100, color = "grey80") +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  ian_marker +
  scale_x_continuous(breaks = 2021:2024) +
  bw_scale +
  labs(
    x = NULL, y = "Index (2021 = 100)", color = NULL, shape = NULL
  ) +
  flat_x

construction_origin %>% 
  filter(origin == "Mexican-born")

ggsave(file.path(fig_dir, "FL_construction_indexed_by_origin.png"),
       p_index, width = 5.5, height = 3.5, dpi = 300, bg = "white")