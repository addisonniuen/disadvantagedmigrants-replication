# ==============================================================================
# 04_eda.R
#
# Sample checks for the three analysis blocks. Nothing here is used in the
# thesis; the checks print tables to the console. It:
#   - Block 1: describes the owner shares and checks the workplace types, the
#     owner shares and where the interactions get their variation
#   - Block 2: checks the cells behind each interaction and the samples
#   - Block 3: checks the risk set, missing values, entry counts and the
#     English-proficiency coding
#
# It inspects the estimation samples and models, so it is run after the
# analysis scripts (it is not part of 00_runall.R):
#   source("project/code/01_setup.R")
#   source("project/code/02_datacleaning.R")
#   source("project/code/03_featureengineering.R")
#   source("project/code/05_analysisblock1.R")
#   source("project/code/06_analysisblock2.R")
#   source("project/code/07_analysisblock3.R")
# ==============================================================================

# --- Helper -------------------------------------------------------------------

show_table <- function(df, caption, ...) {
  knitr::kable(df, caption = caption, format.args = list(big.mark = ","), ...)
}

# --- Block 1: employee wages --------------------------------------------------

# Variables and factor levels of the estimation sample
glimpse(data_regression_1)
lapply(data_regression_1[sapply(data_regression_1, is.factor)], levels)

# Distribution of the owner shares (Type 1) and the all-migrant share
data_regression_1 |>
  select(
    loo_mes_1d_type1,
    loo_mnes_higheng_1d_type1,
    loo_mnes_loweng_1d_type1,
    loo_migrant_share_1d
  ) |>
  pivot_longer(everything(), names_to = "Share", values_to = "value") |>
  filter(!is.na(value)) |>
  summarise(
    Mean = mean(value),
    SD = sd(value),
    P10 = quantile(value, .10),
    Median = median(value),
    P90 = quantile(value, .90),
    Max = max(value),
    `% zero` = 100 * mean(value == 0),
    .by = Share
  ) |>
  knitr::kable(
    digits = 4,
    caption = "Distribution of the leave-one-out owner shares"
  )

# Variation in the shares across industry-year cells
data_regression_1 |>
  summarise(
    across(
      c(loo_mes_1d_type1, loo_mnes_higheng_1d_type1, loo_mnes_loweng_1d_type1),
      mean
    ),
    .by = c(industry_19categories_jbmi61, year)
  ) |>
  summarise(
    n_cells = n(),
    across(
      c(loo_mes_1d_type1, loo_mnes_higheng_1d_type1, loo_mnes_loweng_1d_type1),
      list(sd = sd, n_positive = ~ sum(.x > 0, na.rm = TRUE))
    )
  ) |>
  knitr::kable(
    digits = 4,
    caption = "Between-cell variation in the three shares"
  )

# Employees by small single-site status under the 20, 10 and 50 thresholds
data_regression_1 |>
  count(small_single_site, small_single_site_10, small_single_site_50)

# Check 1: each employee belongs to at most one of the three workplace types
data_regression_1 |>
  filter(!is.na(small_single_site)) |>
  mutate(
    row_sum = coalesce(small_single_site, 0L) +
      coalesce(chain_like, 0L) +
      coalesce(small_site_chain, 0L)
  ) |>
  count(row_sum) |>
  mutate(
    meaning = case_when(
      row_sum == 0 ~ "base firm type (large/other) -- the omitted reference",
      row_sum == 1 ~ "exactly one of the three dummies = 1 (correct)",
      row_sum >
        1 ~ "ERROR: belongs to >1 firm type, dummies not mutually exclusive"
    ),
    pct = sprintf("%.1f%%", 100 * n / sum(n))
  ) |>
  knitr::kable(
    col.names = c("Sum of 3 dummies", "N", "Interpretation", "Share"),
    caption = "Firm-type partition check: each observation belongs to exactly one type",
    align = "rllr"
  )

# Check 2: the three owner shares are non-negative and sum to at most one
data_regression_1 |>
  summarise(
    across(
      c(loo_mes_1d_type0, loo_mnes_higheng_1d_type0, loo_mnes_loweng_1d_type0),
      mean
    ),
    .by = c(industry_19categories_jbmi61, year)
  ) |>
  summarise(
    n_cells = n(),
    max_sum = max(
      loo_mes_1d_type0 + loo_mnes_higheng_1d_type0 + loo_mnes_loweng_1d_type0,
      na.rm = TRUE
    ),
    n_sum_gt_1 = sum(
      loo_mes_1d_type0 + loo_mnes_higheng_1d_type0 + loo_mnes_loweng_1d_type0 >
        1 + 1e-9,
      na.rm = TRUE
    ),
    n_negative = sum(
      pmin(
        loo_mes_1d_type0,
        loo_mnes_higheng_1d_type0,
        loo_mnes_loweng_1d_type0
      ) <
        -1e-9,
      na.rm = TRUE
    )
  ) |>
  knitr::kable(
    caption = "Share partition check: the three group shares plus native ownership exhaust the cell",
    digits = 6
  )

# Check 3: small single-site employees with a positive share of each group
data_regression_1 |>
  filter(small_single_site == 1) |>
  summarise(
    `Treated obs (small single-site)` = n(),
    `With MES > 0` = sum(loo_mes_1d_type0 > 0, na.rm = TRUE),
    `With high-English > 0` = sum(loo_mnes_higheng_1d_type0 > 0, na.rm = TRUE),
    `With low-English > 0` = sum(loo_mnes_loweng_1d_type0 > 0, na.rm = TRUE),
    `% with low-English > 0` = sprintf(
      "%.1f%%",
      100 * mean(loo_mnes_loweng_1d_type0 > 0, na.rm = TRUE)
    )
  ) |>
  knitr::kable(
    caption = "Identifying mass behind each interaction term (small single-site sample)",
    format.args = list(big.mark = ",")
  )

# Check 4: empty cells arrive as NA, not as a zero migrant share
data_regression_1 |>
  count(
    cell_empty = coalesce(n_owners_1d_type0, 0L) == 0L,
    share_na = is.na(loo_mnes_loweng_1d_type0)
  ) |>
  knitr::kable(caption = "Empty industry-year cells are missing, not zero")

# Check 5: rows dropped from Version 1 (singletons), by industry
used_rows <- obs(regression1_v1a)
data_regression_1[-used_rows, ] |>
  count(industry_19categories_jbmi61, sort = TRUE)

# Check 6: people who change workplace type, and occupation group, over the panel
data_regression_1 |>
  filter(
    !is.na(small_single_site),
    !is.na(loo_mnes_loweng_1d_type0),
    !is.na(occupation_agent_plausible_subgroup)
  ) |>
  summarise(
    switches_firm_type = n_distinct(small_single_site) > 1,
    switches_occ_group = n_distinct(occupation_agent_plausible_subgroup) > 1,
    .by = xwaveid
  ) |>
  summarise(
    n_people = n(),
    n_firm_switchers = sum(switches_firm_type),
    n_occ_movers = sum(switches_occ_group),
    n_both = sum(switches_firm_type & !switches_occ_group)
  ) |>
  knitr::kable(
    caption = "Switchers identifying the occupation split",
    format.args = list(big.mark = ",")
  )

# Check 7: small single-site employees with a positive low-English share, by occupation subgroup
data_regression_1 |>
  filter(small_single_site == 1, !is.na(occupation_agent_plausible_subgroup)) |>
  summarise(
    n = n(),
    n_persons = n_distinct(xwaveid),
    n_cells = n_distinct(paste(industry_19categories_jbmi61, year)),
    `low-Eng > 0` = sum(loo_mnes_loweng_1d_type0 > 0, na.rm = TRUE),
    `% low-Eng > 0` = sprintf(
      "%.1f%%",
      100 * mean(loo_mnes_loweng_1d_type0 > 0, na.rm = TRUE)
    ),
    .by = occupation_agent_plausible_subgroup
  ) |>
  knitr::kable(
    caption = "Identifying mass by occupation subgroup, small single-site employees",
    format.args = list(big.mark = ",")
  )

# Check 8: small single-site employees by origin
data_regression_1 |>
  filter(small_single_site == 1, !is.na(origin_group)) |>
  summarise(n = n(), n_persons = n_distinct(xwaveid), .by = origin_group) |>
  knitr::kable(
    caption = "Employee origin composition, small single-site employees",
    format.args = list(big.mark = ",")
  )

# --- Block 2: owner income and risk -------------------------------------------

# Volatility is bounded between 0 and 200
stopifnot(all(data_regression_2_pairs$abs_arc_pct <= 200))
stopifnot(all(data_regression_2_unrestricted$abs_arc_pct <= 200))

# Cells and raw outcomes behind each owner type x origin interaction
cells_b2 |>
  show_table("Cells and outcomes by owner type and origin group", digits = 3)

# Workplace type against workplace size (should match almost exactly)
data_regression_2_pairs |>
  count(firm_type_pair, small_wp_pair) |>
  pivot_wider(names_from = small_wp_pair, values_from = n, values_fill = 0) |>
  show_table("Firm type by small workplace indicator")

# Share of each cell whose status at t-1 differed from its status at t
data_regression_2_unrestricted |>
  summarise(
    person_pairs = n(),
    share_changed_status = mean(is.na(status_l1) | status_l1 != status),
    .by = c(status, small_wp_pair, origin_group)
  ) |>
  arrange(status, small_wp_pair, origin_group) |>
  show_table("Share whose status at t-1 differed from status at t", digits = 3)

# Employer contrasts, unrestricted vs restricted sample
contrasts_b2 |>
  filter(owner_term != "own_acc") |>
  select(outcome, owner_term, group, comparison, sample, estimate, se, mde) |>
  pivot_wider(names_from = sample, values_from = c(estimate, se, mde)) |>
  show_table("Employer contrasts, unrestricted vs restricted sample", digits = 3)

# --- Block 3: entry into business ownership -----------------------------------

# Where the rows of the feature-engineering entry variable come from
panel_block3 |>
  filter(!is.na(entry_owner_fe)) |>
  count(
    status_at_t1 = case_when(
      is.na(owner_l1) ~ "no interview in previous wave",
      is.na(unemployed_l1) ~ "enumerated but not interviewed",
      owner_l1 == 1 ~ "already owner (not at risk)",
      TRUE ~ "valid: non-owner at t-1"
    )
  ) |>
  knitr::kable(
    caption = "Rows with the old entry variable, by status at t-1",
    format.args = list(big.mark = ",")
  )

# Risk set before rows with missing model variables are dropped (a separate
# object, so data_regression_3 is not changed)
risk_set_b3 <- panel_block3 |>
  filter(!is.na(entry_owner)) |>
  select(all_of(c(model3_vars_present, model3_vars_optional)))

# Which model variables cause the dropped rows
risk_set_b3 |>
  summarise(across(all_of(model3_vars_present), ~ sum(is.na(.x)))) |>
  tidyr::pivot_longer(everything(), names_to = "variable", values_to = "n_missing") |>
  filter(n_missing > 0) |>
  arrange(desc(n_missing)) |>
  knitr::kable(
    caption = "Missing values within the risk set",
    format.args = list(big.mark = ",")
  )

# Sample size before and after dropping incomplete rows
tibble(
  step = c("At-risk person-waves", "Estimation sample", "Individuals"),
  n = c(nrow(risk_set_b3), nrow(data_regression_3), n_distinct(data_regression_3$xwaveid))
) |>
  knitr::kable(caption = "Sample for regression 3", format.args = list(big.mark = ","))

# Not being employed at t-1 is not absorbed by the industry fixed effect
data_regression_3 |>
  count(
    not_emp_l1,
    no_job_observed = industry_19categories_jbmi61_fe == "no job observed"
  ) |>
  knitr::kable(caption = "Not employed at t-1 by industry FE category")

# Share with liquidity missing, by origin group
data_regression_3 |>
  summarise(share_liquidity_missing = mean(liquidity_miss_l1), .by = origin_group) |>
  knitr::kable(digits = 3, caption = "Share with liquidity missing, by origin group")

# People with a missing birthplace group
panel_final |>
  distinct(xwaveid, birthplace_group) |>
  count(birthplace_missing = is.na(birthplace_group)) |>
  knitr::kable(
    caption = "Persons with missing birthplace group",
    format.args = list(big.mark = ",")
  )

# Entries by origin group
data_regression_3 |>
  summarise(
    at_risk = n(),
    entries = sum(entry_owner),
    rate = sprintf("%.2f%%", 100 * mean(entry_owner)),
    .by = origin_group
  ) |>
  arrange(origin_group) |>
  knitr::kable(
    caption = "Entry events by origin group",
    format.args = list(big.mark = ",")
  )

# Cells behind the origin x unemployed interactions
data_regression_3 |>
  summarise(
    at_risk = n(),
    entries = sum(entry_owner),
    rate = sprintf("%.2f%%", 100 * mean(entry_owner)),
    .by = c(origin_group, unemployed_l1)
  ) |>
  arrange(origin_group, unemployed_l1) |>
  knitr::kable(
    caption = "Entry events by origin group x unemployed at t-1",
    format.args = list(big.mark = ",")
  )

# Cells behind the origin x low-wage interactions
data_regression_3 |>
  summarise(
    at_risk = n(),
    entries = sum(entry_owner),
    rate = sprintf("%.2f%%", 100 * mean(entry_owner)),
    .by = c(origin_group, low_wage_l1)
  ) |>
  arrange(origin_group, low_wage_l1) |>
  knitr::kable(
    caption = "Entry events by origin group x low wage at t-1",
    format.args = list(big.mark = ",")
  )

# Entries by industry of the most recent job
data_regression_3 |>
  summarise(
    at_risk = n(),
    entries = sum(entry_owner),
    rate = sprintf("%.2f%%", 100 * mean(entry_owner)),
    .by = industry_19categories_jbmi61_fe
  ) |>
  arrange(desc(at_risk)) |>
  knitr::kable(
    caption = "At-risk person-waves and entries by industry of most recent job at t-1",
    format.args = list(big.mark = ",")
  )

# Low English proficiency by birthplace group (nearly all among MNES migrants)
panel_final |>
  distinct(xwaveid, birthplace_group, low_english_timeinvariant) |>
  count(birthplace_group, low_english_timeinvariant) |>
  knitr::kable(
    caption = "Persons by birthplace group x low English proficiency"
  )

# Migrants whose English proficiency changes over the panel
panel_final |>
  filter(!is.na(low_english)) |>
  summarise(
    n_distinct = n_distinct(low_english),
    migrant = first(migrant),
    .by = xwaveid
  ) |>
  filter(migrant == 1L) |>
  count(switcher = n_distinct > 1) |>
  mutate(pct = sprintf("%.1f%%", 100 * n / sum(n))) |>
  knitr::kable(caption = "Within-person variation in low_english (migrants)")

# Wave-to-wave transitions in English proficiency
panel_final |>
  arrange(xwaveid, wave) |>
  mutate(low_english_l1 = lag_contig(low_english, wave), .by = xwaveid) |>
  filter(!is.na(low_english), !is.na(low_english_l1)) |>
  count(low_english_l1, low_english) |>
  mutate(pct = sprintf("%.2f%%", 100 * n / sum(n))) |>
  knitr::kable(caption = "Wave-to-wave transitions in low_english")

# Low-English share by years since arrival
panel_final |>
  filter(migrant == 1L, !is.na(low_english), !is.na(years_in_aus)) |>
  mutate(
    ysa_band = cut(
      years_in_aus,
      breaks = c(0, 5, 10, 20, Inf),
      labels = c("0-5", "6-10", "11-20", "20+"),
      include.lowest = TRUE
    )
  ) |>
  summarise(mean_low_english = mean(low_english), n = n(), .by = ysa_band) |>
  arrange(ysa_band) |>
  knitr::kable(
    digits = 3,
    caption = "Low-English share by years since arrival (migrants)"
  )

# ==============================================================================
# End of 04_eda.R
# ==============================================================================
