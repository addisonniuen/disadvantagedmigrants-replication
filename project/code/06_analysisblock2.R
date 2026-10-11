# ==============================================================================
# 06_analysisblock2.R
#
# Block 2: owner income and risk. This script compares the income, income
# volatility and probability of a large income fall of owners with those of
# employees of the same origin. It:
#   - Builds a person-wave grid with values at t-1 and the three outcomes
#   - Builds the restricted sample (same status at t-1 and t) and the
#     unrestricted sample (status at t) (Tables 5.2, 5.3, A.1, C.4)
#   - Tabulates sample cells and attrition (Table C.1)
#   - Estimates the three outcomes on both samples (Tables 5.2, 5.3, C.2-C.4)
#   - Computes the owner-employee gaps, their differences from the native-born
#     gap, the minimum detectable differences and joint tests (Tables 5.2, 5.3,
#     C.2-C.4)
#
# Notation follows 03_methodology.qmd:
#   theta_S, theta_L, theta_O: small employer, large employer, own-account worker
#   theta_Sg, theta_Lg, theta_Og: owner type x origin group interaction
#
# The tables are formatted in project/thesis/thesis_format.R.
# Sample checks for this block are in 04_eda.R.
#
# This script requires these files to be sourced first:
#   source("project/code/01_setup.R")
#   source("project/code/02_datacleaning.R")
#   source("project/code/03_featureengineering.R")
# ==============================================================================

# --- Helpers ------------------------------------------------------------------

# Owner x origin cells with fewer than five owners are dropped before
# estimation: an interaction identified from one or two people fits them exactly
min_cell_people <- 5

drop_thin_cells <- function(df, owner_var) {
  thin <- df |>
    filter(.data[[owner_var]] == 1) |>
    summarise(n_people = n_distinct(xwaveid), .by = origin_group) |>
    filter(n_people < min_cell_people) |>
    pull(origin_group)
  df |> filter(!(.data[[owner_var]] == 1 & origin_group %in% thin))
}

# --- Create the data sets for block 2 -----------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# One row per interviewed person-wave
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pairs_wave_table <- panel_final |>
  # Interviewed person-years only, so a person not interviewed counts as absent
  filter(!is.na(lf_status_detail_esdtl)) |>
  transmute(
    xwaveid, year,
    wave_num = as.integer(as.character(wave)),
    present = 1L,
    income_labour_and_business,
    status = case_when(
      owner_with_employees == 1 ~ "employer",
      own_account_worker == 1 ~ "own_account",
      is_employee == 1 ~ "employee",
      is_unemployed == 1 | is_not_in_labour_force == 1 ~ "not_employed",
      .default = "other"
    ),
    origin_group = factor(
      as.character(origin_group),
      levels = c("native", "mes", "mnes_higheng", "mnes_loweng")
    ),
    small_wp,
    firm_type = case_when(
      small_single_site == 1L ~ "small_single_site",
      small_site_chain == 1L ~ "small_site_chain",
      chain_like == 1L ~ "chain_like",
      !is.na(small_single_site) & !is.na(small_site_chain) & !is.na(chain_like) ~ "other"
    ),
    industry_chr = as.character(industry_19categories_jbmi61),
    state_hhstate, remoteness_hhs3ra, highest_education_achieved_edhigh1,
    seifa_decile, marital_status_mrcurr, sex_hgsex, age_hgage,
    tenure_current_occ_jbocct
  )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Complete the grid and add values at t-1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# prior_income_mean averages income over waves t-5 to t-2 (at least two waves)
pairs_grid <- pairs_wave_table |>
  complete(xwaveid, wave_num = 1:24, fill = list(present = 0L)) |>
  arrange(xwaveid, wave_num) |>
  mutate(
    present_l1 = lag(present, 1, default = 0L),
    status_l1 = lag(status, 1),
    origin_l1 = lag(origin_group, 1),
    income_l1 = lag(income_labour_and_business, 1),
    income_l2 = lag(income_labour_and_business, 2),
    income_l3 = lag(income_labour_and_business, 3),
    income_l4 = lag(income_labour_and_business, 4),
    income_l5 = lag(income_labour_and_business, 5),
    small_wp_l1 = lag(small_wp, 1),
    firm_type_l1 = lag(firm_type, 1),
    industry_l1 = lag(industry_chr, 1),
    tenure_occ_l1 = lag(tenure_current_occ_jbocct, 1),
    .by = xwaveid
  ) |>
  mutate(
    n_prior_waves = rowSums(!is.na(pick(income_l2, income_l3, income_l4, income_l5))),
    prior_income_mean = if_else(
      n_prior_waves >= 2,
      rowMeans(pick(income_l2, income_l3, income_l4, income_l5), na.rm = TRUE),
      NA_real_
    )
  )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Outcomes, defined only if income is observed at t-1 and t
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pairs_grid <- pairs_grid |>
  mutate(
    pair_observed = present == 1L & present_l1 == 1L &
      !is.na(income_labour_and_business) & !is.na(income_l1),

    # Volatility: absolute arc percent change (0 to 200); absolute values in
    # the denominator keep it defined when business income is negative
    arc_denominator = (abs(income_labour_and_business) + abs(income_l1)) / 2,
    arc = case_when(
      !pair_observed ~ NA_real_,
      arc_denominator == 0 ~ 0,
      .default = (income_labour_and_business - income_l1) / arc_denominator
    ),
    abs_arc_pct = 100 * abs(arc),

    # Large fall: income fell by 25% or more; defined only if income at t-1 > 0
    large_fall = if_else(
      pair_observed & income_l1 > 0,
      as.integer(income_labour_and_business <= 0.75 * income_l1),
      NA_integer_
    )
  )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Status over the pair and job variables (value at t, else t-1)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pairs_grid <- pairs_grid |>
  mutate(
    status_pair = case_when(
      present == 0L | present_l1 == 0L ~ NA_character_,
      status_l1 == "employee" & status == "employee" ~ "stay_employee",
      status_l1 == "employer" & status == "employer" ~ "stay_employer",
      status_l1 == "own_account" & status == "own_account" ~ "stay_own_account",
      .default = "changed_status"
    ),
    small_wp_pair = coalesce(small_wp, small_wp_l1),
    firm_type_pair = factor(
      coalesce(firm_type, firm_type_l1, "missing"),
      levels = c("other", "small_single_site", "small_site_chain", "chain_like", "missing")
    ),
    industry_pair = factor(coalesce(industry_chr, industry_l1)),
    tenure_occ_pair = coalesce(tenure_current_occ_jbocct, tenure_occ_l1)
  ) |>
  # Prior income decile, ranked within each wave (Tin and Tran 2023)
  mutate(
    prior_income_decile = ntile(if_else(present == 1L, prior_income_mean, NA_real_), 10),
    .by = wave_num
  ) |>
  mutate(
    prior_income_decile = forcats::fct_na_value_to_level(
      factor(prior_income_decile, levels = 1:10), "none"
    )
  )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Restricted sample: same status at t-1 and t
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data_regression_2_pairs <- pairs_grid |>
  filter(
    pair_observed,
    wave_num <= 21,
    firm_type_pair != "missing",
    status_pair %in% c("stay_employee", "stay_employer", "stay_own_account"),
    !is.na(small_wp_pair),
    status_pair != "stay_own_account" | small_wp_pair == 1
  ) |>
  drop_na(
    abs_arc_pct, income_labour_and_business, origin_group, industry_pair,
    state_hhstate, age_hgage, sex_hgsex, remoteness_hhs3ra,
    highest_education_achieved_edhigh1, seifa_decile, tenure_occ_pair,
    marital_status_mrcurr
  ) |>
  mutate(
    # Same owner type in t-1 and t
    owner_emp_small = as.integer(status_pair == "stay_employer" & small_wp_pair == 1),
    owner_emp_large = as.integer(status_pair == "stay_employer" & small_wp_pair == 0),
    own_acc = as.integer(status_pair == "stay_own_account"),
    across(c(sex_hgsex, marital_status_mrcurr, remoteness_hhs3ra, state_hhstate), as.factor),
    highest_education_achieved_edhigh1 = forcats::fct_relevel(
      factor(highest_education_achieved_edhigh1), "year 11 and below"
    )
  ) |>
  drop_thin_cells("owner_emp_large") |>
  mutate(
    firm_type_pair = droplevels(firm_type_pair),
    origin_group = droplevels(origin_group)
  )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Unrestricted sample: status at t, any status at t-1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data_regression_2_unrestricted <- pairs_grid |>
  filter(
    pair_observed,
    wave_num <= 21,
    firm_type_pair != "missing",
    status %in% c("employee", "employer", "own_account"),
    !is.na(small_wp_pair),
    status != "own_account" | small_wp_pair == 1
  ) |>
  drop_na(
    abs_arc_pct, income_labour_and_business, origin_group, industry_pair,
    state_hhstate, age_hgage, sex_hgsex, remoteness_hhs3ra,
    highest_education_achieved_edhigh1, seifa_decile, tenure_occ_pair,
    marital_status_mrcurr
  ) |>
  mutate(
    # Owner type at t
    owner_emp_small = as.integer(status == "employer" & small_wp_pair == 1),
    owner_emp_large = as.integer(status == "employer" & small_wp_pair == 0),
    own_acc = as.integer(status == "own_account"),
    across(c(sex_hgsex, marital_status_mrcurr, remoteness_hhs3ra, state_hhstate), as.factor),
    highest_education_achieved_edhigh1 = forcats::fct_relevel(
      factor(highest_education_achieved_edhigh1), "year 11 and below"
    )
  ) |>
  drop_thin_cells("owner_emp_large") |>
  mutate(
    firm_type_pair = droplevels(firm_type_pair),
    origin_group = droplevels(origin_group)
  )

# --- Sample tables (Table C.1) ------------------------------------------------

# Cells, raw outcomes and owner counts behind each interaction
cells_b2 <- data_regression_2_pairs |>
  mutate(
    owner_type = case_when(
      owner_emp_small == 1 ~ "employer_small",
      owner_emp_large == 1 ~ "employer_large",
      own_acc == 1 ~ "own_account",
      small_wp_pair == 1 ~ "employee_small",
      .default = "employee_large"
    )
  ) |>
  summarise(
    individuals = n_distinct(xwaveid),
    person_pairs = n(),
    mean_income = mean(income_labour_and_business),
    median_income = median(income_labour_and_business),
    mean_volatility = mean(abs_arc_pct),
    share_large_fall = mean(large_fall, na.rm = TRUE),
    .by = c(owner_type, origin_group)
  ) |>
  arrange(owner_type, origin_group)

# Owners at t-1 not interviewed at t (missing from both samples)
attrition_b2 <- pairs_grid |>
  filter(
    present_l1 == 1L, wave_num <= 21, !is.na(origin_l1), !is.na(small_wp_l1),
    status_l1 %in% c("employer", "own_account")
  ) |>
  mutate(
    owner_type = case_when(
      status_l1 == "own_account" ~ "own_account",
      small_wp_l1 == 1 ~ "employer_small",
      .default = "employer_large"
    )
  ) |>
  summarise(
    owners_t_minus_1 = n(),
    share_not_interviewed = mean(present == 0L),
    .by = c(owner_type, origin_l1)
  ) |>
  rename(origin_group = origin_l1) |>
  arrange(owner_type, origin_group)

# --- Estimate regression 2 ----------------------------------------------------


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Restricted sample (Tables 5.2, 5.3, C.2-C.4)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Income at t
regression2_income <- feols(
  income_labour_and_business ~
    owner_emp_small +
    owner_emp_large +
    own_acc +
    i(origin_group, ref = "native") +
    i(origin_group, owner_emp_small, ref = "native") +
    i(origin_group, owner_emp_large, ref = "native") +
    i(origin_group, own_acc, ref = "native") +
    i(firm_type_pair, ref = "other") +
    age_hgage +
    I(age_hgage^2) +
    sex_hgsex +
    remoteness_hhs3ra +
    highest_education_achieved_edhigh1 +
    seifa_decile +
    tenure_occ_pair +
    marital_status_mrcurr
    | year + industry_pair^year + state_hhstate^year,
  data = data_regression_2_pairs,
  cluster = ~ xwaveid + industry_pair^year
)

# Volatility
regression2_volatility <- feols(
  abs_arc_pct ~
    owner_emp_small +
    owner_emp_large +
    own_acc +
    i(origin_group, ref = "native") +
    i(origin_group, owner_emp_small, ref = "native") +
    i(origin_group, owner_emp_large, ref = "native") +
    i(origin_group, own_acc, ref = "native") +
    i(firm_type_pair, ref = "other") +
    age_hgage +
    I(age_hgage^2) +
    sex_hgsex +
    remoteness_hhs3ra +
    highest_education_achieved_edhigh1 +
    seifa_decile +
    tenure_occ_pair +
    marital_status_mrcurr +
    i(prior_income_decile, ref = "5")
    | year + industry_pair^year + state_hhstate^year,
  data = data_regression_2_pairs,
  cluster = ~ xwaveid + industry_pair^year
)

# Large fall (linear probability model)
regression2_large_fall <- feols(
  large_fall ~
    owner_emp_small +
    owner_emp_large +
    own_acc +
    i(origin_group, ref = "native") +
    i(origin_group, owner_emp_small, ref = "native") +
    i(origin_group, owner_emp_large, ref = "native") +
    i(origin_group, own_acc, ref = "native") +
    i(firm_type_pair, ref = "other") +
    age_hgage +
    I(age_hgage^2) +
    sex_hgsex +
    remoteness_hhs3ra +
    highest_education_achieved_edhigh1 +
    seifa_decile +
    tenure_occ_pair +
    marital_status_mrcurr +
    i(prior_income_decile, ref = "5")
    | year + industry_pair^year + state_hhstate^year,
  data = data_regression_2_pairs,
  cluster = ~ xwaveid + industry_pair^year
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Unrestricted sample (Tables 5.2, 5.3, C.2-C.4)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Income at t
regression2_income_unrestricted <- feols(
  income_labour_and_business ~
    owner_emp_small +
    owner_emp_large +
    own_acc +
    i(origin_group, ref = "native") +
    i(origin_group, owner_emp_small, ref = "native") +
    i(origin_group, owner_emp_large, ref = "native") +
    i(origin_group, own_acc, ref = "native") +
    i(firm_type_pair, ref = "other") +
    age_hgage +
    I(age_hgage^2) +
    sex_hgsex +
    remoteness_hhs3ra +
    highest_education_achieved_edhigh1 +
    seifa_decile +
    tenure_occ_pair +
    marital_status_mrcurr
    | year + industry_pair^year + state_hhstate^year,
  data = data_regression_2_unrestricted,
  cluster = ~ xwaveid + industry_pair^year
)

# Volatility
regression2_volatility_unrestricted <- feols(
  abs_arc_pct ~
    owner_emp_small +
    owner_emp_large +
    own_acc +
    i(origin_group, ref = "native") +
    i(origin_group, owner_emp_small, ref = "native") +
    i(origin_group, owner_emp_large, ref = "native") +
    i(origin_group, own_acc, ref = "native") +
    i(firm_type_pair, ref = "other") +
    age_hgage +
    I(age_hgage^2) +
    sex_hgsex +
    remoteness_hhs3ra +
    highest_education_achieved_edhigh1 +
    seifa_decile +
    tenure_occ_pair +
    marital_status_mrcurr +
    i(prior_income_decile, ref = "5")
    | year + industry_pair^year + state_hhstate^year,
  data = data_regression_2_unrestricted,
  cluster = ~ xwaveid + industry_pair^year
)

# Large fall (linear probability model)
regression2_large_fall_unrestricted <- feols(
  large_fall ~
    owner_emp_small +
    owner_emp_large +
    own_acc +
    i(origin_group, ref = "native") +
    i(origin_group, owner_emp_small, ref = "native") +
    i(origin_group, owner_emp_large, ref = "native") +
    i(origin_group, own_acc, ref = "native") +
    i(firm_type_pair, ref = "other") +
    age_hgage +
    I(age_hgage^2) +
    sex_hgsex +
    remoteness_hhs3ra +
    highest_education_achieved_edhigh1 +
    seifa_decile +
    tenure_occ_pair +
    marital_status_mrcurr +
    i(prior_income_decile, ref = "5")
    | year + industry_pair^year + state_hhstate^year,
  data = data_regression_2_unrestricted,
  cluster = ~ xwaveid + industry_pair^year
)

# --- Owner-employee gaps, minimum detectable differences and joint tests ------

# For owner type o and origin group g:
#   vs_same_origin_employee: theta_o + theta_og (theta_o for the native-born)
#   gap_differs_from_native: theta_og
# Standard errors use each model's clustered vcov. MDE = 2.8 x SE.

origin_levels <- c("native", "mes", "mnes_higheng", "mnes_loweng")

# Weighted sum of coefficients (NA if a term was not estimated)
lincom_b2 <- function(m, terms) {
  b <- coef(m)
  if (!all(terms %in% names(b))) {
    return(tibble::tibble(estimate = NA_real_, se = NA_real_, p.value = NA_real_))
  }
  w <- as.numeric(names(b) %in% terms)
  est <- sum(w * b)
  se <- sqrt(drop(t(w) %*% vcov(m) %*% w))
  tibble::tibble(
    estimate = est,
    se = se,
    p.value = 2 * pt(-abs(est / se), df = fixest::degrees_freedom(m, type = "t"))
  )
}

# Both contrasts for every owner type and origin group
owner_contrasts <- function(m, owner_terms) {
  tidyr::expand_grid(
    group = origin_levels,
    owner_term = owner_terms,
    comparison = c("vs_same_origin_employee", "gap_differs_from_native")
  ) |>
    filter(group != "native" | comparison == "vs_same_origin_employee") |>
    mutate(
      terms = purrr::pmap(list(group, owner_term, comparison), \(g, o, cmp) {
        if (g == "native") return(o)
        inter <- paste0("origin_group::", g, ":", o)
        if (cmp == "gap_differs_from_native") inter else c(o, inter)
      }),
      res = purrr::map(terms, \(x) lincom_b2(m, x))
    ) |>
    tidyr::unnest(res) |>
    select(-terms)
}

# Owners behind each estimate
owner_counts <- function(m, data, owner_terms) {
  d <- data[fixest::obs(m), ]
  purrr::map_dfr(owner_terms, \(o) {
    d |>
      filter(.data[[o]] == 1) |>
      summarise(n_owners = n_distinct(xwaveid), .by = origin_group) |>
      transmute(group = as.character(origin_group), owner_term = o, n_owners)
  })
}

owner_terms_b2 <- c("owner_emp_small", "owner_emp_large", "own_acc")

models_b2 <- list(
  income_t = regression2_income,
  abs_arc_pct = regression2_volatility,
  large_fall = regression2_large_fall
)
models_b2_unrestricted <- list(
  income_t = regression2_income_unrestricted,
  abs_arc_pct = regression2_volatility_unrestricted,
  large_fall = regression2_large_fall_unrestricted
)

# sample: "unrestricted" = status at t; "restricted" = same status at t-1 and t
contrasts_b2 <- bind_rows(
  purrr::imap_dfr(models_b2_unrestricted, \(m, outcome) {
    owner_contrasts(m, owner_terms_b2) |>
      left_join(owner_counts(m, data_regression_2_unrestricted, owner_terms_b2),
                by = c("group", "owner_term")) |>
      mutate(outcome = outcome, sample = "unrestricted")
  }),
  purrr::imap_dfr(models_b2, \(m, outcome) {
    owner_contrasts(m, owner_terms_b2) |>
      left_join(owner_counts(m, data_regression_2_pairs, owner_terms_b2),
                by = c("group", "owner_term")) |>
      mutate(outcome = outcome, sample = "restricted")
  })
) |>
  mutate(mde = 2.8 * se) |>
  relocate(outcome, sample)

# Joint test that all origin x owner-type interactions are zero
interaction_pattern <- "^origin_group::[a-z_]+:(owner_emp|own_acc)"

joint_tests <- bind_rows(
  unrestricted = purrr::imap_dfr(models_b2_unrestricted, \(m, outcome) {
    w <- fixest::wald(m, keep = interaction_pattern, print = FALSE)
    tibble::tibble(outcome = outcome, F_stat = w$stat, df1 = w$df1, df2 = w$df2, p.value = w$p)
  }),
  restricted = purrr::imap_dfr(models_b2, \(m, outcome) {
    w <- fixest::wald(m, keep = interaction_pattern, print = FALSE)
    tibble::tibble(outcome = outcome, F_stat = w$stat, df1 = w$df1, df2 = w$df2, p.value = w$p)
  }),
  .id = "sample"
)

# ==============================================================================
# End of 06_analysisblock2.R
# ==============================================================================
