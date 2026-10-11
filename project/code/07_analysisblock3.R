# ==============================================================================
# 07_analysisblock3.R
#
# Block 3: entry into business ownership. This script estimates who becomes an
# owner between t-1 and t, among people who are not owners at t-1. It:
#   - Builds the transition sample (data_regression_3)
#   - Estimates the four main logits (Table 5.4, Table D.2)
#   - Estimates the origin x push logit (Table 5.5)
#   - Re-estimates it with status at t-1 and t-2, and with person means
#     (Mundlak) (Table D.3)
#   - Compares the labour market position at t-1 of each origin group (Table D.5)
#   - Estimates linear probability versions of these models (Table D.4)
#
# Models that are not in the thesis (earlier specifications, robustness checks
# and an event study of income after entry) are kept at the end, commented out.
# Sample checks for this block are in 04_eda.R.
#
# This script requires these files to be sourced first:
#   source("project/code/01_setup.R")
#   source("project/code/02_datacleaning.R")
#   source("project/code/03_featureengineering.R")
# ==============================================================================

# --- Variable lists -----------------------------------------------------------

# Variables of the main models: rows missing any of these are dropped
model3_vars_present <- c(
  "entry_owner",
  "origin_group",
  "xwaveid",
  "year",
  "wave",
  "industry_19categories_jbmi61_fe",
  "state_hhstate",
  "unemployed_l1",
  "nilf_l1",
  "low_wage_l1",
  "liquidity_l1",
  "liquidity_miss_l1",
  "highest_education_achieved_edhigh1_l1",
  "tenure_emp_yrs_l1",
  "age_hgage",
  "sex_hgsex",
  "years_in_aus_ctrl",
  "years_in_aus_miss",
  "marital_status_mrcurr_l1",
  "homeowner_l1",
  "seifa_decile_l1",
  "remoteness_hhs3ra_l1"
)

# Variables used by some models only (not used to drop rows)
model3_vars_optional <- c(
  "entry_owner_fe",
  "owner_l1",
  "origin_group_3",
  "migrant",
  "migrant_low_eng_baseline",
  "low_english_timeinvariant",
  "push_l1",
  "not_emp_l1",
  "cashflow_l1",
  "hardship_l1",
  "unemployed_l2",
  "nilf_l2",
  "low_wage_l2",
  "liquidity_l2",
  "liquidity_miss_l2",
  "owner_l2",
  "casual_perm_jbcasab_l1",
  "tenure_emp_missing_l1",
  "remoteness_collapsed_l1"
)

# --- Create the data set for block 3 ------------------------------------------

# Step 1: copy the time-invariant origin variables to every wave of a person
panel_block3 <- panel_final |>
  arrange(xwaveid, wave) |>
  group_by(xwaveid) |>
  tidyr::fill(
    birthplace_group,
    origin_group,
    origin_group_3,
    migrant,
    migrant_low_eng_baseline,
    low_english_timeinvariant,
    .direction = "downup"
  ) |>
  ungroup() |>
  # Step 2: values at t-1 (lag_contig returns NA if the previous wave is missing)
  mutate(
    owner_l1 = lag_contig(is_owner, wave),
    highest_education_achieved_edhigh1_l1 = lag_contig(highest_education_achieved_edhigh1,wave),
    unemployed_l1 = lag_contig(is_unemployed, wave),
    nilf_l1 = lag_contig(is_not_in_labour_force, wave),
    low_wage_l1 = lag_contig(is_low_wage, wave),
    liquidity_l1 = lag_contig(liquidity_constraint, wave),
    cashflow_l1 = lag_contig(cashflow_problem, wave),
    hardship_l1 = lag_contig(hardship, wave),
    homeowner_l1 = lag_contig(is_homeowner, wave),
    marital_status_mrcurr_l1 = lag_contig(marital_status_mrcurr, wave),
    seifa_decile_l1 = lag_contig(seifa_decile, wave),
    remoteness_collapsed_l1 = lag_contig(remoteness_collapsed, wave),
    remoteness_hhs3ra_l1 = lag_contig(remoteness_hhs3ra, wave),
    casual_perm_jbcasab_l1 = lag_contig(casual_perm_jbcasab, wave),
    tenure_current_employer_jbempt_l1 = lag_contig(
      tenure_current_employer_jbempt,
      wave
    ),
    years_in_aus_l1 = lag_contig(years_in_aus, wave),
    industry_19categories_jbmi61_l1 = lag_contig(
      industry_19categories_jbmi61,
      wave
    ),
    .by = xwaveid
  ) |>
  # Step 3: values at t-2 (timing models)
  mutate(
    owner_l2 = lag_contig(owner_l1, wave),
    unemployed_l2 = lag_contig(unemployed_l1, wave),
    nilf_l2 = lag_contig(nilf_l1, wave),
    low_wage_l2 = lag_contig(low_wage_l1, wave),
    liquidity_l2 = lag_contig(liquidity_l1, wave),
    industry_last_l1 = industry_19categories_jbmi61_l1,
    .by = xwaveid
  ) |>
  # Step 4: industry of the most recent job up to t-1, so the industry fixed
  # effect does not absorb not being employed
  group_by(xwaveid) |>
  tidyr::fill(industry_last_l1, .direction = "down") |>
  ungroup() |>
  mutate(
    # Step 5: entry, defined for people at risk (interviewed at t-1, answered
    # the labour force questions, not an owner at t-1); the version from
    # 03_featureengineering.R is kept as entry_owner_fe for a check in 04_eda.R
    entry_owner_fe = entry_owner,
    entry_owner = if_else(
      !is.na(owner_l1) &
        owner_l1 == 0 &
        !is.na(unemployed_l1) &
        !is.na(is_owner),
      as.integer(is_owner == 1),
      NA_integer_
    ),
    # Step 6: recodes
    origin_group = forcats::fct_relevel(factor(origin_group), "native"),
    push_l1 = pmax(unemployed_l1, nilf_l1, low_wage_l1, na.rm = TRUE),
    not_emp_l1 = pmax(unemployed_l1, nilf_l1, na.rm = TRUE),
    # Liquidity is missing when the self-completion questionnaire is not
    # returned: set to 0 with a missing flag
    liquidity_miss_l1 = as.integer(is.na(liquidity_l1)),
    liquidity_l1 = coalesce(as.numeric(liquidity_l1), 0),
    liquidity_miss_l2 = as.integer(is.na(liquidity_l2)),
    liquidity_l2 = coalesce(as.numeric(liquidity_l2), 0),
    # Years in Australia: 0 for natives; 0 plus a missing flag for migrants
    # with no arrival year
    years_in_aus_miss = as.integer(
      origin_group != "native" & is.na(years_in_aus_l1)
    ),
    years_in_aus_ctrl = case_when(
      origin_group == "native" ~ 0,
      is.na(years_in_aus_l1) ~ 0,
      TRUE ~ as.numeric(years_in_aus_l1)
    ),
    industry_19categories_jbmi61_fe = forcats::fct_na_value_to_level(
      factor(industry_last_l1),
      level = "no job observed"
    ),
    casual_perm_jbcasab_l1 = forcats::fct_na_value_to_level(
      factor(casual_perm_jbcasab_l1),
      level = "not employed at t-1"
    ),
    tenure_emp_yrs_l1 = coalesce(tenure_current_employer_jbempt_l1, 0),
    tenure_emp_missing_l1 = as.integer(is.na(
      tenure_current_employer_jbempt_l1
    )),
    highest_education_achieved_edhigh1_l1 = forcats::fct_relevel(
      highest_education_achieved_edhigh1_l1,
      "year 11 and below"
    )
  )

# Transition sample: at risk of entry, no missing model variables
data_regression_3 <- panel_block3 |>
  filter(!is.na(entry_owner)) |>
  select(all_of(c(model3_vars_present, model3_vars_optional)))
data_regression_3 <- data_regression_3 |>
  drop_na(all_of(model3_vars_present)) |>
  droplevels()

# --- Estimate regression 3 ----------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Version 1: origin groups only (Table 5.4, Table D.2)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

regression3_v1a <- feglm(
  entry_owner ~
    origin_group +
    age_hgage +
    I(age_hgage^2) +
    sex_hgsex +
    years_in_aus_ctrl +
    years_in_aus_miss +
    marital_status_mrcurr_l1 +
    homeowner_l1 +
    seifa_decile_l1 +
    remoteness_hhs3ra_l1 |
    year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
  data = data_regression_3,
  family = binomial(link = "logit"),
  cluster = ~xwaveid
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Version 2: labour market position only (Table 5.4, Table D.2)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

regression3_v2a <- feglm(
  entry_owner ~
    unemployed_l1 +
    nilf_l1 +
    low_wage_l1 +
    liquidity_l1 +
    liquidity_miss_l1 +
    age_hgage +
    I(age_hgage^2) +
    sex_hgsex +
    years_in_aus_ctrl +
    years_in_aus_miss +
    marital_status_mrcurr_l1 +
    homeowner_l1 +
    seifa_decile_l1 +
    remoteness_hhs3ra_l1 |
    year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
  data = data_regression_3,
  family = binomial(link = "logit"),
  cluster = ~xwaveid
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Version 3: origin and labour market position (Table 5.4, Table D.2)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

regression3_v3a <- feglm(
  entry_owner ~
    origin_group +
    unemployed_l1 +
    nilf_l1 +
    low_wage_l1 +
    liquidity_l1 +
    liquidity_miss_l1 +
    age_hgage +
    I(age_hgage^2) +
    sex_hgsex +
    years_in_aus_ctrl +
    years_in_aus_miss +
    marital_status_mrcurr_l1 +
    homeowner_l1 +
    seifa_decile_l1 +
    remoteness_hhs3ra_l1 |
    year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
  data = data_regression_3,
  family = binomial(link = "logit"),
  cluster = ~xwaveid
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Version 4: adding education and employer tenure (Table 5.4, Table D.2)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

regression3_v4a <- feglm(
  entry_owner ~
    origin_group +
    unemployed_l1 +
    nilf_l1 +
    low_wage_l1 +
    liquidity_l1 +
    liquidity_miss_l1 +
    highest_education_achieved_edhigh1_l1 +
    tenure_emp_yrs_l1 +
    age_hgage +
    I(age_hgage^2) +
    sex_hgsex +
    years_in_aus_ctrl +
    years_in_aus_miss +
    marital_status_mrcurr_l1 +
    homeowner_l1 +
    seifa_decile_l1 +
    remoteness_hhs3ra_l1 |
    year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
  data = data_regression_3,
  family = binomial(link = "logit"),
  cluster = ~xwaveid
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Version 9a: origin x unemployment and low wage (Table 5.5)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

regression3_v9a <- feglm(
  entry_owner ~
    origin_group *
    (unemployed_l1 + low_wage_l1) +
    nilf_l1 +
    liquidity_l1 +
    liquidity_miss_l1 +
    highest_education_achieved_edhigh1_l1 +
    tenure_emp_yrs_l1 +
    age_hgage +
    I(age_hgage^2) +
    sex_hgsex +
    years_in_aus_ctrl +
    years_in_aus_miss +
    marital_status_mrcurr_l1 +
    homeowner_l1 +
    seifa_decile_l1 +
    remoteness_hhs3ra_l1 |
    year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
  data = data_regression_3,
  family = binomial(link = "logit"),
  cluster = ~xwaveid
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Timing: status at t-1, t-2 and both (Table D.3)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Same sample in all three: status also observed at t-2
data_regression_3_l2 <- data_regression_3 |>
  drop_na(unemployed_l2, nilf_l2, low_wage_l2)

regression3_timing_l1 <- feglm(
  entry_owner ~ origin_group * (unemployed_l1 + low_wage_l1) +
    nilf_l1 + liquidity_l1 + liquidity_miss_l1 + highest_education_achieved_edhigh1_l1 +
    tenure_emp_yrs_l1 +
    age_hgage +
    I(age_hgage^2) +
    sex_hgsex +
    years_in_aus_ctrl +
    years_in_aus_miss +
    marital_status_mrcurr_l1 +
    homeowner_l1 +
    seifa_decile_l1 +
    remoteness_hhs3ra_l1 | year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
  data = data_regression_3_l2,
  family = binomial(link = "logit"),
  cluster = ~xwaveid
)

regression3_timing_l2 <- feglm(
  entry_owner ~ origin_group * (unemployed_l2 + low_wage_l2) +
    nilf_l2 + liquidity_l2 + liquidity_miss_l2 + highest_education_achieved_edhigh1_l1 +
    tenure_emp_yrs_l1 +
    age_hgage +
    I(age_hgage^2) +
    sex_hgsex +
    years_in_aus_ctrl +
    years_in_aus_miss +
    marital_status_mrcurr_l1 +
    homeowner_l1 +
    seifa_decile_l1 +
    remoteness_hhs3ra_l1 | year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
  data = data_regression_3_l2,
  family = binomial(link = "logit"),
  cluster = ~xwaveid
)

regression3_timing_both <- feglm(
  entry_owner ~ origin_group * (unemployed_l1 + low_wage_l1) +
    origin_group:(unemployed_l2 + low_wage_l2) + unemployed_l2 + low_wage_l2 +
    nilf_l1 + nilf_l2 + liquidity_l1 + liquidity_miss_l1 + highest_education_achieved_edhigh1_l1 +
    tenure_emp_yrs_l1 +
    age_hgage +
    I(age_hgage^2) +
    sex_hgsex +
    years_in_aus_ctrl +
    years_in_aus_miss +
    marital_status_mrcurr_l1 +
    homeowner_l1 +
    seifa_decile_l1 +
    remoteness_hhs3ra_l1 | year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
  data = data_regression_3_l2,
  family = binomial(link = "logit"),
  cluster = ~xwaveid
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Person means (Mundlak) (Table D.3)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Each person's mean of the time-varying regressors, so the t-1 coefficients
# compare a person with themselves at other times
data_regression_3_cre <- data_regression_3 |>
  mutate(
    across(
      c(unemployed_l1, low_wage_l1, nilf_l1, liquidity_l1,
        homeowner_l1, seifa_decile_l1),
      \(x) mean(as.numeric(x), na.rm = TRUE),
      .names = "{.col}_mean"
    ),
    .by = xwaveid
  )

regression3_mundlak <- feglm(
  entry_owner ~ origin_group * (unemployed_l1 + low_wage_l1) +
    nilf_l1 + liquidity_l1 + liquidity_miss_l1 +
    unemployed_l1_mean + low_wage_l1_mean + nilf_l1_mean + liquidity_l1_mean +
    homeowner_l1_mean + seifa_decile_l1_mean + highest_education_achieved_edhigh1_l1 +
    tenure_emp_yrs_l1 +
    age_hgage +
    I(age_hgage^2) +
    sex_hgsex +
    years_in_aus_ctrl +
    years_in_aus_miss +
    marital_status_mrcurr_l1 +
    homeowner_l1 +
    seifa_decile_l1 +
    remoteness_hhs3ra_l1 | year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
  data = data_regression_3_cre,
  family = binomial(link = "logit"),
  cluster = ~xwaveid
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Position at t-1 by origin group (Table D.5)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Difference from the native-born in the probability of each position at t-1,
# holding age, sex and year fixed. Liquidity uses returned questionnaires only.
push_positions <- c("unemployed_l1", "nilf_l1", "low_wage_l1", "liquidity_l1")

regression3_push_position <- purrr::map(push_positions, \(v) {
  feols(
    as.formula(paste0(
      "I(100 * ", v, ") ~ i(origin_group, ref = 'native') + age_hgage + I(age_hgage^2) + sex_hgsex | year"
    )),
    data = if (v == "liquidity_l1") filter(data_regression_3, liquidity_miss_l1 == 0) else data_regression_3,
    cluster = ~xwaveid
  )
}) |>
  setNames(push_positions)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Linear probability models (Table D.4)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# OLS versions of the logits on the same person-pairs; outcome x 100, so the
# coefficients are percentage points
data_regression_3_lpm <- data_regression_3 |> mutate(entry_pp = 100 * entry_owner)

lpm_sample <- \(m) data_regression_3_lpm[fixest::obs(m), ]

# Versions 1 to 4
regression3_lpm_v1 <- feols(
  entry_pp ~ origin_group + age_hgage + I(age_hgage^2) + sex_hgsex +
    years_in_aus_ctrl + years_in_aus_miss + marital_status_mrcurr_l1 +
    homeowner_l1 + seifa_decile_l1 + remoteness_hhs3ra_l1 | year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
  data = lpm_sample(regression3_v1a),
  cluster = ~xwaveid
)

regression3_lpm_v2 <- feols(
  entry_pp ~ unemployed_l1 + nilf_l1 + low_wage_l1 + liquidity_l1 +
    liquidity_miss_l1 + age_hgage + I(age_hgage^2) + sex_hgsex +
    years_in_aus_ctrl + years_in_aus_miss + marital_status_mrcurr_l1 +
    homeowner_l1 + seifa_decile_l1 + remoteness_hhs3ra_l1 | year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
  data = lpm_sample(regression3_v2a),
  cluster = ~xwaveid
)

regression3_lpm_v3 <- feols(
  entry_pp ~ origin_group + unemployed_l1 + nilf_l1 + low_wage_l1 +
    liquidity_l1 + liquidity_miss_l1 + age_hgage + I(age_hgage^2) +
    sex_hgsex + years_in_aus_ctrl + years_in_aus_miss +
    marital_status_mrcurr_l1 + homeowner_l1 + seifa_decile_l1 +
    remoteness_hhs3ra_l1 | year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
  data = lpm_sample(regression3_v3a),
  cluster = ~xwaveid
)

regression3_lpm_v4 <- feols(
  entry_pp ~ origin_group + unemployed_l1 + nilf_l1 + low_wage_l1 +
    liquidity_l1 + liquidity_miss_l1 + highest_education_achieved_edhigh1_l1 +
    tenure_emp_yrs_l1 +
    age_hgage +
    I(age_hgage^2) +
    sex_hgsex +
    years_in_aus_ctrl +
    years_in_aus_miss +
    marital_status_mrcurr_l1 +
    homeowner_l1 +
    seifa_decile_l1 +
    remoteness_hhs3ra_l1 | year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
  data = lpm_sample(regression3_v4a),
  cluster = ~xwaveid
)

# Version 9a
regression3_lpm_het <- feols(
  entry_pp ~ origin_group * (unemployed_l1 + low_wage_l1) +
    nilf_l1 + liquidity_l1 + liquidity_miss_l1 + highest_education_achieved_edhigh1_l1 +
    tenure_emp_yrs_l1 +
    age_hgage +
    I(age_hgage^2) +
    sex_hgsex +
    years_in_aus_ctrl +
    years_in_aus_miss +
    marital_status_mrcurr_l1 +
    homeowner_l1 +
    seifa_decile_l1 +
    remoteness_hhs3ra_l1 | year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
  data = lpm_sample(regression3_v9a),
  cluster = ~xwaveid
)

# With individual fixed effects; origin main effects are absorbed, so only the
# interactions are included
regression3_lpm_het_fe <- feols(
  entry_pp ~ unemployed_l1 + low_wage_l1 +
    i(origin_group, unemployed_l1, ref = "native") +
    i(origin_group, low_wage_l1, ref = "native") +
    nilf_l1 + liquidity_l1 + liquidity_miss_l1 + highest_education_achieved_edhigh1_l1 +
    tenure_emp_yrs_l1 +
    age_hgage +
    I(age_hgage^2) +
    sex_hgsex +
    years_in_aus_ctrl +
    years_in_aus_miss +
    marital_status_mrcurr_l1 +
    homeowner_l1 +
    seifa_decile_l1 +
    remoteness_hhs3ra_l1 |
    xwaveid + year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
  data = lpm_sample(regression3_v9a),
  cluster = ~xwaveid
)

# --- Not in the thesis (commented out) ----------------------------------------

# Versions 5 to 8 and 6c: earlier specifications (origin x not employed, linear
# probability models, liquidity components, probit and a three-group origin
# variable), replaced by the models above
# Version 5: origin x constraint

# Four-way partition
# regression3_v5b <- feglm(
#   entry_owner ~
#     origin_group * not_emp_l1 +
#     origin_group * low_wage_l1 +
#     origin_group * liquidity_l1 +
#     liquidity_miss_l1 +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3,
#   family = binomial(link = "logit"),
#   cluster = ~ xwaveid + industry_19categories_jbmi61_fe^year
# )

# Version 6: linear probability model

# regression3_v6a <- feols(
#   entry_owner ~
#     origin_group +
#     unemployed_l1 +
#     nilf_l1 +
#     low_wage_l1 +
#     liquidity_l1 +
#     liquidity_miss_l1 +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3,
#   cluster = ~ xwaveid + industry_19categories_jbmi61_fe^year
# )

# regression3_v6b <- feols(
#   entry_owner ~
#     origin_group *
#     not_emp_l1 +
#     low_wage_l1 +
#     liquidity_l1 +
#     liquidity_miss_l1 +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3,
#   cluster = ~ xwaveid + industry_19categories_jbmi61_fe^year
# )

# Version 7: robustness on the liquidity measure

# Components of the constraint, with home ownership as a collateral proxy
# regression3_v7a <- feglm(
#   entry_owner ~
#     origin_group +
#     unemployed_l1 +
#     nilf_l1 +
#     low_wage_l1 +
#     cashflow_l1 +
#     hardship_l1 +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3,
#   family = binomial(link = "logit"),
#   cluster = ~ xwaveid + industry_19categories_jbmi61_fe^year
# )

# Version 8: probit link and the three-level partition

# regression3_v8a <- feglm(
#   entry_owner ~
#     origin_group +
#     unemployed_l1 +
#     nilf_l1 +
#     low_wage_l1 +
#     liquidity_l1 +
#     liquidity_miss_l1 +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3,
#   family = binomial(link = "probit"),
#   cluster = ~ xwaveid + industry_19categories_jbmi61_fe^year
# )

# regression3_v8b <- feglm(
#   entry_owner ~
#     origin_group_3 +
#     unemployed_l1 +
#     nilf_l1 +
#     low_wage_l1 +
#     liquidity_l1 +
#     liquidity_miss_l1 +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3,
#   family = binomial(link = "logit"),
#   cluster = ~ xwaveid + industry_19categories_jbmi61_fe^year
# )

# Version 6c: linear probability model, origin gaps only

# regression3_v6c <- feols(
#   entry_owner ~
#     origin_group +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3,
#   cluster = ~xwaveid
# )

# Version 9b and 9c: linear probability and probit versions of Version 9a,
# replaced by the linear probability models above
# 9b: linear probability model
# regression3_v9b <- feols(
#   entry_owner ~
#     origin_group *
#     (unemployed_l1 + low_wage_l1) +
#     nilf_l1 +
#     liquidity_l1 +
#     liquidity_miss_l1 +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3,
#   cluster = ~xwaveid
# )

# 9c: probit
# regression3_v9c <- feglm(
#   entry_owner ~
#     origin_group *
#     (unemployed_l1 + low_wage_l1) +
#     nilf_l1 +
#     liquidity_l1 +
#     liquidity_miss_l1 +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3,
#   family = binomial(link = "probit"),
#   cluster = ~xwaveid
# )

# Version 10: robustness checks of Version 9b (complete liquidity cases,
# timing, person means, liquidity components, ages 21 to 64, two-way
# clustering, separate regressions by origin group and first entries only).
# The thesis reports the logit versions of the timing and person-means checks.

# 10a: complete cases for liquidity (drop non-returned questionnaires)
# regression3_v10a <- feols(
#   entry_owner ~
#     origin_group *
#     (unemployed_l1 + low_wage_l1) +
#     nilf_l1 +
#     liquidity_l1 +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3 |> filter(liquidity_miss_l1 == 0),
#   cluster = ~xwaveid
# )

# 10b: status at t-1 and t-2 together
#      Does t-1 status predict entry once t-2 status is held fixed?
# regression3_v10b <- feols(
#   entry_owner ~
#     origin_group *
#     (unemployed_l1 + low_wage_l1) +
#     unemployed_l2 +
#     low_wage_l2 +
#     nilf_l1 +
#     nilf_l2 +
#     liquidity_l1 +
#     liquidity_miss_l1 +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3_l2,
#   cluster = ~xwaveid
# )

# 10c: status at t-2 only (before any plausible preparation for entry)
# regression3_v10c <- feols(
#   entry_owner ~
#     origin_group *
#     (unemployed_l2 + low_wage_l2) +
#     nilf_l2 +
#     liquidity_l2 +
#     liquidity_miss_l2 +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3_l2,
#   cluster = ~xwaveid
# )

# 10d: correlated random effects (Mundlak)
#      Adds each person's average of the time-varying variables, so the t-1
#      coefficients capture within-person changes

# regression3_v10d <- feols(
#   entry_owner ~
#     origin_group *
#     (unemployed_l1 + low_wage_l1) +
#     nilf_l1 +
#     liquidity_l1 +
#     liquidity_miss_l1 +
#     unemployed_l1_mean +
#     low_wage_l1_mean +
#     nilf_l1_mean +
#     liquidity_l1_mean +
#     homeowner_l1_mean +
#     seifa_decile_l1_mean +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3_cre,
#   cluster = ~xwaveid
# )

# 10e: liquidity split into cash-flow problems and hardship
# regression3_v10e <- feols(
#   entry_owner ~
#     origin_group *
#     (unemployed_l1 + low_wage_l1) +
#     nilf_l1 +
#     cashflow_l1 +
#     hardship_l1 +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3,
#   cluster = ~xwaveid
# )

# 10f: ages 21 to 64 only (sample used by Fairlie and Krashinsky 2012)
# regression3_v10f <- feols(
#   entry_owner ~
#     origin_group *
#     (unemployed_l1 + low_wage_l1) +
#     nilf_l1 +
#     liquidity_l1 +
#     liquidity_miss_l1 +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3 |> filter(between(age_hgage, 21, 64)),
#   cluster = ~xwaveid
# )

# 10g: two-way clustering (individual and industry-year) of the main model
# regression3_v10g <- summary(
#   regression3_v9b,
#   cluster = ~ xwaveid + industry_19categories_jbmi61_fe^year
# )

# 10h: separate regressions by origin group (Fairlie and Krashinsky 2012
#      style). The pooled interactions in 9b are the formal test; these show
#      the push within each group.
# regression3_v10h_native <- feols(
#   entry_owner ~
#     unemployed_l1 +
#     low_wage_l1 +
#     nilf_l1 +
#     liquidity_l1 +
#     liquidity_miss_l1 +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3 |> filter(origin_group == "native"),
#   cluster = ~xwaveid
# )

# regression3_v10h_mes <- feols(
#   entry_owner ~
#     unemployed_l1 +
#     low_wage_l1 +
#     nilf_l1 +
#     liquidity_l1 +
#     liquidity_miss_l1 +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3 |> filter(origin_group == "mes"),
#   cluster = ~xwaveid
# )

# regression3_v10h_mnes_higheng <- feols(
#   entry_owner ~
#     unemployed_l1 +
#     low_wage_l1 +
#     nilf_l1 +
#     liquidity_l1 +
#     liquidity_miss_l1 +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3 |> filter(origin_group == "mnes_higheng"),
#   cluster = ~xwaveid
# )

# Low-English group: 37 entries, so state and industry are used as simple
# fixed effects (not interacted with year) to keep the model estimable
# regression3_v10h_mnes_loweng <- feols(
#   entry_owner ~
#     unemployed_l1 +
#     low_wage_l1 +
#     nilf_l1 +
#     liquidity_l1 +
#     liquidity_miss_l1 +
#     highest_education_achieved_edhigh1_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     homeowner_l1 |
#     year + industry_19categories_jbmi61_fe + state_hhstate,
#   data = data_regression_3 |> filter(origin_group == "mnes_loweng"),
#   cluster = ~xwaveid
# )

# 10i-10k: first entries only
#   Drops person-waves where the person owned a business at t-2, so
#   re-entry after a one-wave gap (22.7% of entries) is not counted
#   as entry. Also required for the t-2 models to be interpretable.
# data_regression_3_new <- data_regression_3 |>
#   filter(owner_l2 == 0)

# 10i: main specification, first entries only
# regression3_v10i <- feols(
#   entry_owner ~
#     origin_group *
#     (unemployed_l1 + low_wage_l1) +
#     nilf_l1 +
#     liquidity_l1 +
#     liquidity_miss_l1 +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3_new,
#   cluster = ~xwaveid
# )

# 10j: status at t-1 and t-2 together, first entries only
# regression3_v10j <- feols(
#   entry_owner ~
#     origin_group *
#     (unemployed_l1 + low_wage_l1) +
#     unemployed_l2 +
#     low_wage_l2 +
#     nilf_l1 +
#     nilf_l2 +
#     liquidity_l1 +
#     liquidity_miss_l1 +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3_new |> drop_na(unemployed_l2, nilf_l2, low_wage_l2),
#   cluster = ~xwaveid
# )

# 10k: status at t-2 only, first entries only
# regression3_v10k <- feols(
#   entry_owner ~
#     origin_group *
#     (unemployed_l2 + low_wage_l2) +
#     nilf_l2 +
#     liquidity_l2 +
#     liquidity_miss_l2 +
#     highest_education_achieved_edhigh1_l1 +
#     tenure_emp_yrs_l1 +
#     age_hgage +
#     I(age_hgage^2) +
#     sex_hgsex +
#     years_in_aus_ctrl +
#     years_in_aus_miss +
#     marital_status_mrcurr_l1 +
#     homeowner_l1 +
#     seifa_decile_l1 +
#     remoteness_hhs3ra_l1 |
#     year + industry_19categories_jbmi61_fe^year + state_hhstate^year,
#   data = data_regression_3_new |> drop_na(unemployed_l2, nilf_l2, low_wage_l2),
#   cluster = ~xwaveid
# )

# Entries lost by origin group when re-entrants are dropped
# data_regression_3_new |>
#   summarise(entries = sum(entry_owner), .by = origin_group) |>
#   arrange(origin_group) |>
#   knitr::kable(caption = "Entry events by origin group, first entries only")

# Average marginal effects and tests on Version 9b; the thesis reports odds
# ratios from Version 9a instead
# ame_v4a <- marginaleffects::avg_slopes(
#   regression3_v4a,
#   variables = c(
#     "origin_group",
#     "unemployed_l1",
#     "nilf_l1",
#     "low_wage_l1",
#     "liquidity_l1"
#   ),
#   vcov = FALSE
# )

# Push by origin group from the main model (LPM, with standard errors):
# phi_1 + theta^U_g for unemployment, phi_3 + theta^W_g for low wage
# ame_v9b <- marginaleffects::avg_slopes(
#   regression3_v9b,
#   variables = c("unemployed_l1", "low_wage_l1"),
#   by = "origin_group",
#   newdata = data_regression_3 |> select(all_of(model3_vars_present))
# )

# Push by origin group from the logit (point estimates only: marginaleffects
# cannot compute standard errors for feglm with fixed effects)
# ame_v9a <- marginaleffects::avg_slopes(
#   regression3_v9a,
#   variables = c("unemployed_l1", "low_wage_l1"),
#   by = "origin_group",
#   vcov = FALSE,
#   newdata = data_regression_3[fixest::obs(regression3_v9a), model3_vars_present]
# )

# Is the push larger for low-English than high-English migrants?
# test_v9b_unemp_loweng_vs_higheng <- car::linearHypothesis(
#   regression3_v9b,
#   "origin_groupmnes_loweng:unemployed_l1 = origin_groupmnes_higheng:unemployed_l1",
#   test = "Chisq"
# )
# test_v9b_lowwage_loweng_vs_higheng <- car::linearHypothesis(
#   regression3_v9b,
#   "origin_groupmnes_loweng:low_wage_l1 = origin_groupmnes_higheng:low_wage_l1",
#   test = "Chisq"
# )

# Does the push differ by origin at all? (six interactions jointly zero)
# wald_v9b_interactions <- fixest::wald(
#   regression3_v9b,
#   keep = "^origin_group.*:(unemployed_l1|low_wage_l1)$"
# )

# Do the labour-market and liquidity variables matter? (jointly zero)
# wald_v9b_push <- fixest::wald(
#   regression3_v9b,
#   keep = "^(unemployed_l1|low_wage_l1|nilf_l1|liquidity_l1)$"
# )

# Values for the main table
# p_v9b_unemp <- test_v9b_unemp_loweng_vs_higheng[2, "Pr(>Chisq)"]
# p_v9b_lowwage <- test_v9b_lowwage_loweng_vs_higheng[2, "Pr(>Chisq)"]
# f_stat_v9b_interactions <- wald_v9b_interactions$stat
# p_val_v9b_interactions <- wald_v9b_interactions$p
# f_stat_v9b_push <- wald_v9b_push$stat
# p_val_v9b_push <- wald_v9b_push$p
# mean_entry_v9b <- mean(data_regression_3$entry_owner[fixest::obs(regression3_v9b)])

# Part B: event study of income before and after entry, by origin group
# (dropped from the thesis design)

# Identify entrants and construct event time

# First observed entry per person
# entry_year_df <- panel_block3 |>
#   filter(entry_owner == 1L) |>
#   summarise(entry_year = min(year, na.rm = TRUE), .by = xwaveid)

# event_time = year - entry_year (0 = entry year, -1 = year before entry)
# cohort = entry_year; never-entrants get 10000 (outside the panel), as
#   required by sunab()
# event_bin = event_time with endpoints binned at -4 and 5; never-entrants
#   get -1000 so they form the comparison group in i()
# data_regression_3_income <- panel_block3 |>
#   filter(!is.na(origin_group)) |>
#   left_join(entry_year_df, by = "xwaveid") |>
#   mutate(
#     ever_entrant = !is.na(entry_year),
#     ever_owner = any(is_owner == 1L, na.rm = TRUE),
#     .by = xwaveid
#   ) |>
#   filter(ever_entrant | !ever_owner) |>
#   mutate(
#     event_time = if_else(
#       ever_entrant,
#       as.numeric(year - entry_year),
#       NA_real_
#     ),
#     cohort = if_else(ever_entrant, as.numeric(entry_year), 10000),
#     event_bin = case_when(
#       !ever_entrant ~ -1000,
#       event_time <= -4 ~ -4,
#       event_time >= 5 ~ 5,
#       TRUE ~ event_time
#     )
#   )

# Sun-Abraham sample: entrants within four years before and five years after
# entry, plus all never-entrants
# data_regression_3_income_window <- data_regression_3_income |>
#   filter(!ever_entrant | between(event_time, -4, 5))

# n_obs_es <- nrow(data_regression_3_income)
# n_persons_es <- n_distinct(data_regression_3_income$xwaveid)

# Summary statistics

# Number of entrants by origin group
# data_regression_3_income |>
#   filter(ever_entrant) |>
#   distinct(xwaveid, origin_group, entry_year) |>
#   summarise(
#     n_entrants = n(),
#     min_entry_year = min(entry_year),
#     max_entry_year = max(entry_year),
#     .by = origin_group
#   ) |>
#   arrange(origin_group) |>
#   knitr::kable(caption = "Number of entrants by origin group and entry year range")

# Mean income by event time. Read the low-English regression alongside this
# table: it rests on about 30 entrants.
# data_regression_3_income |>
#   filter(ever_entrant, between(event_time, -4, 5)) |>
#   summarise(
#     n_obs = n(),
#     mean_income = mean(income_labour_and_business, na.rm = TRUE),
#     .by = c(origin_group, event_time)
#   ) |>
#   arrange(origin_group, event_time) |>
#   knitr::kable(
#     digits = 0,
#     caption = "Observations and mean labour and business income by event time",
#     format.args = list(big.mark = ",")
#   )

# Share of entrants still owning a business after entry (replaces the former
# ownership_history spell table)
# data_regression_3_income |>
#   filter(ever_entrant, between(event_time, 0, 5)) |>
#   summarise(
#     share_still_owner = mean(is_owner == 1L, na.rm = TRUE),
#     n = n(),
#     .by = c(origin_group, event_time)
#   ) |>
#   arrange(origin_group, event_time) |>
#   knitr::kable(
#     digits = 3,
#     caption = "Share of entrants still owning a business, by years since entry"
#   )

# Event-study models by origin group
# Two estimators:
#   regression3_income_* two-way fixed effects with binned event time,
#   reference = year before entry (-1)
#   regression3_income_*_sa Sun and Abraham (2021) via sunab(), robust to
#   effects that differ by entry cohort

# Native-born
# regression3_income_native <- feols(
#   income_labour_and_business ~
#     i(event_bin, ref = c(-1, -1000)) +
#     I(age_hgage^2) +
#     highest_education_achieved_edhigh1 +
#     seifa_decile_l1 |
#     xwaveid + state_hhstate^year,
#   data = data_regression_3_income |> filter(origin_group == "native"),
#   cluster = ~xwaveid
# )

# regression3_income_native_sa <- feols(
#   income_labour_and_business ~
#     sunab(cohort, year) +
#     I(age_hgage^2) +
#     highest_education_achieved_edhigh1 +
#     seifa_decile_l1 |
#     xwaveid + state_hhstate^year,
#   data = data_regression_3_income_window |> filter(origin_group == "native"),
#   cluster = ~xwaveid
# )

# All migrants
# regression3_income_migrant <- feols(
#   income_labour_and_business ~
#     i(event_bin, ref = c(-1, -1000)) +
#     I(age_hgage^2) +
#     highest_education_achieved_edhigh1 +
#     seifa_decile_l1 |
#     xwaveid + state_hhstate^year,
#   data = data_regression_3_income |> filter(origin_group != "native"),
#   cluster = ~xwaveid
# )

# regression3_income_migrant_sa <- feols(
#   income_labour_and_business ~
#     sunab(cohort, year) +
#     I(age_hgage^2) +
#     highest_education_achieved_edhigh1 +
#     seifa_decile_l1 |
#     xwaveid + state_hhstate^year,
#   data = data_regression_3_income_window |> filter(origin_group != "native"),
#   cluster = ~xwaveid
# )

# regression3_income_mnes_higheng <- feols(
#   income_labour_and_business ~
#     i(event_bin, ref = c(-1, -1000)) +
#     I(age_hgage^2) +
#     highest_education_achieved_edhigh1 +
#     seifa_decile_l1 |
#     xwaveid + state_hhstate^year,
#   data = data_regression_3_income |> filter(origin_group == "mnes_higheng"),
#   cluster = ~xwaveid
# )

# regression3_income_mnes_higheng_sa <- feols(
#   income_labour_and_business ~
#     sunab(cohort, year) +
#     I(age_hgage^2) +
#     highest_education_achieved_edhigh1 +
#     seifa_decile_l1 |
#     xwaveid + state_hhstate^year,
#   data = data_regression_3_income_window |> filter(origin_group == "mnes_higheng"),
#   cluster = ~xwaveid
# )

# Low-English migrants. Only about 30 entrants: estimates will be imprecise
# and should be read alongside the descriptive table above.
# regression3_income_mnes_loweng <- feols(
#   income_labour_and_business ~
#     i(event_bin, ref = c(-1, -1000)) +
#     I(age_hgage^2) +
#     highest_education_achieved_edhigh1 +
#     seifa_decile_l1 |
#     xwaveid + state_hhstate^year,
#   data = data_regression_3_income |> filter(origin_group == "mnes_loweng"),
#   cluster = ~xwaveid
# )

# regression3_income_mnes_loweng_sa <- feols(
#   income_labour_and_business ~
#     sunab(cohort, year) +
#     I(age_hgage^2) +
#     highest_education_achieved_edhigh1 +
#     seifa_decile_l1 |
#     xwaveid + state_hhstate^year,
#   data = data_regression_3_income_window |> filter(origin_group == "mnes_loweng"),
#   cluster = ~xwaveid
# )

# The four regressions of @eq-event-study

# Pre-entry tests
# Are the coefficients at event time -4, -3 and -2 jointly zero? A rejection
# means income was already changing before entry.

# pre_test_income_native <- fixest::wald(
#   regression3_income_native,
#   keep = "^event_bin::-[234]$"
# )
# pre_test_income_migrant <- fixest::wald(
#   regression3_income_migrant,
#   keep = "^event_bin::-[234]$"
# )
# pre_test_income_mnes_higheng <- fixest::wald(
#   regression3_income_mnes_higheng,
#   keep = "^event_bin::-[234]$"
# )
# pre_test_income_mnes_loweng <- fixest::wald(
#   regression3_income_mnes_loweng,
#   keep = "^event_bin::-[234]$"
# )

# Do migrant income paths differ from native paths?
# Pooled model: common event-time dummies plus event time x migrant. The
# interaction coefficients are the migrant-native difference at each event time.

# regression3_income_diff <- feols(
#   income_labour_and_business ~
#     i(event_bin, ref = c(-1, -1000)) +
#     i(event_bin, migrant, ref = c(-1, -1000)) +
#     I(age_hgage^2) +
#     highest_education_achieved_edhigh1 +
#     seifa_decile_l1 |
#     xwaveid + state_hhstate^year,
#   data = data_regression_3_income,
#   cluster = ~xwaveid
# )

# Difference after entry (event time 0 to 5)
# diff_test_income_post <- fixest::wald(
#   regression3_income_diff,
#   keep = "^event_bin::[0-5]:migrant$"
# )

# Difference before entry (event time -4 to -2)
# diff_test_income_pre <- fixest::wald(
#   regression3_income_diff,
#   keep = "^event_bin::-[234]:migrant$"
# )

# Do low-English income paths differ from high-English paths?
# Same pooled design within migrants from non-English-speaking countries.
# The interaction coefficients are the low- minus high-English difference.

# regression3_income_diff_english <- feols(
#   income_labour_and_business ~
#     i(event_bin, ref = c(-1, -1000)) +
#     i(event_bin, low_english_timeinvariant, ref = c(-1, -1000)) +
#     I(age_hgage^2) +
#     highest_education_achieved_edhigh1 +
#     seifa_decile_l1 |
#     xwaveid + state_hhstate^year,
#   data = data_regression_3_income |>
#     filter(origin_group %in% c("mnes_higheng", "mnes_loweng")),
#   cluster = ~xwaveid
# )

# diff_test_income_english_post <- fixest::wald(
#   regression3_income_diff_english,
#   keep = "^event_bin::[0-5]:low_english_timeinvariant$"
# )
# diff_test_income_english_pre <- fixest::wald(
#   regression3_income_diff_english,
#   keep = "^event_bin::-[234]:low_english_timeinvariant$"
# )

# ==============================================================================
# End of 07_analysisblock3.R
# ==============================================================================
