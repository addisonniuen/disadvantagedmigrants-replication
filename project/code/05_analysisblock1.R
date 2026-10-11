# ==============================================================================
# 05_analysisblock1.R
#
# Block 1: employee wages. This script estimates how the hourly wage of
# employees in small single-site workplaces varies with the share of migrant
# business owners in their industry. It:
#   - Builds the estimation sample (data_regression_1)
#   - Estimates the main models under three decay weightings (Table 5.1)
#   - Re-estimates them with migrant and low-English shares (Table B.7) and
#     with two-digit industries (Table B.8)
#   - Re-estimates them with 10- and 50-employee size thresholds (Tables B.2, B.4, B.6)
#   - Estimates the chain placebo (Table B.3, Figure 5.1)
#   - Tests the owner-share interactions jointly (Tables B.2, B.4-B.6)
#   - Computes wild cluster bootstrap p-values, clustering by industry (Table B.9)
#   - Re-estimates the three main models with owner shares weighted by the
#     longitudinal weight lnwtrp instead of the cross-sectional weight hhwtrp (Table B.10)
#
# Models that are not in the thesis are kept at the end, commented out.
# Sample checks for this block are in 04_eda.R.
#
# This script requires these files to be sourced first:
#   source("project/code/01_setup.R")
#   source("project/code/02_datacleaning.R")
#   source("project/code/03_featureengineering.R")
# ==============================================================================

# --- Create the data set for block 1 ------------------------------------------

loo_typed <- c(
  "loo_mes",
  "loo_mnes",
  "loo_mnes_higheng",
  "loo_mnes_loweng",
  "n_owners"
)
loo_plain <- c(
  "loo_migrant_share",
  "loo_native_share",
  "loo_loweng_share",
  "loo_higheng_share"
)
loo_types <- c("type0", "type1", "type2")
loo_dig <- c("1d", "2d")

all_loo_vars <- c(
  as.vector(outer(
    loo_typed,
    outer(loo_dig, loo_types, paste, sep = "_"),
    paste,
    sep = "_"
  )),
  as.vector(outer(loo_plain, loo_dig, paste, sep = "_"))
)
model1_vars <- c(
  # dependent variable
  "hrwge_main_clean",
  "log_hrwge",
  # firm type
  "small_single_site",
  "chain_like",
  "small_site_chain",
  all_loo_vars,
  # Shares weighted by lnwtrp, for the sensitivity check in Table B.10
  as.vector(outer(
    c("loo_mes_1d_", "loo_mnes_higheng_1d_", "loo_mnes_loweng_1d_"),
    c("type0_lnw", "type1_lnw", "type2_lnw"),
    paste0
  )),
  "small_single_site_10",
  "small_site_chain_10",
  "small_single_site_50",
  "small_site_chain_50",
  # fixed effects
  "xwaveid",
  "year",
  "industry_19categories_jbmi61",
  "industry_sparsecategories_jbmi62",
  "state_hhstate",
  "wave",
  # X_it
  "occupation_1dig_jbmo61",
  "occupation_agent_plausible_subgroup",
  "casual_perm_jbcasab",
  "highest_education_achieved_edhigh1",
  "remoteness_hhs3ra",
  "sex_hgsex",
  "age_hgage",
  "migrant",
  "origin_group",
  "disadvantage_decile_hhs3add",
  "seifa_decile",
  "tenure_current_occ_jbocct",
  "tenure_current_employer_jbempt",
  "hours_weekly_main_job_jbmhruc",
  "union_employeeassociation_membership_jbmtuea"
)

data_regression_1 <- employee_sample |>
  # Waves 22-24 carry no firm-size data, so block 1 is restricted to waves 1-21
  filter(!wave %in% 22:24) |>
  select(all_of(c(model1_vars))) |>
  mutate(
    across(
      c(occupation_1dig_jbmo61, casual_perm_jbcasab, year, wave, migrant),
      factor
    ),
    disadvantage_decile_hhs3add = factor(
      disadvantage_decile_hhs3add,
      ordered = TRUE,
      levels = c(
        "decile 1 (most disadvantaged)",
        "decile 2",
        "decile 3",
        "decile 4",
        "decile 5",
        "decile 6",
        "decile 7",
        "decile 8",
        "decile 9",
        "decile 10 (least disadvantaged)"
      )
    ),
    # "Undetermined" education is missing (about 48 observations)
    highest_education_achieved_edhigh1 = forcats::fct_recode(
      highest_education_achieved_edhigh1,
      NULL = "undetermined"
    ),
    english_speaking_background = factor(
      case_when(
        is.na(origin_group) ~ NA_character_,
        origin_group %in% c("native", "mes") ~ "english",
        TRUE ~ "non-english"
      ),
      levels = c("english", "non-english")
    ),
    highest_education_achieved_edhigh1 = forcats::fct_relevel(
      highest_education_achieved_edhigh1,
      "year 11 and below"
    ),
    disadvantage_decile_hhs3add = forcats::fct_relevel(
      disadvantage_decile_hhs3add,
      "decile 1 (most disadvantaged)"
    )
  )

# --- Decay rates from the calibration (quoted in the thesis) ------------------

lambda_mean <- cal$lambda_mean # type 1 best
lambda_mes <- cal$lambda_mes # type 2 best for mes
lambda_other <- cal$lambda_other # type 2 best for mnes

# --- Estimate regression 1 ----------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Version 1: origin and English-proficiency shares, 19 industries (Table 5.1)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# (a) Type 0 LOO shares
regression1_v1a <- feols(
  log(hrwge_main_clean) ~
    small_single_site +
    chain_like +
    small_site_chain +
    small_single_site:loo_mes_1d_type0 +
    small_single_site:loo_mnes_higheng_1d_type0 +
    small_single_site:loo_mnes_loweng_1d_type0 +
    age_hgage +
    sex_hgsex +
    origin_group +
    casual_perm_jbcasab +
    occupation_1dig_jbmo61 +
    highest_education_achieved_edhigh1 +
    seifa_decile +
    tenure_current_occ_jbocct +
    tenure_current_employer_jbempt +
    union_employeeassociation_membership_jbmtuea |
    year + industry_19categories_jbmi61^year + state_hhstate^year,
  data = data_regression_1,
  cluster = ~ xwaveid + industry_19categories_jbmi61^year
)

# (a'-c') The three specifications with the owner shares weighted by the
# longitudinal weight lnwtrp instead of the cross-sectional weight hhwtrp, with
# decay rates calibrated on the lnwtrp-weighted shares (sensitivity check, Table B.10)
b1_lnw_formula <- function(type) {
  as.formula(paste(
    "log(hrwge_main_clean) ~ small_single_site + chain_like + small_site_chain +",
    paste0("small_single_site:", c("loo_mes_1d_", "loo_mnes_higheng_1d_", "loo_mnes_loweng_1d_"),
           type, "_lnw", collapse = " + "),
    "+ age_hgage + sex_hgsex + origin_group + casual_perm_jbcasab + occupation_1dig_jbmo61 +",
    "highest_education_achieved_edhigh1 + seifa_decile + tenure_current_occ_jbocct +",
    "tenure_current_employer_jbempt + union_employeeassociation_membership_jbmtuea",
    "| year + industry_19categories_jbmi61^year + state_hhstate^year"
  ))
}
regression1_lnw <- purrr::map(
  c(type0 = "type0", type1 = "type1", type2 = "type2"),
  \(type) feols(
    b1_lnw_formula(type),
    data = data_regression_1,
    cluster = ~ xwaveid + industry_19categories_jbmi61^year
  )
)

# (b) Type 1 LOO shares
regression1_v1b <- feols(
  log(hrwge_main_clean) ~
    small_single_site +
    chain_like +
    small_site_chain +
    small_single_site:loo_mes_1d_type1 +
    small_single_site:loo_mnes_higheng_1d_type1 +
    small_single_site:loo_mnes_loweng_1d_type1 +
    age_hgage +
    sex_hgsex +
    origin_group +
    casual_perm_jbcasab +
    occupation_1dig_jbmo61 +
    highest_education_achieved_edhigh1 +
    seifa_decile +
    tenure_current_occ_jbocct +
    tenure_current_employer_jbempt +
    union_employeeassociation_membership_jbmtuea |
    year + industry_19categories_jbmi61^year + state_hhstate^year,
  data = data_regression_1,
  cluster = ~ xwaveid + industry_19categories_jbmi61^year
)

# (c) Type 2 LOO shares
regression1_v1c <- feols(
  log(hrwge_main_clean) ~
    small_single_site +
    chain_like +
    small_site_chain +
    small_single_site:loo_mes_1d_type2 +
    small_single_site:loo_mnes_higheng_1d_type2 +
    small_single_site:loo_mnes_loweng_1d_type2 +
    age_hgage +
    sex_hgsex +
    origin_group +
    casual_perm_jbcasab +
    occupation_1dig_jbmo61 +
    highest_education_achieved_edhigh1 +
    seifa_decile +
    tenure_current_occ_jbocct +
    tenure_current_employer_jbempt +
    union_employeeassociation_membership_jbmtuea |
    year + industry_19categories_jbmi61^year + state_hhstate^year,
  data = data_regression_1,
  cluster = ~ xwaveid + industry_19categories_jbmi61^year
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Version 2: migrant and low-English shares, 19 industries (Table B.7)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

regression1_v2a <- feols(
  log(hrwge_main_clean) ~
    small_single_site +
    chain_like +
    small_site_chain +
    small_single_site:loo_migrant_share_1d +
    small_single_site:loo_loweng_share_1d +
    age_hgage +
    sex_hgsex +
    origin_group +
    casual_perm_jbcasab +
    occupation_1dig_jbmo61 +
    highest_education_achieved_edhigh1 +
    seifa_decile +
    tenure_current_occ_jbocct +
    tenure_current_employer_jbempt +
    union_employeeassociation_membership_jbmtuea |
    year + industry_19categories_jbmi61^year + state_hhstate^year,
  data = data_regression_1,
  cluster = ~ xwaveid + industry_19categories_jbmi61^year
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Version 3: origin and English-proficiency shares, two-digit industries (Table B.8)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# (a) Type 0 LOO shares
regression1_v3a <- feols(
  log(hrwge_main_clean) ~
    small_single_site +
    chain_like +
    small_site_chain +
    small_single_site:loo_mes_2d_type0 +
    small_single_site:loo_mnes_higheng_2d_type0 +
    small_single_site:loo_mnes_loweng_2d_type0 +
    age_hgage +
    sex_hgsex +
    origin_group +
    casual_perm_jbcasab +
    occupation_1dig_jbmo61 +
    highest_education_achieved_edhigh1 +
    seifa_decile +
    tenure_current_occ_jbocct +
    tenure_current_employer_jbempt +
    union_employeeassociation_membership_jbmtuea |
    year + industry_sparsecategories_jbmi62^year + state_hhstate^year,
  data = data_regression_1,
  cluster = ~ xwaveid + industry_19categories_jbmi61^year
)

# (b) Type 1 LOO shares
regression1_v3b <- feols(
  log(hrwge_main_clean) ~
    small_single_site +
    chain_like +
    small_site_chain +
    small_single_site:loo_mes_2d_type1 +
    small_single_site:loo_mnes_higheng_2d_type1 +
    small_single_site:loo_mnes_loweng_2d_type1 +
    age_hgage +
    sex_hgsex +
    origin_group +
    casual_perm_jbcasab +
    occupation_1dig_jbmo61 +
    highest_education_achieved_edhigh1 +
    seifa_decile +
    tenure_current_occ_jbocct +
    tenure_current_employer_jbempt +
    union_employeeassociation_membership_jbmtuea |
    year + industry_sparsecategories_jbmi62^year + state_hhstate^year,
  data = data_regression_1,
  cluster = ~ xwaveid + industry_19categories_jbmi61^year
)

# (c) Type 2 LOO shares
regression1_v3c <- feols(
  log(hrwge_main_clean) ~
    small_single_site +
    chain_like +
    small_site_chain +
    small_single_site:loo_mes_2d_type2 +
    small_single_site:loo_mnes_higheng_2d_type2 +
    small_single_site:loo_mnes_loweng_2d_type2 +
    age_hgage +
    sex_hgsex +
    origin_group +
    casual_perm_jbcasab +
    occupation_1dig_jbmo61 +
    highest_education_achieved_edhigh1 +
    seifa_decile +
    tenure_current_occ_jbocct +
    tenure_current_employer_jbempt +
    union_employeeassociation_membership_jbmtuea |
    year + industry_sparsecategories_jbmi62^year + state_hhstate^year,
  data = data_regression_1,
  cluster = ~ xwaveid + industry_19categories_jbmi61^year
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Version 4: migrant and low-English shares, two-digit industries (Table B.7)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

regression1_v4a <- feols(
  log(hrwge_main_clean) ~
    small_single_site +
    chain_like +
    small_site_chain +
    small_single_site:loo_migrant_share_2d +
    small_single_site:loo_loweng_share_2d +
    age_hgage +
    sex_hgsex +
    origin_group +
    casual_perm_jbcasab +
    occupation_1dig_jbmo61 +
    highest_education_achieved_edhigh1 +
    seifa_decile +
    tenure_current_occ_jbocct +
    tenure_current_employer_jbempt +
    union_employeeassociation_membership_jbmtuea |
    year + industry_sparsecategories_jbmi62^year + state_hhstate^year,
  data = data_regression_1,
  cluster = ~ xwaveid + industry_19categories_jbmi61^year
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Version 1 with 10- and 50-employee size thresholds (Tables B.2, B.4, B.6)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# (a) Type 0 LOO shares
regression1_v1a_check_wpsize10 <- feols(
  log_hrwge ~
    small_single_site_10 +
    chain_like +
    small_site_chain_10 +
    small_single_site_10:loo_mes_1d_type0 +
    small_single_site_10:loo_mnes_higheng_1d_type0 +
    small_single_site_10:loo_mnes_loweng_1d_type0 +
    age_hgage +
    sex_hgsex +
    origin_group +
    casual_perm_jbcasab +
    occupation_1dig_jbmo61 +
    highest_education_achieved_edhigh1 +
    seifa_decile +
    tenure_current_occ_jbocct +
    tenure_current_employer_jbempt +
    union_employeeassociation_membership_jbmtuea |
    year + industry_19categories_jbmi61^year + state_hhstate^year,
  data = data_regression_1,
  cluster = ~ xwaveid + industry_19categories_jbmi61^year
)

regression1_v1a_check_wpsize50 <- feols(
  log_hrwge ~
    small_single_site_50 +
    chain_like +
    small_site_chain_50 +
    small_single_site_50:loo_mes_1d_type0 +
    small_single_site_50:loo_mnes_higheng_1d_type0 +
    small_single_site_50:loo_mnes_loweng_1d_type0 +
    age_hgage +
    sex_hgsex +
    origin_group +
    casual_perm_jbcasab +
    occupation_1dig_jbmo61 +
    highest_education_achieved_edhigh1 +
    seifa_decile +
    tenure_current_occ_jbocct +
    tenure_current_employer_jbempt +
    union_employeeassociation_membership_jbmtuea |
    year + industry_19categories_jbmi61^year + state_hhstate^year,
  data = data_regression_1,
  cluster = ~ xwaveid + industry_19categories_jbmi61^year
)

# (b) Type 1 LOO shares
regression1_v1b_check_wpsize10 <- feols(
  log(hrwge_main_clean) ~
    small_single_site_10 +
    chain_like +
    small_site_chain_10 +
    small_single_site_10:loo_mes_1d_type1 +
    small_single_site_10:loo_mnes_higheng_1d_type1 +
    small_single_site_10:loo_mnes_loweng_1d_type1 +
    age_hgage +
    sex_hgsex +
    origin_group +
    casual_perm_jbcasab +
    occupation_1dig_jbmo61 +
    highest_education_achieved_edhigh1 +
    seifa_decile +
    tenure_current_occ_jbocct +
    tenure_current_employer_jbempt +
    union_employeeassociation_membership_jbmtuea |
    year + industry_19categories_jbmi61^year + state_hhstate^year,
  data = data_regression_1,
  cluster = ~ xwaveid + industry_19categories_jbmi61^year
)

regression1_v1b_check_wpsize50 <- feols(
  log(hrwge_main_clean) ~
    small_single_site_50 +
    chain_like +
    small_site_chain_50 +
    small_single_site_50:loo_mes_1d_type1 +
    small_single_site_50:loo_mnes_higheng_1d_type1 +
    small_single_site_50:loo_mnes_loweng_1d_type1 +
    age_hgage +
    sex_hgsex +
    origin_group +
    casual_perm_jbcasab +
    occupation_1dig_jbmo61 +
    highest_education_achieved_edhigh1 +
    seifa_decile +
    tenure_current_occ_jbocct +
    tenure_current_employer_jbempt +
    union_employeeassociation_membership_jbmtuea |
    year + industry_19categories_jbmi61^year + state_hhstate^year,
  data = data_regression_1,
  cluster = ~ xwaveid + industry_19categories_jbmi61^year
)

# (c) Type 2 LOO shares
regression1_v1c_check_wpsize10 <- feols(
  log(hrwge_main_clean) ~
    small_single_site_10 +
    chain_like +
    small_site_chain_10 +
    small_single_site_10:loo_mes_1d_type2 +
    small_single_site_10:loo_mnes_higheng_1d_type2 +
    small_single_site_10:loo_mnes_loweng_1d_type2 +
    age_hgage +
    sex_hgsex +
    origin_group +
    casual_perm_jbcasab +
    occupation_1dig_jbmo61 +
    highest_education_achieved_edhigh1 +
    seifa_decile +
    tenure_current_occ_jbocct +
    tenure_current_employer_jbempt +
    union_employeeassociation_membership_jbmtuea |
    year + industry_19categories_jbmi61^year + state_hhstate^year,
  data = data_regression_1,
  cluster = ~ xwaveid + industry_19categories_jbmi61^year
)

regression1_v1c_check_wpsize50 <- feols(
  log(hrwge_main_clean) ~
    small_single_site_50 +
    chain_like +
    small_site_chain_50 +
    small_single_site_50:loo_mes_1d_type2 +
    small_single_site_50:loo_mnes_higheng_1d_type2 +
    small_single_site_50:loo_mnes_loweng_1d_type2 +
    age_hgage +
    sex_hgsex +
    origin_group +
    casual_perm_jbcasab +
    occupation_1dig_jbmo61 +
    highest_education_achieved_edhigh1 +
    seifa_decile +
    tenure_current_occ_jbocct +
    tenure_current_employer_jbempt +
    union_employeeassociation_membership_jbmtuea |
    year + industry_19categories_jbmi61^year + state_hhstate^year,
  data = data_regression_1,
  cluster = ~ xwaveid + industry_19categories_jbmi61^year
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Placebo: owner shares interacted with chain workplaces (Table B.3, Figure 5.1)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Chains are rarely migrant-owned, so their interactions should be close to zero
# if the small single-site interactions reflect migrant ownership
placebo_formula_b1 <- function(type) {
  shares <- paste0(c("loo_mes_1d_", "loo_mnes_higheng_1d_", "loo_mnes_loweng_1d_"), type)
  inter <- outer(c("small_single_site", "small_site_chain", "chain_like"), shares, paste, sep = ":")
  as.formula(paste(
    "log(hrwge_main_clean) ~ small_single_site + chain_like + small_site_chain +",
    paste(t(inter), collapse = " + "),
    "+ age_hgage + sex_hgsex + origin_group + casual_perm_jbcasab + occupation_1dig_jbmo61 +",
    "highest_education_achieved_edhigh1 + seifa_decile + tenure_current_occ_jbocct +",
    "tenure_current_employer_jbempt + union_employeeassociation_membership_jbmtuea",
    "| year + industry_19categories_jbmi61^year + state_hhstate^year"
  ))
}

regression1_placebo <- purrr::map(
  c(type0 = "type0", type1 = "type1", type2 = "type2"),
  \(type) feols(
    placebo_formula_b1(type),
    data = data_regression_1,
    cluster = ~ xwaveid + industry_19categories_jbmi61^year
  )
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# F-tests: owner-share interactions jointly zero (Tables B.2, B.4-B.6)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 10-employee threshold
test_10a <- cluster_f_test(
  regression1_v1a_check_wpsize10,
  c(
    "small_single_site_10:loo_mes_1d_type0 = 0",
    "small_single_site_10:loo_mnes_higheng_1d_type0 = 0",
    "small_single_site_10:loo_mnes_loweng_1d_type0 = 0"
  )
)
f_stat_10_a <- test_10a[2, "F"]

test_10b <- cluster_f_test(
  regression1_v1b_check_wpsize10,
  c(
    "small_single_site_10:loo_mes_1d_type1 = 0",
    "small_single_site_10:loo_mnes_higheng_1d_type1 = 0",
    "small_single_site_10:loo_mnes_loweng_1d_type1 = 0"
  )
)
f_stat_10_b <- test_10b[2, "F"]

test_10c <- cluster_f_test(
  regression1_v1c_check_wpsize10,
  c(
    "small_single_site_10:loo_mes_1d_type2 = 0",
    "small_single_site_10:loo_mnes_higheng_1d_type2 = 0",
    "small_single_site_10:loo_mnes_loweng_1d_type2 = 0"
  )
)
f_stat_10_c <- test_10c[2, "F"]

# 20-employee threshold (Version 1)
test_20a <- cluster_f_test(
  regression1_v1a,
  c(
    "small_single_site:loo_mes_1d_type0 = 0",
    "small_single_site:loo_mnes_higheng_1d_type0 = 0",
    "small_single_site:loo_mnes_loweng_1d_type0 = 0"
  )
)
f_stat_20_a <- test_20a[2, "F"]

test_20b <- cluster_f_test(
  regression1_v1b,
  c(
    "small_single_site:loo_mes_1d_type1 = 0",
    "small_single_site:loo_mnes_higheng_1d_type1 = 0",
    "small_single_site:loo_mnes_loweng_1d_type1 = 0"
  )
)
f_stat_20_b <- test_20b[2, "F"]

test_20c <- cluster_f_test(
  regression1_v1c,
  c(
    "small_single_site:loo_mes_1d_type2 = 0",
    "small_single_site:loo_mnes_higheng_1d_type2 = 0",
    "small_single_site:loo_mnes_loweng_1d_type2 = 0"
  )
)
f_stat_20_c <- test_20c[2, "F"]

# 50-employee threshold
test_50a <- cluster_f_test(
  regression1_v1a_check_wpsize50,
  c(
    "small_single_site_50:loo_mes_1d_type0 = 0",
    "small_single_site_50:loo_mnes_higheng_1d_type0 = 0",
    "small_single_site_50:loo_mnes_loweng_1d_type0 = 0"
  )
)
f_stat_50_a <- test_50a[2, "F"]

test_50b <- cluster_f_test(
  regression1_v1b_check_wpsize50,
  c(
    "small_single_site_50:loo_mes_1d_type1 = 0",
    "small_single_site_50:loo_mnes_higheng_1d_type1 = 0",
    "small_single_site_50:loo_mnes_loweng_1d_type1 = 0"
  )
)
f_stat_50_b <- test_50b[2, "F"]

test_50c <- cluster_f_test(
  regression1_v1c_check_wpsize50,
  c(
    "small_single_site_50:loo_mes_1d_type2 = 0",
    "small_single_site_50:loo_mnes_higheng_1d_type2 = 0",
    "small_single_site_50:loo_mnes_loweng_1d_type2 = 0"
  )
)
f_stat_50_c <- test_50c[2, "F"]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Wild cluster bootstrap by industry (Table B.9)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The owner shares move slowly within an industry, so errors may be correlated
# across all years of an industry. Clustering by the 19 industries allows for
# this, and with so few clusters p-values come from a wild cluster bootstrap
# that imposes the null (Cameron, Gelbach and Miller 2008) with Webb weights.
# The fixed effects are partialled out first, so each replication is a small
# matrix calculation.
wild_cluster_boot <- function(m, data, terms, cluster_var, B = 9999, seed = 1) {
  d <- data[fixest::obs(m), ]
  keep <- names(coef(m))
  y <- model.matrix(m, type = "lhs")
  X <- model.matrix(m, type = "rhs")[, keep, drop = FALSE]
  fe <- model.matrix(m, type = "fixef")
  yX <- fixest::demean(cbind(y = y, X), f = fe)
  y <- yX[, 1]
  X <- yX[, -1, drop = FALSE]
  g <- as.integer(factor(d[[cluster_var]]))
  G <- max(g)
  n <- nrow(X)
  k <- ncol(X)
  adj <- (G / (G - 1)) * ((n - 1) / (n - k))

  # Clustered covariance from per-cluster scores
  cr1 <- function(Binv, scores) adj * Binv %*% tcrossprod(scores) %*% Binv

  XtX_g <- lapply(seq_len(G), \(c) crossprod(X[g == c, , drop = FALSE]))
  Binv <- solve(crossprod(X))
  b <- drop(Binv %*% crossprod(X, y))
  u <- y - drop(X %*% b)
  V <- cr1(Binv, t(rowsum(X * u, g)))
  idx <- match(terms, keep)

  # Observed statistics: t for each term, Wald for all terms jointly
  t_obs <- b[idx] / sqrt(diag(V)[idx])
  w_obs <- drop(t(b[idx]) %*% solve(V[idx, idx]) %*% b[idx])

  webb <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
  set.seed(seed)
  W <- matrix(sample(webb, G * B, replace = TRUE), nrow = G)

  # Bootstrap distribution under a null that sets the terms in `drop` to zero
  boot_stats <- function(drop) {
    Xr <- X[, -match(drop, keep), drop = FALSE]
    br <- solve(crossprod(Xr), crossprod(Xr, y))
    fit <- drop(Xr %*% br)
    ur <- y - fit
    C <- t(rowsum(X * fit, g))
    D <- t(rowsum(X * ur, g))
    out <- matrix(NA_real_, B, length(idx) + 1)
    for (r in seq_len(B)) {
      w <- W[, r]
      b_star <- drop(Binv %*% (rowSums(C) + D %*% w))
      scores <- C + sweep(D, 2, w, `*`) -
        vapply(seq_len(G), \(c) drop(XtX_g[[c]] %*% b_star), numeric(k))
      V_star <- cr1(Binv, scores)
      out[r, seq_along(idx)] <- b_star[idx] / sqrt(diag(V_star)[idx])
      out[r, length(idx) + 1] <- drop(t(b_star[idx]) %*% solve(V_star[idx, idx]) %*% b_star[idx])
    }
    out
  }

  # One null per term for the t tests, and all terms together for the joint test
  p_t <- vapply(seq_along(idx), \(j) {
    mean(abs(boot_stats(terms[j])[, j]) >= abs(t_obs[j]))
  }, numeric(1))
  p_joint <- mean(boot_stats(terms)[, length(idx) + 1] >= w_obs)

  tibble::tibble(
    term = c(terms, "joint"),
    estimate = c(b[idx], NA_real_),
    se_industry = c(sqrt(diag(V)[idx]), NA_real_),
    p_wild = c(p_t, p_joint)
  )
}

b1_interactions <- \(type) paste0(
  "small_single_site:", c("loo_mes_1d_", "loo_mnes_higheng_1d_", "loo_mnes_loweng_1d_"), type
)

regression1_wild <- purrr::imap_dfr(
  list(type0 = regression1_v1a, type1 = regression1_v1b, type2 = regression1_v1c),
  \(m, type) wild_cluster_boot(
    m, data_regression_1, b1_interactions(type), "industry_19categories_jbmi61"
  ) |>
    dplyr::mutate(type = type, .before = 1)
)

# Same bootstrap for the lnwtrp-weighted shares (Table B.10)
regression1_wild_lnw <- purrr::imap_dfr(
  regression1_lnw,
  \(m, type) wild_cluster_boot(
    m, data_regression_1, b1_interactions(paste0(type, "_lnw")), "industry_19categories_jbmi61"
  ) |>
    dplyr::mutate(type = type, .before = 1)
)

# --- Not in the thesis (commented out) ----------------------------------------

# Version 4b: identical to Version 4a (the migrant and low-English shares have
# no decay types), so it adds nothing
# regression1_v4b <- feols(
#   log(hrwge_main_clean) ~
#     small_single_site +
#     chain_like +
#     small_site_chain +
#     small_single_site:loo_migrant_share_2d +
#     small_single_site:loo_loweng_share_2d +
#     age_hgage +
#     sex_hgsex +
#     casual_perm_jbcasab +
#     occupation_1dig_jbmo61 +
#     highest_education_achieved_edhigh1 +
#     seifa_decile +
#     tenure_current_occ_jbocct +
#     tenure_current_employer_jbempt +
#     union_employeeassociation_membership_jbmtuea |
#     year + industry_sparsecategories_jbmi62^year + state_hhstate^year,
#   data = data_regression_1,
#   cluster = ~ xwaveid + industry_19categories_jbmi61^year
# )

# Versions 5 to 7: Version 1 estimated separately for each industry and each
# occupation (exploratory, not reported)

# Version 5: Native/MES/MNESHighEng/MNESLowEng specification for each of the 19 industries

# industries <- data_regression_1$industry_19categories_jbmi61 |>
#   levels() |>
#   na.omit()
# results_by_industry <- list()

# for (ind in industries) {
#   data_subset <- data_regression_1 %>%
#     filter(industry_19categories_jbmi61 == ind)
#   cat("Running:", ind, "| N =", nrow(data_subset), "\n")
#   tryCatch({
#     model <- feols(
#       log(hrwge_main_clean) ~
#         small_single_site +
#         chain_like +
#         small_site_chain +
#         small_single_site:loo_mes_1d_type2 +
#         small_single_site:loo_mnes_higheng_1d_type2 +
#         small_single_site:loo_mnes_loweng_1d_type2 +
#         age_hgage +
#         sex_hgsex +
#         migrant +
#         casual_perm_jbcasab +
#         occupation_1dig_jbmo61 +
#         highest_education_achieved_edhigh1 +
#         seifa_decile +
#         tenure_current_occ_jbocct +
#         tenure_current_employer_jbempt +
#         union_employeeassociation_membership_jbmtuea |
#         year + state_hhstate^year,
#       cluster = ~ industry_19categories_jbmi61^year,
#       data = data_subset
#     )
#     results_by_industry[[ind]] <- model
#   })
# }

# view_industry <- function(industry_name) {
#   if (!industry_name %in% names(results_by_industry)) {
#     cat("Available industries:\n")
#     cat(paste(
#       sprintf(
#         "%d. %s",
#         seq_along(names(results_by_industry)),
#         names(results_by_industry)
#       ),
#       collapse = "\n"
#     ))
#     return(invisible(NULL))
#   }
#   model <- results_by_industry[[industry_name]]
#   cat("\n")
#   cat("=====================================\n")
#   cat("INDUSTRY:", industry_name, "\n")
#   cat("=====================================\n")
#   cat("\n")
#   print(summary(model))
#   cat("\n")
#   invisible(model)
# }

# view_industry("agriculture, forestry and fishing")
# view_industry("mining")
# view_industry("manufacturing")
# view_industry("electricity, gas, water and waste services")
# view_industry("construction")
# view_industry("wholesale trade")
# view_industry("retail trade")
# view_industry("accommodation and food services")
# view_industry("transport, postal and warehousing")
# view_industry("information media and telecommunications")
# view_industry("financial and insurance services")
# view_industry("rental, hiring and real estate services")
# view_industry("professional, scientific and technical services")
# view_industry("administrative and support services")
# view_industry("public administration and safety")
# view_industry("education and training")
# view_industry("health care and social assistance")
# view_industry("arts and recreation services")
# view_industry("other services")

# Version 6: Migrant & LowEnglish specification in 19 industries for each industry

# results_by_industry_v2 <- list()

# for (ind in industries) {
#   data_subset <- data_regression_1 %>%
#     filter(industry_19categories_jbmi61 == ind)
#   cat("Running:", ind, "| N =", nrow(data_subset), "\n")
#   tryCatch({
#     model <- feols(
#       log(hrwge_main_clean) ~
#         small_single_site +
#         chain_like +
#         small_site_chain +
#         small_single_site:loo_migrant_share_1d +
#         small_single_site:loo_loweng_share_1d +
#         age_hgage +
#         sex_hgsex +
#         migrant +
#         casual_perm_jbcasab +
#         occupation_1dig_jbmo61 +
#         highest_education_achieved_edhigh1 +
#         seifa_decile +
#         tenure_current_occ_jbocct +
#         tenure_current_employer_jbempt |
#         year + state_hhstate^year,
#       data = data_subset,
#       cluster = ~ state_hhstate^year
#     )
#     results_by_industry_v2[[ind]] <- model
#   })
# }

# view_industry <- function(industry_name) {
#   if (!industry_name %in% names(results_by_industry_v2)) {
#     cat("Available industries:\n")
#     cat(paste(
#       sprintf(
#         "%d. %s",
#         seq_along(names(results_by_industry_v2)),
#         names(results_by_industry_v2)
#       ),
#       collapse = "\n"
#     ))
#     return(invisible(NULL))
#   }
#   model <- results_by_industry_v2[[industry_name]]
#   cat("\n")
#   cat("=====================================\n")
#   cat("INDUSTRY:", industry_name, "\n")
#   cat("=====================================\n")
#   cat("\n")
#   print(summary(model))
#   cat("\n")
#   invisible(model)
# }

# view_industry("agriculture, forestry and fishing")
# view_industry("mining")
# view_industry("manufacturing")
# view_industry("electricity, gas, water and waste services")
# view_industry("construction")
# view_industry("wholesale trade")
# view_industry("retail trade")
# view_industry("accommodation and food services")
# view_industry("transport, postal and warehousing")
# view_industry("information media and telecommunications")
# view_industry("financial and insurance services")
# view_industry("rental, hiring and real estate services")
# view_industry("professional, scientific and technical services")
# view_industry("administrative and support services")
# view_industry("public administration and safety")
# view_industry("education and training")
# view_industry("health care and social assistance")
# view_industry("arts and recreation services")
# view_industry("other services")

# Version 7: Native/MES/MNESHighEng/MNESLowEng specification for each occupation

# occupations <- data_regression_1$occupation_1dig_jbmo61 |> levels() |> na.omit()
# results_by_occupation <- list()
# for (occ in occupations) {
#   data_subset <- data_regression_1 %>%
#     filter(occupation_1dig_jbmo61 == occ)
#   cat("Running:", occ, "| N =", nrow(data_subset), "\n")
#   tryCatch({
#     model <- feols(
#       log(hrwge_main_clean) ~
#         small_single_site +
#         chain_like +
#         small_site_chain +
#         small_single_site:loo_mes_1d_type2 +
#         small_single_site:loo_mnes_higheng_1d_type2 +
#         small_single_site:loo_mnes_loweng_1d_type2 +
#         age_hgage +
#         sex_hgsex +
#         casual_perm_jbcasab +
#         highest_education_achieved_edhigh1 +
#         seifa_decile +
#         tenure_current_occ_jbocct +
#         tenure_current_employer_jbempt |
#         year + industry_19categories_jbmi61^year + state_hhstate^year,
#       data = data_subset,
#       cluster = ~ industry_19categories_jbmi61^year
#     )
#     results_by_occupation[[occ]] <- model
#   })
# }

# view_occupation <- function(occupation_name) {
#   if (!occupation_name %in% names(results_by_occupation)) {
#     cat("Available occupations:\n")
#     cat(paste(
#       sprintf(
#         "%d. %s",
#         seq_along(names(results_by_occupation)),
#         names(results_by_occupation)
#       ),
#       collapse = "\n"
#     ))
#     return(invisible(NULL))
#   }
#   model <- results_by_occupation[[occupation_name]]
#   cat("\n")
#   cat("=====================================\n")
#   cat("OCCUPATION:", occupation_name, "\n")
#   cat("=====================================\n")
#   cat("\n")
#   print(summary(model))
#   cat("\n")
#   invisible(model)
# }

# view_occupation("managers")
# view_occupation("professionals")
# view_occupation("labourers")
# view_occupation("clerical and administrative workers")
# view_occupation("technicians and trades workers")
# view_occupation("community and personal service workers")
# view_occupation("sales workers")
# view_occupation("machinery operators and drivers")

# ==============================================================================
# End of 05_analysisblock1.R
# ==============================================================================
