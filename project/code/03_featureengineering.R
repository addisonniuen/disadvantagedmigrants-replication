# ==============================================================================
# 03_featureengineering.R
#
# This script creates all newly derived variables from the cleaned panel data
# and builds the final sample datasets used in analysis. It:
#   - Harmonises the wave-specific employer-sector and employer-size mnemonics
#   - Carries time-invariant ancestry/migration variables forward and backward
#   - Derives migrant, English-proficiency, firm-size, wage and income measures, as well as liquidity constraint
#   - Builds the disjoint origin-proficiency partition G = {native, MES, MNES high-Eng, MNES low-Eng}
#   - Derives owner/employee status indicators and ownership entry/exit
#   - Computes leave-one-out (LOO) owner shares for each non-native group in G
#
# This script requires these files to be sourced first:
#   source("project/code/01_setup.R")
#   source("project/code/02_datacleaning.R")
# ==============================================================================

# --- Parameters and label sets ------------------------------------------------

# Ancestry/migration items were not asked in all waves
# Time-invariant and filled within person
locf_vars <- c(
  "birthplace_country_ancob",
  "birthplace_group_anbcob",
  "year_arrival_aust_anyoa",
  "visa_category_anmigc",
  "english_first_lang_anengf",
  "edu_country_edcoq"
)
labels_owner_with_employees <- c(
  "employee of own business (with employees)", # esempdt 2
  "employer or self-employed (with employees)" # esempdt 4
)
labels_owner_without_employees <- c(
  "employee of own business (without employees)", # esempdt 3
  "employer or self-employed (without employees)" # esempdt 5
)
labels_owner <- c(labels_owner_with_employees, labels_owner_without_employees)
labels_small_wp_1_4 <- c(
  "fewer than 5 employees",
  "5 to 9 employees",
  "10 to 19 employees",
  "don't know, but fewer than 20"
)
labels_small_wp_5_plus <- c(
  "self only",
  "2 to 4 employees",
  "5 to 9 employees",
  "10 to 19 employees",
  "don't know, but fewer than 20"
)
labels_small_wp10_1_4 <- c("fewer than 5 employees", "5 to 9 employees")
labels_small_wp10_5_plus <- c(
  "self only",
  "2 to 4 employees",
  "5 to 9 employees"
)
labels_small_wp50_1_4 <- c(labels_small_wp_1_4, "20 to 49 employees")
labels_small_wp50_5_plus <- c(labels_small_wp_5_plus, "20 to 49 employees")
labels_large_wp <- c(
  "20 to 49 employees",
  "50 to 99 employees",
  "100 to 199 employees",
  "200 to 499 employees",
  "500 or more employees",
  "don't know, but 20 or more"
)
labels_large_nat <- c(
  "500 to 999 employees",
  "1,000 to 4,999 employees",
  "5,000 to 19,999 employees",
  "20,000 or more employees"
)
labels_low_english <- c(
  "does not speak english well",
  "does not speak english at all"
)
labels_agent_plausible <- c(
  "managers",
  "professionals",
  "clerical and administrative workers",
  "sales workers"
)
# Short industry labels
anzsic_labels <- c(
  "1" = "Agriculture",
  "2" = "Mining",
  "3" = "Manufacturing",
  "4" = "Electricity/Gas/Water",
  "5" = "Construction",
  "6" = "Wholesale",
  "7" = "Retail",
  "8" = "Accom & Food",
  "9" = "Transport",
  "10" = "Media/Telecom",
  "11" = "Finance",
  "12" = "Rental/RE",
  "13" = "Prof. Services",
  "14" = "Admin Services",
  "15" = "Public Admin",
  "16" = "Education",
  "17" = "Health",
  "18" = "Arts & Recreation",
  "19" = "Other Services"
)


# --- Helper functions ---------------------------------------------------------

classify_small_wp <- function(
  var_1_4,
  var_5_plus,
  wave,
  small_labels_1_4,
  small_labels_5_plus,
  ambiguous_label
) {
  case_when(
    wave <= 4L & as.character(var_1_4) == ambiguous_label ~ NA_integer_,
    wave >= 5L & as.character(var_5_plus) == ambiguous_label ~ NA_integer_,
    wave <= 4L & !is.na(var_1_4) ~ as.integer(var_1_4 %in% small_labels_1_4),
    wave >= 5L & !is.na(var_5_plus) ~ as.integer(
      var_5_plus %in% small_labels_5_plus
    ),
    .default = NA_integer_
  )
}

# Helper function: last-observation-carried-forward, then backward-fill at entry
fill_within_person <- function(x) {
  lvl <- if (is.factor(x)) levels(x) else NULL
  filled <- as.character(x) |>
    zoo::na.locf(na.rm = FALSE) |>
    zoo::na.locf(fromLast = TRUE, na.rm = FALSE)
  if (is.null(lvl)) as.numeric(filled) else factor(filled, levels = lvl)
}

# Helper function: lag that returns NA unless the previous row is genuinely wave t-1
lag_contig <- function(x, wave) {
  out <- dplyr::lag(x)
  prev <- dplyr::lag(wave)
  out[is.na(prev) | prev != wave - 1L] <- NA
  out
}

# Helper function: modal value within person; a tie goes to the lowest value
# (for low_english, a tie between 0 and 1 is coded 0, proficient)
mode_within_person <- function(x) {
  x_clean <- x[!is.na(x)]
  if (length(x_clean) == 0) {
    return(NA_integer_)
  }
  tbl <- table(x_clean)
  as.integer(names(tbl)[which.max(tbl)][1])
}

# Helper function: leave-one-out industry-year owner shares, decay/theta applied.
# Used for the MES/MNES family, which has three decay variants (type0/1/2).
# weight_col is the survey weight given to each owner. The thesis uses the
# cross-sectional weight (hhwtrp), since each share describes the owners of one
# wave; the longitudinal weight (lnwtrp) is used only for the sensitivity check
# in Table B.10.
compute_loo_shares <- function(df, ind_col, suffix, theta_col = "theta", weight_col = "respondingperson_crosssectional_weight_hhwtrp") {
  df <- dplyr::mutate(df, theta = .data[[theta_col]], w_survey = .data[[weight_col]])
  df |>
    filter(!is.na(.data[[ind_col]])) |>
    transmute(
      xwaveid,
      wave,
      cell = .data[[ind_col]],
      w_own = if_else(
        coalesce(is_owner == 1L, FALSE) &
          !is.na(origin_group) &
          !is.na(theta) &
          !is.na(w_survey),
        w_survey,
        0
      ),
      num_mes = w_own *
        coalesce(theta, 0) *
        coalesce(origin_group == "mes", FALSE),
      num_higheng = w_own *
        coalesce(theta, 0) *
        coalesce(origin_group == "mnes_higheng", FALSE),
      num_loweng = w_own *
        coalesce(theta, 0) *
        coalesce(origin_group == "mnes_loweng", FALSE)
    ) |>
    mutate(
      n_owners = sum(w_own > 0) - as.integer(w_own > 0),
      across(c(w_own, starts_with("num_")), ~ sum(.x) - .x),
      .by = c(cell, wave)
    ) |>
    transmute(
      xwaveid,
      wave,
      "loo_mes_{suffix}" := if_else(w_own > 0, num_mes / w_own, NA_real_),
      "loo_mnes_{suffix}" := if_else(
        w_own > 0,
        (num_higheng + num_loweng) / w_own,
        NA_real_
      ),
      "loo_mnes_higheng_{suffix}" := if_else(
        w_own > 0,
        num_higheng / w_own,
        NA_real_
      ),
      "loo_mnes_loweng_{suffix}" := if_else(
        w_own > 0,
        num_loweng / w_own,
        NA_real_
      ),
      "n_owners_{suffix}" := n_owners
    )
}

# Helper function: leave-one-out industry-year owner shares, pure survey-weight, no decay/theta applied.
# Used for the migrant/low-English specification
compute_loo_shares_plain <- function(df, ind_col, suffix) {
  df |>
    filter(!is.na(.data[[ind_col]])) |>
    transmute(
      xwaveid,
      wave,
      cell = .data[[ind_col]],
      w_own = if_else(
        coalesce(is_owner == 1L, FALSE) &
          !is.na(origin_group) &
          !is.na(respondingperson_crosssectional_weight_hhwtrp),
        respondingperson_crosssectional_weight_hhwtrp,
        0
      ),
      num_mes = w_own * coalesce(origin_group == "mes", FALSE),
      num_higheng = w_own * coalesce(origin_group == "mnes_higheng", FALSE),
      num_loweng = w_own * coalesce(origin_group == "mnes_loweng", FALSE)
    ) |>
    mutate(
      n_owners = sum(w_own > 0) - as.integer(w_own > 0),
      across(c(w_own, starts_with("num_")), ~ sum(.x) - .x),
      .by = c(cell, wave)
    ) |>
    transmute(
      xwaveid,
      wave,
      "loo_migrant_share_{suffix}" := if_else(
        w_own > 0,
        (num_mes + num_higheng + num_loweng) / w_own,
        NA_real_
      ),
      "loo_native_share_{suffix}" := if_else(
        w_own > 0,
        (w_own - num_mes - num_higheng - num_loweng) / w_own,
        NA_real_
      ),
      "loo_loweng_share_{suffix}" := if_else(
        w_own > 0,
        num_loweng / w_own,
        NA_real_
      ),
      "loo_higheng_share_{suffix}" := if_else(
        w_own > 0,
        num_higheng / w_own,
        NA_real_
      ),
      "n_owners_{suffix}" := n_owners
    )
}

# Helper function: check panel data after feature engineering step
fe_checks <- function() {
  list(
    # Individuals whose "time-invariant" ancestry variables disagree across waves
    ancestry_conflicts = panel_clean |>
      summarise(
        across(all_of(locf_vars), ~ n_distinct(.x[!is.na(.x)]) > 1),
        .by = xwaveid
      ) |>
      filter(if_any(all_of(locf_vars))),

    # Rows recovered by the forward/backward fill
    locf_recovery = tibble(variable = locf_vars) |>
      rowwise() |>
      mutate(
        nas_before = sum(is.na(panel_clean[[variable]])),
        nas_after = sum(is.na(panel_derived[[variable]])),
        recovered = nas_before - nas_after
      ) |>
      ungroup(),

    # Firm-type dummies should be mutually exclusive (type_sum never exceeds 1)
    firm_type_exclusive = panel_final |>
      filter(wave <= 21L) |>
      count(
        any_missing = is.na(small_single_site) |
          is.na(chain_like) |
          is.na(small_site_chain),
        type_sum = coalesce(small_single_site, 0L) +
          coalesce(chain_like, 0L) +
          coalesce(small_site_chain, 0L)
      ),

    # Why small_single_site is missing, employees only
    firm_size_missing = panel_final |>
      filter(is_employee == 1L) |>
      count(
        reason = case_when(
          !is.na(small_single_site) ~ "non-missing",
          wave %in% 22:24 ~ "waves 22-24 (no workplace-size data)",
          is.na(multi_site_jbmmwp) ~ "multi-location indicator missing",
          is.na(employer_size_nat) ~ "national employer size missing",
          is.na(large_nat) ~ "national size band straddles the cutoff",
          .default = "other"
        )
      ),

    # Wage floor applied, and main vs all-jobs agreement
    wage_summary = panel_final |>
      filter(!is.na(hrwge_main_clean)) |>
      summarise(
        min = min(hrwge_main_clean),
        median = median(hrwge_main_clean),
        max = max(hrwge_main_clean),
        n = n()
      ),

    # After the structural-skip fix, low_english should be NA only where
    # anlote itself is missing
    low_english_missing = panel_final |>
      count(
        anlote_missing = is.na(speaks_lote_home_anlote),
        low_eng_missing = is.na(low_english)
      ),

    # Not used (the arrival-year imputation is not run):
    # # Arrival year: reported, PMM-imputed, or still unresolved
    # arrival_imputation = panel_derived |>
    #   summarise(
    #     migrant = migrant[!is.na(migrant)][1],
    #     reported = any(!is.na(year_arrival_aust_anyoa)),
    #     imputed = any(coalesce(arrival_imputed, 0L) == 1L),
    #     resolved = any(!is.na(year_arrival_used)),
    #     .by = xwaveid
    #   ) |>
    #   count(
    #     migrant = case_when(
    #       migrant == 1L ~ "migrant",
    #       migrant == 0L ~ "australian-born",
    #       .default = "birthplace missing"
    #     ),
    #     status = case_when(
    #       reported ~ "reported",
    #       imputed ~ "PMM-imputed",
    #       resolved ~ "resolved without a report or an imputation flag",
    #       .default = "unresolved"
    #     )
    #   ),

    # # Imputed draws should sit in the same range as the reported values
    # arrival_imputed_years = panel_derived |>
    #   distinct(xwaveid, arrival_imputed, year_arrival_used) |>
    #   filter(!is.na(year_arrival_used)) |>
    #   summarise(
    #     n = n(),
    #     min = min(year_arrival_used),
    #     median = median(year_arrival_used),
    #     max = max(year_arrival_used),
    #     .by = arrival_imputed
    #   ),

    arrival_frame_coverage = panel_derived |>
      summarise(
        in_frame = any(coalesce(migrant, 0L) == 1L),
        has_arrival_report = any(!is.na(year_arrival_aust_anyoa)),
        .by = xwaveid
      ) |>
      count(in_frame, has_arrival_report),

    # HILDA's own imputation, distinct from the arrival-year PMM above
    hilda_imputation_flags = panel_derived |>
      summarise(across(
        starts_with("impute_flag_"),
        list(
          n_imputed = ~ sum(.x == "imputed", na.rm = TRUE),
          pct = ~ 100 * mean(.x == "imputed", na.rm = TRUE)
        )
      )) |>
      pivot_longer(
        everything(),
        names_to = c("variable", ".value"),
        names_pattern = "^(.*)_(n_imputed|pct)$"
      ),

    # Composition of G, and the ownership switchers each theta_g rests on
    origin_group_counts = panel_final |>
      distinct(xwaveid, origin_group) |>
      count(origin_group),

    owner_switchers_by_group = panel_final |>
      summarise(
        switcher = n_distinct(owner_with_employees) > 1,
        .by = c(xwaveid, origin_group)
      ) |>
      summarise(
        n_people = n(),
        n_switchers = sum(switcher),
        .by = origin_group
      ),

    # Empty LOO cells must arrive as NA, never as a zero migrant share
    loo_empty_cells = panel_final |>
      filter(is_employee == 1L) |>
      count(
        share_missing = is.na(loo_mes_1d_type1),
        cell_empty = coalesce(n_owners_1d_type1, 0L) == 0L
      ),

    # Entry/exit are only defined on the at-risk person-waves
    transition_risk_sets = panel_final |>
      count(
        is_owner_l1 = coalesce(as.character(is_owner_l1), "no adjacent t-1"),
        entry_defined = !is.na(entry_owner),
        exit_defined = !is.na(exit_owner)
      ),

    # Sample sizes
    sample_sizes = tibble(
      object = c(
        "panel_final",
        "owner_and_employee_sample",
        "owner_sample",
        "employee_sample"
      ),
      n = c(
        nrow(panel_final),
        nrow(owner_and_employee_sample),
        nrow(owner_sample),
        nrow(employee_sample)
      )
    )
  )
}

# Helper function: convert one yes/no SCQ stress factor to 0/1
yes_to_1 <- function(f) {
  as.integer(startsWith(as.character(f), "yes"))
}

# Helper function: 1 if ANY of the given yes/no SCQ factors is "yes"
any_yes <- function(cols) {
  do.call(pmax, c(lapply(cols, yes_to_1), na.rm = TRUE))
}

# Helper function: PMM imputation for years arrived in Australia (kept for
# reference; not called, since years in Australia uses the reported year only)
#  - Collapses to one row per person internally; `df` itself is never subset
#  - `subset` restricts WHO is imputed (anyoa is a structural skip for the Australian-born)
impute_arrival_year <- function(
  df,
  subset,
  predictor_vars,
  impute_var = "year_arrival_aust_anyoa",
  id_var = "xwaveid",
  m = 20,
  maxit = 1,
  donors = 5,
  seed = 12345,
  draw = 1
) {
  derived_vars <- c("birth_year", "year_first_wave")
  panel_vars <- setdiff(predictor_vars, derived_vars)
  missing_vars <- setdiff(c(impute_var, panel_vars), names(df))
  if (length(missing_vars) > 0) {
    stop("Not in df: ", paste(missing_vars, collapse = ", "))
  }
  person <- df |>
    filter({{ subset }}) |>
    arrange(.data[[id_var]], wave) |>
    summarise(
      across(all_of(c(impute_var, panel_vars)), ~ .x[!is.na(.x)][1]),
      birth_year = (year - age_hgage)[!is.na(age_hgage)][1],
      year_first_wave = min(year),
      .by = all_of(id_var)
    ) |>
    mutate(across(
      !all_of(id_var) & where(~ is.character(.x) || is.logical(.x)),
      as.factor
    ))
  meth <- mice::make.method(person)
  meth[] <- ""
  meth[impute_var] <- "pmm"
  pred <- mice::make.predictorMatrix(person)
  pred[,] <- 0
  pred[impute_var, predictor_vars] <- 1
  set.seed(seed)
  imp <- mice::mice(
    person,
    m = m,
    method = meth,
    predictorMatrix = pred,
    maxit = maxit,
    donors = donors,
    seed = seed,
    printFlag = FALSE
  )
  out <- df |>
    left_join(
      person |>
        mutate(
          arrival_pmm = mice::complete(imp, draw)[[impute_var]],
          arrival_imputed = as.integer(
            is.na(.data[[impute_var]]) & !is.na(arrival_pmm)
          ),
          # Observed arrival years are used as reported; only the imputed draw is
          # censored to a person-feasible window (not before birth, not after the first interview)
          year_arrival_used = if_else(
            arrival_imputed == 1L,
            pmin(pmax(arrival_pmm, birth_year), year_first_wave),
            .data[[impute_var]]
          )
        ) |>
        select(all_of(id_var), year_arrival_used, arrival_imputed),
      by = id_var
    )
  # Diagnostics
  attr(out, "arrival_imputation") <- list(
    mice = imp,
    method = meth,
    predictorMatrix = pred,
    loggedEvents = imp$loggedEvents,
    draw = draw,
    n_persons = nrow(person),
    n_missing = sum(is.na(person[[impute_var]]))
  )
  out
}

# --- Derived variables --------------------------------------------------------

panel_derived <- panel_clean |>
  arrange(xwaveid, wave) |>
  mutate(across(all_of(locf_vars), fill_within_person), .by = xwaveid) |>
  mutate(
    # -- Occupation subgroup --
    occupation_agent_plausible_subgroup = factor(
      case_when(
        is.na(occupation_1dig_jbmo61) ~ NA_character_,
        occupation_1dig_jbmo61 %in% labels_agent_plausible ~ "agent-plausible",
        .default = "other occupations"
      ),
      levels = c("other occupations", "agent-plausible")
    ),

    # -- Employer sector --
    # jbmmpl (w1), jbmmplr (w2) and jbmmply (w3-24) are wave-specific variables for the SAME question
    employer_sector = coalesce(
      as.character(employer_sector_jbmmply),
      as.character(employer_sector_jbmmplr),
      as.character(employer_sector_jbmmpl)
    ) |>
      factor(levels = levels(employer_sector_jbmmpl)),
    # Kept so any sector result can be checked against the wave block it came from
    employer_sector_source = case_when(
      !is.na(employer_sector_jbmmply) ~ "jbmmply",
      !is.na(employer_sector_jbmmplr) ~ "jbmmplr",
      !is.na(employer_sector_jbmmpl) ~ "jbmmpl",
      .default = NA_character_
    ),

    # -- National employer size --
    # jbmemsz (w1-21) and jbmems2 (w22-24) are wave-specific variables for the same question: people employed at locations throughout Australia
    employer_size_nat = coalesce(
      as.character(employer_size_jbmemsz),
      as.character(employer_size_jbmems2)
    ),
    employer_size_nat_source = case_when(
      !is.na(employer_size_jbmemsz) ~ "jbmemsz",
      !is.na(employer_size_jbmems2) ~ "jbmems2",
      .default = NA_character_
    ),

    # -- Migration and English proficiency --
    migrant = case_when(
      is.na(birthplace_group_anbcob) ~ NA_integer_,
      birthplace_group_anbcob == "australia" ~ 0L,
      .default = 1L
    ),
    birthplace_group = forcats::fct_relevel(
      birthplace_group_anbcob,
      "australia"
    ),
    english_first_language = case_when(
      is.na(english_first_lang_anengf) ~ NA_integer_,
      english_first_lang_anengf == "english is first language" ~ 1L,
      .default = 0L
    ),
    english_reading_skill_num = as.numeric(as.character(
      english_reading_skill_sktarea
    )),
    english_proficiency = if_else(
      wave == 1,
      english_proficiency_hgeab,
      english_proficiency_aneab
    ),
    low_english = case_when(
      coalesce(speaks_lote_home_anlote == "no", FALSE) ~ 0L,
      coalesce(english_proficiency %in% labels_low_english, FALSE) ~ 1L,
      coalesce(english_reading_skill_num < 4, FALSE) ~ 1L,
      is.na(english_proficiency) &
        is.na(english_reading_skill_num) ~ NA_integer_,
      .default = 0L
    ),
    migrant_low_eng = if_else(
      is.na(migrant) | is.na(low_english),
      NA_integer_,
      as.integer(migrant == 1L & low_english == 1L)
    )
  ) |>
  mutate(
    low_english_timeinvariant = mode_within_person(low_english),
    migrant_low_eng_baseline = if_else(
      is.na(migrant) | is.na(low_english_timeinvariant),
      NA_integer_,
      as.integer(migrant == 1L & low_english_timeinvariant == 1L)
    ),
    .by = xwaveid
  ) |>
  mutate(
    # -- Employment status --
    # Missing when the person was not interviewed (no labour force status) or
    # is employed with no detailed status, rather than counted as a non-owner
    status_known = !is.na(lf_status_detail_esdtl) &
      !(startsWith(as.character(lf_status_detail_esdtl), "employed") &
        is.na(emp_status_detail_esempdt)),
    owner_with_employees = if_else(
      status_known,
      as.integer(emp_status_detail_esempdt %in% labels_owner_with_employees),
      NA_integer_
    ),
    own_account_worker = if_else(
      status_known,
      as.integer(emp_status_detail_esempdt %in% labels_owner_without_employees),
      NA_integer_
    ),
    is_owner = if_else(
      status_known,
      as.integer(emp_status_detail_esempdt %in% labels_owner),
      NA_integer_
    ),
    is_employee = as.integer(emp_status_detail_esempdt == "employee"),
    is_unemployed = as.integer(startsWith(
      as.character(lf_status_detail_esdtl),
      "unemployed"
    )),
    is_not_in_labour_force = as.integer(startsWith(
      as.character(lf_status_detail_esdtl),
      "not in the labour force"
    )),

    # -- Firm size and type --
    small_wp = case_when(
      wave <= 4L & !is.na(workplace_size_jbmwpsz) ~ as.integer(
        workplace_size_jbmwpsz %in% labels_small_wp_1_4
      ),
      wave >= 5L & !is.na(workplace_size_jbmwps) ~ as.integer(
        workplace_size_jbmwps %in% labels_small_wp_5_plus
      ),
      .default = NA
    ),
    small_wp_10 = classify_small_wp(
      workplace_size_jbmwpsz,
      workplace_size_jbmwps,
      wave,
      labels_small_wp10_1_4,
      labels_small_wp10_5_plus,
      ambiguous_label = "don't know, but fewer than 20"
    ),
    small_wp_50 = classify_small_wp(
      workplace_size_jbmwpsz,
      workplace_size_jbmwps,
      wave,
      labels_small_wp50_1_4,
      labels_small_wp50_5_plus,
      ambiguous_label = "don't know, but 20 or more"
    ),
    large_wp = case_when(
      wave <= 4L & !is.na(workplace_size_jbmwpsz) ~ as.integer(
        workplace_size_jbmwpsz %in% labels_large_wp
      ),
      wave >= 5L & !is.na(workplace_size_jbmwps) ~ as.integer(
        workplace_size_jbmwps %in% labels_large_wp
      ),
      .default = NA
    ),
    large_nat = case_when(
      is.na(employer_size_nat) ~ NA_integer_,
      employer_size_nat == "don't know, but 100 or more" ~ NA_integer_,
      .default = as.integer(employer_size_nat %in% labels_large_nat)
    ),
    is_multisite = as.integer(
      multi_site_jbmmwp %in%
        "employer operates from more than one location in australia"
    ),
    is_singlesite = as.integer(
      multi_site_jbmmwp %in% "employer operates from one location in australia"
    ),
    small_single_site = if_else(
      !is.na(small_wp) & !is.na(multi_site_jbmmwp),
      as.integer(small_wp & is_singlesite),
      NA_integer_
    ),
    small_single_site_10 = if_else(
      !is.na(small_wp_10) & !is.na(multi_site_jbmmwp),
      as.integer(small_wp_10 & is_singlesite),
      NA_integer_
    ),
    small_site_chain_10 = if_else(
      !is.na(small_wp_10) & !is.na(multi_site_jbmmwp),
      as.integer(small_wp_10 & is_multisite),
      NA_integer_
    ),
    small_single_site_50 = if_else(
      !is.na(small_wp_50) & !is.na(multi_site_jbmmwp),
      as.integer(small_wp_50 & is_singlesite),
      NA_integer_
    ),
    small_site_chain_50 = if_else(
      !is.na(small_wp_50) & !is.na(multi_site_jbmmwp),
      as.integer(small_wp_50 & is_multisite),
      NA_integer_
    ),
    chain_like = case_when(
      is.na(multi_site_jbmmwp) | is.na(large_wp) ~ NA_integer_,
      is_multisite == 0L ~ 0L,
      is.na(large_nat) ~ NA_integer_,
      .default = as.integer(large_wp == 1L & large_nat == 1L)
    ),
    small_site_chain = if_else(
      !is.na(small_wp) & !is.na(multi_site_jbmmwp),
      as.integer(small_wp & is_multisite),
      NA_integer_
    ),
    size_missing_flag = as.integer(wave %in% 22:24),

    # -- Hourly wages --
    hrwge_main = if_else(
      wage_weekly_main_job_wscmei > 0 & hours_weekly_main_job_jbmhruc > 0,
      wage_weekly_main_job_wscmei / hours_weekly_main_job_jbmhruc,
      NA_real_
    ),
    hrwge_all = if_else(
      wage_weekly_all_job_wscei > 0 & hours_weekly_all_job_jbhruc > 0,
      wage_weekly_all_job_wscei / hours_weekly_all_job_jbhruc,
      NA_real_
    ),
    hrwge_main_clean = if_else(hrwge_main >= 1, hrwge_main, NA_real_), # $1/hr floor
    hrwge_all_clean = if_else(hrwge_all >= 1, hrwge_all, NA_real_), # $1/hr floor
    log_hrwge = if_else(hrwge_main_clean >= 1, log(hrwge_main_clean), NA_real_),

    # -- Annual income --
    # HILDA stores positive and negative parts as separate magnitudes
    income_regular = fy_income_pos_tifefp - fy_income_neg_tifefn,
    income_business = fy_biz_income_pos_bifip - fy_biz_income_neg_bifin,
    income_labour_and_business = fy_gross_wages_salaries_wsfei +
      income_business,
    income_labour_and_business_log = log(na_if(
      pmax(income_labour_and_business, 0),
      0
    )),
    has_positive_income = if_else(
      is.na(income_labour_and_business),
      NA_integer_,
      as.integer(income_labour_and_business > 0)
    ),

    # -- Liquidity constraint --
    cashflow_problem = any_yes(
      pick(all_of(c(
        "cannot_pay_bills_on_time_fiprbeg",
        "cannot_pay_mortgage_rent_on_time_fiprbmr",
        "asked_financial_help_friends_family_fiprbfh"
      )))
    ),
    hardship = any_yes(
      pick(all_of(c(
        "went_without_meals_fiprbwm",
        "unable_heat_home_fiprbuh",
        "pawned_or_sold_something_fiprbps",
        "asked_financial_help_welfare_community_fiprbwo"
      )))
    ),
    liquidity_constraint = pmax(cashflow_problem, hardship, na.rm = TRUE),
    is_homeowner = case_when(
      is.na(housing_tenure_hstenr) ~ NA_integer_,
      housing_tenure_hstenr == "own/currently paying off mortgage" ~ 1L,
      .default = 0L
    ),

    # -- Disadvantage by SEIFA --
    seifa_decile = readr::parse_number(as.character(
      disadvantage_decile_hhs3add
    )),
    remoteness_collapsed = forcats::fct_collapse(
      remoteness_hhs3ra,
      `outer regional/remote` = c(
        "remote australia",
        "outer regional australia"
      )
    )
  ) |>
  # -- Origin-proficiency partition G --
  mutate(
    origin_group = case_when(
      is.na(birthplace_group_anbcob) ~ NA_character_,
      birthplace_group_anbcob == "australia" ~ "native",
      birthplace_group_anbcob == "main english-speaking country" ~ "mes",
      birthplace_group_anbcob == "other country" &
        is.na(low_english_timeinvariant) ~ NA_character_,
      birthplace_group_anbcob == "other country" &
        low_english_timeinvariant == 1L ~ "mnes_loweng",
      birthplace_group_anbcob == "other country" &
        low_english_timeinvariant == 0L ~ "mnes_higheng",
      .default = NA_character_
    ) |>
      factor(levels = c("native", "mes", "mnes_higheng", "mnes_loweng")),
    origin_group_3 = forcats::fct_collapse(
      origin_group,
      mnes = c("mnes_higheng", "mnes_loweng")
    ),
    .by = xwaveid
  ) |>
  # Not used: arrival year was imputed by PMM, but years in Australia uses the
  # reported arrival year only, so the imputation is not run
  # impute_arrival_year(
  #   subset = migrant == 1L,
  #   predictor_vars = c(
  #     "birth_year",
  #     "year_first_wave",
  #     "sex_hgsex",
  #     "highest_education_achieved_edhigh1",
  #     "marital_status_mrcurr",
  #     "state_hhstate"
  #   )
  # ) |>
  mutate(
    years_in_aus = if_else(
      !is.na(year_arrival_aust_anyoa) &
        year_arrival_aust_anyoa > 0 &
        year_arrival_aust_anyoa <= year,
      as.numeric(year - year_arrival_aust_anyoa),
      NA_real_
    ),
    # "Undetermined" education is not a level, it is missing (~48 obs)
    highest_education_achieved_edhigh1 = forcats::fct_drop(
      dplyr::na_if(highest_education_achieved_edhigh1, "undetermined")
    ),
    foreign_qual = if_else(
      migrant == 1 & !is.na(edu_country_edcoq) & edu_country_edcoq != "1101",
      1L,
      if_else(
        migrant == 1 & !is.na(edu_country_edcoq) & edu_country_edcoq == "1101",
        0L,
        NA_integer_
      )
    )
  ) |>
  # Low wage: bottom quintile of the hourly wage among EMPLOYEES within each year
  mutate(
    wage_p20 = quantile(
      hrwge_main_clean[is_employee == 1L],
      probs = 0.20,
      na.rm = TRUE
    ),
    is_low_wage = case_when(
      is_unemployed == 1L | is_not_in_labour_force == 1L ~ 0L,
      is_owner == 1L ~ 0L,
      is.na(hrwge_main_clean) ~ NA_integer_,
      hrwge_main_clean <= wage_p20 ~ 1L,
      .default = 0L
    ),
    # Downside-risk outcome: below the tenth percentile of the year-specific income distribution
    income_p10 = quantile(
      income_labour_and_business,
      probs = 0.10,
      na.rm = TRUE
    ),
    is_bottom_decile_income = if_else(
      is.na(income_labour_and_business),
      NA_integer_,
      as.integer(income_labour_and_business <= income_p10)
    ),
    .by = year
  ) |>
  # select(- wage_p20) |>
  mutate(
    waves_adjacent = as.integer(n() == max(wave) - min(wave) + 1L),
    in_all_waves = as.integer(n() == length(waves)),

    # Ownership transitions
    is_owner_l1 = lag_contig(is_owner, wave),
    entry_owner = if_else(
      is_owner == 1L & is_owner_l1 == 0L,
      1L,
      0L
    ),
    exit_owner = if_else(
      is_owner_l1 == 1L, # Was owner last year
      if_else(is_owner == 0L, 1L, 0L), # Exit if not owner this year
      NA_integer_ # Exclude if wasn't owner last year
    ),
    owner_with_employees_l1 = lag_contig(owner_with_employees, wave),
    exit_employer_status = if_else(
      coalesce(owner_with_employees_l1, 0L) == 0L,
      NA_integer_,
      1L - owner_with_employees
    ),
    # Trailing five-wave within-person standard deviation of income
    income_volatility = zoo::rollapplyr(
      income_labour_and_business,
      width = 5,
      partial = 3,
      fill = NA_real_,
      FUN = sd,
      na.rm = TRUE
    ),
    .by = xwaveid
  )


# -- Construct lambda and theta ------------------------------------------------

lambda_controls <- c(
  "log_hrwge",
  "small_single_site",
  "chain_like",
  "small_site_chain",
  "age_hgage",
  "sex_hgsex",
  "state_hhstate",
  "industry_19categories_jbmi61",
  "origin_group",
  "casual_perm_jbcasab",
  "occupation_1dig_jbmo61",
  "highest_education_achieved_edhigh1",
  "seifa_decile",
  "tenure_current_occ_jbocct",
  "tenure_current_employer_jbempt",
  "union_employeeassociation_membership_jbmtuea"
)

fit_lambda_regression <- function(reg_data) {
  feols(
    log_hrwge ~
      small_single_site +
      chain_like +
      small_site_chain +
      small_single_site:loo_mes_grid +
      small_single_site:loo_mnes_higheng_grid +
      small_single_site:loo_mnes_loweng_grid +
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
    data = reg_data,
    cluster = ~ xwaveid + industry_19categories_jbmi61^year
  )
}

# This estimate_lambda function estimate the optimal lambda without cross-validation

# estimate_lambda <- function(df, lambda_grid = seq(0, 0.30, by = 0.01), lambda_grid_type2 = lambda_grid, run_type2 = TRUE) {
#   evaluate_lambda <- function(lambda_mes, lambda_mnes = lambda_mes) {
#     df_theta <- df |>
#       mutate(
#         theta_grid = case_when(
#           origin_group == "native" ~ 0,
#           origin_group == "mes" & years_in_aus >= 0 ~ exp(-lambda_mes * years_in_aus),
#           origin_group %in% c("mnes_higheng", "mnes_loweng") & years_in_aus >= 0 ~ exp(-lambda_mnes * years_in_aus),
#           .default = NA_real_
#         )
#       )
#     shares <- compute_loo_shares(df_theta, "industry_19categories_jbmi61", "grid", theta_col = "theta_grid")
#     reg_data <- df_theta |>
#       left_join(shares, by = c("xwaveid", "wave")) |>
#       filter(is_owner == 0L, age_hgage >= 18, age_hgage <= 64, !is.na(migrant), responding_person_longitudinal_weight_lnwtrp > 0) |>
#       tidyr::drop_na(
#         tidyselect::all_of(lambda_controls),
#         loo_mes_grid,
#         loo_mnes_higheng_grid,
#         loo_mnes_loweng_grid
#       )
#     reg_data <<- reg_data
#     if (nrow(reg_data) == 0) {return(tibble(mse = NA_real_, n_obs = 0L))}
#     m_reg <- tryCatch(fit_lambda_regression(reg_data), error = function(e) NULL)
#     if (is.null(m_reg)) {return(tibble(mse = NA_real_, n_obs = nrow(reg_data)))}
#     res <- residuals(m_reg)
#     w_reg <- weights(m_reg)
#     if (is.null(w_reg) || length(w_reg) != length(res)) {w_reg <- rep(1, length(res))}
#     mse_val <- weighted.mean(res^2, w = w_reg, na.rm = TRUE)
#     tibble(
#       mse = mse_val,
#       rmse = sqrt(mse_val),
#       aic = AIC(m_reg),
#       bic = BIC(m_reg),
#       npar = length(coef(m_reg)) + sum(m_reg$fixef_sizes),
#       n_obs = length(res)
#     )
#   }
#   type1 <- purrr::map_df(lambda_grid, function(l) {
#     evaluate_lambda(l) |> mutate(lambda = l, .before = 1)
#   })
#   lambda_mean <- type1 |>
#     filter(!is.na(mse)) |>
#     slice_min(mse, n = 1, with_ties = FALSE) |>
#     pull(lambda)
#   type2 <- NULL
#   lambda_mes <- NA_real_
#   lambda_other <- NA_real_
#   if (run_type2) {
#     grid2 <- tidyr::expand_grid(lambda_mes = lambda_grid_type2, lambda_mnes = lambda_grid_type2)
#     type2 <- purrr::pmap_df(grid2, function(lambda_mes, lambda_mnes) {
#       evaluate_lambda(lambda_mes, lambda_mnes) |>
#         mutate(lambda_mes = lambda_mes, lambda_mnes = lambda_mnes, .before = 1)
#     })
#     best2 <- type2 |>
#       filter(!is.na(mse)) |>
#       slice_min(mse, n = 1, with_ties = FALSE)
#     lambda_mes <- best2$lambda_mes
#     lambda_other <- best2$lambda_mnes
#   }
#   list(
#     lambda_mean = lambda_mean,
#     lambda_mes = lambda_mes,
#     lambda_other = lambda_other,
#     type1 = type1,
#     type2 = type2
#   )
# }

# This version of estimate_lambda estimates the optimal lambda using grouped cross-validation on individual level

set.seed(1)
fold_map <- panel_derived |>
  distinct(xwaveid) |>
  mutate(fold = sample(rep(1:5, length.out = n())))
estimate_lambda <- function(
  df,
  lambda_grid = seq(0, 0.30, by = 0.01),
  lambda_grid_type2 = lambda_grid,
  run_type2 = TRUE,
  n_folds = 5,
  weight_col = "respondingperson_crosssectional_weight_hhwtrp"
) {
  evaluate_lambda <- function(lambda_mes, lambda_mnes = lambda_mes) {
    df_theta <- df |>
      mutate(
        theta_grid = case_when(
          origin_group == "native" ~ 0,
          origin_group == "mes" & years_in_aus >= 0 ~ exp(
            -lambda_mes * years_in_aus
          ),
          origin_group %in%
            c("mnes_higheng", "mnes_loweng") &
            years_in_aus >= 0 ~ exp(-lambda_mnes * years_in_aus),
          .default = NA_real_
        )
      )
    shares <- compute_loo_shares(
      df_theta,
      "industry_19categories_jbmi61",
      "grid",
      theta_col = "theta_grid",
      weight_col = weight_col
    )
    reg_data <- df_theta |>
      left_join(shares, by = c("xwaveid", "wave")) |>
      left_join(fold_map, by = "xwaveid") |>
      # Same sample as the wage regressions: employees in waves 1 to 21
      filter(
        emp_status_detail_esempdt == "employee",
        !wave %in% 22:24
      ) |>
      tidyr::drop_na(
        tidyselect::all_of(lambda_controls),
        loo_mes_grid,
        loo_mnes_higheng_grid,
        loo_mnes_loweng_grid
      )
    if (nrow(reg_data) == 0) {
      return(tibble(mse = NA_real_, n_obs = 0L))
    }
    m_reg <- tryCatch(fit_lambda_regression(reg_data, vcov = "iid"), error = function(e) NULL)
    if (is.null(m_reg)) {
      return(tibble(mse = NA_real_, n_obs = nrow(reg_data)))
    }
    # K-fold out-of-fold residuals replace the old in-sample residuals for mse/rmse
    oof <- purrr::map_df(seq_len(n_folds), function(k) {
      train <- filter(reg_data, fold != k)
      test <- filter(reg_data, fold == k)
      m_k <- tryCatch(fit_lambda_regression(train, vcov = "iid"), error = function(e) NULL)
      if (is.null(m_k) || nrow(test) == 0) {
        return(tibble(sq_err = numeric(0)))
      }
      pred <- tryCatch(predict(m_k, newdata = test), error = function(e) {
        rep(NA_real_, nrow(test))
      })
      tibble(sq_err = (test$log_hrwge - pred)^2) |>
        filter(!is.na(sq_err))
    })
    if (nrow(oof) == 0) {
      return(tibble(mse = NA_real_, n_obs = nrow(reg_data)))
    }
    mse_val <- mean(oof$sq_err, na.rm = TRUE)
    tibble(
      mse = mse_val,
      rmse = sqrt(mse_val),
      aic = AIC(m_reg),
      bic = BIC(m_reg),
      npar = length(coef(m_reg)) + sum(m_reg$fixef_sizes),
      n_obs = length(res <- oof$sq_err)
    )
  }
  df <- dplyr::select(
    df, xwaveid, wave, year, is_owner, years_in_aus, emp_status_detail_esempdt,
    tidyselect::all_of(c(weight_col, lambda_controls))
  )
  type1 <- lapply(lambda_grid, function(l) {
    evaluate_lambda(l) |> mutate(lambda = l, .before = 1)
  }) |>
    dplyr::bind_rows()
  lambda_mean <- type1 |>
    filter(!is.na(mse)) |>
    slice_min(mse, n = 1, with_ties = FALSE) |>
    pull(lambda)
  type2 <- NULL
  lambda_mes <- NA_real_
  lambda_other <- NA_real_
  if (run_type2) {
    grid2 <- tidyr::expand_grid(
      lambda_mes = lambda_grid_type2,
      lambda_mnes = lambda_grid_type2
    )
    type2 <- lapply(seq_len(nrow(grid2)), function(r) {
      evaluate_lambda(grid2$lambda_mes[r], grid2$lambda_mnes[r]) |>
        mutate(lambda_mes = grid2$lambda_mes[r], lambda_mnes = grid2$lambda_mnes[r], .before = 1)
    }) |>
      dplyr::bind_rows()
    best2 <- type2 |>
      filter(!is.na(mse)) |>
      slice_min(mse, n = 1, with_ties = FALSE)
    lambda_mes <- best2$lambda_mes
    lambda_other <- best2$lambda_mnes
  }
  list(
    lambda_mean = lambda_mean,
    lambda_mes = lambda_mes,
    lambda_other = lambda_other,
    type1 = type1,
    type2 = type2
  )
}

# --- Decay-rate calibration ------------------------------
#
# The grid search takes about 40 minutes, so each result is cached in
# output/data and reused if the file exists (delete a file to re-estimate it)
#
#   lambda_calibration_hhwtrp.rds  owners weighted by the cross-sectional
#                                  responding-person weight (hhwtrp). Used for
#                                  every estimate in the thesis.
#   lambda_calibration_lnwtrp.rds  owners weighted by the longitudinal
#                                  responding-person weight (lnwtrp). Used only
#                                  for the sensitivity check in Table B.10.
#                                  This file was produced by an earlier version
#                                  of estimate_lambda(), which used a narrower
#                                  employee sample (65,988 rather than 156,786
#                                  person-years) and did not control for the
#                                  worker's origin. Deleting it re-estimates it
#                                  with the current code.
#
# The hhwtrp version is the more appropriate one. Each owner share describes
# the owners of an industry in a single wave, which is what a cross-sectional
# weight is designed for. The longitudinal weight is zero in wave 1 and
# positive only for people who have responded in every wave since wave 1, so it
# drops a growing share of owners over time (about two thirds by wave 21),
# including most low-English owners and the 2011 top-up sample.
#
# In both versions the wage regression itself is unweighted, because sampling
# is exogenous to wages, so unweighted OLS is consistent and typically more
# precise (Solon, Haider and Wooldridge, 2015).

calibrate_lambda <- function(weight_col, file_name) {
  path <- file.path(directory_data_final, file_name)
  if (file.exists(path)) {
    message("Loaded cached lambda calibration from ", path)
    return(readRDS(path))
  }
  message("Running lambda calibration (", weight_col, "), this takes about 40 minutes...")
  out <- estimate_lambda(
    panel_derived,
    lambda_grid = seq(0, 0.30, by = 0.01),
    lambda_grid_type2 = seq(0, 0.50, by = 0.01),
    weight_col = weight_col
  )
  saveRDS(out, path)
  message("Saved lambda calibration to ", path)
  out
}

cal_noorigin <- calibrate_lambda("respondingperson_crosssectional_weight_hhwtrp", "lambda_calibration_hhwtrp_noorigin.rds")
cal <- calibrate_lambda("respondingperson_crosssectional_weight_hhwtrp", "lambda_calibration_hhwtrp.rds")
cal_lnw <- calibrate_lambda("responding_person_longitudinal_weight_lnwtrp", "lambda_calibration_lnwtrp.rds")

lambda_mean <- cal$lambda_mean # type 1 best
lambda_mes <- cal$lambda_mes # type 2 best for mes
lambda_other <- cal$lambda_other # type 2 best for mnes

# Decay rates calibrated with lnwtrp-weighted shares (Table B.10 only)
lambda_mean_lnw <- cal_lnw$lambda_mean
lambda_mes_lnw <- cal_lnw$lambda_mes
lambda_other_lnw <- cal_lnw$lambda_other

panel_derived_2 <- panel_derived |>
  mutate(
    theta_type0 = case_when(
      origin_group == "native" ~ 0,
      years_in_aus >= 0 ~ 1,
      .default = NA_real_
    ),
    theta_type1 = case_when(
      origin_group == "native" ~ 0,
      years_in_aus >= 0 ~ exp(-lambda_mean * years_in_aus),
      .default = NA_real_
    ),
    theta_type2 = case_when(
      origin_group == "native" ~ 0,
      origin_group == "mes" & years_in_aus >= 0 ~ exp(
        -lambda_mes * years_in_aus
      ),
      origin_group %in%
        c("mnes_higheng", "mnes_loweng") &
        years_in_aus >= 0 ~ exp(-lambda_other * years_in_aus),
      .default = NA_real_
    ),
    # Same weightings with the decay rates calibrated on lnwtrp-weighted shares
    theta_type1_lnw = case_when(
      origin_group == "native" ~ 0,
      years_in_aus >= 0 ~ exp(-lambda_mean_lnw * years_in_aus),
      .default = NA_real_
    ),
    theta_type2_lnw = case_when(
      origin_group == "native" ~ 0,
      origin_group == "mes" & years_in_aus >= 0 ~ exp(
        -lambda_mes_lnw * years_in_aus
      ),
      origin_group %in%
        c("mnes_higheng", "mnes_loweng") &
        years_in_aus >= 0 ~ exp(-lambda_other_lnw * years_in_aus),
      .default = NA_real_
    )
  )


# --- Master panel -------------------------------------------------------------

panel_final <- panel_derived_2 |>
  left_join(
    compute_loo_shares(
      panel_derived_2,
      "industry_19categories_jbmi61",
      "1d_type0",
      theta_col = "theta_type0"
    ),
    by = c("xwaveid", "wave")
  ) |>
  left_join(
    compute_loo_shares(
      panel_derived_2,
      "industry_sparsecategories_jbmi62",
      "2d_type0",
      theta_col = "theta_type0"
    ),
    by = c("xwaveid", "wave")
  ) |>
  # Sensitivity check (Table B.10): the same shares weighted by the longitudinal
  # weight lnwtrp instead of the cross-sectional weight, with decay rates
  # calibrated on lnwtrp-weighted shares
  left_join(
    compute_loo_shares(
      panel_derived_2,
      "industry_19categories_jbmi61",
      "1d_type0_lnw",
      theta_col = "theta_type0",
      weight_col = "responding_person_longitudinal_weight_lnwtrp"
    ),
    by = c("xwaveid", "wave")
  ) |>
  left_join(
    compute_loo_shares(
      panel_derived_2,
      "industry_19categories_jbmi61",
      "1d_type1_lnw",
      theta_col = "theta_type1_lnw",
      weight_col = "responding_person_longitudinal_weight_lnwtrp"
    ),
    by = c("xwaveid", "wave")
  ) |>
  left_join(
    compute_loo_shares(
      panel_derived_2,
      "industry_19categories_jbmi61",
      "1d_type2_lnw",
      theta_col = "theta_type2_lnw",
      weight_col = "responding_person_longitudinal_weight_lnwtrp"
    ),
    by = c("xwaveid", "wave")
  ) |>
  left_join(
    compute_loo_shares(
      panel_derived_2,
      "industry_19categories_jbmi61",
      "1d_type1",
      theta_col = "theta_type1"
    ),
    by = c("xwaveid", "wave")
  ) |>
  left_join(
    compute_loo_shares(
      panel_derived_2,
      "industry_sparsecategories_jbmi62",
      "2d_type1",
      theta_col = "theta_type1"
    ),
    by = c("xwaveid", "wave")
  ) |>
  left_join(
    compute_loo_shares(
      panel_derived_2,
      "industry_19categories_jbmi61",
      "1d_type2",
      theta_col = "theta_type2"
    ),
    by = c("xwaveid", "wave")
  ) |>
  left_join(
    compute_loo_shares(
      panel_derived_2,
      "industry_sparsecategories_jbmi62",
      "2d_type2",
      theta_col = "theta_type2"
    ),
    by = c("xwaveid", "wave")
  ) |>
  left_join(
    compute_loo_shares_plain(
      panel_derived_2,
      "industry_19categories_jbmi61",
      "1d"
    ),
    by = c("xwaveid", "wave")
  ) |>
  left_join(
    compute_loo_shares_plain(
      panel_derived_2,
      "industry_sparsecategories_jbmi62",
      "2d"
    ),
    by = c("xwaveid", "wave")
  )


# --- Analysis samples ---------------------------------------------------------

owner_and_employee_sample <- panel_final |>
  filter(emp_status_detail_esempdt %in% c("employee", labels_owner))
owner_sample <- panel_final |>
  filter(emp_status_detail_esempdt %in% c(labels_owner))
employee_sample <- panel_final |>
  filter(emp_status_detail_esempdt %in% c("employee"))

save_final(panel_final, "panel_final.csv")
save_final(owner_and_employee_sample, "owner_and_employee_sample.csv")
save_final(owner_sample, "owner_sample.csv")
save_final(employee_sample, "employee_sample.csv")

fe_checks()

# ==============================================================================
# End of 03_featureengineering.R
# ==============================================================================
