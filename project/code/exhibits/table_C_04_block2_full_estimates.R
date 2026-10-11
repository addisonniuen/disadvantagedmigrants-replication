# ==============================================================================
# Table C.4: Income level, volatility and downside risk by ownership status, business size and origin group (full estimation results).
#
# Thesis label: @tbl-b2-appendix (Appendix C; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Full Block 2 regression estimates for both samples and all three outcomes.
# Output: project/output/tables/table_C_04.html and table_C_04.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

fmt_int <- function(x) formatC(x, format = "d", big.mark = ",")
fmt_num <- function(x, d) formatC(x, format = "f", digits = d)

# Whole dollars for the income level, three decimals for the other outcomes
fmt_mixed <- \(x) ifelse(
  abs(x) >= 100,
  formatC(x, format = "f", digits = 0, big.mark = ","),
  formatC(x, format = "f", digits = 3)
)

origin_label <- c(mes = "MES", mnes_higheng = "MNES high-English", mnes_loweng = "MNES low-English")
owner_label <- c(
  owner_emp_small = "Small employer",
  owner_emp_large = "Large employer",
  own_acc = "Own-account worker"
)

b2_groups <- list(
  "Ownership and origin" = c(
    owner_label,
    setNames(origin_label, paste0("origin_group::", names(origin_label))),
    unlist(purrr::map(names(owner_label), \(o) setNames(
      paste(origin_label, "x", owner_label[[o]]),
      paste0("origin_group::", names(origin_label), ":", o)
    )))
  ),
  "Demographics" = c(
    "age_hgage" = "Age",
    "I(I(age_hgage^2))" = "Age squared",
    "sex_hgsexfemale" = "Female",
    "marital_status_mrcurrde facto" = "De facto",
    "marital_status_mrcurrseparated" = "Separated",
    "marital_status_mrcurrdivorced" = "Divorced",
    "marital_status_mrcurrwidowed" = "Widowed",
    "marital_status_mrcurrnever married and not de facto" = "Never married"
  ),
  "Education (reference: year 11 and below)" = c(
    "highest_education_achieved_edhigh1postgrad (master or doctorate)" = "Postgraduate",
    "highest_education_achieved_edhigh1grad diploma, grad certificate" = "Graduate diploma or certificate",
    "highest_education_achieved_edhigh1bachelor or honours" = "Bachelor or honours",
    "highest_education_achieved_edhigh1adv diploma, diploma" = "Advanced diploma or diploma",
    "highest_education_achieved_edhigh1certificate iii or iv" = "Certificate III or IV",
    "highest_education_achieved_edhigh1year 12" = "Year 12"
  ),
  "Location and job characteristics" = c(
    "remoteness_hhs3rainner regional australia" = "Inner regional",
    "remoteness_hhs3raouter regional australia" = "Outer regional",
    "remoteness_hhs3raremote australia" = "Remote",
    "remoteness_hhs3ravery remote australia" = "Very remote",
    "seifa_decile" = "SEIFA disadvantage decile (1 = most disadvantaged)",
    "tenure_occ_pair" = "Occupational tenure (years)",
    "firm_type_pair::small_single_site" = "Small single-site workplace",
    "firm_type_pair::small_site_chain" = "Small site of multi-site employer",
    "firm_type_pair::chain_like" = "Chain-like workplace"
  ),
  "Prior income decile (reference: decile 5)" = c(
    setNames(
      paste0("Decile ", c(1:4, 6:10), c(" (lowest)", rep("", 3), rep("", 4), " (highest)")),
      paste0("prior_income_decile::", c(1:4, 6:10))
    ),
    "prior_income_decile::none" = "Fewer than two prior waves"
  )
)

b2_models_full <- list(
  "Income (dollars)" = regression2_income_unrestricted,
  "Volatility (pp)" = regression2_volatility_unrestricted,
  "Large fall (share)" = regression2_large_fall_unrestricted,
  "Income (dollars) " = regression2_income,
  "Volatility (pp) " = regression2_volatility,
  "Large fall (share) " = regression2_large_fall
)

# Keep only rows that some model estimates, so the group positions are right
terms_present <- unique(unlist(lapply(b2_models_full, \(m) names(coef(m)))))
b2_groups <- lapply(b2_groups, \(g) g[names(g) %in% terms_present])
b2_groups <- b2_groups[lengths(b2_groups) > 0]

coef_b2_full <- do.call(c, unname(b2_groups))
n_rows_group <- sapply(b2_groups, \(g) length(unique(g)))
n_groups <- length(b2_groups)
n_coef <- 2 * sum(n_rows_group)
group_rows <- as.list(setNames(
  1 + 2 * c(0, cumsum(n_rows_group)[-n_groups]),
  names(b2_groups)
))

# Joint tests in column order: unrestricted then restricted, income, volatility, large fall
jt_order <- tidyr::expand_grid(sample = c("unrestricted", "restricted"), outcome = c("income_t", "abs_arc_pct", "large_fall")) |>
  dplyr::left_join(joint_tests, by = c("sample", "outcome"))
jt_df <- paste0(fmt_num(jt_order$F_stat, 2), " (", jt_order$df1, ", ", fmt_int(jt_order$df2), ")")
jt_pv <- fmt_num(jt_order$p.value, 3)

n_individuals <- \(m, d) fmt_int(dplyr::n_distinct(d$xwaveid[fixest::obs(m)]))

b2_data_full <- rep(list(data_regression_2_unrestricted, data_regression_2_pairs), each = 3)
b2_rows <- tibble::tibble(
  term = c("Sample", "Same status at t−1 required", "Year FEs", "Industry-year FEs", "State-year FEs",
           "F-test (df)", "F-test (p-value)", "Individuals"),
  !!!setNames(lapply(seq_along(b2_models_full), \(k) c(
    if (k <= 3) "Unrestricted" else "Restricted",
    if (k <= 3) "No" else "Yes",
    "Yes", "Yes", "Yes",
    jt_df[k], jt_pv[k],
    n_individuals(b2_models_full[[k]], b2_data_full[[k]])
  )), names(b2_models_full))
)
attr(b2_rows, "position") <- n_coef + 1:8

exhibit <- modelsummary::modelsummary(
  b2_models_full,
  coef_map = coef_b2_full,
  gof_map = gof_comma(tibble::tribble(
    ~raw,        ~clean,         ~fmt,
    "nobs",      "Observations (person-pairs)", 0,
    "r.squared", "R-squared",    3
  )),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = thesis_stars,
  fmt = fmt_mixed,
  add_rows = b2_rows,
  output = "tinytable"
) |>
  tinytable::theme_empty() |>
  tinytable::group_tt(i = group_rows, j = list("Unrestricted sample: status at t" = 2:4, "Restricted sample: same status at t−1 and t" = 5:7)) |>
  tinytable::style_tt(i = -1, line = "t") |>
  tinytable::style_tt(i = 0, line = "b") |>
  tinytable::style_tt(i = n_coef + n_groups, line = "b") |>
  tinytable::style_tt(i = n_coef + n_groups + 8 + 2, line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:7, align = "c") |>
  pair_se_rows(unlist(group_rows))

save_table(exhibit, "table_C_04")
