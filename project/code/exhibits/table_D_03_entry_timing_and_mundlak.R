# ==============================================================================
# Table D.3: Timing of labour market status and within-person estimates of entry into business ownership (logit coefficients).
#
# Thesis label: @tbl-b3-robust (Appendix D; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Entry logits with labour market status at t-1, t-2 or both, and with person means (Mundlak).
# Output: project/output/tables/table_D_03.html and table_D_03.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

b3_origins <- c(mes = "MES", mnes_higheng = "MNES high English", mnes_loweng = "MNES low English")
b3_push <- function(lag) c(
  setNames(paste0(c("Unemployed", "Bottom wage quintile"), " at t−", lag, ""), paste0(c("unemployed_l", "low_wage_l"), lag)),
  unlist(lapply(c("unemployed", "low_wage"), \(p) setNames(
    paste0(b3_origins, " x ", if (p == "unemployed") "Unemployed" else "Bottom wage quintile", " at t−", lag, ""),
    paste0("origin_group", names(b3_origins), ":", p, "_l", lag)
  )))
)

b3_robust_models <- list(
  "Status at t−1" = regression3_timing_l1,
  "Status at t−2" = regression3_timing_l2,
  "Status at t−1 and t−2" = regression3_timing_both,
  "With person means" = regression3_mundlak
)
# Full results, with every coefficient grouped into panels
b3_robust_layout <- full_coef_layout(b3_robust_models, list(
  "Push from the labour market, by origin" = c(b3_push(1), b3_push(2)),
  "Origin and English proficiency" = cm3_origin,
  "Other labour-market position" = c(
    "nilf_l1" = "Not in labour force at t−1",
    "nilf_l2" = "Not in labour force at t−2",
    "liquidity_l1" = "Liquidity constrained at t−1",
    "liquidity_l2" = "Liquidity constrained at t−2",
    "liquidity_miss_l1" = "Liquidity measure missing at t−1",
    "liquidity_miss_l2" = "Liquidity measure missing at t−2"
  ),
  "Person means" = c(
    "unemployed_l1_mean" = "Person mean of unemployment",
    "low_wage_l1_mean" = "Person mean of bottom wage quintile",
    "nilf_l1_mean" = "Person mean of not in labour force",
    "liquidity_l1_mean" = "Person mean of liquidity constraint",
    "homeowner_l1_mean" = "Person mean of homeownership",
    "seifa_decile_l1_mean" = "Person mean of SEIFA decile"
  ),
  "Human capital and job attributes at t−1" = cm3_human,
  "Demographics" = cm3_demog,
  "Household and location at t−1" = cm3_house
))
b3_joint_p <- \(m) fmt_p(fixest::wald(m, keep = ":(unemployed|low_wage)_l[12]$", print = FALSE)$p)

exhibit <- modelsummary::modelsummary(
  b3_robust_models,
  coef_map = b3_robust_layout$map,
  gof_map = gof_comma(tribble(
    ~raw,   ~clean,         ~fmt,
    "nobs", "Observations (person-pairs)", 0
  )),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = thesis_stars,
  fmt = modelsummary::fmt_decimal(digits = 3, big.mark = ","),
  add_rows = tibble::tibble(
    term = c("Individuals", "Labour market status measured at", "Sample", "Joint test of interactions (p-value)"),
    `Status at t−1` = c(fmt_int(n_individuals_model(regression3_timing_l1)), "t−1", "Observed at t−2", b3_joint_p(regression3_timing_l1)),
    `Status at t−2` = c(fmt_int(n_individuals_model(regression3_timing_l2)), "t−2", "Observed at t−2", b3_joint_p(regression3_timing_l2)),
    `Status at t−1 and t−2` = c(fmt_int(n_individuals_model(regression3_timing_both)), "t−1 and t−2", "Observed at t−2", b3_joint_p(regression3_timing_both)),
    `With person means` = c(fmt_int(n_individuals_model(regression3_mundlak)), "t−1, with person means", "Full", b3_joint_p(regression3_mundlak))
  ),
  output = "tinytable"
) |>
  tinytable::theme_empty() |>
  tinytable::group_tt(
    j = list("Dependent variable: entry into business ownership (logit)" = 2:5),
    i = b3_robust_layout$groups
  ) |>
  tinytable::style_tt(i = b3_robust_layout$headers, italic = TRUE) |>
  tinytable::style_tt(i = -1, line = "t") |>
  tinytable::style_tt(i = 0, line = "b") |>
  # Coefficient rows, then 1 fit row and 4 added rows
  tinytable::style_tt(i = b3_robust_layout$coef_end + c(0, 5), line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:5, align = "c") |>
  pair_se_rows(b3_robust_layout$starts)

save_table(exhibit, "table_D_03")
