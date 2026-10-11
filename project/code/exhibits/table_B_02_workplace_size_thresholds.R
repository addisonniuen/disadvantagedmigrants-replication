# ==============================================================================
# Table B.2: Log hourly wages under alternative small-firm size thresholds.
#
# Thesis label: @tbl-b1-robustness (Appendix B; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Block 1 regressions with small workplaces defined at 10, 20 and 50 people.
# Output: project/output/tables/table_B_02.html and table_B_02.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

b1_robust_main <- list(
  "10 employees" = regression1_v1a_check_wpsize10,
  "20 employees (baseline)" = regression1_v1a,
  "50 employees" = regression1_v1a_check_wpsize50
)

b1_robust_layout <- b1_full_layout(c(
  "Firm structure" = 3,
  "Interactions of firm structure with owner density" = 3
))

coef_b1_robust_main <- c(
  # Firm structure
  "small_single_site_10" = "Small single-site",
  "small_single_site" = "Small single-site",
  "small_single_site_50" = "Small single-site",
  "chain_like_10" = "Chain-like",
  "chain_like" = "Chain-like",
  "chain_like_50" = "Chain-like",
  "small_site_chain_10" = "Small-site chain",
  "small_site_chain" = "Small-site chain",
  "small_site_chain_50" = "Small-site chain",
  # Interactions
  "small_single_site_10:loo_mes_1d_type0" = "Small single-site x MES owner density",
  "small_single_site:loo_mes_1d_type0" = "Small single-site x MES owner density",
  "small_single_site_50:loo_mes_1d_type0" = "Small single-site x MES owner density",
  "small_single_site_10:loo_mnes_higheng_1d_type0" = "Small single-site x MNES high-English owner density",
  "small_single_site:loo_mnes_higheng_1d_type0" = "Small single-site x MNES high-English owner density",
  "small_single_site_50:loo_mnes_higheng_1d_type0" = "Small single-site x MNES high-English owner density",
  "small_single_site_10:loo_mnes_loweng_1d_type0" = "Small single-site x MNES low-English owner density",
  "small_single_site:loo_mnes_loweng_1d_type0" = "Small single-site x MNES low-English owner density",
  "small_single_site_50:loo_mnes_loweng_1d_type0" = "Small single-site x MNES low-English owner density"
)

exhibit <- modelsummary::modelsummary(
  b1_robust_main,
  coef_map = c(coef_b1_robust_main, b1_controls),
  gof_map = gof_comma(tribble(
    ~raw            , ~clean               , ~fmt ,
    "nobs"          , "Observations (person-years)"       ,    0 ,
    "r.squared"     , "R-Squared"          ,    3 ,
    "adj.r.squared" , "Adjusted R-Squared" ,    3
  )),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = thesis_stars,
  fmt = modelsummary::fmt_decimal(digits = 3, big.mark = ","),
  add_rows = tibble::tribble(
    ~term                                          , ~`10 employees` , ~`20 employees (baseline)` , ~`50 employees` ,
    "Individuals"                                  , fmt_int(n_individuals_model(regression1_v1a_check_wpsize10)), fmt_int(n_individuals_model(regression1_v1a)), fmt_int(n_individuals_model(regression1_v1a_check_wpsize50)) ,
    "Year FEs"                                     , "Yes"  , "Yes"  , "Yes"  ,
    "Industry-year FEs"                            , "Yes"  , "Yes"  , "Yes"  ,
    "State-year FEs"                               , "Yes"  , "Yes"  , "Yes"  ,
    "F-Statistic (df)"                             , f_df(test_10a), f_df(test_20a), f_df(test_50a) ,
    "F-test p-value"                               , fmt_p(test_10a[2, "Pr(>F)"]), fmt_p(test_20a[2, "Pr(>F)"]), fmt_p(test_50a[2, "Pr(>F)"])
  ),
  output = "tinytable"
) |>
  tinytable::theme_empty() |>
  tinytable::group_tt(
    j = list("Dependent variable: log hourly wage in main job" = 2:4),
    i = b1_robust_layout$groups
  ) |>
  tinytable::style_tt(i = -1, line = "t") |>
  tinytable::style_tt(i = 0, bold = FALSE, line = "b") |>
  tinytable::style_tt(i = b1_robust_layout$headers, italic = TRUE) |>
  # Coefficient rows, then 3 fit rows and 6 added rows
  tinytable::style_tt(i = b1_robust_layout$coef_end + 1, line = "t") |>
  tinytable::style_tt(i = b1_robust_layout$coef_end + 9, line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:4, align = "c") |>
  pair_se_rows(b1_robust_layout$starts)

save_table(exhibit, "table_B_02")
