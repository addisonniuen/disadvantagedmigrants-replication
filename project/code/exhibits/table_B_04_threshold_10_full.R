# ==============================================================================
# Table B.4: Log hourly wages under 10-employee small-firm size threshold (full estimation results).
#
# Thesis label: @tbl-b1-robustness-10 (Appendix B; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Full Block 1 estimates with a 10-person small-workplace threshold.
# Output: project/output/tables/table_B_04.html and table_B_04.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

# Individuals per spec
n_ind_10_a <- fixest::obs(regression1_v1a_check_wpsize10) |> (\(idx) data_regression_1$xwaveid[idx])() |> dplyr::n_distinct()
n_ind_10_b <- fixest::obs(regression1_v1b_check_wpsize10) |> (\(idx) data_regression_1$xwaveid[idx])() |> dplyr::n_distinct()
n_ind_10_c <- fixest::obs(regression1_v1c_check_wpsize10) |> (\(idx) data_regression_1$xwaveid[idx])() |> dplyr::n_distinct()

# F-tests, clustered by individual and industry-year, joint null on all three firm-size x owner-share interactions
test_10_a <- cluster_f_test(
  regression1_v1a_check_wpsize10,
  c(
    "small_single_site_10:loo_mes_1d_type0 = 0",
    "small_single_site_10:loo_mnes_higheng_1d_type0 = 0",
    "small_single_site_10:loo_mnes_loweng_1d_type0 = 0"
  )
)
f_stat_10_a <- test_10_a[2, "F"]
df1_10_a    <- test_10_a[2, "Df"]
df2_10_a    <- test_10_a[2, "Res.Df"]
pval_10_a   <- test_10_a[2, "Pr(>F)"]

test_10_b <- cluster_f_test(
  regression1_v1b_check_wpsize10,
  c(
    "small_single_site_10:loo_mes_1d_type1 = 0",
    "small_single_site_10:loo_mnes_higheng_1d_type1 = 0",
    "small_single_site_10:loo_mnes_loweng_1d_type1 = 0"
  )
)
f_stat_10_b <- test_10_b[2, "F"]
df1_10_b    <- test_10_b[2, "Df"]
df2_10_b    <- test_10_b[2, "Res.Df"]
pval_10_b   <- test_10_b[2, "Pr(>F)"]

test_10_c <- cluster_f_test(
  regression1_v1c_check_wpsize10,
  c(
    "small_single_site_10:loo_mes_1d_type2 = 0",
    "small_single_site_10:loo_mnes_higheng_1d_type2 = 0",
    "small_single_site_10:loo_mnes_loweng_1d_type2 = 0"
  )
)
f_stat_10_c <- test_10_c[2, "F"]
df1_10_c    <- test_10_c[2, "Df"]
df2_10_c    <- test_10_c[2, "Res.Df"]
pval_10_c   <- test_10_c[2, "Pr(>F)"]

b1_layout_10 <- b1_full_layout(c(
  "Firm structure" = 3,
  "Interactions of firm structure with owner density" = 3
))

b1_robust_10 <- list(
  "No decay (Type 0)" = regression1_v1a_check_wpsize10,
  "Pooled decay (Type 1)" = regression1_v1b_check_wpsize10,
  "Origin-specific decay (Type 2)" = regression1_v1c_check_wpsize10
)

coef_b1_robust_10 <- c(
  # Firm structure
  "small_single_site_10" = "Small single-site",
  "chain_like" = "Chain-like",
  "small_site_chain_10" = "Small-site chain",
  # Interactions
  "small_single_site_10:loo_mes_1d_type0" = "Small single-site x MES owner density",
  "small_single_site_10:loo_mes_1d_type1" = "Small single-site x MES owner density",
  "small_single_site_10:loo_mes_1d_type2" = "Small single-site x MES owner density",
  "small_single_site_10:loo_mnes_higheng_1d_type0" = "Small single-site x MNES high-English owner density",
  "small_single_site_10:loo_mnes_higheng_1d_type1" = "Small single-site x MNES high-English owner density",
  "small_single_site_10:loo_mnes_higheng_1d_type2" = "Small single-site x MNES high-English owner density",
  "small_single_site_10:loo_mnes_loweng_1d_type0" = "Small single-site x MNES low-English owner density",
  "small_single_site_10:loo_mnes_loweng_1d_type1" = "Small single-site x MNES low-English owner density",
  "small_single_site_10:loo_mnes_loweng_1d_type2" = "Small single-site x MNES low-English owner density"
)

exhibit <- modelsummary::modelsummary(
  b1_robust_10,
  coef_map = c(coef_b1_robust_10, b1_controls),
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
    ~term,               ~`No decay (Type 0)`,                                              ~`Pooled decay (Type 1)`,                                              ~`Origin-specific decay (Type 2)`,
    "Individuals",       format(n_ind_10_a, big.mark = ","),                   format(n_ind_10_b, big.mark = ","),                   format(n_ind_10_c, big.mark = ","),
    "Year FEs",          "Yes",                                               "Yes",                                               "Yes",
    "Industry-year FEs", "Yes",                                               "Yes",                                               "Yes",
    "State-year FEs",    "Yes",                                               "Yes",                                               "Yes",
    "F-Statistic (df)",  sprintf("%.3f (%d, %d)", f_stat_10_a, df1_10_a, df2_10_a), sprintf("%.3f (%d, %d)", f_stat_10_b, df1_10_b, df2_10_b), sprintf("%.3f (%d, %d)", f_stat_10_c, df1_10_c, df2_10_c),
    "F-test p-value",    fmt_p(pval_10_a),                          fmt_p(pval_10_b),                          fmt_p(pval_10_c)
  ),
  output = "tinytable"
) |>
  tinytable::theme_empty() |>
  tinytable::group_tt(
    j = list("Dependent variable: log hourly wage in main job" = 2:4),
    i = b1_layout_10$groups
  ) |>
  tinytable::style_tt(i = -1, line = "t") |>
  tinytable::style_tt(i = 0, bold = FALSE, line = "b") |>
  tinytable::style_tt(i = b1_layout_10$headers, italic = TRUE) |>
  # Coefficient rows, then 3 fit rows and 6 added rows
  tinytable::style_tt(i = b1_layout_10$coef_end + 1, line = "t") |>
  tinytable::style_tt(i = b1_layout_10$coef_end + 9, line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:4, align = "c") |>
  pair_se_rows(b1_layout_10$starts)

save_table(exhibit, "table_B_04")
