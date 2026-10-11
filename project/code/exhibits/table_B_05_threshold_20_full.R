# ==============================================================================
# Table B.5: Employee wage results under the three weightings of the owner shares, with all controls.
#
# Thesis label: @tbl-b1-appendix (Appendix B; source chunk in project/thesis/07_appendix.qmd)
# What it shows: The employee wage regressions under no decay (the main specification of Table 5.1)
#   and the two decay weightings, with the 20-person threshold and all control coefficients.
# Output: project/output/tables/table_B_05.html and table_B_05.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

# Individuals per spec
n_ind_20_a <- fixest::obs(regression1_v1a) |> (\(idx) data_regression_1$xwaveid[idx])() |> dplyr::n_distinct()
n_ind_20_b <- fixest::obs(regression1_v1b) |> (\(idx) data_regression_1$xwaveid[idx])() |> dplyr::n_distinct()
n_ind_20_c <- fixest::obs(regression1_v1c) |> (\(idx) data_regression_1$xwaveid[idx])() |> dplyr::n_distinct()

# F-tests, clustered by individual and industry-year, joint null on all three firm-size x owner-share interactions
test_20_a <- cluster_f_test(
  regression1_v1a,
  c(
    "small_single_site:loo_mes_1d_type0 = 0",
    "small_single_site:loo_mnes_higheng_1d_type0 = 0",
    "small_single_site:loo_mnes_loweng_1d_type0 = 0"
  )
)
f_stat_20_a <- test_20_a[2, "F"]
df1_20_a    <- test_20_a[2, "Df"]
df2_20_a    <- test_20_a[2, "Res.Df"]
pval_20_a   <- test_20_a[2, "Pr(>F)"]

test_20_b <- cluster_f_test(
  regression1_v1b,
  c(
    "small_single_site:loo_mes_1d_type1 = 0",
    "small_single_site:loo_mnes_higheng_1d_type1 = 0",
    "small_single_site:loo_mnes_loweng_1d_type1 = 0"
  )
)
f_stat_20_b <- test_20_b[2, "F"]
df1_20_b    <- test_20_b[2, "Df"]
df2_20_b    <- test_20_b[2, "Res.Df"]
pval_20_b   <- test_20_b[2, "Pr(>F)"]

test_20_c <- cluster_f_test(
  regression1_v1c,
  c(
    "small_single_site:loo_mes_1d_type2 = 0",
    "small_single_site:loo_mnes_higheng_1d_type2 = 0",
    "small_single_site:loo_mnes_loweng_1d_type2 = 0"
  )
)
f_stat_20_c <- test_20_c[2, "F"]
df1_20_c    <- test_20_c[2, "Df"]
df2_20_c    <- test_20_c[2, "Res.Df"]
pval_20_c   <- test_20_c[2, "Pr(>F)"]

b1_layout_20 <- b1_full_layout(c(
  "Firm structure" = 3,
  "Interactions of firm structure with owner density" = 3
))

b1_robust_20 <- list(
  "No decay (Type 0)" = regression1_v1a,
  "Pooled decay (Type 1)" = regression1_v1b,
  "Origin-specific decay (Type 2)" = regression1_v1c
)

coef_b1_robust_20 <- c(
  # Firm structure
  "small_single_site" = "Small single-site",
  "chain_like" = "Chain-like",
  "small_site_chain" = "Small-site chain",
  # Interactions
  "small_single_site:loo_mes_1d_type0" = "Small single-site x MES owner density",
  "small_single_site:loo_mes_1d_type1" = "Small single-site x MES owner density",
  "small_single_site:loo_mes_1d_type2" = "Small single-site x MES owner density",
  "small_single_site:loo_mnes_higheng_1d_type0" = "Small single-site x MNES high-English owner density",
  "small_single_site:loo_mnes_higheng_1d_type1" = "Small single-site x MNES high-English owner density",
  "small_single_site:loo_mnes_higheng_1d_type2" = "Small single-site x MNES high-English owner density",
  "small_single_site:loo_mnes_loweng_1d_type0" = "Small single-site x MNES low-English owner density",
  "small_single_site:loo_mnes_loweng_1d_type1" = "Small single-site x MNES low-English owner density",
  "small_single_site:loo_mnes_loweng_1d_type2" = "Small single-site x MNES low-English owner density"
)

exhibit <- modelsummary::modelsummary(
  b1_robust_20,
  coef_map = c(coef_b1_robust_20, b1_controls),
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
    "Individuals",       format(n_ind_20_a, big.mark = ","),                   format(n_ind_20_b, big.mark = ","),                   format(n_ind_20_c, big.mark = ","),
    "Year FEs",          "Yes",                                               "Yes",                                               "Yes",
    "Industry-year FEs", "Yes",                                               "Yes",                                               "Yes",
    "State-year FEs",    "Yes",                                               "Yes",                                               "Yes",
    "F-Statistic (df)",  sprintf("%.3f (%d, %d)", f_stat_20_a, df1_20_a, df2_20_a), sprintf("%.3f (%d, %d)", f_stat_20_b, df1_20_b, df2_20_b), sprintf("%.3f (%d, %d)", f_stat_20_c, df1_20_c, df2_20_c),
    "F-test p-value",    fmt_p(pval_20_a),                          fmt_p(pval_20_b),                          fmt_p(pval_20_c)
  ),
  output = "tinytable") |>
  tinytable::theme_empty() |>
  tinytable::group_tt(
    j = list("Dependent variable: log hourly wage in main job" = 2:4),
    i = b1_layout_20$groups
  ) |>
  tinytable::style_tt(i = -1, line = "t") |>
  tinytable::style_tt(i = 0, bold = FALSE, line = "b") |>
  tinytable::style_tt(i = b1_layout_20$headers, italic = TRUE) |>
  # Coefficient rows, then 3 fit rows and 6 added rows
  tinytable::style_tt(i = b1_layout_20$coef_end + 1, line = "t") |>
  tinytable::style_tt(i = b1_layout_20$coef_end + 9, line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:4, align = "c") |>
  pair_se_rows(b1_layout_20$starts)

save_table(exhibit, "table_B_05")
