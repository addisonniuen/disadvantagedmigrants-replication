# ==============================================================================
# Table B.7: Log hourly wages under 20-employee small-firm size threshold, migrant and English-proficiency owner share specification, by industry classification granularity.
#
# Thesis label: @tbl-b1v2-appendix (Appendix B; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Block 1 with all-migrant and low-English owner shares, at one- and two-digit industry level.
# Output: project/output/tables/table_B_07.html and table_B_07.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

test_20_v2a <- cluster_f_test(
  regression1_v2a,
  c("small_single_site:loo_migrant_share_1d = 0", "small_single_site:loo_loweng_share_1d = 0")
)
f_stat_20_v2a  <- test_20_v2a[2, "F"]
df1_20_v2a     <- test_20_v2a[2, "Df"]
df2_20_v2a     <- test_20_v2a[2, "Res.Df"]
pval_20_v2a    <- test_20_v2a[2, "Pr(>F)"]

test_20_v4a <- cluster_f_test(
  regression1_v4a,
  c("small_single_site:loo_migrant_share_2d = 0", "small_single_site:loo_loweng_share_2d = 0")
)
f_stat_20_v4a  <- test_20_v4a[2, "F"]
df1_20_v4a     <- test_20_v4a[2, "Df"]
df2_20_v4a     <- test_20_v4a[2, "Res.Df"]
pval_20_v4a    <- test_20_v4a[2, "Pr(>F)"]

b1v2_layout <- b1_full_layout(c(
  "Firm structure" = 3,
  "Interactions of firm structure with owner density" = 2
))

b1v2_robust_20 <- list(
  "One-digit industry" = regression1_v2a,
  "Two-digit industry" = regression1_v4a
)

n_ind_20_v2a <- fixest::obs(regression1_v2a) |> (\(idx) data_regression_1$xwaveid[idx])() |> dplyr::n_distinct()
n_ind_20_v4a <- fixest::obs(regression1_v4a) |> (\(idx) data_regression_1$xwaveid[idx])() |> dplyr::n_distinct()

coef_b1v2_robust_20 <- c(
  # Firm structure
  "small_single_site" = "Small single-site",
  "chain_like" = "Chain-like",
  "small_site_chain" = "Small-site chain",
  # Interactions
  "small_single_site:loo_migrant_share_1d" = "Small single-site x Migrant owner density",
  "small_single_site:loo_migrant_share_2d" = "Small single-site x Migrant owner density",
  "small_single_site:loo_loweng_share_1d" = "Small single-site x Low-English owner density",
  "small_single_site:loo_loweng_share_2d" = "Small single-site x Low-English owner density"
)

exhibit <- modelsummary::modelsummary(
  b1v2_robust_20,
  coef_map = c(coef_b1v2_robust_20, b1_controls),
  gof_map = gof_comma(tribble(
    ~raw,            ~clean,               ~fmt,
    "nobs",          "Observations (person-years)",       0,
    "r.squared",     "R-Squared",          3,
    "adj.r.squared", "Adjusted R-Squared", 3
  )),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = thesis_stars,
  fmt = modelsummary::fmt_decimal(digits = 3, big.mark = ","),
  add_rows = tibble::tribble(
    ~term,                 ~`One-digit industry`,                                                  ~`Two-digit industry`,
    "Individuals",         format(n_ind_20_v2a, big.mark = ","),                     format(n_ind_20_v4a, big.mark = ","),
    "Industry classification", "1-digit (19 categories)",                           paste0("2-digit (", dplyr::n_distinct(data_regression_1$industry_sparsecategories_jbmi62[fixest::obs(regression1_v4a)]), " categories)"),
    "Year FEs",             "Yes",                                                   "Yes",
    "Industry-year FEs",    "Yes",                                                   "Yes",
    "State-year FEs",       "Yes",                                                   "Yes",
    "F-Statistic (df)",     sprintf("%.3f (%d, %d)", f_stat_20_v2a, df1_20_v2a, df2_20_v2a), sprintf("%.3f (%d, %d)", f_stat_20_v4a, df1_20_v4a, df2_20_v4a),
    "F-test p-value",       fmt_p(pval_20_v2a),                            fmt_p(pval_20_v4a)
  ),
  output = "tinytable") |>
  tinytable::theme_empty() |>
  tinytable::group_tt(
    j = list("Dependent variable: log hourly wage in main job" = 2:3),
    i = b1v2_layout$groups
  ) |>
  tinytable::style_tt(i = -1, line = "t") |>
  tinytable::style_tt(i = 0, bold = FALSE, line = "b") |>
  tinytable::style_tt(i = b1v2_layout$headers, italic = TRUE) |>
  # Coefficient rows, then 3 fit rows and 7 added rows
  tinytable::style_tt(i = b1v2_layout$coef_end + 1, line = "t") |>
  tinytable::style_tt(i = b1v2_layout$coef_end + 10, line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:3, align = "c") |>
  pair_se_rows(b1v2_layout$starts)

save_table(exhibit, "table_B_07")
