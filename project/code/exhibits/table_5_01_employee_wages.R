# ==============================================================================
# Table 5.1: Main regression results for employee wages.
#
# Thesis label: @tbl-b1-main (Chapter 5 (Results), Section 5.1; source chunk in project/thesis/04_results.qmd)
# What it shows: The employee wage regressions: log hourly wages on workplace types and their interactions
#   with the migrant owner shares. The first column (plain shares, no decay) is the main specification;
#   the two decay weightings are shown for comparison.
# Output: project/output/tables/table_5_01.html and table_5_01.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

# F-tests of the three owner-share interactions, one per weighting
b1_main_types <- c(type0 = "No decay (Type 0, main)", type1 = "Pooled decay (Type 1)", type2 = "Origin-specific decay (Type 2)")
b1_main_models <- list(type0 = regression1_v1a, type1 = regression1_v1b, type2 = regression1_v1c)
b1_main_f <- purrr::imap(b1_main_models, \(m, type) cluster_f_test(
  m, paste0("small_single_site:", c("loo_mes_1d_", "loo_mnes_higheng_1d_", "loo_mnes_loweng_1d_"), type, " = 0")
))

# The main specification uses the plain owner shares (no decay, first column).
# The decay weightings are shown for comparison only.
b1_models <- setNames(b1_main_models, b1_main_types)

coef_b1_main <- c(
  # Firm structure
  "small_single_site" = "Small single-site",
  "chain_like" = "Chain-like",
  "small_site_chain" = "Small-site chain"
)
b1_main_groups <- c(mes = "MES", mnes_higheng = "MNES high-English", mnes_loweng = "MNES low-English")
for (g in names(b1_main_groups)) {
  for (type in names(b1_main_types)) {
    coef_b1_main[paste0("small_single_site:loo_", g, "_1d_", type)] <-
      paste("Small single-site x", b1_main_groups[[g]], "owner density")
  }
}

b1_main_rows <- dplyr::bind_rows(
  c(term = "Individuals", setNames(purrr::map_chr(b1_main_models, \(m) fmt_int(n_individuals_model(m))), b1_main_types)),
  c(term = "Controls", setNames(rep("Yes", 3), b1_main_types)),
  c(term = "Year FEs", setNames(rep("Yes", 3), b1_main_types)),
  c(term = "Industry-year FEs", setNames(rep("Yes", 3), b1_main_types)),
  c(term = "State-year FEs", setNames(rep("Yes", 3), b1_main_types)),
  c(term = "F-Statistic (df)", setNames(purrr::map_chr(b1_main_f, \(f) sprintf("%.3f (%d, %d)", f[2, "F"], f[2, "Df"], f[2, "Res.Df"])), b1_main_types)),
  c(term = "F-test p-value", setNames(purrr::map_chr(b1_main_f, \(f) fmt_p(f[2, "Pr(>F)"])), b1_main_types))
)

exhibit <- modelsummary::modelsummary(
  b1_models,
  coef_map = coef_b1_main,
  gof_map = gof_comma(tribble(
    ~raw            , ~clean                        , ~fmt ,
    "nobs"          , "Observations (person-years)" ,    0 ,
    "r.squared"     , "R-Squared"                   ,    3 ,
    "adj.r.squared" , "Adjusted R-Squared"          ,    3
  )),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = thesis_stars,
  fmt = modelsummary::fmt_decimal(digits = 3, big.mark = ","),
  add_rows = b1_main_rows,
  output = "tinytable"
) |>
  tinytable::theme_empty() |>
  tinytable::group_tt(
    i = list(
      "Firm structure" = 1,
      "Interactions of firm structure with owner density" = 7
    ),
    j = list("Dependent variable: log hourly wage in main job" = 2:4)
  ) |>
  tinytable::style_tt(i = -1, line = "t") |>
  tinytable::style_tt(i = 0, bold = FALSE, line = "b") |>
  tinytable::style_tt(i = c(1, 8), italic = TRUE) |>
  # The main specification (first column) in bold
  tinytable::style_tt(i = 0, j = 2, bold = TRUE) |>
  # Rule under the coefficients (2 group headers + 6 coefficients x 2 rows =
  # 14 rows) and at the bottom of the table
  tinytable::style_tt(i = c(14, 24), line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:4, align = "c") |>
  pair_se_rows(c(1, 7))

save_table(exhibit, "table_5_01")
