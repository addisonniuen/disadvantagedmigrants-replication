# ==============================================================================
# Table D.2: Full estimation results for entry into business ownership between t−1 and $t$.
#
# Thesis label: @tbl-b3-full (Appendix D; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Full logit estimates behind Table 5.4.
# Output: project/output/tables/table_D_02.html and table_D_02.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

models3_full <- list(
  "Origin only" = regression3_v1a,
  "Labour market only" = regression3_v2a,
  "Origin and labour market" = regression3_v3a,
  "Adding human capital" = regression3_v4a
)

# --- Coefficient labels, by panel ---------------------------------------------

# cm3_origin, cm3_labour, cm3_human, cm3_demog and cm3_house are in thesis_format.R
cm3_panels <- list(
  "Origin and English proficiency" = cm3_origin,
  "Labour-market position at t−1" = cm3_labour,
  "Human capital and job attributes at t−1" = cm3_human,
  "Demographics" = cm3_demog,
  "Household and location at t−1" = cm3_house
)
# Coefficient map and row positions of the panel headers and rules
b3_full_layout <- full_coef_layout(models3_full, cm3_panels)

# --- Wald tests: weak labour-market outcomes jointly zero ---------------------

push_wald <- function(m) {
  w <- wald_chisq(m, c("unemployed_l1", "nilf_l1", "low_wage_l1", "liquidity_l1"))
  sprintf("%.2f (%s)", w$stat, fmt_p(w$p))
}

# --- Individuals per model ----------------------------------------------------

n_ind <- function(m) {
  format(n_distinct(data_regression_3$xwaveid[fixest::obs(m)]), big.mark = ",")
}

rows3_full <- tibble::tribble(
  ~term,               ~`Origin only`,                      ~`Labour market only`,                      ~`Origin and labour market`,                      ~`Adding human capital`,
  "Individuals",       n_ind(regression3_v1a),      n_ind(regression3_v2a),      n_ind(regression3_v3a),      n_ind(regression3_v4a),
  "Year FEs",          "Yes",                       "Yes",                       "Yes",                       "Yes",
  "Industry-year FEs", "Yes",                       "Yes",                       "Yes",                       "Yes",
  "State-year FEs",    "Yes",                       "Yes",                       "Yes",                       "Yes",
  "Wald χ² (p-value)", "",
                       push_wald(regression3_v2a),
                       push_wald(regression3_v3a),
                       push_wald(regression3_v4a)
)

n_gof <- 3
last_row <- b3_full_layout$coef_end + n_gof + nrow(rows3_full)

# --- Table ----------------------------------------------------------------------

exhibit <- modelsummary::modelsummary(
  models3_full,
  coef_map = b3_full_layout$map,
  gof_map = gof_comma(tibble::tribble(
    ~raw,   ~clean,         ~fmt,
    "nobs", "Observations (person-pairs)", 0,
    "aic",  "AIC",          0,
    "bic",  "BIC",          0
  )),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = thesis_stars,
  add_rows = rows3_full,
  output = "tinytable"
) |>
  tinytable::theme_empty() |>
  tinytable::group_tt(
    j = list("Dependent variable: entry into business ownership (logit)" = 2:5),
    i = b3_full_layout$groups) |>
  tinytable::style_tt(i = b3_full_layout$headers, italic = TRUE) |>
  tinytable::style_tt(i = c(0, b3_full_layout$coef_end, last_row), line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:5, align = "c") |>
  tinytable::style_tt(i = -1, line = "t") |>
  pair_se_rows(b3_full_layout$starts)

save_table(exhibit, "table_D_02")
