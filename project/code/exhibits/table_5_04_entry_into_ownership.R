# ==============================================================================
# Table 5.4: Entry into business ownership between t−1 and $t$ (logit coefficients).
#
# Thesis label: @tbl-b3-main (Chapter 5 (Results), Section 5.3; source chunk in project/thesis/04_results.qmd)
# What it shows: Main Block 3 logit regressions of entry into business ownership, adding regressors in four steps.
# Output: project/output/tables/table_5_04.html and table_5_04.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

# Wald chi-squared test that the four labour market and liquidity terms are zero
push_wald <- function(m) {
  w <- wald_chisq(m, c("unemployed_l1", "nilf_l1", "low_wage_l1", "liquidity_l1"))
  sprintf("%.2f (%s)", w$stat, fmt_p(w$p))
}

models3 <- list(
  "Origin only" = regression3_v1a,
  "Labour market only" = regression3_v2a,
  "Origin and labour market" = regression3_v3a,
  "Adding human capital" = regression3_v4a
)

cm3_main <- c(
  "origin_groupmes" = "Migrant with English-speaking origin",
  "origin_groupmnes_higheng" = "Migrant from other origin with high English proficiency",
  "origin_groupmnes_loweng" = "Migrant from other origin with low English proficiency",
  "unemployed_l1" = "Unemployed",
  "nilf_l1" = "Not in labour force",
  "low_wage_l1" = "Bottom wage quintile",
  "liquidity_l1" = "Liquidity constrained"
)

exhibit <- modelsummary::modelsummary(
  models3,
  coef_map = cm3_main,
  gof_map = gof_comma(tribble(
    ~raw   , ~clean        , ~fmt ,
    "nobs" , "Observations (person-pairs)" ,    0 ,
    "aic"  , "AIC"         ,    0 ,
    "bic"  , "BIC"         ,    0
  )),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = thesis_stars,
  add_rows = tibble::tribble(
    ~term               , ~`Origin only` , ~`Labour market only`                     , ~`Origin and labour market`                     , ~`Adding human capital`                     ,
    "Individuals"       , fmt_int(n_individuals_model(regression3_v1a)), fmt_int(n_individuals_model(regression3_v2a)), fmt_int(n_individuals_model(regression3_v3a)), fmt_int(n_individuals_model(regression3_v4a)),
    "Year FEs"          , "Yes"  , "Yes"                      , "Yes"                      , "Yes"                      ,
    "Industry-year FEs" , "Yes"  , "Yes"                      , "Yes"                      , "Yes"                      ,
    "State-year FEs"    , "Yes"  , "Yes"                      , "Yes"                      , "Yes"                      ,
    "Education and employer tenure" , "No" , "No"            , "No"                       , "Yes"                      ,
    "Wald χ² (p-value)" , ""     , push_wald(regression3_v2a) , push_wald(regression3_v3a) , push_wald(regression3_v4a)
  ),
  output = "tinytable"
) |>
  tinytable::theme_empty() |>
  tinytable::group_tt(
    j = list("Dependent variable: entry into business ownership (logit)" = 2:5),
    i = list(
      "Origin and English proficiency" = 1,
      "Labour-market position at t−1" = 7
    )
  ) |>
  tinytable::style_tt(i = c(1, 8), italic = TRUE) |>
  tinytable::style_tt(i = c(0, 16, 25), line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:5, align = "c") |>
  tinytable::style_tt(i = -1, line = "t") |>
  pair_se_rows(c(1, 7))

save_table(exhibit, "table_5_04")
