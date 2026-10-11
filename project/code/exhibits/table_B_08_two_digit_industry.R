# ==============================================================================
# Table B.8: Baseline regression results for employee wages, two-digit ANZSIC industry classification (full estimation results).
#
# Thesis label: @tbl-b1-robustness-2digit (Appendix B; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Block 1 with owner shares and fixed effects at the two-digit industry level.
# Output: project/output/tables/table_B_08.html and table_B_08.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

# F-tests of the three owner-share interactions, one per weighting
b1_2digit_f <- purrr::imap(
  list(type0 = regression1_v3a, type1 = regression1_v3b, type2 = regression1_v3c),
  \(m, type) cluster_f_test(m, paste0("small_single_site:", c("loo_mes_2d_", "loo_mnes_higheng_2d_", "loo_mnes_loweng_2d_"), type, " = 0"))
)

b1_2digit_layout <- b1_full_layout(c(
  "Firm structure" = 3,
  "Interactions of firm structure with owner density" = 3
))

b1_2digit_models <- list(
  "No decay (Type 0)" = regression1_v3a,
  "Pooled decay (Type 1)" = regression1_v3b,
  "Origin-specific decay (Type 2)" = regression1_v3c
)

coef_b1_2digit <- c(
  "small_single_site" = "Small single-site",
  "chain_like" = "Chain-like",
  "small_site_chain" = "Small-site chain",
  "small_single_site:loo_mes_2d_type0" = "Small single-site x MES owner density",
  "small_single_site:loo_mes_2d_type1" = "Small single-site x MES owner density",
  "small_single_site:loo_mes_2d_type2" = "Small single-site x MES owner density",
  "small_single_site:loo_mnes_higheng_2d_type0" = "Small single-site x MNES high-English owner density",
  "small_single_site:loo_mnes_higheng_2d_type1" = "Small single-site x MNES high-English owner density",
  "small_single_site:loo_mnes_higheng_2d_type2" = "Small single-site x MNES high-English owner density",
  "small_single_site:loo_mnes_loweng_2d_type0" = "Small single-site x MNES low-English owner density",
  "small_single_site:loo_mnes_loweng_2d_type1" = "Small single-site x MNES low-English owner density",
  "small_single_site:loo_mnes_loweng_2d_type2" = "Small single-site x MNES low-English owner density"
)

exhibit <- modelsummary::modelsummary(
  b1_2digit_models,
  coef_map = c(coef_b1_2digit, b1_controls),
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
    ~term                          , ~`No decay (Type 0)` , ~`Pooled decay (Type 1)` , ~`Origin-specific decay (Type 2)` ,
    "Individuals"                  , fmt_int(n_individuals_model(regression1_v3a)), fmt_int(n_individuals_model(regression1_v3b)), fmt_int(n_individuals_model(regression1_v3c)) ,
    "Year FEs"                     , "Yes"  , "Yes"  , "Yes"  ,
    "Industry-year FEs (2-digit)"  , "Yes"  , "Yes"  , "Yes"  ,
    "State-year FEs"               , "Yes"  , "Yes"  , "Yes"  ,
    "F-Statistic (df)"             , f_df(b1_2digit_f$type0), f_df(b1_2digit_f$type1), f_df(b1_2digit_f$type2) ,
    "F-test p-value"               , fmt_p(b1_2digit_f$type0[2, "Pr(>F)"]), fmt_p(b1_2digit_f$type1[2, "Pr(>F)"]), fmt_p(b1_2digit_f$type2[2, "Pr(>F)"])
  ),
  output = "tinytable"
) |>
  tinytable::theme_empty() |>
  tinytable::group_tt(
    j = list("Dependent variable: log hourly wage in main job" = 2:4),
    i = b1_2digit_layout$groups
  ) |>
  tinytable::style_tt(i = -1, line = "t") |>
  tinytable::style_tt(i = 0, bold = FALSE, line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(i = b1_2digit_layout$headers, italic = TRUE) |>
  # Coefficient rows, then 3 fit rows and 6 added rows
  tinytable::style_tt(i = b1_2digit_layout$coef_end + c(0, 9), line = "b") |>
  tinytable::style_tt(j = 2:4, align = "c") |>
  pair_se_rows(b1_2digit_layout$starts)

save_table(exhibit, "table_B_08")
