# ==============================================================================
# Table B.10: Employee wage results with owner shares weighted by the longitudinal survey weight (sensitivity check).
#
# Thesis label: @tbl-b1-lnw (Appendix B; source chunk in project/thesis/07_appendix.qmd)
# What it shows: The employee wage models under the three decay weightings,
#   with each owner in the shares weighted by the longitudinal weight lnwtrp
#   instead of the cross-sectional weight hhwtrp, with wild cluster bootstrap
#   p-values clustered by industry.
# Output: project/output/tables/table_B_10.html and table_B_10.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

# Main results with the owner shares weighted by the longitudinal weight
# (lnwtrp) instead of the cross-sectional weight, with decay rates calibrated
# on the lnwtrp-weighted shares
b1_lnw_types <- c(
  "No decay (Type 0)" = "type0",
  "Pooled decay (Type 1)" = "type1",
  "Origin-specific decay (Type 2)" = "type2"
)
b1_lnw_models <- setNames(regression1_lnw[b1_lnw_types], names(b1_lnw_types))
b1_lnw_inter <- \(type) paste0(
  "small_single_site:", c("loo_mes_1d_", "loo_mnes_higheng_1d_", "loo_mnes_loweng_1d_"), type, "_lnw"
)

coef_b1_lnw <- c(
  "small_single_site" = "Small single-site",
  "chain_like" = "Chain-like",
  "small_site_chain" = "Small-site chain"
)
b1_lnw_groups <- c(mes = "MES", mnes_higheng = "MNES high-English", mnes_loweng = "MNES low-English")
for (g in names(b1_lnw_groups)) {
  for (type in b1_lnw_types) {
    coef_b1_lnw[paste0("small_single_site:loo_", g, "_1d_", type, "_lnw")] <-
      paste("Small single-site x", b1_lnw_groups[[g]], "owner density")
  }
}

b1_lnw_layout <- b1_full_layout(c(
  "Firm structure" = 3,
  "Interactions of firm structure with owner density" = 3
))

# Joint F-tests and wild bootstrap p-values, one column per specification
b1_lnw_f <- purrr::map(b1_lnw_types, \(type) {
  cluster_f_test(regression1_lnw[[type]], paste(b1_lnw_inter(type), "= 0"))
})
b1_lnw_wild <- \(k) purrr::map_chr(b1_lnw_types, \(type) {
  fmt_p(regression1_wild_lnw$p_wild[regression1_wild_lnw$type == type][k])
})
b1_lnw_rows <- dplyr::bind_rows(
  c(term = "Individuals", purrr::map_chr(b1_lnw_models, \(m) fmt_int(n_individuals_model(m)))),
  c(term = "Year FEs", setNames(rep("Yes", 3), names(b1_lnw_types))),
  c(term = "Industry-year FEs", setNames(rep("Yes", 3), names(b1_lnw_types))),
  c(term = "State-year FEs", setNames(rep("Yes", 3), names(b1_lnw_types))),
  c(term = "F-Statistic (df)", purrr::map_chr(b1_lnw_f, \(f) sprintf("%.3f (%d, %d)", f[2, "F"], f[2, "Df"], f[2, "Res.Df"]))),
  c(term = "F-test p-value", purrr::map_chr(b1_lnw_f, \(f) fmt_p(f[2, "Pr(>F)"]))),
  c(term = "Wild bootstrap p-value, MES", b1_lnw_wild(1)),
  c(term = "Wild bootstrap p-value, MNES high-English", b1_lnw_wild(2)),
  c(term = "Wild bootstrap p-value, MNES low-English", b1_lnw_wild(3)),
  c(term = "Wild bootstrap p-value, joint", b1_lnw_wild(4))
)

exhibit <- modelsummary::modelsummary(
  b1_lnw_models,
  coef_map = c(coef_b1_lnw, b1_controls),
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
  add_rows = b1_lnw_rows,
  output = "tinytable"
) |>
  tinytable::theme_empty() |>
  tinytable::group_tt(
    i = b1_lnw_layout$groups,
    j = list("Dependent variable: log hourly wage in main job" = 2:4)
  ) |>
  tinytable::style_tt(i = -1, line = "t") |>
  tinytable::style_tt(i = 0, bold = FALSE, line = "b") |>
  tinytable::style_tt(i = b1_lnw_layout$headers, italic = TRUE) |>
  # Rule under the coefficients, and at the bottom after 3 fit rows and 10 added rows
  tinytable::style_tt(i = b1_lnw_layout$coef_end + c(0, 13), line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:4, align = "c") |>
  pair_se_rows(b1_lnw_layout$starts)

save_table(exhibit, "table_B_10")
