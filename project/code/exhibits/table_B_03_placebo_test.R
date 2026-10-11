# ==============================================================================
# Table B.3: Owner density interacted with chain workplaces.
#
# Thesis label: @tbl-b1-placebo (Appendix B; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Robustness check: owner shares interacted with small sites of chains and chain-like workplaces.
# Output: project/output/tables/table_B_03.html and table_B_03.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

share_labels <- c(
  loo_mes = "MES owner density",
  loo_mnes_higheng = "MNES high-English owner density",
  loo_mnes_loweng = "MNES low-English owner density"
)
site_labels <- c(
  small_single_site = "Small single-site",
  small_site_chain = "Small-site chain",
  chain_like = "Chain-like"
)

# One row per workplace type x share, merged across the three decay types
coef_b1_placebo <- unlist(lapply(names(site_labels), \(w) {
  unlist(lapply(names(share_labels), \(s) {
    setNames(
      rep(paste(site_labels[[w]], "x", share_labels[[s]]), 3),
      paste0(w, ":", s, "_1d_type", 0:2)
    )
  }))
}))

b1_placebo_layout <- b1_full_layout(c(
  "Firm structure" = 3,
  "Main interactions" = 3,
  "Small sites of multi-site employers" = 3,
  "Chain-like workplaces" = 3
))

placebo_joint_p <- \(m, w) fmt_p(fixest::wald(m, keep = paste0("^", w, ":loo_"), print = FALSE)$p)

exhibit <- modelsummary::modelsummary(
  setNames(regression1_placebo, c("No decay (Type 0)", "Pooled decay (Type 1)", "Origin-specific decay (Type 2)")),
  coef_map = c(site_labels, coef_b1_placebo, b1_controls),
  gof_map = gof_comma(tribble(
    ~raw,   ~clean,         ~fmt,
    "nobs", "Observations (person-years)", 0
  )),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = thesis_stars,
  fmt = modelsummary::fmt_decimal(digits = 3, big.mark = ","),
  add_rows = tibble::tibble(
    term = c("Individuals", "Joint test, small-site chain (p-value)", "Joint test, chain-like (p-value)"),
    `No decay (Type 0)` = c(fmt_int(n_individuals_model(regression1_placebo$type0)), placebo_joint_p(regression1_placebo$type0, "small_site_chain"), placebo_joint_p(regression1_placebo$type0, "chain_like")),
    `Pooled decay (Type 1)` = c(fmt_int(n_individuals_model(regression1_placebo$type1)), placebo_joint_p(regression1_placebo$type1, "small_site_chain"), placebo_joint_p(regression1_placebo$type1, "chain_like")),
    `Origin-specific decay (Type 2)` = c(fmt_int(n_individuals_model(regression1_placebo$type2)), placebo_joint_p(regression1_placebo$type2, "small_site_chain"), placebo_joint_p(regression1_placebo$type2, "chain_like"))
  ),
  output = "tinytable"
) |>
  tinytable::theme_empty() |>
  tinytable::group_tt(
    j = list("Dependent variable: log hourly wage in main job" = 2:4),
    i = b1_placebo_layout$groups
  ) |>
  tinytable::style_tt(i = -1, line = "t") |>
  tinytable::style_tt(i = 0, bold = FALSE, line = "b") |>
  tinytable::style_tt(i = b1_placebo_layout$headers, italic = TRUE) |>
  # Coefficient rows, then 1 fit row and 3 added rows
  tinytable::style_tt(i = b1_placebo_layout$coef_end, line = "b") |>
  tinytable::style_tt(i = b1_placebo_layout$coef_end + 4, line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:4, align = "c") |>
  pair_se_rows(b1_placebo_layout$starts)

save_table(exhibit, "table_B_03")
