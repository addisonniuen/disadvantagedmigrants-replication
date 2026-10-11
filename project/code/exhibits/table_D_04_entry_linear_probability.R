# ==============================================================================
# Table D.4: Linear probability models of entry into business ownership (percentage points).
#
# Thesis label: @tbl-b3-lpm (Appendix D; source chunk in project/thesis/07_appendix.qmd)
# What it shows: OLS (linear probability) versions of the four main entry logits and the
#   origin x push logit, on the same person-pairs, plus the origin x push model with
#   individual fixed effects. Coefficients are percentage-point changes in the annual
#   probability of entry.
# Output: project/output/tables/table_D_04.html and table_D_04.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

b3_lpm_models <- list(
  "Origin only" = regression3_lpm_v1,
  "Labour market only" = regression3_lpm_v2,
  "Origin and labour market" = regression3_lpm_v3,
  "Adding human capital" = regression3_lpm_v4,
  "Origin × push" = regression3_lpm_het,
  "Origin × push, individual FEs" = regression3_lpm_het_fe
)

# The individual fixed-effects model names its interactions with fixest's i()
# ("origin_group::mes:unemployed_l1"), so both spellings map to the same row
b3_lpm_inter <- function(g, label) {
  out <- character()
  for (p in c("unemployed_l1", "low_wage_l1")) {
    lab <- paste0(label, " × ", if (p == "unemployed_l1") "Unemployed" else "Bottom wage quintile")
    out[paste0("origin_group", g, ":", p)] <- lab
    out[paste0("origin_group::", g, ":", p)] <- lab
  }
  out
}
cm3_lpm <- c(
  "origin_groupmes" = "MES",
  "origin_groupmnes_higheng" = "MNES high English",
  "origin_groupmnes_loweng" = "MNES low English",
  "unemployed_l1" = "Unemployed",
  "nilf_l1" = "Not in labour force",
  "low_wage_l1" = "Bottom wage quintile",
  "liquidity_l1" = "Liquidity constrained",
  b3_lpm_inter("mes", "MES"),
  b3_lpm_inter("mnes_higheng", "MNES high English"),
  b3_lpm_inter("mnes_loweng", "MNES low English")
)
# Interaction rows ordered by push, then group, as in @tbl-b3-robust
b3_lpm_inter_all <- cm3_lpm[grepl(":", names(cm3_lpm))]
b3_lpm_inter_all <- b3_lpm_inter_all[order(!grepl("unemployed_l1$", names(b3_lpm_inter_all)))]

# Full results, with every coefficient grouped into panels
b3_lpm_layout <- full_coef_layout(b3_lpm_models, list(
  "Origin and English proficiency" = cm3_origin,
  "Labour-market position at t−1" = cm3_labour,
  "Origin × labour-market position at t−1" = b3_lpm_inter_all,
  "Human capital and job attributes at t−1" = cm3_human,
  "Demographics" = cm3_demog,
  "Household and location at t−1" = cm3_house
))

b3_lpm_joint_p <- \(m) fmt_p(fixest::wald(m, keep = ":(unemployed|low_wage)_l1$", print = FALSE)$p)
b3_lpm_mean <- \(m) sprintf("%.2f", mean(fitted(m) + resid(m)))

b3_lpm_n <- \(m) fmt_int(nobs(m))

b3_lpm_rows <- tibble::tibble(
  term = c("Observations (person-pairs)", "Individuals", "Mean entry rate (%)", "Year, industry-year and state-year FEs",
           "Individual FEs", "Joint test of interactions (p-value)"),
  `Origin only` = c(b3_lpm_n(regression3_lpm_v1), fmt_int(n_individuals_model(regression3_lpm_v1)), b3_lpm_mean(regression3_lpm_v1), "Yes", "No", ""),
  `Labour market only` = c(b3_lpm_n(regression3_lpm_v2), fmt_int(n_individuals_model(regression3_lpm_v2)), b3_lpm_mean(regression3_lpm_v2), "Yes", "No", ""),
  `Origin and labour market` = c(b3_lpm_n(regression3_lpm_v3), fmt_int(n_individuals_model(regression3_lpm_v3)), b3_lpm_mean(regression3_lpm_v3), "Yes", "No", ""),
  `Adding human capital` = c(b3_lpm_n(regression3_lpm_v4), fmt_int(n_individuals_model(regression3_lpm_v4)), b3_lpm_mean(regression3_lpm_v4), "Yes", "No", ""),
  `Origin × push` = c(b3_lpm_n(regression3_lpm_het), fmt_int(n_individuals_model(regression3_lpm_het)), b3_lpm_mean(regression3_lpm_het), "Yes", "No", b3_lpm_joint_p(regression3_lpm_het)),
  `Origin × push, individual FEs` = c(b3_lpm_n(regression3_lpm_het_fe), fmt_int(n_individuals_model(regression3_lpm_het_fe)), b3_lpm_mean(regression3_lpm_het_fe), "Yes", "Yes", b3_lpm_joint_p(regression3_lpm_het_fe))
)

# Coefficient rows (with their panel headers), then the added rows
b3_lpm_last_coef <- b3_lpm_layout$coef_end
b3_lpm_last_row <- b3_lpm_last_coef + nrow(b3_lpm_rows)

exhibit <- modelsummary::modelsummary(
  b3_lpm_models,
  coef_map = b3_lpm_layout$map,
  gof_map = NA,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = thesis_stars,
  fmt = modelsummary::fmt_decimal(digits = 3, big.mark = ","),
  add_rows = b3_lpm_rows,
  # A wide label column, so the interaction labels do not wrap onto three lines
  width = c(0.26, rep(0.11, 6)),
  output = "tinytable"
) |>
  tinytable::theme_empty() |>
  tinytable::group_tt(
    j = list("Dependent variable: entry into business ownership × 100 (OLS)" = 2:7),
    i = b3_lpm_layout$groups
  ) |>
  tinytable::style_tt(i = b3_lpm_layout$headers, italic = TRUE) |>
  tinytable::style_tt(i = c(0, b3_lpm_last_coef, b3_lpm_last_row), line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:7, align = "c") |>
  tinytable::style_tt(i = -1, line = "t") |>
  pair_se_rows(b3_lpm_layout$starts)

save_table(exhibit, "table_D_04")
