# ==============================================================================
# Table 5.5: Push into business ownership from unemployment and low wages, by origin group.
#
# Thesis label: @tbl-b3-het (Chapter 5 (Results), Section 5.3; source chunk in project/thesis/04_results.qmd)
# What it shows: Odds ratios of entry for the unemployed and low-paid by origin group, from the interacted logit.
# Output: project/output/tables/table_5_05.html and table_5_05.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

# Odds ratio exp(sum of the coefficients in `terms`) with its stars, and its
# 95% confidence interval in brackets for the row below
b3_or_cells <- function(m, terms) {
  b <- coef(m)
  w <- as.numeric(names(b) %in% terms)
  est <- sum(w * b)
  se <- sqrt(drop(t(w) %*% vcov(m) %*% w))
  c(
    sprintf("%.2f%s", exp(est), add_stars(2 * pnorm(-abs(est / se)))),
    sprintf("[%.2f, %.2f]", exp(est - 1.96 * se), exp(est + 1.96 * se))
  )
}

# Push for group g (main effect plus interaction), and the interaction alone
b3_push_cells <- function(m, push, g) {
  if (g == "native") return(c(b3_or_cells(m, push), "Ref.", ""))
  inter <- paste0("origin_group", g, ":", push)
  c(b3_or_cells(m, c(push, inter)), b3_or_cells(m, inter))
}

entries_unemp <- data_regression_3 |>
  dplyr::filter(unemployed_l1 == 1) |>
  dplyr::summarise(n = sum(entry_owner), .by = origin_group)

b3_het <- purrr::map_dfr(names(group_labels), \(g) {
  u <- b3_push_cells(regression3_v9a, "unemployed_l1", g)
  w <- b3_push_cells(regression3_v9a, "low_wage_l1", g)
  # Odds of entry of group g relative to the native-born among employees who
  # are not low-paid (the omitted labour market category): exp(phi_g)
  s <- if (g == "native") c("Ref.", "") else b3_or_cells(regression3_v9a, paste0("origin_group", g))
  tibble::tibble(
    `Origin group` = c(group_labels[[g]], ""),
    `Odds ratio` = u[1:2], `Relative to native-born` = u[3:4],
    `Odds ratio ` = w[1:2], `Relative to native-born ` = w[3:4],
    `Odds ratio when stably employed` = s,
    `Entries among the unemployed` = c(fmt_int(entries_unemp$n[entries_unemp$origin_group == g]), "")
  )
})

exhibit <- b3_het |>
  dplyr::bind_rows(tibble::tibble(
    `Origin group` = c("Observations (person-pairs)", "Individuals", "Joint test of interactions (p-value)"),
    `Odds ratio` = c(
      fmt_int(nobs(regression3_v9a)),
      fmt_int(n_individuals_model(regression3_v9a)),
      fmt_p(fixest::wald(regression3_v9a, keep = ":(unemployed|low_wage)_l1$", print = FALSE)$p)
    )
  )) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), \(x) dplyr::coalesce(x, ""))) |>
  tinytable::tt(width = c(0.20, rep(0.12, 6))) |>
  tinytable::theme_empty() |>
  tinytable::group_tt(j = list("Unemployed at t−1" = 2:3, "Bottom wage quintile at t−1" = 4:5)) |>
  tinytable::group_tt(j = list("Dependent variable: entry into business ownership (logit)" = 2:7)) |>
  tinytable::style_tt(i = -2, line = "t") |>
  tinytable::style_tt(i = 0, line = "b") |>
  tinytable::style_tt(i = c(8, 11), line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:7, align = "c") |>
  pair_se_rows()

save_table(exhibit, "table_5_05")
