# ==============================================================================
# Table D.5: Labour market position and liquidity at t−1 among people at risk of entering business ownership, by origin group.
#
# Thesis label: @tbl-b3-push-shares (Appendix D; source chunk in project/thesis/07_appendix.qmd)
# What it shows: How often each origin group is unemployed, out of the labour force,
#   low-paid or liquidity constrained at t-1, raw (Panel A), relative to the
#   native-born holding age, sex and year fixed (Panel B), and among entrants (Panel C).
# Output: project/output/tables/table_D_05.html and table_D_05.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

push_vars <- c(
  unemployed_l1 = "Unemployed",
  nilf_l1 = "Not in labour force",
  low_wage_l1 = "Bottom wage quintile",
  liquidity_l1 = "Liquidity constrained"
)

# Liquidity is observed only for people who returned the self-completion
# questionnaire, so it uses those person-pairs alone
push_sample <- function(d, v) {
  if (v == "liquidity_l1") dplyr::filter(d, liquidity_miss_l1 == 0) else d
}

# Share of each group in each position, in percent
push_shares <- function(d) {
  out <- tibble::tibble(Group = unname(group_labels))
  for (v in names(push_vars)) {
    dv <- push_sample(d, v)
    s <- tapply(dv[[v]], dv$origin_group, mean)[names(group_labels)]
    out[[push_vars[[v]]]] <- ifelse(is.na(s), "–", sprintf("%.1f", 100 * s))
  }
  out
}

# Difference from the native-born in percentage points (07_analysisblock3.R)
push_diff <- regression3_push_position

# Each difference (with its stars) over its standard error
diff_rows <- tibble::tibble(Group = as.vector(rbind(unname(group_labels[-1]), "")))
for (v in names(push_vars)) {
  ct <- fixest::coeftable(push_diff[[v]])
  terms <- paste0("origin_group::", names(group_labels)[-1])
  diff_rows[[push_vars[[v]]]] <- as.vector(rbind(
    sprintf("%.1f%s", ct[terms, 1], add_stars(ct[terms, 4])),
    sprintf("(%.1f)", ct[terms, 2])
  ))
}

entrants <- dplyr::filter(data_regression_3, entry_owner == 1)
n_col <- function(d) fmt_int(as.integer(table(d$origin_group)[names(group_labels)]))

exhibit <- dplyr::bind_rows(
  dplyr::mutate(push_shares(data_regression_3), `Person-pairs` = n_col(data_regression_3)),
  dplyr::mutate(diff_rows, `Person-pairs` = ""),
  dplyr::mutate(push_shares(entrants), `Person-pairs` = n_col(entrants))
) |>
  dplyr::rename(" " = Group) |>
  tinytable::tt(width = c(0.25, rep(0.15, 4), 0.15)) |>
  tinytable::theme_empty() |>
  tinytable::group_tt(i = list(
    "Panel A: Share of person-pairs at risk of entry (%)" = 1,
    "Panel B: Difference from the native-born, adjusted (percentage points)" = 5,
    "Panel C: Share of entrants (%)" = 11
  )) |>
  tinytable::style_tt(i = 0, line = "tb") |>
  tinytable::style_tt(i = c(1, 6, 13), italic = TRUE) |>
  tinytable::style_tt(i = 17, line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:6, align = "c") |>
  pair_se_rows(c(1, 5, 11))

save_table(exhibit, "table_D_05")
