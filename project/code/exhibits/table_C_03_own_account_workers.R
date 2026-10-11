# ==============================================================================
# Table C.3: Income and risk of own-account workers relative to employees of the same origin.
#
# Thesis label: @tbl-b2-ownacc (Appendix C; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Owner-employee gap, difference from the native-born gap and minimum detectable difference for own-account workers.
# Output: project/output/tables/table_C_03.html and table_C_03.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

b2_other_columns <- list(
  "Income (dollars)" = list(outcome = "income_t", sample = "unrestricted"),
  "Volatility (pp)" = list(outcome = "abs_arc_pct", sample = "unrestricted"),
  "Large fall (pp)" = list(outcome = "large_fall", sample = "unrestricted"),
  "Income (dollars) " = list(outcome = "income_t", sample = "restricted"),
  "Volatility (pp) " = list(outcome = "abs_arc_pct", sample = "restricted"),
  "Large fall (pp) " = list(outcome = "large_fall", sample = "restricted")
)
x <- b2_panels(contrasts_b2, "own_acc", b2_other_columns)

exhibit <- dplyr::bind_rows(x$gap, x$diff, x$mde, b2_footer_rows(b2_other_columns)) |>
  dplyr::mutate(n_owners = dplyr::if_else(dplyr::row_number() <= 8, n_owners, "")) |>
  dplyr::rename("Dependent variable:" = Group, "Own-account workers (restricted sample)" = n_owners) |>
  tinytable::tt(width = c(0.16, rep(0.11, 6), 0.10)) |>
  tinytable::theme_empty() |>
  tinytable::group_tt(
    i = list(
      "Panel A: Owner\u2013employee gap" = 1,
      "Panel B: Difference from the native-born owner\u2013employee gap" = 9,
      "Panel C: Minimum detectable difference" = 15
    ),
    j = list(
      "Unrestricted sample: status at t" = 2:4,
      "Restricted sample: same status at t\u22121 and t" = 5:7
    )
  ) |>
  tinytable::style_tt(i = -1, line = "t") |>
  tinytable::style_tt(i = 0, line = "b") |>
  tinytable::style_tt(i = c(1, 10, 17), italic = TRUE) |>
  tinytable::style_tt(i = c(20, 22), line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:8, align = "c") |>
  pair_se_rows(c(1, 9, 15))

save_table(exhibit, "table_C_03")
