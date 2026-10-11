# ==============================================================================
# Table 5.2: Owner–employee gaps in income and income risk for small employers.
#
# Thesis label: @tbl-b2-main (Chapter 5 (Results), Section 5.2; source chunk in project/thesis/04_results.qmd)
# What it shows: Owner-employee gap for small employers by origin group: income, volatility and large falls, in the unrestricted and restricted samples.
# Output: project/output/tables/table_5_02.html and table_5_02.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

# Columns: dependent variable x sample. Restricted-sample names carry a
# trailing space only so that column names are unique.
b2_columns <- list(
  "Income (dollars)" = list(outcome = "income_t", sample = "unrestricted"),
  "Volatility (pp)" = list(outcome = "abs_arc_pct", sample = "unrestricted"),
  "Large fall (pp)" = list(outcome = "large_fall", sample = "unrestricted"),
  "Income (dollars) " = list(outcome = "income_t", sample = "restricted"),
  "Volatility (pp) " = list(outcome = "abs_arc_pct", sample = "restricted"),
  "Large fall (pp) " = list(outcome = "large_fall", sample = "restricted")
)
b2_sample_header <- list(
  "Unrestricted sample: status at t" = 2:4,
  "Restricted sample: same status at t−1 and t" = 5:7
)
b2_small <- b2_panels(contrasts_b2, "owner_emp_small", b2_columns)

b2_footer <- b2_footer_rows(b2_columns)

exhibit <- dplyr::bind_rows(b2_small$gap, b2_footer) |>
  dplyr::rename("Dependent variable:" = Group, "Small employers (restricted sample)" = n_owners) |>
  tinytable::tt(width = c(0.16, rep(0.11, 6), 0.10)) |>
  tinytable::theme_empty() |>
  tinytable::group_tt(j = b2_sample_header) |>
  tinytable::style_tt(i = -1, line = "t") |>
  tinytable::style_tt(i = 0, line = "b") |>
  tinytable::style_tt(i = c(8, 10), line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:8, align = "c") |>
  pair_se_rows()

save_table(exhibit, "table_5_02")
