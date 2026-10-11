# ==============================================================================
# Table A.1: Estimation samples.
#
# Thesis label: @tbl-sample-flow (Appendix A; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Observations and individuals in the main estimation sample of each block.
# Output: project/output/tables/table_A_01.html and table_A_01.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

sample_row <- function(analysis, sample, unit, waves, m, data) {
  d <- data[fixest::obs(m), ]
  tibble::tibble(
    Analysis = analysis,
    Sample = sample,
    Unit = unit,
    Waves = waves,
    Observations = format(nobs(m), big.mark = ","),
    Individuals = format(dplyr::n_distinct(d$xwaveid), big.mark = ","),
    `Low-English individuals` = format(
      dplyr::n_distinct(d$xwaveid[d$origin_group %in% "mnes_loweng"]), big.mark = ","
    )
  )
}

exhibit <- dplyr::bind_rows(
  sample_row("Employee wages", "Employees", "Person-year", "1 to 21", regression1_v1a, data_regression_1),
  sample_row("Owner income and risk", "Restricted pairs (same status)", "Person-pair", "1 to 21", regression2_income, data_regression_2_pairs),
  sample_row("Owner income and risk", "Unrestricted pairs", "Person-pair", "1 to 21", regression2_income_unrestricted, data_regression_2_unrestricted),
  sample_row("Entry into ownership", "Non-owners at t−1", "Person-pair", "1 to 24", regression3_v4a, data_regression_3)
) |>
  tinytable::tt() |>
  tinytable::theme_empty() |>
  tinytable::style_tt(i = 0, line = "tb") |>
  tinytable::style_tt(i = 4, line = "b") |>
  tinytable::style_tt(j = 1:4, align = "l") |>
  tinytable::style_tt(j = 5:7, align = "c")

save_table(exhibit, "table_A_01")
