# ==============================================================================
# Table A.2: Distribution of firm structures observed in the employee sample.
#
# Thesis label: @tbl-firmtype (Appendix A; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Distribution of workplace types in the employee sample.
# Output: project/output/tables/table_A_02.html and table_A_02.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

fmt_n <- function(x) format(x, big.mark = ",", trim = TRUE)
fmt_num <- function(x, d = 2) {
  formatC(x, format = "f", digits = d, big.mark = ",")
}
firm_base <- employee_sample |> filter(wave <= 21L)

exhibit <- tibble::tibble(
  Category = c(
    "Small single-site",
    "Chain-like",
    "Small-site chain",
    "None of the three",
    "Firm structure missing"
  ),
  n = c(
    sum(firm_base$small_single_site == 1L, na.rm = TRUE),
    sum(firm_base$chain_like == 1L, na.rm = TRUE),
    sum(firm_base$small_site_chain == 1L, na.rm = TRUE),
    sum(
      coalesce(firm_base$small_single_site, 0L) +
        coalesce(firm_base$chain_like, 0L) +
        coalesce(firm_base$small_site_chain, 0L) ==
        0L &
        !is.na(firm_base$small_single_site)
    ),
    sum(is.na(firm_base$small_single_site))
  )
) |>
  mutate(
    `Person-years` = fmt_n(n),
    `Share (%)` = fmt_num(100 * n / nrow(firm_base), 1)
  ) |>
  select(-n) |>
  tt(
    width = c(0.40, 0.30, 0.30)
  ) |>
  style_tt(j = 2:3, align = "r") |>
  style_tt(i = 0, bold = TRUE, align = "c") |>
  style_tt(i = 0, line = "b", line_width = 0.05)

save_table(exhibit, "table_A_02")
