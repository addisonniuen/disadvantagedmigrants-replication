# ==============================================================================
# Table 5.3: Difference between each migrant group's owner–employee gap and the native-born gap, for small employers.
#
# Thesis label: @tbl-b2-premium (Chapter 5 (Results), Section 5.2; source chunk in project/thesis/04_results.qmd)
# What it shows: Difference between each migrant group's owner-employee gap and the native-born gap, and its minimum detectable difference.
# Output: project/output/tables/table_5_03.html and table_5_03.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

# --- Objects shared with tbl-b2-main (defined there in the thesis) ---------------

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

# --- Table 5.3 --------------------------------------------------------------------

jt <- joint_tests |>
  dplyr::mutate(p = purrr::map_chr(p.value, fmt_p))
jt_p <- c(
  jt$p[jt$sample == "unrestricted"][match(c("income_t", "abs_arc_pct", "large_fall"), jt$outcome[jt$sample == "unrestricted"])],
  jt$p[jt$sample == "restricted"][match(c("income_t", "abs_arc_pct", "large_fall"), jt$outcome[jt$sample == "restricted"])]
)
b2_joint <- tibble::tibble(Group = "Joint test of all interactions (p-value)", !!!setNames(as.list(jt_p), names(b2_columns)))

exhibit <- dplyr::bind_rows(
  dplyr::select(b2_small$diff, -n_owners),
  dplyr::select(b2_small$mde, -n_owners),
  b2_joint,
  dplyr::select(b2_footer, -n_owners)
) |>
  dplyr::rename("Dependent variable:" = Group) |>
  tinytable::tt(width = c(0.22, rep(0.115, 6))) |>
  tinytable::theme_empty() |>
  tinytable::group_tt(
    i = list(
      "Panel A: Difference from the native-born owner–employee gap" = 1,
      "Panel B: Minimum detectable difference" = 7
    ),
    j = b2_sample_header
  ) |>
  tinytable::style_tt(i = -1, line = "t") |>
  tinytable::style_tt(i = 0, line = "b") |>
  tinytable::style_tt(i = c(1, 8), italic = TRUE) |>
  tinytable::style_tt(i = c(11, 14), line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:7, align = "c") |>
  pair_se_rows(c(1, 7))

save_table(exhibit, "table_5_03")
