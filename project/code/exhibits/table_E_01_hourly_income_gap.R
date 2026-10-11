# ==============================================================================
# Table E.1: Median hourly income gap between employees and owners with employees, by industry.
#
# Thesis label: @tbl-gap-industry (Appendix E; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Median hourly income gap between employees and owners with employees, by industry.
# Output: project/output/tables/table_E_01.html and table_E_01.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

# --- Objects shared with gap-data (defined there in the thesis) ---------------

# Run this chunk before tbl-gap-industry.
# Rebuilds the industry-year gap data from panel_final (waves 1 to 21).
gap_cells <- panel_final |>
  dplyr::filter(as.integer(as.character(wave)) <= 21) |>
  dplyr::mutate(
    year_num = as.integer(as.character(year)),
    industry = stringr::str_to_sentence(as.character(industry_19categories_jbmi61)),
    hourly_income = dplyr::if_else(
      hours_weekly_all_job_jbhruc > 0,
      income_labour_and_business / (52 * hours_weekly_all_job_jbhruc),
      NA_real_
    ),
    is_small_employee = is_employee %in% 1 & small_single_site %in% 1,
    is_small_owner = owner_with_employees %in% 1 & dplyr::coalesce(small_wp == 1, FALSE) & !is.na(hourly_income)
  ) |>
  dplyr::filter(!is.na(industry))

gap_kt <- gap_cells |>
  dplyr::summarise(
    wage_employee = median(hrwge_main_clean[is_small_employee], na.rm = TRUE),
    income_owner = median(hourly_income[is_small_owner], na.rm = TRUE),
    n_owner = sum(is_small_owner),
    share_migrant = mean(migrant[is_small_owner] %in% 1),
    .by = c(industry, year_num)
  ) |>
  dplyr::filter(n_owner >= 10, wage_employee > 0, income_owner > 0) |>
  dplyr::mutate(gap_hourly = log(wage_employee) - log(income_owner))

gap_by_industry <- gap_kt |>
  dplyr::summarise(
    n_years = dplyr::n(),
    gap = stats::weighted.mean(gap_hourly, n_owner),
    se = sqrt(sum(n_owner^2 * (gap_hourly - gap)^2)) / sum(n_owner),
    share_migrant = stats::weighted.mean(share_migrant, n_owner),
    .by = industry
  ) |>
  dplyr::filter(n_years >= 3)

time_industries <- gap_kt |>
  dplyr::count(industry) |>
  dplyr::filter(n >= 10) |>
  dplyr::pull(industry)

# Checks against the numbers quoted in the text
nrow(gap_kt)                                          # 273 industry-years
stats::weighted.mean(gap_kt$gap_hourly, gap_kt$n_owner)  # about -0.085
mean(gap_kt$gap_hourly > 0)                           # about 0.37

# --- Table D.4 --------------------------------------------------------------------

fmt3 <- \(x) formatC(x, format = "f", digits = 3)

gap_by_year <- gap_kt |>
  dplyr::summarise(
    n_years = dplyr::n(),
    share_positive = mean(gap_hourly > 0),
    .by = industry
  )

gap_industries <- gap_by_industry |>
  dplyr::select(industry, gap, se, share_migrant) |>
  dplyr::left_join(gap_by_year, by = "industry") |>
  dplyr::arrange(dplyr::desc(gap))

gap_overall <- tibble::tibble(
  industry = "All industry-years",
  gap = stats::weighted.mean(gap_kt$gap_hourly, gap_kt$n_owner),
  se = NA_real_,
  share_migrant = stats::weighted.mean(gap_kt$share_migrant, gap_kt$n_owner),
  n_years = nrow(gap_kt),
  share_positive = mean(gap_kt$gap_hourly > 0)
)

# One row per industry, with the standard error of its mean gap on the row
# below; the pooled row has no standard error
gap_row <- \(d) tibble::tibble(
  Industry = c(d$industry, if (!is.na(d$se)) ""),
  `Industry-years` = c(as.character(d$n_years), if (!is.na(d$se)) ""),
  `Mean gap (log points)` = c(fmt3(d$gap), if (!is.na(d$se)) paste0("(", fmt3(d$se), ")")),
  `Years with positive gap` = c(scales::percent(d$share_positive, accuracy = 1), if (!is.na(d$se)) ""),
  `Migrant share of owners` = c(scales::percent(d$share_migrant, accuracy = 1), if (!is.na(d$se)) "")
)
gap_display <- dplyr::bind_rows(gap_industries, gap_overall) |>
  split(seq_len(nrow(gap_industries) + 1)) |>
  purrr::map_dfr(gap_row)

exhibit <- tinytable::tt(gap_display) |>
  tinytable::theme_empty() |>
  tinytable::style_tt(i = 0, line = "tb") |>
  tinytable::style_tt(i = 2 * nrow(gap_industries), line = "b") |>
  tinytable::style_tt(i = nrow(gap_display), line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:5, align = "c") |>
  pair_se_rows()

save_table(exhibit, "table_E_01")
