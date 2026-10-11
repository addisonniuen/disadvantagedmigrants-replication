# ==============================================================================
# Table A.6: Panel persistence of all respondents by origin group.
#
# Thesis label: @tbl-panel (Appendix A; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Panel persistence of all respondents by origin group.
# Output: project/output/tables/table_A_06.html and table_A_06.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

exhibit <- # Waves in which each person is interviewed, for every respondent (owners and
# non-owners), since panel persistence affects every sample that needs
# consecutive waves
panel_final |>
  filter(!is.na(origin_group), !is.na(lf_status_detail_esdtl)) |>
  summarise(
    n_waves = dplyr::n_distinct(wave),
    adjacent = max(as.integer(wave)) - min(as.integer(wave)) + 1L == dplyr::n_distinct(wave),
    .by = c(xwaveid, origin_group)
  ) |>
  summarise(
    Persons = n(),
    `Mean waves` = mean(n_waves),
    `Median waves` = median(n_waves),
    `1 wave (%)` = 100 * mean(n_waves == 1),
    `10+ waves (%)` = 100 * mean(n_waves >= 10),
    `Adjacent (%)` = 100 * mean(adjacent),
    `All waves (%)` = 100 * mean(n_waves == 24),
    .by = origin_group
  ) |>
  rename(Group = origin_group) |>
  mutate(
    Group = recode_values(
      Group,
      "native" ~ "Native-born",
      "mes" ~ "MES",
      "mnes_higheng" ~ "MNES (high English)",
      "mnes_loweng" ~ "MNES (low English)"
    )
  ) |>
  arrange(match(
    Group,
    c("Native-born", "MES", "MNES (high English)", "MNES (low English)")
  )) |>
  tt(
    width = c(0.19, 0.10, 0.12, 0.13, 0.10, 0.12, 0.12, 0.12)
  ) |>
  format_tt(j = 2, digits = 0, num_fmt = "decimal", num_mark_big = ",") |>
  format_tt(j = 3:4, digits = 2, num_fmt = "decimal") |>
  format_tt(j = 5:8, digits = 1, num_fmt = "decimal") |>
  style_tt(j = 2:8, align = "r") |>
  style_tt(j = 1, bold = FALSE, align = "l") |>
  style_tt(i = 0, line = "b", line_width = 0.05)

save_table(exhibit, "table_A_06")
