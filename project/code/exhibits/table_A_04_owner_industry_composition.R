# ==============================================================================
# Table A.4: Industry composition of business owners (%) by origin and English proficiency group.
#
# Thesis label: @tbl-owner-industry (Appendix A; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Industry composition of business owners by origin and English proficiency group.
# Output: project/output/tables/table_A_04.html and table_A_04.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

owner_base <- panel_final |>
  filter(
    is_owner == 1L,
    !is.na(industry_19categories_jbmi61),
    !is.na(origin_group)
  )

owner_counts <- owner_base |>
  summarise(n_obs = n(), n_persons = n_distinct(xwaveid), .by = origin_group) |>
  pivot_longer(c(n_obs, n_persons), names_to = "Industry", values_to = "v") |>
  mutate(
    Industry = if_else(Industry == "n_obs", "Person-years", "Persons"),
    v = format(v, big.mark = ",", trim = TRUE)
  ) |>
  pivot_wider(names_from = origin_group, values_from = v)

exhibit <- owner_base |>
  mutate(
    industry_19categories_jbmi61 = stringr::str_replace(
      as.character(industry_19categories_jbmi61),
      "^\\w",
      toupper
    )
  ) |>
  summarise(n = n(), .by = c(industry_19categories_jbmi61, origin_group)) |>
  mutate(pct = 100 * n / sum(n), .by = origin_group) |>
  select(-n) |>
  pivot_wider(names_from = origin_group, values_from = pct, values_fill = 0) |>
  rename(Industry = industry_19categories_jbmi61) |>
  arrange(desc(native)) |>
  mutate(across(-Industry, ~ sprintf("%.2f", .x))) |>
  bind_rows(owner_counts) |>
  rename(
    `Native-born` = native,
    MES = mes,
    `High English` = mnes_higheng,
    `Low English` = mnes_loweng
  ) |>
  relocate(Industry, `Native-born`, MES, `High English`, `Low English`) |>
  tt(width = c(0.30, 0.18, 0.12, 0.20, 0.20)) |>
  group_tt(j = list("MNES" = 4:5), bold = FALSE) |>
  style_tt(j = 2:5, align = "c") |>
  style_tt(i = 0, j = 1, bold = FALSE, align = "l") |>
  style_tt(i = 0, j = 2:5, bold = FALSE, align = "c") |>
  style_tt(i = 20:21, italic = FALSE, bold = FALSE) |>
  style_tt(i = 19, line = "b", line_width = 0.05) |>
  style_tt(i = 0, line = "b", line_width = 0.05)

save_table(exhibit, "table_A_04")
