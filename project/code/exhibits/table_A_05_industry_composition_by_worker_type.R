# ==============================================================================
# Table A.5: Industry composition by worker type.
#
# Thesis label: @tbl-industry-composition (Appendix A; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Industry composition of employees, employers and own-account workers.
# Output: project/output/tables/table_A_05.html and table_A_05.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

ind_base <- panel_final |>
  filter(!is.na(industry_19categories_jbmi61)) |>
  mutate(
    industry_19categories_jbmi61 = stringr::str_replace(
      as.character(industry_19categories_jbmi61),
      "^\\w",
      toupper
    ),
    Group = case_when(
      owner_with_employees == 1L ~ "Self-employed worker with employees",
      own_account_worker == 1L ~ "Own-account worker",
      is_employee == 1L ~ "Employees",
      .default = NA_character_
    )
  ) |>
  filter(!is.na(Group))

ind_counts <- ind_base |>
  summarise(n_obs = n(), n_persons = n_distinct(xwaveid), .by = Group) |>
  pivot_longer(c(n_obs, n_persons), names_to = "Industry", values_to = "v") |>
  mutate(
    Industry = if_else(Industry == "n_obs", "Person-years", "Persons"),
    v = format(v, big.mark = ",", trim = TRUE)
  ) |>
  pivot_wider(names_from = Group, values_from = v)

exhibit <- ind_base |>
  summarise(n = n(), .by = c(industry_19categories_jbmi61, Group)) |>
  mutate(pct = 100 * n / sum(n), .by = Group) |>
  select(-n) |>
  pivot_wider(names_from = Group, values_from = pct, values_fill = 0) |>
  rename(Industry = industry_19categories_jbmi61) |>
  arrange(desc(`Self-employed worker with employees`)) |>
  mutate(across(-Industry, ~ sprintf("%.1f", .x))) |>
  bind_rows(ind_counts) |>
  relocate(
    Industry,
    Employees,
    `Self-employed worker with employees`,
    `Own-account worker`
  ) |>
  tt(width = c(0.32, 0.13, 0.35, 0.20)) |>
  style_tt(j = 2:4, align = "c") |>
  style_tt(i = 0, j = 1, bold = FALSE, align = "l") |>
  style_tt(i = 0, j = 2:4, bold = FALSE, align = "r") |>
  style_tt(i = 20:21, italic = FALSE, bold = FALSE) |>
  style_tt(i = 19, line = "b", line_width = 0.1) |>
  style_tt(i = 0, line = "b", line_width = 0.05)

save_table(exhibit, "table_A_05")
