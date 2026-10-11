# ==============================================================================
# Table C.1: Sample cells, raw outcomes and attrition by ownership status, business size and origin group.
#
# Thesis label: @tbl-b2-cells (Appendix C; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Block 2 cell sizes, raw outcomes and attrition by owner type and origin group.
# Output: project/output/tables/table_C_01.html and table_C_01.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

owner_type_labels <- c(
  employee_small = "Employees, small workplace",
  employee_large = "Employees, large workplace",
  employer_small = "Small employers",
  employer_large = "Large employers",
  own_account = "Own-account workers"
)
origin_labels <- c(
  native = "Native-born",
  mes = "MES",
  mnes_higheng = "MNES high-English",
  mnes_loweng = "MNES low-English"
)
pct <- \(x) ifelse(is.na(x), "", formatC(100 * x, format = "f", digits = 1))

b2_cells <- cells_b2 |>
  dplyr::left_join(
    attrition_b2 |> dplyr::select(owner_type, origin_group, share_not_interviewed),
    by = c("owner_type", "origin_group")
  ) |>
  dplyr::mutate(owner_type = factor(owner_type, levels = names(owner_type_labels))) |>
  dplyr::arrange(owner_type, origin_group)

b2_cells_display <- b2_cells |>
  dplyr::transmute(
    Group = origin_labels[as.character(origin_group)],
    Individuals = fmt_int(individuals),
    `Person-pairs` = fmt_int(person_pairs),
    `Mean income` = fmt_int(round(mean_income)),
    `Median income` = fmt_int(round(median_income)),
    `Volatility` = formatC(mean_volatility, format = "f", digits = 1),
    `Large fall (%)` = pct(share_large_fall),
    `Not re-interviewed (%)` = pct(share_not_interviewed)
  )

cell_groups <- b2_cells |>
  dplyr::mutate(row = dplyr::row_number()) |>
  dplyr::summarise(start = min(row), .by = owner_type)

exhibit <- tinytable::tt(b2_cells_display) |>
  tinytable::theme_empty() |>
  tinytable::group_tt(i = as.list(setNames(
    cell_groups$start, owner_type_labels[as.character(cell_groups$owner_type)]
  ))) |>
  tinytable::style_tt(i = 0, line = "tb") |>
  tinytable::style_tt(i = nrow(b2_cells_display) + nrow(cell_groups), line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:8, align = "c")

save_table(exhibit, "table_C_01")
