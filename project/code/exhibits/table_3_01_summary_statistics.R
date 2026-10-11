# ==============================================================================
# Table 3.1: Characteristics of business owners by origin and English proficiency group.
#
# Thesis label: @tbl-owner-desc (Chapter 3 (Data), Section 3.6; source chunk in project/thesis/02_data.qmd)
# What it shows: Means and standard deviations of owner characteristics by origin and English proficiency group, with t-tests against the native-born.
# Output: project/output/tables/table_3_01.html and table_3_01.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

owner_base <- panel_final |>
  filter(!is.na(origin_group), is_owner == 1) |>
  mutate(
    origin_group = forcats::fct_recode(
      origin_group,
      "Native-born" = "native",
      "MES" = "mes",
      "MNES (high English)" = "mnes_higheng",
      "MNES (low English)" = "mnes_loweng"
    )
  )

panel_desc_tbl <- owner_base |>
  transmute(
    origin_group,
    `Age` = age_hgage,
    `Female` = as.numeric(sex_hgsex == "female"),
    `Married or de facto` = as.numeric(
      marital_status_mrcurr %in% c("legally married", "de facto")
    ),
    `Post-school qualification` = as.numeric(
      highest_education_achieved_edhigh1 %in%
        c(
          "postgrad (master or doctorate)",
          "grad diploma, grad certificate",
          "bachelor or honours",
          "adv diploma, diploma",
          "certificate iii or iv"
        )
    ),
    `Low English proficiency` = low_english_timeinvariant,
    `Years since arrival in Australia` = years_in_aus,
    `Own-account worker` = own_account_worker,
    `Owner with employees` = owner_with_employees,
    `Market income` = income_labour_and_business,
    `Cashflow problem` = cashflow_problem,
    `Hardship` = hardship,
    `Liquidity constraint` = liquidity_constraint,
    `Home owner` = is_homeowner,
    `SEIFA disadvantage decile` = seifa_decile
  )

stat_vars <- setdiff(names(panel_desc_tbl), "origin_group")
grp_levels <- levels(owner_base$origin_group)
ref_group <- "Native-born"
skip_vars <- c("Low English proficiency", "Years since arrival in Australia")

pvals <- purrr::map_dfr(setdiff(stat_vars, skip_vars), function(v) {
  base <- tibble::tibble(
    xwaveid = owner_base$xwaveid,
    grp = owner_base$origin_group,
    y = panel_desc_tbl[[v]]
  ) |>
    filter(!is.na(y))
  if (nrow(base) == 0 || stats::var(base$y) == 0) {
    return(tibble::tibble(
      variable = character(),
      grp = character(),
      p = numeric()
    ))
  }
  # Each migrant group against native-born owners
  purrr::map_dfr(setdiff(grp_levels, ref_group), function(g) {
    d <- base |>
      filter(grp %in% c(g, ref_group)) |>
      mutate(in_grp = as.integer(grp == g))
    if (dplyr::n_distinct(d$in_grp) < 2) {
      return(tibble::tibble(variable = v, grp = g, p = NA_real_))
    }
    m <- fixest::feols(y ~ in_grp, data = d, cluster = ~xwaveid)
    tibble::tibble(variable = v, grp = g, p = fixest::pvalue(m)[["in_grp"]])
  })
})

stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "\u2217\u2217\u2217",
    p < 0.05 ~ "\u2217\u2217",
    p < 0.10 ~ "\u2217",
    .default = ""
  )
}

fmt_num <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")
fmt_n <- function(x) format(x, big.mark = ",", trim = TRUE)

grp_cells <- purrr::map_dfr(stat_vars, function(v) {
  owner_base |>
    mutate(y = panel_desc_tbl[[v]]) |>
    summarise(
      m = mean(y, na.rm = TRUE),
      s = stats::sd(y, na.rm = TRUE),
      .by = origin_group
    ) |>
    mutate(variable = v)
}) |>
  left_join(pvals, by = c("variable", "origin_group" = "grp")) |>
  mutate(
    Mean = ifelse(is.nan(m), "", paste0(fmt_num(m), stars(p))),
    SD = ifelse(is.nan(s) | is.na(s), "", fmt_num(s))
  ) |>
  select(variable, origin_group, Mean, SD) |>
  tidyr::pivot_wider(
    names_from = origin_group,
    values_from = c(Mean, SD),
    names_glue = "{origin_group}__{.value}"
  )

all_cells <- purrr::map_dfr(stat_vars, function(v) {
  y <- panel_desc_tbl[[v]]
  tibble::tibble(
    variable = v,
    `All__Mean` = ifelse(all(is.na(y)), "", fmt_num(mean(y, na.rm = TRUE))),
    `All__SD` = ifelse(all(is.na(y)), "", fmt_num(stats::sd(y, na.rm = TRUE)))
  )
})

col_order <- c(
  "variable",
  as.vector(rbind(paste0(grp_levels, "__Mean"), paste0(grp_levels, "__SD"))),
  "All__Mean",
  "All__SD"
)

body <- grp_cells |>
  left_join(all_cells, by = "variable") |>
  arrange(match(variable, stat_vars)) |>
  select(all_of(col_order))

grp_counts <- owner_base |>
  summarise(
    obs = n(),
    persons = dplyr::n_distinct(xwaveid),
    .by = origin_group
  ) |>
  arrange(match(origin_group, grp_levels))

count_row <- function(label, vals, total) {
  out <- c(
    label,
    as.vector(rbind(fmt_n(vals), rep("", length(vals)))),
    fmt_n(total),
    ""
  )
  as.list(setNames(out, col_order))
}

n_rows <- bind_rows(
  tibble::as_tibble(count_row(
    "Person-years",
    grp_counts$obs,
    nrow(owner_base)
  )),
  tibble::as_tibble(count_row(
    "Persons",
    grp_counts$persons,
    dplyr::n_distinct(owner_base$xwaveid)
  ))
)

tab <- bind_rows(body, n_rows)
n_stat_rows <- length(stat_vars)
header <- c("", as.vector(rbind(rep("Mean", 4), rep("SD", 4))), "Mean", "SD")

sig <- pvals |>
  filter(!is.na(p), p < 0.10) |>
  mutate(
    i = match(variable, stat_vars),
    j = 2 * match(grp, grp_levels)
  ) |>
  filter(!is.na(i), !is.na(j))

tbl <- tt(setNames(tab, header), width = c(0.20, rep(0.08, 10))) |>
  group_tt(
    j = list(
      "Native-born" = 2:3,
      "MES" = 4:5,
      "MNES (high English)" = 6:7,
      "MNES (low English)" = 8:9,
      "All owners" = 10:11
    )
  ) |>
  style_tt(j = 2:11, align = "r") |>
  style_tt(i = 0, bold = FALSE, align = "c")

for (k in seq_len(nrow(sig))) {
  tbl <- style_tt(tbl, i = sig$i[k], j = sig$j[k], bold = TRUE)
}

exhibit <- tbl |>
  style_tt(i = n_stat_rows, line = "b", line_width = 0.05)

save_table(exhibit, "table_3_01")
