# ==============================================================================
# Table B.1: Distribution of the leave-one-out owner shares across employees in the wage sample.
#
# Thesis label: @tbl-loo-shares-b1 (Appendix B; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Distribution of the owner shares under the three assimilation weightings.
# Output: project/output/tables/table_B_01.html and table_B_01.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

summary_data <- data_regression_1 |>
  select(
    loo_mes_1d_type0,
    loo_mnes_higheng_1d_type0,
    loo_mnes_loweng_1d_type0,
    loo_mes_1d_type1,
    loo_mnes_higheng_1d_type1,
    loo_mnes_loweng_1d_type1,
    loo_mes_1d_type2,
    loo_mnes_higheng_1d_type2,
    loo_mnes_loweng_1d_type2
  ) |>
  tidyr::pivot_longer(everything(), names_to = "var", values_to = "v") |>
  filter(!is.na(v)) |>
  tidyr::separate(var, into = c("share_type", "decay_type"), sep = "_1d_") |>
  summarise(
    Mean = mean(v),
    `Std. Dev.` = sd(v),
    `Q1` = quantile(v, 0.25),
    Median = median(v),
    `Q3` = quantile(v, 0.75),
    Max = max(v),
    `% Zero` = 100 * mean(v == 0),
    .by = c(share_type, decay_type)
  ) |>
  mutate(
    share_type = recode_values(
      share_type,
      "loo_mes" ~ "MES owner density",
      "loo_mnes_higheng" ~ "MNES high English owner density",
      "loo_mnes_loweng" ~ "MNES low English owner density"
    ),
    decay_type = recode_values(
      decay_type,
      "type0" ~ "Type 0 (no weighting)",
      "type1" ~ "Type 1 (pooled - same λ applied to all migrants)",
      "type2" ~ "Type 2 (λ is split by migrant origin group)"
    )
  ) |>
  arrange(
    factor(
      decay_type,
      levels = c(
        "Type 0 (no weighting)",
        "Type 1 (pooled - same λ applied to all migrants)",
        "Type 2 (λ is split by migrant origin group)"
      )
    ),
    share_type
  )

exhibit <- bind_rows(
  tibble(
    Density = "Type 0 (no weighting)",
    Mean = NA_real_,
    `Std. Dev.` = NA_real_,
    `Q1` = NA_real_,
    Median = NA_real_,
    `Q3` = NA_real_,
    Max = NA_real_,
    `% Zero` = NA_real_
  ),
  summary_data |>
    filter(decay_type == "Type 0 (no weighting)") |>
    select(
      Density = share_type,
      Mean,
      `Std. Dev.`,
      `Q1`,
      Median,
      `Q3`,
      Max,
      `% Zero`
    ),
  tibble(
    Density = "Type 1 (pooled - same λ applied to all migrants)",
    Mean = NA_real_,
    `Std. Dev.` = NA_real_,
    `Q1` = NA_real_,
    Median = NA_real_,
    `Q3` = NA_real_,
    Max = NA_real_,
    `% Zero` = NA_real_
  ),
  summary_data |>
    filter(decay_type == "Type 1 (pooled - same λ applied to all migrants)") |>
    select(
      Density = share_type,
      Mean,
      `Std. Dev.`,
      `Q1`,
      Median,
      `Q3`,
      Max,
      `% Zero`
    ),
  tibble(
    Density = "Type 2 (λ is split by migrant origin group)",
    Mean = NA_real_,
    `Std. Dev.` = NA_real_,
    `Q1` = NA_real_,
    Median = NA_real_,
    `Q3` = NA_real_,
    Max = NA_real_,
    `% Zero` = NA_real_
  ),
  summary_data |>
    filter(decay_type == "Type 2 (λ is split by migrant origin group)") |>
    select(
      Density = share_type,
      Mean,
      `Std. Dev.`,
      `Q1`,
      Median,
      `Q3`,
      Max,
      `% Zero`
    )
) |>
  mutate(across(2:8, ~ ifelse(is.na(.), NA, sprintf(if (cur_column() == "% Zero") "%.1f" else "%.3f", .)))) |>
  mutate(across(2:8, as.character)) |>
  mutate(across(2:8, ~ replace_na(., ""))) |>
  tt() |>
  style_tt(i = c(1, 5, 9), italic = TRUE) |>
  style_tt(i = 0, line = "tb", bold = FALSE)

save_table(exhibit, "table_B_01")
