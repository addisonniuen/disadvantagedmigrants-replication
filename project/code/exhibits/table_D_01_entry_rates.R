# ==============================================================================
# Table D.1: Transitions into business ownership by migrant and language proficiency group.
#
# Thesis label: @tbl-b3-sample (Appendix D; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Unconditional entry rates into business ownership by origin group.
# Output: project/output/tables/table_D_01.html and table_D_01.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

# Variables for the standardisation: age bands and industry of most recent
# job at t-1 (the 19 ANZSIC divisions, i.e. 1-digit industry)
sample_b3 <- data_regression_3 |>
  mutate(
    age_band = cut(
      age_hgage,
      breaks = c(-Inf, 24, 34, 44, 54, 64, Inf),
      labels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+")
    ),
    industry_1d = industry_19categories_jbmi61_fe
  )

# Entry counts by origin group
entry_counts <- sample_b3 |>
  summarise(
    n_atrisk = n(),
    n_entries = sum(entry_owner == 1),
    .by = origin_group
  )

baseline <- entry_counts |> filter(origin_group == "native")

# Fisher exact test against the Australian-born baseline
entry_tests <- entry_counts |>
  mutate(
    rate = 100 * n_entries / n_atrisk,
    p_fisher = purrr::map2_dbl(
      n_entries,
      n_atrisk,
      ~ if (.y == baseline$n_atrisk) {
        NA_real_
      } else {
        fisher.test(matrix(
          c(
            .x,
            .y - .x,
            baseline$n_entries,
            baseline$n_atrisk - baseline$n_entries
          ),
          nrow = 2,
          byrow = TRUE
        ))$p.value
      }
    )
  )

# Homogeneity test across all four groups
chisq_b3 <- chisq.test(
  cbind(entry_counts$n_entries, entry_counts$n_atrisk - entry_counts$n_entries)
)

# Person-clustered bootstrap CI for each group's entry rate
# (resamples individuals, so repeated waves of the same person are respected)
boot_entry_ci <- function(dat, group, R = 2000, seed = 1) {
  set.seed(seed)
  per_person <- dat |>
    filter(origin_group == group) |>
    summarise(waves = n(), entries = sum(entry_owner == 1), .by = xwaveid)
  n_id <- nrow(per_person)
  rates <- replicate(R, {
    i <- sample.int(n_id, n_id, replace = TRUE)
    100 * sum(per_person$entries[i]) / sum(per_person$waves[i])
  })
  tibble::tibble(
    lo = quantile(rates, 0.025),
    hi = quantile(rates, 0.975)
  )
}

entry_ci <- entry_counts |>
  mutate(
    ci = purrr::map(as.character(origin_group), \(g) {
      boot_entry_ci(sample_b3, g)
    })
  ) |>
  tidyr::unnest(ci) |>
  select(origin_group, lo, hi)

# Entry rate standardised to the Australian-born age and industry mix. A logit
# of entry on origin, age band and industry is used, rather than cell-by-cell
# rates, because many origin x age x industry cells hold one or two people.
# Each group's standardised rate is its average predicted entry rate over the
# Australian-born person-pairs.
std_model <- glm(
  entry_owner ~ origin_group + age_band + industry_1d,
  family = binomial(), data = sample_b3
)
native_pairs <- filter(sample_b3, origin_group == "native")
standardised <- tibble::tibble(
  origin_group = factor(levels(sample_b3$origin_group), levels = levels(sample_b3$origin_group)),
  standardised_rate = vapply(levels(sample_b3$origin_group), \(g) {
    nd <- dplyr::mutate(native_pairs, origin_group = factor(g, levels = levels(sample_b3$origin_group)))
    100 * mean(predict(std_model, newdata = nd, type = "response"))
  }, numeric(1))
)

# Assemble the table
b3_group_labels <- c(
  native = "Australian-born",
  mes = "Migrant, main English-speaking country",
  mnes_higheng = "Migrant, other country, high English",
  mnes_loweng = "Migrant, other country, low English"
)

exhibit <- entry_tests |>
  left_join(entry_ci, by = "origin_group") |>
  left_join(standardised, by = "origin_group") |>
  mutate(origin_group = factor(origin_group, levels = names(b3_group_labels))) |>
  arrange(origin_group) |>
  transmute(
    Group = b3_group_labels[as.character(origin_group)],
    `At-risk person-pairs` = format(n_atrisk, big.mark = ","),
    Entries = format(n_entries, big.mark = ","),
    `Entry rate (%)` = sprintf("%.2f", rate),
    `95% CI` = sprintf("[%.2f, %.2f]", lo, hi),
    `p vs Australian-born` = if_else(
      is.na(p_fisher),
      "",
      fmt_p(p_fisher)
    ),
    `Standardised rate (%)` = sprintf("%.2f", standardised_rate)
  ) |>
  tinytable::tt() |>
  tinytable::theme_empty() |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:7, align = "c") |>
  tinytable::style_tt(i = 0, line = "tb") |>
  tinytable::style_tt(i = 4, line = "b")

save_table(exhibit, "table_D_01")
