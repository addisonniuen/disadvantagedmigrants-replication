# ==============================================================================
# Table A.3: Distribution of the leave-one-out ownership exposure shares across industry-year (waves 1-21).
#
# Thesis label: @tbl-share-dist (Appendix A; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Distribution of the leave-one-out owner shares across industry-years.
# Output: project/output/tables/table_A_03.html and table_A_03.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

SHARE_SUFFIX <- "1d_type0"
fmt_n <- function(x) format(x, big.mark = ",", trim = TRUE)
fmt_num <- function(x, d = 2) {
  formatC(x, format = "f", digits = d, big.mark = ",")
}
sv <- function(stem) paste0(stem, "_", SHARE_SUFFIX)

cell_shares <- panel_final |>
  filter(!is.na(industry_19categories_jbmi61), !wave %in% 22:24) |>
  summarise(
    across(
      all_of(c(sv("loo_mes"), sv("loo_mnes_higheng"), sv("loo_mnes_loweng"))),
      ~ first(.x[!is.na(.x)])
    ),
    n_owners = first(.data[[sv("n_owners")]]),
    .by = c(industry_19categories_jbmi61, wave)
  )

owner_group_map <- c(
  "MES" = "mes",
  "MNES (high English proficiency)" = "mnes_higheng",
  "MNES (low English proficiency)" = "mnes_loweng"
)

owner_counts <- purrr::imap_dfr(owner_group_map, function(grp, lab) {
  owners_grp <- panel_final |>
    filter(
      is_owner == 1L,
      origin_group == grp,
      !is.na(industry_19categories_jbmi61),
      !wave %in% 22:24
    )
  tibble::tibble(
    Share = lab,
    Owners = n_distinct(owners_grp$xwaveid)
  )
})

share_map <- c(
  "MES" = sv("loo_mes"),
  "MNES (high English proficiency)" = sv("loo_mnes_higheng"),
  "MNES (low English proficiency)" = sv("loo_mnes_loweng")
)

exhibit <- purrr::imap_dfr(share_map, function(v, lab) {
  x <- cell_shares[[v]]
  x <- x[!is.na(x)]
  tibble::tibble(
    Share = lab,
    `Industry-year (N)` = fmt_n(length(x)),
    `Zero (%)` = fmt_num(100 * mean(x == 0), 3),
    Mean = fmt_num(mean(x), 3),
    SD = fmt_num(sd(x), 3),
    Median = fmt_num(median(x), 3),
    Max = fmt_num(max(x), 3)
  )
}) |>
  left_join(owner_counts, by = "Share") |>
  relocate(Share, Owners, `Industry-year (N)`) |>
  mutate(Owners = fmt_n(Owners)) |>
  tt(
    width = c(0.30, 0.10, 0.12, 0.10, 0.10, 0.09, 0.09, 0.10)
  ) |>
  style_tt(j = 1, align = "l") |>
  style_tt(j = 2:8, align = "r") |>
  style_tt(i = 0, align = "c") |>
  style_tt(i = 0, line = "b", line_width = 0.05)

save_table(exhibit, "table_A_03")
