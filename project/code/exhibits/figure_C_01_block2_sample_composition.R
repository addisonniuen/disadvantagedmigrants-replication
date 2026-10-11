# ==============================================================================
# Figure C.1: Composition of the estimation sample by origin group.
#
# Thesis label: @fig-b2-distribution (Appendix C; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Composition of the Block 2 sample by year and of business owners by industry, by origin group.
# Output: project/output/figures/figure_C_01.svg and figure_C_01.png
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

p_year <- data_regression_2_pairs |>
  dplyr::mutate(
    origin_group = origin_recode(origin_group),
    year = as.numeric(as.character(year))
  ) |>
  dplyr::count(year, origin_group) |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = n, fill = origin_group)) +
  ggplot2::geom_col(width = 0.8) +
  scale_fill_origin() +
  ggplot2::scale_x_continuous(breaks = seq(2002, 2021, 4), expand = c(0, 0)) +
  ggplot2::scale_y_continuous(
    labels = scales::label_comma(),
    expand = ggplot2::expansion(mult = c(0, 0.05))
  ) +
  ggplot2::labs(x = "Year", y = "Person-pairs") +
  theme_thesis()

owner_industry <- data_regression_2_pairs |>
  dplyr::filter(status_pair != "stay_employee") |>
  dplyr::mutate(
    origin_group = origin_recode(origin_group),
    industry = stringr::str_to_sentence(as.character(industry_pair))
  ) |>
  dplyr::count(industry, origin_group) |>
  dplyr::mutate(share = n / sum(n), .by = industry)

migrant_share <- owner_industry |>
  dplyr::filter(origin_group != "Native-born") |>
  dplyr::summarise(migrant_share = sum(share), .by = industry)

p_industry <- owner_industry |>
  dplyr::left_join(migrant_share, by = "industry") |>
  ggplot2::ggplot(ggplot2::aes(
    x = share,
    y = reorder(industry, migrant_share),
    fill = origin_group
  )) +
  ggplot2::geom_col(width = 0.75) +
  scale_fill_origin() +
  ggplot2::scale_x_continuous(
    labels = scales::label_percent(accuracy = 1),
    expand = c(0, 0),
    breaks = seq(0, 1, 0.25)
  ) +
  ggplot2::labs(x = "Share of business owners", y = NULL) +
  theme_thesis_h()

# The thesis shows the two panels as subfigures (a) and (b) with their own
# captions; the panel titles below stand in for those captions
exhibit <- patchwork::wrap_plots(
  p_year + ggplot2::labs(title = "(a) Person-pairs by year."),
  p_industry + ggplot2::labs(title = "(b) Business owners by industry."),
  ncol = 2
)

save_figure(exhibit, "figure_C_01", width = 12, height = 5)
