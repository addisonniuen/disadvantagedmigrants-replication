# ==============================================================================
# Figure 5.1: Wage gaps of each workplace type relative to other large workplaces in the same industry and year, as the share of non-English-speaking migrant owners rises.
#
# Thesis label: @fig-b1-placebo (Chapter 5 (Results), Section 5.1; source chunk in project/thesis/04_results.qmd)
# What it shows: Predicted wage gap of each workplace type relative to other large workplaces as the plain share of MNES owners rises (placebo model).
# Output: project/output/figures/figure_5_01.svg and figure_5_01.png
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

# Predicted gap of each workplace type w relative to the reference category
# (other large workplaces), beta_w + beta_wg * S, from the placebo model with
# plain owner shares (Type 0, no assimilation weighting), as the owner share S
# of group g rises (other shares at zero).
b1_type_curve <- function(m, site, share, group) {
  b <- coef(m)
  V <- vcov(m)
  inter <- paste0(site, ":", share)
  smax <- quantile(data_regression_1[[share]], 0.95, na.rm = TRUE)
  tibble::tibble(s = seq(0, smax, length.out = 50)) |>
    dplyr::mutate(
      est = b[[site]] + b[[inter]] * s,
      se = sqrt(V[site, site] + s^2 * V[inter, inter] + 2 * s * V[site, inter]),
      site = workplace_labels[[site]],
      group = group
    )
}

exhibit <- tidyr::expand_grid(
  site = names(workplace_labels),
  share = c("loo_mnes_higheng_1d_type0", "loo_mnes_loweng_1d_type0")
) |>
  purrr::pmap_dfr(\(site, share) b1_type_curve(
    regression1_placebo$type0, site, share,
    if (grepl("higheng", share)) group_labels[["mnes_higheng"]] else group_labels[["mnes_loweng"]]
  )) |>
  dplyr::mutate(
    lo = est - 1.96 * se,
    hi = est + 1.96 * se,
    dplyr::across(c(est, lo, hi), \(x) 100 * (exp(x) - 1)),
    site = factor(site, levels = workplace_labels),
    panel = paste("Share of", group, "owners")
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = 100 * s, y = est, colour = site, fill = site, linetype = site)) +
  ggplot2::geom_hline(yintercept = 0, colour = "grey40") +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::scale_colour_manual(values = workplace_colours) +
  ggplot2::scale_fill_manual(values = workplace_colours) +
  ggplot2::scale_linetype_manual(values = workplace_linetypes) +
  ggplot2::facet_wrap(~panel, scales = "free_x") +
  ggplot2::labs(
    x = "Share of business owners in the industry-year (%)",
    y = "Wage relative to other large\nworkplaces (%)"
  ) +
  theme_thesis()

save_figure(exhibit, "figure_5_01", width = 7, height = 3.8)
