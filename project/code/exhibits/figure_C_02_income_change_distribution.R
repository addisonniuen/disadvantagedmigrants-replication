# ==============================================================================
# Figure C.2: Distribution of year-to-year income changes by ownership status and origin group.
#
# Thesis label: @fig-b2-arc-density (Appendix C; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Distribution of year-to-year income changes by ownership status and origin group.
# Output: project/output/figures/figure_C_02.svg and figure_C_02.png
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

status_pair_of <- c(
  employee    = "stay_employee",
  employer    = "stay_employer",
  own_account = "stay_own_account"
)

plot_arc_density <- function(s) {
  data_regression_2_pairs |>
    filter(status_pair == status_pair_of[[s]]) |>
    mutate(
      arc_pct = 100 * arc,
      origin_group = factor(group_labels[as.character(origin_group)], levels = group_labels)
    ) |>
    ggplot(aes(x = arc_pct, colour = origin_group, linetype = origin_group)) +
    geom_density(adjust = 1.5, linewidth = 0.5) +
    scale_colour_origin() +
    scale_linetype_origin() +
    coord_cartesian(xlim = c(-200, 200)) +
    scale_x_continuous(breaks = seq(-200, 200, 100)) +
    labs(x = "Arc percent change in income, t-1 to t", y = "Density") +
    theme_thesis() +
    guides(colour = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))
}


# The thesis shows the three panels as subfigures (a) to (c) with their own
# captions; the panel titles below stand in for those captions
subcaps <- c("(a) Employees.", "(b) Owners with employees.", "(c) Own-account workers.")
exhibit <- patchwork::wrap_plots(
  Map(function(s, cap) plot_arc_density(s) + ggplot2::labs(title = cap),
      names(status_pair_of), subcaps),
  ncol = 3
)

save_figure(exhibit, "figure_C_02", width = 13, height = 4.5)
