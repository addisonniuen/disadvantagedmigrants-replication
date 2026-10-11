# ==============================================================================
# thesis_format.R
#
# Everything that formats the thesis output, in one place. It is sourced by the
# setup chunks of 04_results.qmd and 07_appendix.qmd, and by
# project/code/exhibits/exhibit_helpers.R so the exhibit files match the PDF.
# It:
#   - Sets the colour scheme, shapes, line types and theme of every figure
#   - Formats numbers in tables and in the text
#   - Provides helpers for the regression tables (observation and individual
#     rows, standard errors kept under their coefficients)
#   - Builds the owner income and risk tables from the contrasts computed in
#     project/code/06_analysisblock2.R
#
# Nothing here changes an estimate; it only changes how results are shown.
# ==============================================================================

# --- Figures ------------------------------------------------------------------

# Colours are the Okabe-Ito palette, which stays distinguishable for readers
# with colour-vision deficiencies. Every figure that separates groups by colour
# also separates them by point shape or line type, so it still reads when
# printed in greyscale.

okabe_ito <- c(
  orange = "#E69F00",
  sky_blue = "#56B4E9",
  green = "#009E73",
  yellow = "#F0E442",
  blue = "#0072B2",
  vermillion = "#D55E00",
  purple = "#CC79A7",
  black = "#000000"
)

# Origin groups: the same colour, shape and line type in every figure.
# Low-English is vermillion so it stands out; yellow is avoided because it is
# hard to see on white.
group_labels <- c(
  native = "Native-born",
  mes = "MES",
  mnes_higheng = "MNES high-English",
  mnes_loweng = "MNES low-English"
)
origin_colours <- setNames(unname(okabe_ito[c("blue", "sky_blue", "green", "vermillion")]), group_labels)
origin_shapes <- setNames(c(16, 17, 15, 18), group_labels)
origin_linetypes <- setNames(c("solid", "dashed", "dotdash", "longdash"), group_labels)

# Block 2 samples: black and orange, which differ strongly in greyscale and
# do not reuse an origin-group colour
sample_colours <- c("Unrestricted" = okabe_ito[["black"]], "Restricted" = okabe_ito[["orange"]])
sample_shapes <- c("Unrestricted" = 16, "Restricted" = 17)

# Workplace types (Block 1). These do not reuse the origin-group colours.
workplace_labels <- c(
  small_single_site = "Small single-site",
  small_site_chain = "Small-site chain",
  chain_like = "Chain-like"
)
workplace_colours <- setNames(unname(okabe_ito[c("black", "orange", "purple")]), workplace_labels)
workplace_linetypes <- setNames(c("solid", "dashed", "dotdash"), workplace_labels)

# Owner types (stacked bar charts)
status_labels <- c(
  employee = "Employee",
  employer = "Owner with employees",
  own_account = "Own-account worker"
)

origin_recode <- function(x) factor(group_labels[as.character(x)], levels = group_labels)

scale_colour_origin <- function(...) ggplot2::scale_colour_manual(values = origin_colours, ...)
scale_fill_origin <- function(...) ggplot2::scale_fill_manual(values = origin_colours, ...)
scale_shape_origin <- function(...) ggplot2::scale_shape_manual(values = origin_shapes, ...)
scale_linetype_origin <- function(...) ggplot2::scale_linetype_manual(values = origin_linetypes, ...)

# Theme: minimal, serif text (set in Libertinus by Typst, see below), light
# vertical gridlines for horizontal estimate plots.
theme_thesis <- function(base_size = 10) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "serif") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "grey88", linewidth = 0.3),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(4, 8, 4, 4)
    )
}

# Figures are written as SVG with svglite so their labels stay real text, which
# Typst then sets in Libertinus Serif to match the body. R cannot see Typst's
# built-in copy of Libertinus, so Times New Roman (installed with macOS) is used
# only to measure text; fix_text_size = FALSE stops those measurements from
# stretching the labels.
serif_files <- file.path(
  "/System/Library/Fonts/Supplemental",
  c("Times New Roman.ttf", "Times New Roman Bold.ttf")
)
knitr::opts_chunk$set(
  dev = "svglite",
  dev.args = list(
    fix_text_size = FALSE,
    user_fonts = list(serif = list(
      plain = list(alias = "Libertinus Serif", file = serif_files[1]),
      bold = list(alias = "Libertinus Serif", file = serif_files[2]),
      italic = list(alias = "Libertinus Serif", file = serif_files[1]),
      bolditalic = list(alias = "Libertinus Serif", file = serif_files[2])
    ))
  )
)

# Horizontal bar and estimate plots: gridlines along the value axis only
theme_thesis_h <- function(base_size = 10) {
  theme_thesis(base_size) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

# --- Numbers in tables and text -----------------------------------------------

fmt_int <- function(x) formatC(x, format = "d", big.mark = ",")
fmt_num <- function(x, d = 2) formatC(x, format = "f", digits = d, big.mark = ",")
fmt_n <- function(x) format(x, big.mark = ",", trim = TRUE)
fmt_p <- function(p) ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p))
# p-value with its relation sign, for inline text: "< 0.001" or "= 0.023"
fmt_p_eq <- function(p) ifelse(p < 0.001, "< 0.001", sprintf("= %.3f", p))

# --- Regression tables --------------------------------------------------------

# Significance stars used in every table: * p < 0.10, ** p < 0.05, *** p < 0.01.
# Unicode asterisks, since plain asterisks clash with Typst markup.
thesis_stars <- c("\u2217" = 0.10, "\u2217\u2217" = 0.05, "\u2217\u2217\u2217" = 0.01)

# Stars for a vector of p-values, matching thesis_stars
add_stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "\u2217\u2217\u2217",
    p < 0.05 ~ "\u2217\u2217",
    p < 0.10 ~ "\u2217",
    .default = ""
  )
}

# Number of individuals in a model's estimation sample, for the regression tables
n_individuals_model <- function(m) {
  d <- eval(m$call$data, envir = globalenv())
  dplyr::n_distinct(d$xwaveid[fixest::obs(m)])
}

# In tables with standard errors (or confidence intervals, in brackets) on their
# own row, make each row label span the coefficient row and the row below it,
# so a label that wraps does not push a coefficient and its standard error
# apart. group_starts are the row positions given to group_tt(i = ...).
pair_se_rows <- function(x, group_starts = integer(0)) {
  d <- x@data
  is_se <- vapply(seq_len(nrow(d)), \(r) {
    d[r, 1] == "" && any(grepl("^[\\(\\[]", unlist(d[r, -1])))
  }, logical(1))
  coef_rows <- which(c(is_se[-1], FALSE) & d[[1]] != "")
  rows <- coef_rows + vapply(coef_rows, \(r) sum(group_starts <= r), numeric(1))
  tinytable::style_tt(x, i = rows, j = 1, rowspan = 2, alignv = "t")
}

# Control coefficients of the employee wage regressions, by group, so that every
# appendix table of these regressions reports the full results
b1_control_groups <- list(
  "Demographics" = c(
    "age_hgage" = "Age",
    "sex_hgsexfemale" = "Female",
    "origin_groupmes" = "MES worker",
    "origin_groupmnes_higheng" = "MNES high-English worker",
    "origin_groupmnes_loweng" = "MNES low-English worker",
    "seifa_decile" = "SEIFA decile"
  ),
  "Occupation" = c(
    "occupation_1dig_jbmo61professionals" = "Professionals",
    "occupation_1dig_jbmo61technicians and trades workers" = "Technicians and trades",
    "occupation_1dig_jbmo61community and personal service workers" = "Community and personal service",
    "occupation_1dig_jbmo61clerical and administrative workers" = "Clerical and administrative",
    "occupation_1dig_jbmo61sales workers" = "Sales",
    "occupation_1dig_jbmo61machinery operators and drivers" = "Machinery operators and drivers",
    "occupation_1dig_jbmo61labourers" = "Labourers"
  ),
  "Education" = c(
    "highest_education_achieved_edhigh1postgrad (master or doctorate)" = "Postgraduate",
    "highest_education_achieved_edhigh1grad diploma, grad certificate" = "Graduate diploma or certificate",
    "highest_education_achieved_edhigh1bachelor or honours" = "Bachelor or honours",
    "highest_education_achieved_edhigh1adv diploma, diploma" = "Advanced diploma or diploma",
    "highest_education_achieved_edhigh1certificate iii or iv" = "Certificate III or IV",
    "highest_education_achieved_edhigh1year 12" = "Year 12"
  ),
  "Job characteristics" = c(
    "casual_perm_jbcasabpermanent" = "Permanent (vs casual)",
    "tenure_current_occ_jbocct" = "Occupational tenure (years)",
    "tenure_current_employer_jbempt" = "Employer tenure (years)",
    "union_employeeassociation_membership_jbmtueayes" = "Union member"
  )
)
b1_controls <- unlist(unname(b1_control_groups))

# Row positions of a regression table with each coefficient on one row and its
# standard error on the row below. `sizes` is the number of coefficients in each
# group, in order. Returns the group_tt positions (`groups`, also as the vector
# `starts` for pair_se_rows()), the rows of the group headers (for italics) and
# the last standard-error row (for the rule under the coefficients).
coef_se_layout <- function(sizes) {
  starts <- cumsum(c(1, head(2 * sizes, -1)))
  list(
    groups = as.list(setNames(starts, names(sizes))),
    starts = unname(starts),
    headers = unname(starts) + seq_along(starts) - 1,
    coef_end = 2 * sum(sizes) + length(sizes)
  )
}

# Row layout of a full employee wage table. `focus` gives the number of
# coefficients in each group shown before the controls.
b1_full_layout <- function(focus) {
  coef_se_layout(c(focus, lengths(b1_control_groups)))
}

# Coefficient labels of the entry regressions (Appendix D), by panel
cm3_origin <- c(
  "origin_groupmes" = "Migrant with English-speaking origin",
  "origin_groupmnes_higheng" = "Migrant from other origin with high English proficiency",
  "origin_groupmnes_loweng" = "Migrant from other origin with low English proficiency"
)

cm3_labour <- c(
  "unemployed_l1" = "Unemployed",
  "nilf_l1" = "Not in labour force",
  "low_wage_l1" = "Bottom wage quintile",
  "liquidity_l1" = "Liquidity constrained",
  "liquidity_miss_l1" = "Liquidity measure missing"
)

cm3_human <- c(
  "highest_education_achieved_edhigh1_l1postgrad (master or doctorate)" = "Postgraduate",
  "highest_education_achieved_edhigh1_l1grad diploma, grad certificate" = "Graduate diploma or certificate",
  "highest_education_achieved_edhigh1_l1bachelor or honours" = "Bachelor or honours",
  "highest_education_achieved_edhigh1_l1adv diploma, diploma" = "Advanced diploma or diploma",
  "highest_education_achieved_edhigh1_l1certificate iii or iv" = "Certificate III or IV",
  "highest_education_achieved_edhigh1_l1year 12" = "Year 12",
  "tenure_emp_yrs_l1" = "Tenure with current employer (years)"
)

cm3_demog <- c(
  "age_hgage" = "Age",
  "I(age_hgage^2)" = "Age squared",
  "I(I(age_hgage^2))" = "Age squared",
  "sex_hgsexfemale" = "Female",
  "years_in_aus_ctrl" = "Years since arrival in Australia",
  "years_in_aus_miss" = "Years since arrival missing",
  "marital_status_mrcurr_l1de facto" = "De facto",
  "marital_status_mrcurr_l1separated" = "Separated",
  "marital_status_mrcurr_l1divorced" = "Divorced",
  "marital_status_mrcurr_l1widowed" = "Widowed",
  "marital_status_mrcurr_l1never married and not de facto" = "Never married and not de facto"
)

cm3_house <- c(
  "homeowner_l1" = "Homeowner",
  "seifa_decile_l1" = "SEIFA disadvantage decile",
  "remoteness_hhs3ra_l1inner regional australia" = "Inner regional Australia",
  "remoteness_hhs3ra_l1outer regional australia" = "Outer regional Australia",
  "remoteness_hhs3ra_l1remote australia" = "Remote Australia",
  "remoteness_hhs3ra_l1very remote australia" = "Very remote Australia"
)

# Full coefficient map for a set of models. `panels` is a named list of named
# label vectors; only terms that some model estimates are kept, in panel order.
# Returns the map and the row positions of coef_se_layout(). Warns if a model
# has a coefficient without a label, so that the appendix tables always report
# the full results.
full_coef_layout <- function(models, panels) {
  present <- unique(unlist(lapply(models, \(m) names(coef(m)))))
  panels <- lapply(panels, \(p) p[names(p) %in% present])
  panels <- panels[lengths(panels) > 0]
  map <- unlist(unname(panels))
  unmapped <- setdiff(present, names(map))
  if (length(unmapped) > 0) {
    warning("Coefficients without a label: ", paste(unmapped, collapse = ", "))
  }
  rows <- vapply(panels, \(p) length(unique(p)), integer(1))
  c(list(map = map), coef_se_layout(rows))
}

# F-statistic of a cluster_f_test() result with its degrees of freedom
f_df <- function(test) sprintf("%.3f (%d, %d)", test[2, "F"], test[2, "Df"], test[2, "Res.Df"])

# modelsummary gof_map with a thousands separator on the observation count
gof_comma <- function(g) {
  lapply(seq_len(nrow(g)), \(i) list(
    raw = g$raw[i],
    clean = g$clean[i],
    fmt = if (g$raw[i] == "nobs") \(x) formatC(x, format = "d", big.mark = ",") else g$fmt[i]
  ))
}

# --- Owner income and risk tables ---------------------------------------------

# One row per origin group and quantity, for one owner type and a set of
# (outcome, sample) columns. Each estimate (with its stars) is followed by a row
# with its standard error in parentheses.
format_b2_panel <- function(contrasts, term, columns, owner_sample = "restricted") {
  fmt <- function(x, outcome) {
    if (outcome == "income_t") {
      formatC(x, format = "f", digits = 0, big.mark = ",")
    } else {
      formatC(x * if (outcome == "abs_arc_pct") 1 else 100, format = "f", digits = 1)
    }
  }
  cell <- function(r, what) {
    if (nrow(r) == 0 || is.na(r$estimate)) return(if (what == "se") "" else "\u2013")
    switch(what,
      est = paste0(fmt(r$estimate, r$outcome), add_stars(r$p.value)),
      se = paste0("(", fmt(r$se, r$outcome), ")"),
      mde = fmt(r$mde, r$outcome)
    )
  }
  groups <- c("native", "mes", "mnes_higheng", "mnes_loweng")
  rows <- dplyr::bind_rows(
    tidyr::expand_grid(group = groups, what = c("est", "se")) |>
      dplyr::mutate(row = "gap", comparison = "vs_same_origin_employee"),
    tidyr::expand_grid(group = groups[-1], what = c("est", "se")) |>
      dplyr::mutate(row = "diff", comparison = "gap_differs_from_native"),
    tibble::tibble(group = groups[-1], what = "mde", row = "mde", comparison = "gap_differs_from_native")
  ) |>
    dplyr::select(group, row, comparison, what)
  out <- rows
  for (col in names(columns)) {
    spec <- columns[[col]]
    out[[col]] <- purrr::pmap_chr(rows, \(group, row, comparison, what) {
      r <- contrasts |>
        filter(
          outcome == spec$outcome, sample == spec$sample, owner_term == term,
          group == !!group, comparison == !!comparison
        )
      cell(r, what)
    })
  }
  n_owners <- contrasts |>
    filter(outcome == "income_t", sample == owner_sample, owner_term == term,
           comparison == "vs_same_origin_employee") |>
    select(group, n_owners)
  out |>
    left_join(n_owners, by = "group") |>
    mutate(
      n_owners = case_when(
        row != "gap" | what == "se" ~ "",
        is.na(n_owners) ~ "\u2013",
        .default = formatC(n_owners, format = "d", big.mark = ",")
      )
    )
}

# Splits format_b2_panel into the three thesis tables: gap (with owner
# counts), difference from the native-born gap, and MDE. Standard-error rows
# have an empty group label.
b2_panels <- function(contrasts, term, columns, owner_sample = "restricted") {
  group_names <- c(
    native = "Native-born", mes = "MES",
    mnes_higheng = "MNES high-English", mnes_loweng = "MNES low-English"
  )
  x <- format_b2_panel(contrasts, term, columns, owner_sample) |>
    dplyr::mutate(Group = dplyr::if_else(what == "se", "", group_names[group]))
  pick <- function(r) {
    x |>
      dplyr::filter(row == r) |>
      dplyr::select(Group, dplyr::all_of(names(columns)), n_owners)
  }
  list(gap = pick("gap"), diff = pick("diff"), mde = pick("mde"))
}

# Observations and individuals of the regression behind each (outcome, sample)
# column, as two rows to append to a b2_panels table
b2_footer_rows <- function(columns) {
  models <- list(unrestricted = models_b2_unrestricted, restricted = models_b2)
  fmt <- \(x) formatC(x, format = "d", big.mark = ",")
  tibble::tibble(
    Group = c("Observations (person-pairs)", "Individuals"),
    !!!purrr::map(columns, \(spec) {
      m <- models[[spec$sample]][[spec$outcome]]
      c(fmt(nobs(m)), fmt(n_individuals_model(m)))
    }),
    n_owners = ""
  )
}

# ==============================================================================
# End of thesis_format.R
# ==============================================================================
