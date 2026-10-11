# ==============================================================================
# Table B.9: Owner-share interactions with p-values that cluster by industry (wild cluster bootstrap).
#
# Thesis label: @tbl-b1-wild (Appendix B; source chunk in project/thesis/07_appendix.qmd)
# What it shows: For each weighting, the owner-share interactions of Table 5.1 with
#   their p-values under the main clustering, the standard error clustered by
#   industry, and wild cluster bootstrap p-values with industry clusters.
# Output: project/output/tables/table_B_09.html and table_B_09.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

b1_wild_models <- list(type0 = regression1_v1a, type1 = regression1_v1b, type2 = regression1_v1c)

b1_wild_table <- purrr::map_dfr(names(b1_wild_models), \(type) {
  m <- b1_wild_models[[type]]
  w <- dplyr::filter(regression1_wild, type == !!type)
  terms <- w$term[w$term != "joint"]
  ct <- fixest::coeftable(m)
  joint <- cluster_f_test(m, paste(terms, "= 0"))
  tibble::tibble(
    `Owner share` = c(
      "Small single-site x MES owner share",
      "Small single-site x MNES high-English owner share",
      "Small single-site x MNES low-English owner share",
      "All three interactions jointly"
    ),
    Coefficient = c(sprintf("%.3f", ct[terms, 1]), ""),
    `p-value, individual and industry-year clusters` = c(fmt_p(ct[terms, 4]), fmt_p(joint[2, "Pr(>F)"])),
    `Standard error, industry clusters` = c(sprintf("%.3f", w$se_industry[seq_along(terms)]), ""),
    `Wild bootstrap p-value, industry clusters` = fmt_p(w$p_wild)
  )
})

exhibit <- b1_wild_table |>
  tinytable::tt(width = c(0.36, 0.12, 0.18, 0.16, 0.18)) |>
  tinytable::theme_empty() |>
  tinytable::group_tt(i = list(
    "No decay (Type 0)" = 1,
    "Pooled decay (Type 1)" = 5,
    "Origin-specific decay (Type 2)" = 9
  )) |>
  tinytable::style_tt(i = 0, line = "tb") |>
  tinytable::style_tt(i = c(1, 6, 11), italic = TRUE) |>
  tinytable::style_tt(i = 15, line = "b") |>
  tinytable::style_tt(j = 1, align = "l") |>
  tinytable::style_tt(j = 2:5, align = "c")

save_table(exhibit, "table_B_09")
