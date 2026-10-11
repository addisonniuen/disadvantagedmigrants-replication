# ==============================================================================
# Table F.1: Best-fitting $\\lambda$ estimates for three specifications.
#
# Thesis label: @tbl-lambda-calibration (Appendix F; source chunk in project/thesis/07_appendix.qmd)
# What it shows: Best-fitting assimilation decay rates from cross-validation.
# Output: project/output/tables/table_F_01.html and table_F_01.tex
#
# Run on its own, or through 08_exhibits.R. exhibit_helpers.R runs the analysis
# pipeline (01 to 07) first if its results are not already in memory.
# ==============================================================================

source(file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits", "exhibit_helpers.R"
))

mse_ref <- cal$type1 |> filter(lambda == 0) |> pull(mse)

# Half-life in years; a zero decay rate never halves, shown as infinity
half_life <- \(l) ifelse(l == 0, "∞", sprintf("%.1f", log(2) / l))

t1 <- cal$type1 |>
  arrange(mse) |>
  slice_head(n = 5) |>
  transmute(
    spec = "Type 1",
    lam = sprintf("%.2f", lambda),
    lam_mes = "",
    lam_mn = "",
    hl = half_life(lambda),
    hl_mes = "",
    hl_mn = "",
    mse = mse
  )

t2 <- cal$type2 |>
  arrange(mse) |>
  slice_head(n = 5) |>
  transmute(
    spec = "Type 2",
    lam = "",
    lam_mes = sprintf("%.2f", lambda_mes),
    lam_mn = sprintf("%.2f", lambda_mnes),
    hl = "",
    hl_mes = half_life(lambda_mes),
    hl_mn = half_life(lambda_mnes),
    mse = mse
  )

ref <- tibble(
  spec = "Type 0",
  lam = "0.00",
  lam_mes = "",
  lam_mn = "",
  hl = "--",
  hl_mes = "",
  hl_mn = "",
  mse = mse_ref
)

tab <- bind_rows(ref, t1, t2) |>
  mutate(
    mse_fmt = sprintf("%.8f", mse),
    delta = sprintf("%+.2f", 1e5 * (mse - mse_ref)),
    spec = if_else(duplicated(spec), "", spec)
  )

exhibit <- tab |>
  select(spec, lam, hl, lam_mes, lam_mn, hl_mes, hl_mn, mse_fmt, delta) |>
  setNames(
    c(
      "Type",
      "$lambda$",
      "Half-life",
      "$lambda_\"MES\"$",
      "$lambda_\"MNES\"$",
      "Half-life (MES)",
      "Half-life (MNES)",
      "MSE",
      "Difference (in $10^5$ scale)"
    )
  ) |>
  tt(
    width = c(0.10, 0.09, 0.09, 0.09, 0.10, 0.10, 0.10, 0.16, 0.17)
  ) |>
  style_tt(i = c(2, 7), j = 2:9, bold = TRUE) |>
  style_tt(i = 0, line = "tb") |>
  style_tt(i = c(2, 7), line = "t") |>
  style_tt(j = 2:9, align = "r") |>
  style_tt(i = 0, bold = FALSE, align = "c") |>
  style_tt(i = 0, line = "b")

save_table(exhibit, "table_F_01")
