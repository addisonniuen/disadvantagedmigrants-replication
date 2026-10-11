# ==============================================================================
# 01_setup.R
#
# This script prepares the project for analysis. It:
#   - Loads all required R packages
#   - Sets project file paths
#   - Defines HILDA wave names and variable lists
#   - Includes functions to read HILDA wave files
#   - Combines the raw wave data into a single panel dataset (panel_raw)
#
# Required data files: HILDA data files
# ==============================================================================

required_packages <- c(
  "here",
  "rprojroot",
  "svglite",
  "tidyverse",
  "dplyr",
  "tidyr",
  "readxl",
  "haven",
  "conflicted",
  "zoo",
  "ggplot2",
  "ozmaps",
  "patchwork",
  "sf",
  "kableExtra",
  "knitr",
  "scales",
  "DT",
  "naniar",
  "modelsummary",
  "fixest",
  "polycor",
  "DescTools",
  "tinytable",
  "modelsummary",
  "mice",
  "biostat3",
  "gt",
  "tinytable",
  "car"
  # "plm", "lmtest", "forcats", "purrr", "gt"
)

load_or_install <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

load_or_install(required_packages)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::slice)
conflicts_prefer(dplyr::count)
conflicts_prefer(dplyr::lag)
conflicts_prefer(fixest::pvalue)
conflicted::conflicts_prefer(
  modelsummary::Mean,
  modelsummary::SD,
  .quiet = TRUE
)

# --- Locations -------------------------------------------------------------

# All paths are built from the project folder, found by searching upwards from
# the working directory for project/code/01_setup.R. The package therefore runs
# from any folder inside the replication package without editing paths.
project_dir <- file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project"
)

# Inputs: raw HILDA files (not redistributable, see project/data/README.md)
directory_data_raw <- file.path(project_dir, "data", "raw")
directory_data_raw_ak <- file.path(
  directory_data_raw,
  "hilda",
  "2. STATA 240c (Zip file 1 of 4 - Combined Data Files a-k)"
)
directory_data_raw_lx <- file.path(
  directory_data_raw,
  "hilda",
  "2. STATA 240c (Zip file 2 of 4 - Combined Data Files l-x)"
)

# Outputs: everything the code produces is written under project/output
directory_output <- file.path(project_dir, "output")
directory_data_final <- file.path(directory_output, "data")      # constructed datasets
directory_output_tables <- file.path(directory_output, "tables")  # tables in the thesis
directory_output_figures <- file.path(directory_output, "figures") # figures in the thesis
for (d in c(directory_data_final, directory_output_tables, directory_output_figures)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

save_final <- function(df, filename) {
  write.csv(df, file.path(directory_data_final, filename), row.names = FALSE)
  cat("Saved in project/output/data:", filename, "\n")
}

# --- Tests -------------------------------------------------------------------

# Joint F-test that the named coefficients of a linear model are zero, using the
# model's clustered covariance; the denominator degrees of freedom are fixest's
# clustered degrees of freedom (as in the owner income tables). Returns the
# same layout as car::linearHypothesis, so row 2 holds F, Df, Res.Df and Pr(>F).
cluster_f_test <- function(m, hypotheses, ...) {
  terms <- trimws(sub("=.*$", "", hypotheses))
  pattern <- paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", terms), "$")
  w <- fixest::wald(m, keep = pattern, print = FALSE)
  stopifnot(w$df1 == length(terms))
  data.frame(
    F = c(NA, w$stat), Df = c(NA, w$df1), Res.Df = c(NA, w$df2), `Pr(>F)` = c(NA, w$p),
    check.names = FALSE
  )
}

# Wald chi-squared test that the named coefficients of a logit are zero
wald_chisq <- function(m, terms) {
  b <- coef(m)[terms]
  stat <- drop(t(b) %*% solve(vcov(m)[terms, terms]) %*% b)
  list(stat = stat, df = length(terms), p = pchisq(stat, length(terms), lower.tail = FALSE))
}

# --- Wave ------------------------------------------------------------------

waves <- letters[1:24]
wave_nums <- setNames(1:24, waves)
wave_yrs <- setNames(2001:2024, waves)

# Helper function to read each HILDA wave
# Remove wave-specific prefixes from variable names and add wave and survey year identifiers
read_wave <- function(w) {
  fname <- if (w %in% letters[1:11]) {
    file.path(directory_data_raw_ak, paste0("Combined_", w, "240c.dta"))
  } else {
    file.path(directory_data_raw_lx, paste0("Combined_", w, "240c.dta"))
  }
  if (!file.exists(fname)) {
    message("Not found: ", basename(fname))
    return(NULL)
  }
  need <- c("xwaveid", vars_pull)
  pfx <- ifelse(need == "xwaveid", "xwaveid", paste0(w, need))
  df <- tryCatch(
    read_dta(fname, col_select = any_of(pfx)) |> zap_labels(),
    error = function(e) {
      message("Wave ", w, ": ", e$message)
      NULL
    }
  )
  if (is.null(df)) {
    return(NULL)
  }
  old <- intersect(names(df), pfx)
  new <- need[match(old, pfx)]
  df <- rename_with(df, ~ new[match(.x, old)], .cols = all_of(old))
  df$wave <- wave_nums[[w]]
  df$year <- wave_yrs[[w]]
  cat("  Wave", w, wave_yrs[[w]], "—", nrow(df), "rows\n")
  df
}


# --- Variables ------------------------------------------------------------

vars_pull <- c(
  # Geography
  "hhstate",
  "hhs3ra",
  "hhs3add",
  "hhs3gcc",
  "hhs3sos",
  # Demographics
  "hgage",
  "hgsex",
  "mrcurr",
  # Housing
  "hstenr",
  "hsvalui",
  # Finance
  "hsmgowe",
  "hsdebt",
  "hwhmhl",
  "hsmgpd",
  "hsmguse",
  "hsrntby",
  "hsmg",
  "hsmgi",
  "hsrnt",
  "hsrnti",
  "hsyr",
  "fiprbeg",
  "fiprbmr",
  "fiprbfh",
  "fiprbps",
  "fiprbuh",
  "fiprbwm",
  "fiprbwo",
  "fiprosp",
  "fiemerf",
  # Ancestry / migrant
  "ancob",
  "anbcob",
  "aneab",
  "hgeab",
  "anengf",
  "anyoa",
  "anmigc",
  "anrefn",
  "anlote",
  "sktarea",
  # Education
  "edsscmp",
  "edlhqn",
  "edhigh1",
  "edcoq",
  "edpsqau",
  # Employment status
  "es",
  "esbrd",
  "esempdt",
  "esdtl",
  "ehtujyr",
  "ehtsemt",
  # Job characteristics
  "jbmi61",
  "jbmi62",
  "jbmwpsz",
  "jbmwps",
  "jbmemsz",
  "jbmems2",
  "jbmmwp",
  "jbmmply",
  "jbmmplr",
  "jbmmpl",
  "jbcasab",
  "jbmcnt",
  "jbemlyr",
  "jbocct",
  "jbmsch",
  "jbempt",
  "jbmo61",
  "jbmo62",
  "jbhrqf",
  "jbmday",
  "jbmo6s",
  "jbmsl",
  "jbmsvsr",
  "jbmtuea",
  "jbn",
  # Job insecurity / search
  "jbmpgj",
  "jbmplej",
  "jbmploj",
  "jbnewjs",
  "jbhrcpr",
  # Job satisfaction
  "jbmsall",
  "jbmsflx",
  "jbmshrs",
  "jbmspay",
  "jbmssec",
  "jbmswrk",
  # Life events
  "lefni",
  "lefnw",
  "lefrd",
  # Income
  "wscmei",
  "wscmef",
  "wscei",
  "wscef",
  "jbmhruc",
  "jbhruc",
  "tifefp",
  "tifefn",
  "tifeip",
  "bifip",
  "bifin",
  "bifp",
  "bifn",
  "bifhave",
  "biff",
  "wsfei",
  "hifdip",
  "hifdin",
  # Survey weights
  "hhwth",
  "hhwtrp",
  "lnwtrp"
)


# ---- Create the raw combined panel ----------------------------------------

# Waves 22–24 are missing jbmwpsz, jbmwps, jbmemsz, and jbmmwp variables which are required for some part of the analysis
# Hence, these waves were excluded from any analysis block that requires these firm size classification variables
no_size_waves <- 22:24

panel_raw <- map(waves, read_wave) |>
  compact() |>
  bind_rows() |>
  arrange(xwaveid, wave)

# ==============================================================================
# End of 01_setup.R
# ==============================================================================
