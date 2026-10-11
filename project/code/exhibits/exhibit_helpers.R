# ==============================================================================
# exhibit_helpers.R
#
# Shared set-up for the scripts in project/code/exhibits/, each of which
# reproduces one table or figure of the thesis. Every exhibit script sources
# this file first. It:
#   1. locates the replication package (no machine-specific paths);
#   2. runs the analysis pipeline (01_setup.R to 07_analysisblock3.R) if its
#      results are not already in memory, so any exhibit script can be run on
#      its own;
#   3. loads the packages, formatting helpers and figure style used by the
#      thesis (project/thesis/thesis_format.R);
#   4. defines save_table() and save_figure(), which write to project/output.
# ==============================================================================

root_dir <- rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R"))
code_dir <- file.path(root_dir, "project", "code")

# --- 1. Analysis results ------------------------------------------------------

# regression3_lpm_het_fe is the last object created by 07_analysisblock3.R, so
# if it exists the whole pipeline has already been run in this session.
if (!exists("regression3_lpm_het_fe")) {
  message("Running the analysis pipeline (01 to 07); this takes about 6 minutes...")
  for (f in c("01_setup.R", "02_datacleaning.R", "03_featureengineering.R",
              "05_analysisblock1.R", "06_analysisblock2.R", "07_analysisblock3.R")) {
    source(file.path(code_dir, f))
  }
}

# --- 2. Packages and options (as in the thesis set-up chunks) -----------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(tinytable)
  library(modelsummary)
})
options(modelsummary_factory_default = "tinytable")
options(modelsummary_format_numeric_latex = "plain")

# --- 3. Formatting (the same file the thesis uses) ----------------------------

# Figure style, number formatting and table helpers
source(file.path(root_dir, "project", "thesis", "thesis_format.R"), encoding = "UTF-8")

# Owners in each Block 2 cell (restricted sample), used by some Block 2 exhibits
n_owners_cell <- cells_b2 |>
  dplyr::mutate(cell = paste(owner_type, origin_group, sep = ":")) |>
  dplyr::select(cell, individuals) |>
  tibble::deframe()

# --- 4. Saving ------------------------------------------------------------------

# Tables are saved as HTML (to view in a browser) and LaTeX (to reuse).
save_table <- function(x, name) {
  for (ext in c("html", "tex")) {
    tinytable::save_tt(x, file.path(directory_output_tables, paste0(name, ".", ext)), overwrite = TRUE)
  }
  message("Saved project/output/tables/", name, ".html and .tex")
  invisible(x)
}

# Figures are saved as SVG (vector, as in the thesis) and PNG.
save_figure <- function(p, name, width, height) {
  ggplot2::ggsave(file.path(directory_output_figures, paste0(name, ".svg")), p,
                  width = width, height = height, device = svglite::svglite, fix_text_size = FALSE)
  ggplot2::ggsave(file.path(directory_output_figures, paste0(name, ".png")), p,
                  width = width, height = height, dpi = 300, bg = "white")
  message("Saved project/output/figures/", name, ".svg and .png")
  invisible(p)
}
