# ==============================================================================
# render.R
#
# Renders the qmds into the thesis PDF (Quarto book, Typst output)
# The chapters run the analysis code themselves (via the _freeze cache)
# Takes about 10 minutes when the cache is empty
# ==============================================================================

thesis_dir <- file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project",
  "thesis"
)

# A UTF-8 locale is needed, otherwise the Unicode characters in the tables (en dashes, minus signs, em spaces) are written as <U+....> escapes
Sys.setenv(LANG = "en_AU.UTF-8", LC_ALL = "en_AU.UTF-8")

quarto::quarto_render(thesis_dir)

file.copy(
  file.path(thesis_dir, "_book", "thesis.pdf"),
  file.path(thesis_dir, "..", "output", "thesis.pdf"),
  overwrite = TRUE
)
