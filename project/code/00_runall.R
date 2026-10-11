# ==============================================================================
# 00_runall.R
#
# Runs the full replication package in order. Run from anywhere inside the
# package, e.g. Rscript project/code/00_runall.R
#
#   01_setup.R              packages, paths, reads the raw HILDA waves
#   02_datacleaning.R       recodes and labels the HILDA variables
#   03_featureengineering.R constructs the analysis variables and samples,
#                           and the migrant owner shares (writes output/data)
#   05_analysisblock1.R     Block 1: employee wages
#   06_analysisblock2.R     Block 2: owner income and risk
#   07_analysisblock3.R     Block 3: entry into business ownership
#   08_exhibits.R           every table and figure in the thesis
#                           (writes output/tables and output/figures)
#
# The thesis PDF itself is built separately by project/thesis/render.R.
# 04_eda.R (sample checks) is not run here: it prints checks only, and it
# inspects the samples built by 05 to 07, so it is run after them by hand.
# ==============================================================================

code_dir <- file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code"
)

for (f in c("01_setup.R", "02_datacleaning.R", "03_featureengineering.R",
            "05_analysisblock1.R", "06_analysisblock2.R", "07_analysisblock3.R",
            "08_exhibits.R")) {
  message("==== ", f)
  source(file.path(code_dir, f))
}
