# `output/`: everything the code produces

Every file in this folder is generated. Deleting the folder's contents and rerunning `project/code/00_runall.R` (and `project/thesis/render.R` for the PDF) recreates them. No input data or hand-edited files live here.

```
output/
├── data/       constructed analysis datasets (not distributed, see below)
├── tables/     every table in the thesis, one file per table
├── figures/    every figure in the thesis, one file per figure
└── thesis.pdf  the compiled thesis (copied here by project/thesis/render.R)
```

## `data/`

Written by `code/03_featureengineering.R`.

| File | Contents |
|---|---|
| `panel_final.csv` | Person × wave panel with all derived variables. |
| `owner_and_employee_sample.csv` | Business owners and employees. This is the base sample for Block 2. |
| `owner_sample.csv` | Business owners only. |
| `employee_sample.csv` | Employees only. This is the base sample for Block 1. |
| `lambda_calibration_cv_no_weighting.rds` | Cross-validated calibration of the decay rate λ used in the Type 1/2 owner shares (Table F.1). It is cached: if the file is present, the code reuses it instead of re-estimating. |

**These files are derived from HILDA microdata, so they cannot be redistributed.** They are excluded through `.gitignore` and are not part of the replication package. A replicator recreates them from the raw data.

## `tables/` and `figures/`

Each file is written by the matching script in `code/exhibits/` and named after the exhibit number in the thesis PDF:

- **Tables:** `table_5_02.html` and `table_5_02.tex` are Table 5.2, written by `code/exhibits/table_5_02_owner_employee_gap.R`.
- **Figures:** `figure_5_01.svg` (vector) and `figure_5_01.png` (300 dpi) are Figure 5.1.

The full list mapping each exhibit to its script and output file is in `project/README.md`. `tables/variable_summary.csv` is the variable dictionary written by `code/variablesummary.R`.

The thesis PDF is rendered from the `.qmd` files, which contain the same code as the exhibit scripts. The two therefore give the same numbers, and the files here let a reviewer inspect each exhibit without rendering the whole thesis.
