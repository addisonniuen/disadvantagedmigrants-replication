# `code/`: analysis and exhibit scripts

All code is in R. Paths are found automatically with `rprojroot` (the package root is the folder that contains `project/code/01_setup.R`). Nothing needs to be edited before running, and no script calls `setwd()` or uses an absolute path.

## Quick start

```bash
Rscript project/code/00_runall.R
```

This runs the full pipeline and writes every table and figure in the thesis to `project/output/`. To also rebuild the PDF, see `project/thesis/README.md`.

## Main pipeline (numbered scripts)

The numbered scripts run in this order. Each one relies on the objects the earlier ones create in the R session, so they must be run in sequence in a single session. `00_runall.R` does this for you.

| Script | What it does | Reads | Writes |
|---|---|---|---|
| `00_runall.R` | Runs 01 to 08 in order. | | |
| `01_setup.R` | Loads the packages and sets the paths (`directory_data_raw`, `directory_output`, `directory_data_final` = `output/data`, `directory_output_tables`, `directory_output_figures`). Reads the variables used from waves 1 to 24 of the HILDA Combined files. | `data/raw/hilda/…/Combined_[a-x]240c.dta` | |
| `02_datacleaning.R` | Recodes HILDA missing-value codes, labels the categorical variables and harmonises variables across waves. | | |
| `03_featureengineering.R` | Builds the analysis variables. These include: origin and English-proficiency groups (native-born, MES, MNES high-English, MNES low-English), workplace types (`small_single_site`, `chain_like`, `small_site_chain`), owner types (employer and own-account worker, by size), income, volatility and large-fall outcomes, entry into ownership, and the leave-one-out migrant owner shares under Type 0/1/2 decay. It also calibrates the decay rate λ, and defines the estimation samples. | | `output/data/panel_final.csv`, `owner_and_employee_sample.csv`, `owner_sample.csv`, `employee_sample.csv`, `lambda_calibration_hhwtrp.rds`, `lambda_calibration_lnwtrp.rds` |
| `05_analysisblock1.R` | **Block 1:** employee log hourly wages on workplace type × migrant owner shares. Covers the main models, the 10/50-employee and two-digit-industry robustness checks, and the chain placebo. | | |
| `06_analysisblock2.R` | **Block 2:** owner–employee gaps in income, volatility and large income falls, by origin. Fits the restricted (same status in *t*−1 and *t*) and unrestricted (status at *t*−1) samples, then the contrasts, joint tests and minimum detectable effects. | | |
| `07_analysisblock3.R` | **Block 3:** entry into business ownership. Covers the four main logits, the origin × push logit, *t*−1/*t*−2 timing, the Mundlak (person-means) model and linear probability versions. | | |
| `04_eda.R` | Sample checks for the three blocks (printed to the console). Not used by the thesis and not run by `00_runall.R`; run it after `07_analysisblock3.R`. | | |
| `08_exhibits.R` | Runs every script in `exhibits/` in thesis order. | | `output/tables/`, `output/figures/` |
| `variablesummary.R` | Optional. Builds a variable dictionary and is not run by `00_runall.R`. Run it after `01_setup.R`. | | `output/tables/variable_summary.csv` |

Each analysis script starts with a summary of what it does and which tables and figures it feeds. Within each script, the code behind the thesis comes first. Models that are not in the thesis are kept at the end under *Not in the thesis (commented out)*, each with a one-line note on what it was.

**λ calibration.** `03_featureengineering.R` estimates the decay rates twice and caches each result in `output/data`. `lambda_calibration_hhwtrp.rds` builds the owner shares with the cross-sectional weight (hhwtrp) and is used for every estimate in the thesis. `lambda_calibration_lnwtrp.rds` uses the longitudinal weight (lnwtrp) and is used only for the sensitivity check in Table B.10; it comes from an earlier version of the calibration code, with a narrower employee sample and no control for the worker's origin, and is re-estimated with the current code if deleted. The hhwtrp version is the appropriate one, since each owner share describes a single wave, while the longitudinal weight is zero for everyone who has missed a wave since wave 1 and so drops a growing share of owners. Each file is reused if it exists; delete it to re-estimate (about 40 minutes per file).

## `exhibits/`: one script per table and figure

Each script reproduces exactly one table or figure in the thesis PDF. The scripts are named after the exhibit number:

```
table_<chapter>_<NN>_<short description>.R     e.g. table_5_01_employee_wages.R  -> Table 5.1
figure_<chapter>_<NN>_<short description>.R    e.g. figure_5_01_workplace_type_wage_gaps.R -> Figure 5.1
table_<letter>_<NN>_<short description>.R      e.g. table_D_04_entry_linear_probability.R -> Table D.4
```

Numbers are zero-padded so that the files sort in the order they appear in the thesis. Chapter 3 is Data and chapter 5 is Results. The Appendix is unnumbered: its sections are lettered A to F, and its tables and figures are numbered within each section (Table A.1, Figure C.2), so appendix scripts carry the section letter instead of a chapter number.

Every script starts with a header giving:

- the exhibit number and its caption;
- the thesis cross-reference label and the `.qmd` file that contains the matching chunk;
- what the exhibit shows;
- the output files it writes.

Each script first sources `exhibits/exhibit_helpers.R`, which does the following:

1. Runs the pipeline (01 to 07) if its results are not already in memory. This means any exhibit script can be run on its own, for example `Rscript project/code/exhibits/table_5_02_owner_employee_gap.R`, at the cost of about 6 minutes of set-up.
2. Loads the thesis formatting, `project/thesis/thesis_format.R` (figure style, number formatting and table helpers). It is the same file the thesis uses, so the exhibits match the PDF. All formatting lives in the thesis folder; the scripts in `code/` only estimate.
3. Defines `save_table()`, which writes `.html` and `.tex` files to `output/tables/`, and `save_figure()`, which writes `.svg` and 300 dpi `.png` files to `output/figures/`.

The code inside each exhibit script is the same as the code in the matching chunk of the thesis `.qmd` files. The full mapping from exhibit to script to output is in `project/README.md`.

## Software

R 4.5.3. CRAN packages:

- tidyverse 2.0.0 (dplyr 1.2.1, tidyr 1.3.2, ggplot2 4.0.3, purrr 1.2.2, stringr 1.6.0, forcats 1.0.1, tibble 3.3.1)
- haven 2.5.5
- fixest 0.14.0
- modelsummary 2.6.0
- tinytable 0.17.0
- car 3.1.5
- mice 3.19.0
- zoo 1.8.15
- data.table 1.18.4
- scales 1.4.0
- patchwork 1.3.2
- svglite 2.2.2
- systemfonts 1.3.1
- rprojroot 2.1.1
- here 1.0.2
- conflicted 1.2.0
- readxl 1.5.0
- withr 3.0.2
- knitr 1.51

`01_setup.R` installs any missing package.
