# Replication package: *Disadvantaged migrants* (Honours thesis)

**Author:** Hong Minh Dang (Addison) Nguyen
**Degree:** Bachelor of Commerce (Honours), Department of Econometrics & Business Statistics, Monash University, 2026
**Supervisor:** Professor Dan Zhu

This package reproduces every table and figure in the thesis, and the thesis PDF itself. The thesis studies three questions using the HILDA Survey:

1. **Block 1:** how employee wages in small single-site workplaces vary with the share of migrant business owners in the industry.
2. **Block 2:** the income and income risk of migrant business owners, relative to employees of the same origin.
3. **Block 3:** what pushes people into business ownership.

## Contents

```
project/
├── README.md        this file
├── code/            R scripts: the analysis pipeline (00 to 08) and one script per exhibit (exhibits/)
├── data/            raw input data (HILDA, not distributed, see below)
├── output/          everything the code produces: data/, tables/, figures/, thesis.pdf
├── thesis/          Quarto source of the thesis
└── images/          unused, can be deleted
```

Each folder has its own README with details.

## Data availability statement

**HILDA Survey (restricted access, not included).** The analysis uses the Household, Income and Labour Dynamics in Australia (HILDA) Survey, General Release 24.0 (waves 1 to 24, 2001 to 2024), released in December 2025.

- **Funding and management.** HILDA is funded by the Australian Government Department of Social Services (DSS) and managed by the Melbourne Institute of Applied Economic and Social Research, University of Melbourne.
- **Access.** The unit-record data are available to approved users only. Researchers apply through the National Centre for Longitudinal Data on the ADA Dataverse (https://dataverse.ada.edu.au/dataverse/DSSLongitudinalStudies) and sign a deed of licence.
- **Redistribution.** The licence does not allow the data to be redistributed, so neither the raw files nor any dataset derived from them (`output/data/`) are included in this package.
- **Version used.** The author accessed the General Release (not the Restricted Release) in Stata format, through an approved-user application. Approval normally takes a few weeks.

A replicator with HILDA access should download the **"2. STATA 240c (Zip file 1 of 4 - Combined Data Files a-k)"** and **"(Zip file 2 of 4 - Combined Data Files l-x)"** archives. They should then unzip them into `data/raw/hilda/`, as described in `data/README.md`. No other data are needed.

| Data source | Files used | Provided | Access |
|---|---|---|---|
| HILDA Survey, General Release 24.0 (DSS and Melbourne Institute, 2025) | `Combined_[a-x]240c.dta` | No | Approved users, via ADA Dataverse |

**Disclaimer required by the HILDA licence:** This paper uses unit record data from Household, Income and Labour Dynamics in Australia Survey (HILDA) conducted by the Australian Government Department of Social Services (DSS). The findings and views reported in this paper, however, are those of the author and should not be attributed to the Australian Government, DSS, or any of DSS' contractors or partners.

## Computational requirements

**Software:**

- **R 4.5.3.** `code/01_setup.R` installs any missing package from CRAN. The versions used are listed in `code/README.md`. The main packages are fixest 0.14.0, modelsummary 2.6.0, tinytable 0.17.0, tidyverse 2.0.0, haven 2.5.5, ggplot2 4.0.3, svglite 2.2.2 and rprojroot 2.1.1.
- **Quarto 1.9.36**, which includes Typst. This is needed only to build the thesis PDF. The `quarto` R package is needed if you build with `thesis/render.R`.
- **Times New Roman.** The figure text uses Times New Roman metrics, and this font comes with macOS and Windows.

**Hardware used:** MacBook with an Apple M5 chip (10 cores), 16 GB RAM, running macOS 26.6.2.

**Memory and storage:** the two HILDA Combined folders take about 4.1 GB on disk. The pipeline ran without problems in 16 GB of RAM.

**Runtime on this machine:**

| Step | Time |
|---|---|
| `Rscript project/code/00_runall.R` (full pipeline and all 39 exhibits) | about 4 minutes |
| Re-estimating the decay rates λ, if the cached `output/data/lambda_calibration_hhwtrp.rds` or `lambda_calibration_lnwtrp.rds` is deleted | about 40 more minutes per file |
| `Rscript project/thesis/render.R` (thesis PDF, empty cache) | about 10 minutes |

## Instructions to replicators

1. Obtain the HILDA data and place it in `data/raw/hilda/` (see `data/README.md`).
2. From the folder that contains `project/`, run:
   ```bash
   Rscript project/code/00_runall.R
   ```
   This builds the analysis samples (written to `output/data/`) and estimates every model. It also writes every table to `output/tables/` and every figure to `output/figures/`.
3. To rebuild the thesis PDF, which is written to `output/thesis.pdf`:
   ```bash
   Rscript project/thesis/render.R
   ```

Paths are found automatically. No script needs editing, and the package can live anywhere on disk.

**Running a single exhibit.** Each exhibit can be reproduced on its own. For example:

```bash
Rscript project/code/exhibits/table_5_02_owner_employee_gap.R
```

This first runs the analysis pipeline (about 4 minutes) if its results are not already loaded in the R session.

**Randomness.** Three steps use random numbers, and each one sets a fixed seed:

- the predictive mean matching imputation with `mice`, in `03_featureengineering.R`;
- the cross-validation folds for the λ calibration, in `03_featureengineering.R`;
- the person-clustered bootstrap for the confidence intervals in Table D.1.

All other estimates are deterministic.

## List of tables and figures

Every exhibit in the thesis has one script in `code/exhibits/`, named after its number in the PDF. Each script's header gives the exhibit's title, its label in the thesis source, what it shows and the files it writes. Chapter 3 is Data and chapter 5 is Results; the Appendix is unnumbered, with lettered sections A to F and exhibits numbered within each section (Table A.1). Numbers quoted in the text are computed inline in the `.qmd` chapters, from the same objects.

| Exhibit | Title | Script (`code/exhibits/`) | Output (`output/`) | Thesis label |
|---|---|---|---|---|
| Table 3.1 | Characteristics of business owners by origin and English proficiency group. | `table_3_01_summary_statistics.R` | `tables/table_3_01.html`, `.tex` | `tbl-owner-desc` |
| Table 5.1 | Main regression results for employee wages. | `table_5_01_employee_wages.R` | `tables/table_5_01.html`, `.tex` | `tbl-b1-main` |
| Figure 5.1 | Wage gaps of each workplace type relative to other large workplaces in the same industry and year, as the share of non-English-speaking migrant owners rises. | `figure_5_01_workplace_type_wage_gaps.R` | `figures/figure_5_01.svg`, `.png` | `fig-b1-placebo` |
| Table 5.2 | Owner–employee gaps in income and income risk for small employers. | `table_5_02_owner_employee_gap.R` | `tables/table_5_02.html`, `.tex` | `tbl-b2-main` |
| Table 5.3 | Difference between each migrant group's owner–employee gap and the native-born gap, for small employers. | `table_5_03_difference_from_native_gap.R` | `tables/table_5_03.html`, `.tex` | `tbl-b2-premium` |
| Table 5.4 | Entry into business ownership between t−1 and t (logit coefficients). | `table_5_04_entry_into_ownership.R` | `tables/table_5_04.html`, `.tex` | `tbl-b3-main` |
| Table 5.5 | Push into business ownership from unemployment and low wages, by origin group. | `table_5_05_push_by_origin.R` | `tables/table_5_05.html`, `.tex` | `tbl-b3-het` |
| Table A.1 | Estimation samples. | `table_A_01_estimation_samples.R` | `tables/table_A_01.html`, `.tex` | `tbl-sample-flow` |
| Table A.2 | Distribution of firm structures observed in the employee sample. | `table_A_02_workplace_types.R` | `tables/table_A_02.html`, `.tex` | `tbl-firmtype` |
| Table A.3 | Distribution of the leave-one-out ownership exposure shares across industry-year (waves 1-21). | `table_A_03_owner_share_distribution.R` | `tables/table_A_03.html`, `.tex` | `tbl-share-dist` |
| Table A.4 | Industry composition of business owners (%) by origin and English proficiency group. | `table_A_04_owner_industry_composition.R` | `tables/table_A_04.html`, `.tex` | `tbl-owner-industry` |
| Table A.5 | Industry composition by worker type. | `table_A_05_industry_composition_by_worker_type.R` | `tables/table_A_05.html`, `.tex` | `tbl-industry-composition` |
| Table A.6 | Panel persistence of all respondents by origin group. | `table_A_06_panel_structure.R` | `tables/table_A_06.html`, `.tex` | `tbl-panel` |
| Table B.1 | Distribution of the leave-one-out owner shares across employees in the wage sample. | `table_B_01_owner_shares_by_weighting.R` | `tables/table_B_01.html`, `.tex` | `tbl-loo-shares-b1` |
| Table B.2 | Log hourly wages under alternative small-firm size thresholds. | `table_B_02_workplace_size_thresholds.R` | `tables/table_B_02.html`, `.tex` | `tbl-b1-robustness` |
| Table B.3 | Owner density interacted with chain workplaces. | `table_B_03_placebo_test.R` | `tables/table_B_03.html`, `.tex` | `tbl-b1-placebo` |
| Table B.4 | Log hourly wages under 10-employee small-firm size threshold (full estimation results). | `table_B_04_threshold_10_full.R` | `tables/table_B_04.html`, `.tex` | `tbl-b1-robustness-10` |
| Table B.5 | Employee wage results under the three weightings of the owner shares, with all controls. | `table_B_05_threshold_20_full.R` | `tables/table_B_05.html`, `.tex` | `tbl-b1-appendix` |
| Table B.6 | Log hourly wages under 50-employee small-firm size threshold (full estimation results). | `table_B_06_threshold_50_full.R` | `tables/table_B_06.html`, `.tex` | `tbl-b1-robustness-50` |
| Table B.7 | Log hourly wages under 20-employee small-firm size threshold, migrant and English-proficiency owner share specification, by industry classification granularity. | `table_B_07_migrant_and_low_english_shares.R` | `tables/table_B_07.html`, `.tex` | `tbl-b1v2-appendix` |
| Table B.8 | Baseline regression results for employee wages, two-digit ANZSIC industry classification (full estimation results). | `table_B_08_two_digit_industry.R` | `tables/table_B_08.html`, `.tex` | `tbl-b1-robustness-2digit` |
| Table B.9 | Owner-share interactions with p-values that cluster by industry (wild cluster bootstrap). | `table_B_09_wild_bootstrap.R` | `tables/table_B_09.html`, `.tex` | `tbl-b1-wild` |
| Table B.10 | Employee wage results with owner shares weighted by the longitudinal survey weight. | `table_B_10_owner_share_weights.R` | `tables/table_B_10.html`, `.tex` | `tbl-b1-lnw` |
| Figure C.1 | Composition of the estimation sample by origin group. | `figure_C_01_block2_sample_composition.R` | `figures/figure_C_01.svg`, `.png` | `fig-b2-distribution` |
| Table C.1 | Sample cells, raw outcomes and attrition by ownership status, business size and origin group. | `table_C_01_block2_cells_and_attrition.R` | `tables/table_C_01.html`, `.tex` | `tbl-b2-cells` |
| Table C.2 | Income and risk of large employers relative to employees of the same origin. | `table_C_02_large_employers.R` | `tables/table_C_02.html`, `.tex` | `tbl-b2-large` |
| Table C.3 | Income and risk of own-account workers relative to employees of the same origin. | `table_C_03_own_account_workers.R` | `tables/table_C_03.html`, `.tex` | `tbl-b2-ownacc` |
| Table C.4 | Income level, volatility and downside risk by ownership status, business size and origin group (full estimation results). | `table_C_04_block2_full_estimates.R` | `tables/table_C_04.html`, `.tex` | `tbl-b2-appendix` |
| Figure C.2 | Distribution of year-to-year income changes by ownership status and origin group. | `figure_C_02_income_change_distribution.R` | `figures/figure_C_02.svg`, `.png` | `fig-b2-arc-density` |
| Table D.1 | Transitions into business ownership by migrant and language proficiency group. | `table_D_01_entry_rates.R` | `tables/table_D_01.html`, `.tex` | `tbl-b3-sample` |
| Table D.2 | Full estimation results for entry into business ownership between t−1 and t. | `table_D_02_entry_full_estimates.R` | `tables/table_D_02.html`, `.tex` | `tbl-b3-full` |
| Table D.3 | Timing of labour market status and within-person estimates of entry into business ownership (logit coefficients). | `table_D_03_entry_timing_and_mundlak.R` | `tables/table_D_03.html`, `.tex` | `tbl-b3-robust` |
| Table D.4 | Linear probability models of entry into business ownership (percentage points). | `table_D_04_entry_linear_probability.R` | `tables/table_D_04.html`, `.tex` | `tbl-b3-lpm` |
| Table D.5 | Labour market position and liquidity at t−1 among people at risk of entering business ownership, by origin group. | `table_D_05_position_by_origin.R` | `tables/table_D_05.html`, `.tex` | `tbl-b3-push-shares` |
| Table E.1 | Median hourly income gap between employees and owners with employees, by industry. | `table_E_01_hourly_income_gap.R` | `tables/table_E_01.html`, `.tex` | `tbl-gap-industry` |
| Table F.1 | Best-fitting λ estimates for three specifications. | `table_F_01_decay_calibration.R` | `tables/table_F_01.html`, `.tex` | `tbl-lambda-calibration` |

`output/tables/variable_summary.csv` (variable dictionary, not in the thesis) is written by `code/variablesummary.R`.

## References

Department of Social Services; Melbourne Institute of Applied Economic and Social Research (2025). *The Household, Income and Labour Dynamics in Australia (HILDA) Survey, General Release 24 (Waves 1–24)*. ADA Dataverse. DOI: *[add from the Dataverse page]*.

Summerfield, M., Garrard, B., Kamath, R., Nesa, M.K., Macalalad, N., Watson, N. and Wilkins, R. (2025). *HILDA User Manual – Release 24.* Melbourne Institute: Applied Economic and Social Research, University of Melbourne.
