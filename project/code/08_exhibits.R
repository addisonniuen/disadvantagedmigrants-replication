# ==============================================================================
# 08_exhibits.R
#
# Reproduces every table and figure in the thesis from the analysis results,
# by running each script in project/code/exhibits/ in the order they appear in
# the thesis. Outputs go to project/output/tables and project/output/figures.
# Run after 01 to 07 (00_runall.R does this); otherwise exhibit_helpers.R
# runs them first.
# ==============================================================================

exhibit_dir <- file.path(
  rprojroot::find_root(rprojroot::has_file("project/code/01_setup.R")),
  "project", "code", "exhibits"
)

exhibit_scripts <- c(
  "table_3_01_summary_statistics.R",  # Table 3.1
  "table_5_01_employee_wages.R",  # Table 5.1
  "figure_5_01_workplace_type_wage_gaps.R",  # Figure 5.1
  "table_5_02_owner_employee_gap.R",  # Table 5.2
  "table_5_03_difference_from_native_gap.R",  # Table 5.3
  "table_5_04_entry_into_ownership.R",  # Table 5.4
  "table_5_05_push_by_origin.R",  # Table 5.5
  "table_A_01_estimation_samples.R",  # Table A.1
  "table_A_02_workplace_types.R",  # Table A.2
  "table_A_03_owner_share_distribution.R",  # Table A.3
  "table_A_04_owner_industry_composition.R",  # Table A.4
  "table_A_05_industry_composition_by_worker_type.R",  # Table A.5
  "table_A_06_panel_structure.R",  # Table A.6
  "table_B_01_owner_shares_by_weighting.R",  # Table B.1
  "table_B_02_workplace_size_thresholds.R",  # Table B.2
  "table_B_03_placebo_test.R",  # Table B.3
  "table_B_04_threshold_10_full.R",  # Table B.4
  "table_B_05_threshold_20_full.R",  # Table B.5
  "table_B_06_threshold_50_full.R",  # Table B.6
  "table_B_07_migrant_and_low_english_shares.R",  # Table B.7
  "table_B_08_two_digit_industry.R",  # Table B.8
  "table_B_09_wild_bootstrap.R",  # Table B.9
  "table_B_10_owner_share_weights.R",  # Table B.10
  "figure_C_01_block2_sample_composition.R",  # Figure C.1
  "table_C_01_block2_cells_and_attrition.R",  # Table C.1
  "table_C_02_large_employers.R",  # Table C.2
  "table_C_03_own_account_workers.R",  # Table C.3
  "table_C_04_block2_full_estimates.R",  # Table C.4
  "figure_C_02_income_change_distribution.R",  # Figure C.2
  "table_D_01_entry_rates.R",  # Table D.1
  "table_D_02_entry_full_estimates.R",  # Table D.2
  "table_D_03_entry_timing_and_mundlak.R",  # Table D.3
  "table_D_04_entry_linear_probability.R",  # Table D.4
  "table_D_05_position_by_origin.R",  # Table D.5
  "table_E_01_hourly_income_gap.R",  # Table E.1
  "table_F_01_decay_calibration.R"   # Table F.1
)

for (f in exhibit_scripts) {
  message("== ", f)
  source(file.path(exhibit_dir, f), local = new.env())
}
