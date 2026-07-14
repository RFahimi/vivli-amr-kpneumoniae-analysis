# Phase 8: Final Deliverable Assembly
# Project: Vivli AMR / Pfizer ATLAS Dataset
#
# Purpose:
# Assemble final reporting materials from Phase 7 into a clean deliverable package.
#
# This phase does NOT perform new analysis.
# It only organises final report assets, slide assets, inventories, and summary text.


# Step 1: Load packages
library(data.table)


# Step 2: Create Phase 8 output folders
phase8_dir <- "phase8_outputs"

phase8_report_dir <- file.path(phase8_dir, "report_package")
phase8_slide_dir <- file.path(phase8_dir, "slide_package")
phase8_submission_dir <- file.path(phase8_dir, "submission_package")

phase8_report_figures_dir <- file.path(phase8_report_dir, "figures")
phase8_report_tables_dir <- file.path(phase8_report_dir, "tables")
phase8_report_text_dir <- file.path(phase8_report_dir, "text")

phase8_slide_figures_dir <- file.path(phase8_slide_dir, "figures")
phase8_slide_tables_dir <- file.path(phase8_slide_dir, "tables")
phase8_slide_text_dir <- file.path(phase8_slide_dir, "text")

dir.create(phase8_dir, showWarnings = FALSE)
dir.create(phase8_report_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(phase8_slide_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(phase8_submission_dir, recursive = TRUE, showWarnings = FALSE)

dir.create(phase8_report_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(phase8_report_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(phase8_report_text_dir, recursive = TRUE, showWarnings = FALSE)

dir.create(phase8_slide_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(phase8_slide_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(phase8_slide_text_dir, recursive = TRUE, showWarnings = FALSE)


# Step 3: Define Phase 7 source folders
phase7_dir <- "phase7_outputs"
phase7_figures_dir <- file.path(phase7_dir, "final_figures")
phase7_tables_dir <- file.path(phase7_dir, "final_tables")
phase7_text_dir <- file.path(phase7_dir, "final_text")


# Step 4: Phase 7 input check
phase7_input_check <- data.table(
  input_name = c(
    "Phase 7 root folder",
    "Phase 7 final figures folder",
    "Phase 7 final tables folder",
    "Phase 7 final text folder",
    "Phase 7 figure inventory",
    "Phase 7 table inventory",
    "Phase 7 text inventory",
    "Phase 7 reporting checklist",
    "Phase 7 output file list"
  ),
  path = c(
    phase7_dir,
    phase7_figures_dir,
    phase7_tables_dir,
    phase7_text_dir,
    file.path(phase7_dir, "phase7_final_figures_inventory.csv"),
    file.path(phase7_dir, "phase7_final_tables_inventory.csv"),
    file.path(phase7_dir, "phase7_final_text_inventory.csv"),
    file.path(phase7_dir, "phase7_reporting_package_checklist.csv"),
    file.path(phase7_dir, "phase7_output_file_list.csv")
  )
)

phase7_input_check[, exists := file.exists(path) | dir.exists(path)]

phase7_input_check

fwrite(
  phase7_input_check,
  file.path(phase8_dir, "phase8_phase7_input_check.csv")
)

if (any(phase7_input_check$exists == FALSE)) {
  stop("One or more required Phase 7 inputs are missing.")
}


# Step 5: Read Phase 7 inventories
phase7_figure_inventory <- fread(
  file.path(phase7_dir, "phase7_final_figures_inventory.csv")
)

phase7_table_inventory <- fread(
  file.path(phase7_dir, "phase7_final_tables_inventory.csv")
)

phase7_text_inventory <- fread(
  file.path(phase7_dir, "phase7_final_text_inventory.csv")
)

phase7_reporting_package_checklist <- fread(
  file.path(phase7_dir, "phase7_reporting_package_checklist.csv")
)


# Step 6: Copy all final Phase 7 figures into report package
report_figures_to_copy <- data.table(
  source_file = file.path(phase7_figures_dir, list.files(phase7_figures_dir, pattern = "\\.png$")),
  destination_file = file.path(phase8_report_figures_dir, list.files(phase7_figures_dir, pattern = "\\.png$"))
)

report_figures_to_copy[, source_exists := file.exists(source_file)]
report_figures_to_copy[, copy_success := file.copy(
  source_file,
  destination_file,
  overwrite = TRUE
)]

report_figures_to_copy

fwrite(
  report_figures_to_copy,
  file.path(phase8_dir, "phase8_report_figures_copy_check.csv")
)


# Step 7: Copy all final Phase 7 tables into report package
report_tables_to_copy <- data.table(
  source_file = file.path(phase7_tables_dir, list.files(phase7_tables_dir, pattern = "\\.csv$")),
  destination_file = file.path(phase8_report_tables_dir, list.files(phase7_tables_dir, pattern = "\\.csv$"))
)

report_tables_to_copy[, source_exists := file.exists(source_file)]
report_tables_to_copy[, copy_success := file.copy(
  source_file,
  destination_file,
  overwrite = TRUE
)]

report_tables_to_copy

fwrite(
  report_tables_to_copy,
  file.path(phase8_dir, "phase8_report_tables_copy_check.csv")
)


# Step 8: Copy Phase 7 text summaries into report package
report_text_to_copy <- data.table(
  source_file = file.path(phase7_text_dir, list.files(phase7_text_dir, pattern = "\\.txt$")),
  destination_file = file.path(phase8_report_text_dir, list.files(phase7_text_dir, pattern = "\\.txt$"))
)

report_text_to_copy[, source_exists := file.exists(source_file)]
report_text_to_copy[, copy_success := file.copy(
  source_file,
  destination_file,
  overwrite = TRUE
)]

report_text_to_copy

fwrite(
  report_text_to_copy,
  file.path(phase8_dir, "phase8_report_text_copy_check.csv")
)


# Step 9: Select slide-ready figures
slide_figure_files <- c(
  "figure_01_phase4_yearly_resistance_trends_filtered.png",
  "figure_02_phase4_top_country_resistance_ordered.png",
  "figure_03_phase5b_choropleth_all_antibiotics.png",
  "figure_07_phase5c_temporal_trend_by_region_n500.png",
  "figure_09_phase5e_morans_i_summary.png",
  "figure_10_phase6e_final_auc_comparison.png",
  "figure_11_phase6e_auc_delta_rf_minus_logistic.png",
  "figure_12_phase6e_calibration_error.png"
)

slide_figures_to_copy <- data.table(
  source_file = file.path(phase7_figures_dir, slide_figure_files),
  destination_file = file.path(phase8_slide_figures_dir, slide_figure_files)
)

slide_figures_to_copy[, source_exists := file.exists(source_file)]
slide_figures_to_copy[, copy_success := file.copy(
  source_file,
  destination_file,
  overwrite = TRUE
)]

slide_figures_to_copy

fwrite(
  slide_figures_to_copy,
  file.path(phase8_dir, "phase8_slide_figures_copy_check.csv")
)


# Step 10: Select slide-ready tables
slide_table_files <- c(
  "table_04_phase4_overall_antibiotic_summary.csv",
  "table_06_phase5_country_level_summary.csv",
  "table_09_phase5_primary_morans_i_k4_report.csv",
  "table_11_phase6_selected_model_card.csv",
  "table_12_phase6_model_performance_report.csv",
  "table_18_key_results_summary.csv",
  "table_19_limitations_summary.csv"
)

slide_tables_to_copy <- data.table(
  source_file = file.path(phase7_tables_dir, slide_table_files),
  destination_file = file.path(phase8_slide_tables_dir, slide_table_files)
)

slide_tables_to_copy[, source_exists := file.exists(source_file)]
slide_tables_to_copy[, copy_success := file.copy(
  source_file,
  destination_file,
  overwrite = TRUE
)]

slide_tables_to_copy

fwrite(
  slide_tables_to_copy,
  file.path(phase8_dir, "phase8_slide_tables_copy_check.csv")
)


# Step 11: Create final executive summary
executive_summary <- c(
  "Executive Summary",
  "Project: Vivli AMR / Pfizer ATLAS Dataset",
  "",
  "This project analysed antimicrobial resistance patterns in Klebsiella pneumoniae using the Vivli AMR / Pfizer ATLAS dataset.",
  "The final analytical scope focused on Cefepime, Ceftazidime, and Meropenem because these antibiotic pairs had adequate interpretation records, country coverage, year coverage, and clinically relevant resistance patterns.",
  "",
  "Key descriptive finding:",
  "Cefepime and Ceftazidime showed high resistance levels, while Meropenem resistance was lower overall but remained clinically important.",
  "",
  "Key spatial finding:",
  "Country-level maps and spatial summaries showed geographically structured resistance patterns among spatial-ready countries.",
  "Exploratory Moran's I analysis indicated positive spatial autocorrelation for all three antibiotics, strongest for Cefepime and Ceftazidime and weaker for Meropenem.",
  "",
  "Key ML finding:",
  "The primary ML prediction unit was the isolate-antibiotic observation.",
  "The primary outcome was Resistant versus Susceptible plus Intermediate.",
  "Temporal validation used train years 2004–2018, validation years 2019–2021, and test years 2022–2024.",
  "Random Forest did not improve temporal test discrimination compared with logistic regression.",
  "Therefore, pooled logistic regression was selected as the primary model due to comparable or slightly better AUC, greater interpretability, and simpler reporting.",
  "",
  "Final ML model:",
  "Primary model: pooled logistic regression across Cefepime, Ceftazidime, and Meropenem.",
  "Benchmark model: Random Forest, retained as a flexible benchmark and sensitivity model.",
  "",
  "Core limitations:",
  "The dataset contains surveillance isolates, not patient-level data.",
  "Spatial autocorrelation should not be interpreted as causal evidence of transmission.",
  "Regional summaries are based on spatial-ready countries and may not represent full regional prevalence.",
  "The ML model was evaluated under retrospective temporal hold-out validation, not prospective external validation."
)

writeLines(
  executive_summary,
  file.path(phase8_submission_dir, "phase8_executive_summary.txt")
)

writeLines(
  executive_summary,
  file.path(phase8_report_text_dir, "phase8_executive_summary.txt")
)


# Step 12: Create final report outline
final_report_outline <- c(
  "Final Report Outline",
  "",
  "Title:",
  "Describing and Predicting Klebsiella pneumoniae Antimicrobial Resistance Using the Vivli AMR / Pfizer ATLAS Dataset",
  "",
  "1. Background",
  "- Antimicrobial resistance as a global surveillance and prediction problem.",
  "- Rationale for focusing on Klebsiella pneumoniae.",
  "- Clinical relevance of Cefepime, Ceftazidime, and Meropenem.",
  "",
  "2. Data Source and Study Unit",
  "- Vivli AMR / Pfizer ATLAS dataset, 2004–2024.",
  "- Isolate-level rows; not patient-level data.",
  "- Susceptibility interpretations cleaned to Susceptible, Intermediate, and Resistant.",
  "",
  "3. Scope Selection",
  "- Candidate organism-antibiotic pairs screened by record count, country coverage, year coverage, and outcome variation.",
  "- Final scope: K. pneumoniae with Cefepime, Ceftazidime, and Meropenem.",
  "",
  "4. Descriptive Resistance Analysis",
  "- Overall resistance and non-susceptibility summaries.",
  "- Country-level and temporal descriptive trends.",
  "- Sample-size filtering to reduce sparse-stratum over-interpretation.",
  "",
  "5. Spatial Analysis",
  "- Country-level spatial-ready criteria.",
  "- Choropleth maps and country rankings.",
  "- World Bank region summaries.",
  "- Moran's I using symmetrised k-nearest-neighbour weights.",
  "",
  "6. Machine Learning Analysis",
  "- Prediction unit: isolate-antibiotic observation.",
  "- Primary outcome: Resistant versus Susceptible plus Intermediate.",
  "- Features: antibiotic, year, country, gender, age group, speciality, source.",
  "- Temporal validation: train 2004–2018, validation 2019–2021, test 2022–2024.",
  "- Models: logistic regression and Random Forest benchmark.",
  "- Final model: pooled logistic regression.",
  "",
  "7. Key Findings",
  "- High Cefepime and Ceftazidime resistance.",
  "- Lower but clinically important Meropenem resistance.",
  "- Positive spatial autocorrelation for all three antibiotics.",
  "- Random Forest did not improve temporal test discrimination compared with logistic regression.",
  "",
  "8. Limitations",
  "- Surveillance isolates, not population or patient-level prevalence.",
  "- Spatial autocorrelation is exploratory, not causal.",
  "- Regional summaries are not fully representative of entire regions.",
  "- Temporal hold-out validation is not prospective external validation.",
  "",
  "9. Conclusions",
  "- The project provides a reproducible descriptive, spatial, and predictive AMR reporting pipeline.",
  "- The final primary ML model is pooled logistic regression, with Random Forest retained as a benchmark."
)

writeLines(
  final_report_outline,
  file.path(phase8_report_text_dir, "phase8_final_report_outline.txt")
)

writeLines(
  final_report_outline,
  file.path(phase8_submission_dir, "phase8_final_report_outline.txt")
)


# Step 13: Create slide deck outline
slide_deck_outline <- c(
  "Slide Deck Outline",
  "",
  "Slide 1 — Title",
  "Describing and Predicting K. pneumoniae Resistance Using the Vivli AMR / Pfizer ATLAS Dataset",
  "",
  "Slide 2 — Project Objective",
  "Describe, map, and predict K. pneumoniae resistance to Cefepime, Ceftazidime, and Meropenem.",
  "",
  "Slide 3 — Dataset and Study Unit",
  "Dataset: Vivli AMR / Pfizer ATLAS, 2004–2024.",
  "Unit: isolate-level data; ML unit: isolate-antibiotic observation.",
  "",
  "Slide 4 — Scope Selection",
  "Final scope: K. pneumoniae × Cefepime, Ceftazidime, Meropenem.",
  "Selection based on record count, country coverage, year coverage, and outcome variation.",
  "",
  "Slide 5 — Descriptive Resistance Summary",
  "Use table_04_phase4_overall_antibiotic_summary.csv.",
  "Key message: Cefepime and Ceftazidime resistance high; Meropenem lower but clinically important.",
  "",
  "Slide 6 — Temporal Resistance Trends",
  "Use figure_01_phase4_yearly_resistance_trends_filtered.png.",
  "",
  "Slide 7 — Country-Level Spatial Patterns",
  "Use figure_03_phase5b_choropleth_all_antibiotics.png.",
  "",
  "Slide 8 — Regional Trends",
  "Use figure_07_phase5c_temporal_trend_by_region_n500.png.",
  "Caveat: regional summaries are based on spatial-ready countries.",
  "",
  "Slide 9 — Moran's I Spatial Autocorrelation",
  "Use figure_09_phase5e_morans_i_summary.png.",
  "Message: positive spatial autocorrelation for all three antibiotics.",
  "",
  "Slide 10 — ML Problem Definition",
  "Outcome: Resistant versus Susceptible plus Intermediate.",
  "Validation: train 2004–2018, validation 2019–2021, test 2022–2024.",
  "",
  "Slide 11 — ML Model Comparison",
  "Use figure_10_phase6e_final_auc_comparison.png.",
  "",
  "Slide 12 — Final ML Model Decision",
  "Use figure_11_phase6e_auc_delta_rf_minus_logistic.png.",
  "Message: Random Forest did not improve temporal test discrimination; pooled logistic regression selected.",
  "",
  "Slide 13 — Calibration and Caveats",
  "Use figure_12_phase6e_calibration_error.png.",
  "RF calibration was reviewed, but lower calibration error alone did not override lack of temporal AUC improvement.",
  "",
  "Slide 14 — Limitations",
  "Surveillance isolates, not patient-level prevalence.",
  "Spatial autocorrelation is exploratory, not causal.",
  "Regional summaries are not fully representative.",
  "ML validation is retrospective temporal hold-out, not prospective external validation.",
  "",
  "Slide 15 — Final Takeaway",
  "The project delivers a reproducible descriptive, spatial, and predictive AMR reporting pipeline.",
  "Primary ML model: pooled logistic regression.",
  "Random Forest: benchmark/sensitivity model."
)

writeLines(
  slide_deck_outline,
  file.path(phase8_slide_text_dir, "phase8_slide_deck_outline.txt")
)

writeLines(
  slide_deck_outline,
  file.path(phase8_submission_dir, "phase8_slide_deck_outline.txt")
)


# Step 14: Create submission README
submission_readme <- c(
  "Phase 8 Final Deliverable Assembly README",
  "",
  "This folder contains organised deliverable materials assembled from the final Phase 7 reporting package.",
  "",
  "Main folders:",
  "1. report_package/",
  "   - Full set of final report tables, figures, and text summaries.",
  "",
  "2. slide_package/",
  "   - Selected slide-ready figures, selected tables, and slide deck outline.",
  "",
  "3. submission_package/",
  "   - Executive summary, final report outline, slide deck outline, and final inventories.",
  "",
  "No new analysis was performed in Phase 8.",
  "",
  "Recommended final deliverables:",
  "- Written report based on report_package/",
  "- Slide deck based on slide_package/",
  "- Executive summary from submission_package/phase8_executive_summary.txt",
  "",
  "Required reporting caveats:",
  "- Results are based on surveillance isolates, not patient-level or population prevalence.",
  "- Spatial autocorrelation is exploratory and should not be interpreted causally.",
  "- Regional summaries are based on spatial-ready countries and may not represent full regions.",
  "- ML evaluation used retrospective temporal hold-out validation, not prospective external validation.",
  "- The model should be described as a predictive model under temporal hold-out validation, not as a deployable clinical tool."
)

writeLines(
  submission_readme,
  file.path(phase8_submission_dir, "README_phase8_final_deliverable.txt")
)


# Step 15: Create final deliverable inventory
phase8_deliverable_inventory <- data.table(
  file_path = list.files(
    phase8_dir,
    recursive = TRUE,
    full.names = FALSE
  )
)

phase8_deliverable_inventory

fwrite(
  phase8_deliverable_inventory,
  file.path(phase8_dir, "phase8_deliverable_inventory.csv")
)


# Step 16: Copy inventories into submission package
inventory_files <- c(
  "phase8_phase7_input_check.csv",
  "phase8_report_figures_copy_check.csv",
  "phase8_report_tables_copy_check.csv",
  "phase8_report_text_copy_check.csv",
  "phase8_slide_figures_copy_check.csv",
  "phase8_slide_tables_copy_check.csv",
  "phase8_deliverable_inventory.csv"
)

inventory_copy_check <- data.table(
  source_file = file.path(phase8_dir, inventory_files),
  destination_file = file.path(phase8_submission_dir, inventory_files)
)

inventory_copy_check[, source_exists := file.exists(source_file)]
inventory_copy_check[, copy_success := file.copy(
  source_file,
  destination_file,
  overwrite = TRUE
)]

inventory_copy_check

fwrite(
  inventory_copy_check,
  file.path(phase8_dir, "phase8_inventory_copy_check.csv")
)


# Step 17: Final package checklist
phase8_package_checklist <- data.table(
  item = c(
    "phase7_inputs_available",
    "report_figures_copied",
    "report_tables_copied",
    "report_text_copied",
    "slide_figures_copied",
    "slide_tables_copied",
    "executive_summary_created",
    "final_report_outline_created",
    "slide_deck_outline_created",
    "submission_readme_created",
    "deliverable_inventory_created",
    "inventory_files_copied"
  ),
  status = c(
    all(phase7_input_check$exists == TRUE),
    all(report_figures_to_copy$copy_success == TRUE),
    all(report_tables_to_copy$copy_success == TRUE),
    all(report_text_to_copy$copy_success == TRUE),
    all(slide_figures_to_copy$copy_success == TRUE),
    all(slide_tables_to_copy$copy_success == TRUE),
    file.exists(file.path(phase8_submission_dir, "phase8_executive_summary.txt")),
    file.exists(file.path(phase8_submission_dir, "phase8_final_report_outline.txt")),
    file.exists(file.path(phase8_submission_dir, "phase8_slide_deck_outline.txt")),
    file.exists(file.path(phase8_submission_dir, "README_phase8_final_deliverable.txt")),
    file.exists(file.path(phase8_dir, "phase8_deliverable_inventory.csv")),
    all(inventory_copy_check$copy_success == TRUE)
  )
)

phase8_package_checklist

fwrite(
  phase8_package_checklist,
  file.path(phase8_dir, "phase8_package_checklist.csv")
)


# Step 18: Rewrite final deliverable inventory so it includes latest checklist files
phase8_deliverable_inventory <- data.table(
  file_path = list.files(
    phase8_dir,
    recursive = TRUE,
    full.names = FALSE
  )
)

fwrite(
  phase8_deliverable_inventory,
  file.path(phase8_dir, "phase8_deliverable_inventory.csv")
)

phase8_deliverable_inventory <- data.table(
  file_path = list.files(
    phase8_dir,
    recursive = TRUE,
    full.names = FALSE
  )
)

fwrite(
  phase8_deliverable_inventory,
  file.path(phase8_dir, "phase8_deliverable_inventory.csv")
)

phase8_deliverable_inventory

list.files(phase8_dir, recursive = TRUE)