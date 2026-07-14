# Phase 7: Final Integrated Reporting Package
# Project: Vivli AMR / Pfizer ATLAS Dataset
#
# Purpose:
# Compile final outputs from Phases 1–6 into a report-ready package.
#
# This phase does NOT perform new analysis.
# It only collects, summarizes, copies, and documents final outputs.


# Step 1: Load packages
library(data.table)
library(stringr)


# Step 2: Create dedicated Phase 7 output folder
phase7_dir <- "phase7_outputs"
phase7_figures_dir <- file.path(phase7_dir, "final_figures")
phase7_tables_dir <- file.path(phase7_dir, "final_tables")
phase7_text_dir <- file.path(phase7_dir, "final_text")

dir.create(phase7_dir, showWarnings = FALSE)
dir.create(phase7_figures_dir, showWarnings = FALSE)
dir.create(phase7_tables_dir, showWarnings = FALSE)
dir.create(phase7_text_dir, showWarnings = FALSE)


# Step 3: Define phase output folders
phase_dirs <- data.table(
  phase = c(
    "phase1", "phase2", "phase3", "phase4",
    "phase5", "phase5b", "phase5c", "phase5d_a", "phase5d_b", "phase5e",
    "phase6a", "phase6b", "phase6c", "phase6d", "phase6e"
  ),
  folder = c(
    "phase1_outputs",
    "phase2_outputs",
    "phase3_outputs",
    "phase4_outputs",
    "phase5_outputs",
    "phase5b_outputs",
    "phase5c_outputs",
    "phase5d_outputs",
    "phase5d_outputs",
    "phase5e_outputs",
    "phase6a_outputs",
    "phase6b_outputs",
    "phase6c_outputs",
    "phase6d_outputs",
    "phase6e_outputs"
  )
)

phase_dirs[, folder_exists := dir.exists(folder)]

phase_dirs

fwrite(
  phase_dirs,
  file.path(phase7_dir, "phase7_phase_folder_check.csv")
)


# Step 4: Define required key files for final reporting
required_files <- data.table(
  section = c(
    # Phase 1
    "data_audit",
    "data_audit",
    
    # Phase 2
    "scope_selection",
    "scope_selection",
    
    # Phase 3
    "scope_selection",
    "scope_selection",
    
    # Phase 4
    "descriptive_results",
    "descriptive_results",
    "descriptive_results",
    
    # Phase 5 / spatial feasibility
    "spatial_feasibility",
    "spatial_visualisation",
    "spatial_visualisation",
    "regional_results",
    "regional_results",
    "moran_results",
    "moran_results",
    "spatial_integrated_summary",
    
    # Phase 6 / ML
    "ml_problem_definition",
    "ml_problem_definition",
    "ml_logistic",
    "ml_random_forest",
    "ml_model_selection",
    "ml_model_selection",
    "ml_reporting"
  ),
  file_label = c(
    # Phase 1
    "Phase 1 row/unit check",
    "Phase 1 variable catalog",
    
    # Phase 2
    "Phase 2 candidate pairs",
    "Phase 2 priority candidate pairs",
    
    # Phase 3
    "Phase 3 scope decision table",
    "Phase 3 overall selected pair summary",
    
    # Phase 4
    "Phase 4 overall antibiotic summary",
    "Phase 4 team summary table",
    "Phase 4 filtered yearly trend figure",
    
    # Phase 5
    "Phase 5 spatial feasibility summary",
    "Phase 5B country-antibiotic choropleth-ready table",
    "Phase 5B country ranking table",
    "Phase 5C region-antibiotic summary",
    "Phase 5C n500 temporal trend figure",
    "Phase 5D-B Moran's I interpreted results",
    "Phase 5D-B Moran scatterplot k4",
    "Phase 5E integrated spatial summary narrative",
    
    # Phase 6
    "Phase 6A ML problem definition",
    "Phase 6A feature catalog",
    "Phase 6B logistic test AUC summary",
    "Phase 6C RF test AUC summary",
    "Phase 6D primary model decision",
    "Phase 6D model selection table",
    "Phase 6E selected model card"
  ),
  file_path = c(
    # Phase 1
    file.path("phase1_outputs", "phase1_row_unit_check.csv"),
    file.path("phase1_outputs", "phase1_variable_catalog.csv"),
    
    # Phase 2
    file.path("phase2_outputs", "phase2_candidate_pairs.csv"),
    file.path("phase2_outputs", "phase2_priority_candidate_pairs.csv"),
    
    # Phase 3
    file.path("phase3_outputs", "phase3_scope_decision_table.csv"),
    file.path("phase3_outputs", "phase3_overall_pair_summary.csv"),
    
    # Phase 4
    file.path("phase4_outputs", "phase4_overall_antibiotic_summary.csv"),
    file.path("phase4_outputs", "phase4_team_summary_table.csv"),
    file.path("phase4_outputs", "phase4_fig_yearly_resistance_trends_filtered.png"),
    
    # Phase 5
    file.path("phase5_outputs", "phase5_spatial_feasibility_summary.csv"),
    file.path("phase5b_outputs", "phase5b_country_antibiotic_choropleth_ready.csv"),
    file.path("phase5b_outputs", "phase5b_country_ranking_table.csv"),
    file.path("phase5c_outputs", "phase5c_region_antibiotic_summary.csv"),
    file.path("phase5c_outputs", "phase5c_fig_temporal_trend_by_region_n500.png"),
    file.path("phase5d_b_outputs", "phase5d_morans_i_results_interpreted.csv"),
    file.path("phase5d_b_outputs", "phase5d_fig_moran_scatter_k4.png"),
    file.path("phase5e_outputs", "phase5e_integrated_summary_narrative.txt"),
    
    # Phase 6
    file.path("phase6a_outputs", "phase6a_ml_problem_definition.csv"),
    file.path("phase6a_outputs", "phase6a_feature_catalog.csv"),
    file.path("phase6b_outputs", "phase6b_test_auc_summary.csv"),
    file.path("phase6c_outputs", "phase6c_test_auc_summary.csv"),
    file.path("phase6d_outputs", "phase6d_primary_model_decision.csv"),
    file.path("phase6d_outputs", "phase6d_model_selection_table.csv"),
    file.path("phase6e_outputs", "phase6e_selected_model_card.csv")
  )
)

required_files[, file_exists := file.exists(file_path)]

required_files

fwrite(
  required_files,
  file.path(phase7_dir, "phase7_required_file_check.csv")
)

if (any(required_files$file_exists == FALSE)) {
  warning("Some required files are missing. Review phase7_required_file_check.csv before final reporting.")
}


# Step 5: Define key report-ready tables to copy
key_tables <- data.table(
  section = c(
    "data_audit",
    "scope_selection",
    "scope_selection",
    "descriptive_results",
    "descriptive_results",
    "spatial_results",
    "spatial_results",
    "regional_results",
    "moran_results",
    "moran_results",
    "ml_results",
    "ml_results",
    "ml_results",
    "ml_results",
    "ml_results",
    "ml_results"
  ),
  source_file = c(
    file.path("phase1_outputs", "phase1_row_unit_check.csv"),
    file.path("phase3_outputs", "phase3_scope_decision_table.csv"),
    file.path("phase3_outputs", "phase3_overall_pair_summary.csv"),
    file.path("phase4_outputs", "phase4_overall_antibiotic_summary.csv"),
    file.path("phase4_outputs", "phase4_team_summary_table.csv"),
    file.path("phase5e_outputs", "phase5e_country_level_summary.csv"),
    file.path("phase5e_outputs", "phase5e_top10_countries_by_antibiotic.csv"),
    file.path("phase5e_outputs", "phase5e_region_report_table.csv"),
    file.path("phase5e_outputs", "phase5e_primary_morans_i_k4_report.csv"),
    file.path("phase5e_outputs", "phase5e_morans_i_report_table.csv"),
    file.path("phase6e_outputs", "phase6e_selected_model_card.csv"),
    file.path("phase6e_outputs", "phase6e_model_performance_report_table.csv"),
    file.path("phase6e_outputs", "phase6e_final_model_selection_summary.csv"),
    file.path("phase6e_outputs", "phase6e_calibration_report_table.csv"),
    file.path("phase6e_outputs", "phase6e_caveats_and_recommendations.csv"),
    file.path("phase6e_outputs", "phase6e_feature_report.csv")
  ),
  destination_file = c(
    file.path(phase7_tables_dir, "table_01_phase1_row_unit_check.csv"),
    file.path(phase7_tables_dir, "table_02_phase3_scope_decision.csv"),
    file.path(phase7_tables_dir, "table_03_phase3_selected_pair_summary.csv"),
    file.path(phase7_tables_dir, "table_04_phase4_overall_antibiotic_summary.csv"),
    file.path(phase7_tables_dir, "table_05_phase4_team_summary.csv"),
    file.path(phase7_tables_dir, "table_06_phase5_country_level_summary.csv"),
    file.path(phase7_tables_dir, "table_07_phase5_top10_countries_by_antibiotic.csv"),
    file.path(phase7_tables_dir, "table_08_phase5_region_report_table.csv"),
    file.path(phase7_tables_dir, "table_09_phase5_primary_morans_i_k4_report.csv"),
    file.path(phase7_tables_dir, "table_10_phase5_morans_i_report_table.csv"),
    file.path(phase7_tables_dir, "table_11_phase6_selected_model_card.csv"),
    file.path(phase7_tables_dir, "table_12_phase6_model_performance_report.csv"),
    file.path(phase7_tables_dir, "table_13_phase6_model_selection_summary.csv"),
    file.path(phase7_tables_dir, "table_14_phase6_calibration_report.csv"),
    file.path(phase7_tables_dir, "table_15_phase6_caveats_and_recommendations.csv"),
    file.path(phase7_tables_dir, "table_16_phase6_feature_report.csv")
  )
)

key_tables[, source_exists := file.exists(source_file)]
key_tables[, copy_success := file.copy(source_file, destination_file, overwrite = TRUE)]

key_tables

fwrite(
  key_tables,
  file.path(phase7_dir, "phase7_key_tables_copy_check.csv")
)


# Step 6: Define key report-ready figures to copy
key_figures <- data.table(
  section = c(
    "descriptive_results",
    "descriptive_results",
    "spatial_results",
    "spatial_results",
    "spatial_results",
    "regional_results",
    "moran_results",
    "moran_results",
    "ml_results",
    "ml_results",
    "ml_results"
  ),
  source_file = c(
    file.path("phase4_outputs", "phase4_fig_yearly_resistance_trends_filtered.png"),
    file.path("phase4_outputs", "phase4_fig_top_country_resistance_ordered.png"),
    file.path("phase5b_outputs", "phase5b_fig_choropleth_all_antibiotics.png"),
    file.path("phase5b_outputs", "phase5b_fig_choropleth_cefepime.png"),
    file.path("phase5b_outputs", "phase5b_fig_choropleth_meropenem.png"),
    file.path("phase5c_outputs", "phase5c_fig_temporal_trend_by_region_n500.png"),
    file.path("phase5d_b_outputs", "phase5d_fig_moran_scatter_k4.png"),
    file.path("phase5e_outputs", "phase5e_fig_morans_i_summary.png"),
    file.path("phase6e_outputs", "phase6e_fig_final_auc_comparison.png"),
    file.path("phase6e_outputs", "phase6e_fig_auc_delta_rf_minus_logistic.png"),
    file.path("phase6e_outputs", "copied_phase6d_fig_calibration_error.png")
  ),
  destination_file = c(
    file.path(phase7_figures_dir, "figure_01_phase4_yearly_resistance_trends_filtered.png"),
    file.path(phase7_figures_dir, "figure_02_phase4_top_country_resistance_ordered.png"),
    file.path(phase7_figures_dir, "figure_03_phase5b_choropleth_all_antibiotics.png"),
    file.path(phase7_figures_dir, "figure_04_phase5b_choropleth_cefepime.png"),
    file.path(phase7_figures_dir, "figure_05_phase5b_choropleth_meropenem.png"),
    file.path(phase7_figures_dir, "figure_06_phase5c_temporal_trend_by_region_n500.png"),
    file.path(phase7_figures_dir, "figure_07_phase5d_moran_scatter_k4.png"),
    file.path(phase7_figures_dir, "figure_08_phase5e_morans_i_summary.png"),
    file.path(phase7_figures_dir, "figure_09_phase6e_final_auc_comparison.png"),
    file.path(phase7_figures_dir, "figure_10_phase6e_auc_delta_rf_minus_logistic.png"),
    file.path(phase7_figures_dir, "figure_11_phase6e_calibration_error.png")
  )
)

key_figures[, source_exists := file.exists(source_file)]
key_figures[, copy_success := file.copy(source_file, destination_file, overwrite = TRUE)]

key_figures

fwrite(
  key_figures,
  file.path(phase7_dir, "phase7_key_figures_copy_check.csv")
)


# Step 7: Read selected report tables for summary text
safe_fread <- function(path) {
  if (file.exists(path)) {
    return(fread(path))
  }
  data.table()
}

overall_antibiotic_summary <- safe_fread(
  file.path("phase4_outputs", "phase4_overall_antibiotic_summary.csv")
)

spatial_summary <- safe_fread(
  file.path("phase5e_outputs", "phase5e_country_level_summary.csv")
)

moran_primary <- safe_fread(
  file.path("phase5e_outputs", "phase5e_primary_morans_i_k4_report.csv")
)

ml_card <- safe_fread(
  file.path("phase6e_outputs", "phase6e_selected_model_card.csv")
)

ml_performance <- safe_fread(
  file.path("phase6e_outputs", "phase6e_model_performance_report_table.csv")
)


# Step 8: Final project narrative
final_project_narrative <- c(
  "Final Integrated Project Narrative",
  "Project: Vivli AMR / Pfizer ATLAS Dataset",
  "Scope: Predicting and describing antimicrobial resistance in Klebsiella pneumoniae",
  "",
  "Dataset and audit:",
  "The ATLAS/Vivli dataset was analysed at isolate level. The raw dataset contained 1,011,168 isolate rows and 127 columns spanning 2004–2024.",
  "Rows represented isolates, not patients. Antimicrobial susceptibility interpretation columns were cleaned to Susceptible, Intermediate, and Resistant.",
  "",
  "Scope selection:",
  "The final organism-antibiotic scope was Klebsiella pneumoniae with Cefepime, Ceftazidime, and Meropenem.",
  "This scope was selected because these organism-antibiotic pairs had adequate records, country coverage, year coverage, and clinically meaningful resistance patterns.",
  "",
  "Descriptive findings:",
  "Resistance was high for Cefepime and Ceftazidime and lower but clinically important for Meropenem.",
  "Temporal descriptive plots used sample-size filtering to avoid over-interpreting sparse early-year antibiotic strata.",
  "",
  "Spatial findings:",
  "Country-level choropleth maps and ranking tables were produced for spatial-ready countries.",
  "Regional summaries used World Bank region as the primary regional grouping variable.",
  "Moran's I analysis used symmetrised k-nearest-neighbour spatial weights because queen contiguity produced disconnected graphs and zero-neighbour units.",
  "Positive spatial autocorrelation was observed for resistance to all three antibiotics, strongest for Cefepime and Ceftazidime and weaker for Meropenem.",
  "",
  "Machine-learning findings:",
  "The ML prediction unit was the isolate-antibiotic observation.",
  "The primary outcome was Resistant versus Susceptible plus Intermediate.",
  "Temporal validation used training years 2004–2018, validation years 2019–2021, and test years 2022–2024.",
  "Logistic regression and Random Forest models were compared.",
  "Random Forest did not improve temporal test discrimination compared with logistic regression, so logistic regression was selected as the primary model due to comparable or slightly better AUC, greater interpretability, and simpler reporting.",
  "",
  "Final model:",
  "Primary ML model: pooled logistic regression across Cefepime, Ceftazidime, and Meropenem.",
  "Random Forest role: flexible benchmark and sensitivity model, not primary model.",
  "",
  "Limitations:",
  "The dataset is isolate-level, not patient-level.",
  "The analysis is based on retrospective surveillance data and temporal hold-out validation, not prospective external validation.",
  "Country is an important predictor, so deployment to countries with sparse or no historical data should be treated cautiously.",
  "Spatial and regional results describe patterns among available and spatial-ready countries, not necessarily fully representative regional prevalence.",
  "Random Forest variable importance should not be interpreted causally, especially because impurity-based importance can favour high-cardinality predictors such as country."
)

writeLines(
  final_project_narrative,
  con = file.path(phase7_text_dir, "phase7_final_project_narrative.txt")
)


# Step 9: Methods summary
methods_summary <- data.table(
  section = c(
    "Data unit",
    "Data cleaning",
    "Scope selection",
    "Primary descriptive outcome",
    "Spatial visualisation",
    "Regional analysis",
    "Moran's I",
    "ML prediction unit",
    "ML primary outcome",
    "ML validation",
    "ML model comparison",
    "Final model selection"
  ),
  method_summary = c(
    "Analyses used isolate-level ATLAS/Vivli rows; the dataset was not patient-level.",
    "Blank or invalid interpretation values were standardised; susceptibility interpretations were cleaned to Susceptible, Intermediate, and Resistant.",
    "Feasible organism-antibiotic pairs were screened by interpretation count, country coverage, year coverage, and outcome variation.",
    "Resistance percentage was defined as Resistant divided by Susceptible + Intermediate + Resistant.",
    "Country-level choropleths used spatial-ready countries with adequate interpretation records and successful map joins.",
    "World Bank region was used as the primary regional grouping variable.",
    "Moran's I used symmetrised k-nearest-neighbour weights, k = 4 primary and k = 6 sensitivity; queen contiguity was not used as primary due to disconnected graphs.",
    "The ML prediction unit was the isolate-antibiotic observation.",
    "The primary ML outcome was Resistant versus Susceptible plus Intermediate.",
    "Temporal validation used train 2004–2018, validation 2019–2021, and test 2022–2024.",
    "Baseline logistic regression was compared with Random Forest benchmark models.",
    "Pooled logistic regression was selected as the primary model because Random Forest did not improve temporal test discrimination and logistic regression was simpler, more interpretable, and easier to report."
  )
)

methods_summary

fwrite(
  methods_summary,
  file.path(phase7_tables_dir, "table_17_methods_summary.csv")
)

writeLines(
  paste0(methods_summary$section, ": ", methods_summary$method_summary),
  con = file.path(phase7_text_dir, "phase7_methods_summary.txt")
)


# Step 10: Key results summary
key_results_summary <- data.table(
  result_domain = c(
    "Dataset",
    "Scope",
    "Descriptive resistance",
    "Spatial feasibility",
    "Moran's I",
    "ML selected model",
    "ML test AUC",
    "ML benchmark conclusion"
  ),
  final_result = c(
    "Raw dataset contained 1,011,168 isolate rows, 127 columns, and years 2004–2024.",
    "Final scope: Klebsiella pneumoniae with Cefepime, Ceftazidime, and Meropenem.",
    "Cefepime and Ceftazidime showed high resistance; Meropenem was lower overall but clinically important.",
    "Country-level spatial visualisation was feasible for all three selected antibiotics among spatial-ready countries.",
    "Primary kNN Moran's I indicated positive spatial autocorrelation for all three antibiotics.",
    "Primary ML model: pooled logistic regression.",
    "Selected pooled logistic regression temporal test AUC: 0.7619.",
    "Random Forest did not improve temporal test discrimination compared with logistic regression."
  )
)

key_results_summary

fwrite(
  key_results_summary,
  file.path(phase7_tables_dir, "table_18_key_results_summary.csv")
)

writeLines(
  paste0(key_results_summary$result_domain, ": ", key_results_summary$final_result),
  con = file.path(phase7_text_dir, "phase7_key_results_summary.txt")
)


# Step 11: Limitations summary
limitations_summary <- data.table(
  limitation_domain = c(
    "Data unit",
    "Representativeness",
    "Temporal validation",
    "Spatial interpretation",
    "Regional interpretation",
    "Feature availability",
    "ML deployment",
    "Random Forest interpretation"
  ),
  limitation = c(
    "The dataset is isolate-level, not patient-level; patient-level repeated observations cannot be assessed.",
    "The dataset reflects surveillance data and may not represent population-level resistance prevalence.",
    "ML evaluation used retrospective temporal hold-out validation, not prospective external validation.",
    "Spatial autocorrelation is exploratory and should not be interpreted as causal evidence of spatial transmission.",
    "Regional summaries are based on spatial-ready countries and may not represent all countries in a region.",
    "MIC values, interpretation columns, resistance gene markers, State, Study, and identifiers were excluded from the primary model.",
    "Model use in countries with sparse or unseen historical data should be treated cautiously.",
    "Random Forest impurity-based variable importance can be biased toward high-cardinality categorical predictors such as country."
  )
)

limitations_summary

fwrite(
  limitations_summary,
  file.path(phase7_tables_dir, "table_19_limitations_summary.csv")
)

writeLines(
  paste0(limitations_summary$limitation_domain, ": ", limitations_summary$limitation),
  con = file.path(phase7_text_dir, "phase7_limitations_summary.txt")
)


# Step 12: Final figure inventory
phase7_figure_inventory <- key_figures[
  ,
  .(
    figure_id = basename(destination_file),
    section,
    source_file,
    copied_file = destination_file,
    source_exists,
    copy_success
  )
][order(section, figure_id)]

phase7_figure_inventory

fwrite(
  phase7_figure_inventory,
  file.path(phase7_dir, "phase7_final_figures_inventory.csv")
)


# Step 13: Final table inventory
phase7_table_inventory <- key_tables[
  ,
  .(
    table_id = basename(destination_file),
    section,
    source_file,
    copied_file = destination_file,
    source_exists,
    copy_success
  )
][order(section, table_id)]

extra_tables <- data.table(
  table_id = c(
    "table_17_methods_summary.csv",
    "table_18_key_results_summary.csv",
    "table_19_limitations_summary.csv"
  ),
  section = c(
    "methods",
    "key_results",
    "limitations"
  ),
  source_file = NA_character_,
  copied_file = file.path(
    phase7_tables_dir,
    c(
      "table_17_methods_summary.csv",
      "table_18_key_results_summary.csv",
      "table_19_limitations_summary.csv"
    )
  ),
  source_exists = TRUE,
  copy_success = TRUE
)

phase7_table_inventory <- rbindlist(
  list(
    phase7_table_inventory,
    extra_tables
  ),
  fill = TRUE
)

phase7_table_inventory

fwrite(
  phase7_table_inventory,
  file.path(phase7_dir, "phase7_final_tables_inventory.csv")
)


# Step 14: Final text inventory
phase7_text_inventory <- data.table(
  text_file = list.files(phase7_text_dir),
  path = file.path(phase7_text_dir, list.files(phase7_text_dir))
)

phase7_text_inventory

fwrite(
  phase7_text_inventory,
  file.path(phase7_dir, "phase7_final_text_inventory.csv")
)


# Step 15: Final reporting package checklist
phase7_reporting_package_checklist <- data.table(
  item = c(
    "phase_folder_check_created",
    "required_file_check_created",
    "key_tables_copied",
    "key_figures_copied",
    "final_project_narrative_created",
    "methods_summary_created",
    "key_results_summary_created",
    "limitations_summary_created",
    "figure_inventory_created",
    "table_inventory_created",
    "text_inventory_created"
  ),
  status = c(
    file.exists(file.path(phase7_dir, "phase7_phase_folder_check.csv")),
    file.exists(file.path(phase7_dir, "phase7_required_file_check.csv")),
    all(key_tables$copy_success == TRUE),
    all(key_figures$copy_success == TRUE),
    file.exists(file.path(phase7_text_dir, "phase7_final_project_narrative.txt")),
    file.exists(file.path(phase7_text_dir, "phase7_methods_summary.txt")),
    file.exists(file.path(phase7_text_dir, "phase7_key_results_summary.txt")),
    file.exists(file.path(phase7_text_dir, "phase7_limitations_summary.txt")),
    file.exists(file.path(phase7_dir, "phase7_final_figures_inventory.csv")),
    file.exists(file.path(phase7_dir, "phase7_final_tables_inventory.csv")),
    file.exists(file.path(phase7_dir, "phase7_final_text_inventory.csv"))
  )
)

phase7_reporting_package_checklist

fwrite(
  phase7_reporting_package_checklist,
  file.path(phase7_dir, "phase7_reporting_package_checklist.csv")
)


# Step 16: Output file list
phase7_output_file_list <- data.table(
  file_path = list.files(
    phase7_dir,
    recursive = TRUE,
    full.names = FALSE
  )
)

phase7_output_file_list

fwrite(
  phase7_output_file_list,
  file.path(phase7_dir, "phase7_output_file_list.csv")
)

list.files(phase7_dir, recursive = TRUE)