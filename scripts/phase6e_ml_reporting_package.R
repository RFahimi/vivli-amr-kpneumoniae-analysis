# Phase 6E: ML Reporting Package
# Project: Vivli AMR / Pfizer ATLAS Dataset
# Scope:
# Klebsiella pneumoniae resistance to Cefepime, Ceftazidime, and Meropenem
#
# Purpose:
# Create report-ready ML summary tables, selected model card, final caveats,
# and copy key figures for final reporting.
#
# This phase does NOT fit new models.


# Step 1: Load packages
library(data.table)
library(stringr)
library(ggplot2)


# Step 2: Create dedicated Phase 6E output folder
phase6e_dir <- "phase6e_outputs"
dir.create(phase6e_dir, showWarnings = FALSE)


# Step 3: Define input folders
phase6a_dir <- "phase6a_outputs"
phase6b_dir <- "phase6b_outputs"
phase6c_dir <- "phase6c_outputs"
phase6d_dir <- "phase6d_outputs"


# Step 4: Required input file check
input_file_check <- data.table(
  input_name = c(
    "Phase 6A ML problem definition",
    "Phase 6A feature catalog",
    "Phase 6A outcome balance by antibiotic",
    "Phase 6A temporal split feasibility",
    "Phase 6B logistic test performance summary",
    "Phase 6B logistic test calibration table",
    "Phase 6C random forest test performance summary",
    "Phase 6C random forest variable importance",
    "Phase 6D integrated test AUC combined",
    "Phase 6D logistic vs RF comparison",
    "Phase 6D calibration summary",
    "Phase 6D model selection table",
    "Phase 6D primary model decision",
    "Phase 6D thresholds combined",
    "Phase 6D leakage summary",
    "Phase 6D narrative summary"
  ),
  file_path = c(
    file.path(phase6a_dir, "phase6a_ml_problem_definition.csv"),
    file.path(phase6a_dir, "phase6a_feature_catalog.csv"),
    file.path(phase6a_dir, "phase6a_outcome_balance_by_antibiotic.csv"),
    file.path(phase6a_dir, "phase6a_temporal_split_feasibility.csv"),
    file.path(phase6b_dir, "phase6b_test_performance_summary.csv"),
    file.path(phase6b_dir, "phase6b_test_calibration_table.csv"),
    file.path(phase6c_dir, "phase6c_test_performance_summary.csv"),
    file.path(phase6c_dir, "phase6c_variable_importance.csv"),
    file.path(phase6d_dir, "phase6d_test_auc_combined.csv"),
    file.path(phase6d_dir, "phase6d_logistic_vs_rf_comparison.csv"),
    file.path(phase6d_dir, "phase6d_calibration_summary.csv"),
    file.path(phase6d_dir, "phase6d_model_selection_table.csv"),
    file.path(phase6d_dir, "phase6d_primary_model_decision.csv"),
    file.path(phase6d_dir, "phase6d_thresholds_combined.csv"),
    file.path(phase6d_dir, "phase6d_leakage_summary.csv"),
    file.path(phase6d_dir, "phase6d_narrative_summary.txt")
  )
)

input_file_check[, file_exists := file.exists(file_path)]

input_file_check

fwrite(
  input_file_check,
  file.path(phase6e_dir, "phase6e_input_file_check.csv")
)

if (any(input_file_check$file_exists == FALSE)) {
  stop("One or more required Phase 6 input files are missing.")
}


# Step 5: Read input tables
phase6a_ml_problem_definition <- fread(
  file.path(phase6a_dir, "phase6a_ml_problem_definition.csv")
)

phase6a_feature_catalog <- fread(
  file.path(phase6a_dir, "phase6a_feature_catalog.csv")
)

phase6a_outcome_balance_by_antibiotic <- fread(
  file.path(phase6a_dir, "phase6a_outcome_balance_by_antibiotic.csv")
)

phase6a_temporal_split_feasibility <- fread(
  file.path(phase6a_dir, "phase6a_temporal_split_feasibility.csv")
)

phase6b_test_performance_summary <- fread(
  file.path(phase6b_dir, "phase6b_test_performance_summary.csv")
)

phase6b_test_calibration_table <- fread(
  file.path(phase6b_dir, "phase6b_test_calibration_table.csv")
)

phase6c_test_performance_summary <- fread(
  file.path(phase6c_dir, "phase6c_test_performance_summary.csv")
)

phase6c_variable_importance <- fread(
  file.path(phase6c_dir, "phase6c_variable_importance.csv")
)

phase6d_test_auc_combined <- fread(
  file.path(phase6d_dir, "phase6d_test_auc_combined.csv")
)

phase6d_logistic_vs_rf_comparison <- fread(
  file.path(phase6d_dir, "phase6d_logistic_vs_rf_comparison.csv")
)

phase6d_calibration_summary <- fread(
  file.path(phase6d_dir, "phase6d_calibration_summary.csv")
)

phase6d_model_selection_table <- fread(
  file.path(phase6d_dir, "phase6d_model_selection_table.csv")
)

phase6d_primary_model_decision <- fread(
  file.path(phase6d_dir, "phase6d_primary_model_decision.csv")
)

phase6d_thresholds_combined <- fread(
  file.path(phase6d_dir, "phase6d_thresholds_combined.csv")
)

phase6d_leakage_summary <- fread(
  file.path(phase6d_dir, "phase6d_leakage_summary.csv")
)


# Step 6: Helper functions
get_decision <- function(component_name) {
  out <- phase6d_primary_model_decision[
    decision_component == component_name,
    decision
  ]
  if (length(out) == 0) {
    return(NA_character_)
  }
  out[1]
}

get_problem_definition <- function(component_name) {
  out <- phase6a_ml_problem_definition[
    component == component_name,
    definition
  ]
  if (length(out) == 0) {
    return(NA_character_)
  }
  out[1]
}


# Step 7: Selected primary model card
selected_model_id <- get_decision("primary_model_id")
selected_model_family <- get_decision("primary_model_family")
selected_model_scope <- get_decision("primary_model_scope")
selected_test_auc <- get_decision("primary_test_auc")

selected_model_test_row <- phase6d_test_auc_combined[
  model_id == selected_model_id
]

selected_model_threshold_row <- phase6d_thresholds_combined[
  model_id == selected_model_id
]

phase6e_selected_model_card <- data.table(
  item = c(
    "project_scope",
    "prediction_unit",
    "organism_scope",
    "antibiotic_scope",
    "primary_outcome",
    "selected_model_id",
    "selected_model_family",
    "selected_model_scope",
    "selected_model_test_auc",
    "selected_model_test_brier_score",
    "selected_model_test_log_loss",
    "selected_threshold",
    "test_rows",
    "test_positive_cases",
    "test_prevalence_pct",
    "test_sensitivity",
    "test_specificity",
    "test_ppv",
    "test_npv",
    "test_f1",
    "model_selection_statement"
  ),
  value = c(
    "Klebsiella pneumoniae resistance to Cefepime, Ceftazidime, and Meropenem",
    get_problem_definition("prediction_unit"),
    get_problem_definition("organism_scope"),
    get_problem_definition("antibiotic_scope"),
    get_problem_definition("primary_outcome"),
    selected_model_id,
    selected_model_family,
    selected_model_scope,
    as.character(selected_test_auc),
    as.character(selected_model_test_row$brier_score),
    as.character(selected_model_test_row$log_loss),
    as.character(selected_model_threshold_row$selected_threshold),
    as.character(selected_model_test_row$n),
    as.character(selected_model_test_row$positives),
    as.character(selected_model_test_row$prevalence_pct),
    as.character(selected_model_test_row$sensitivity),
    as.character(selected_model_test_row$specificity),
    as.character(selected_model_test_row$ppv),
    as.character(selected_model_test_row$npv),
    as.character(selected_model_test_row$f1),
    "Random Forest did not improve temporal test discrimination compared with logistic regression, so logistic regression was selected as the primary model due to comparable or slightly better AUC, greater interpretability, and simpler reporting."
  )
)

phase6e_selected_model_card

fwrite(
  phase6e_selected_model_card,
  file.path(phase6e_dir, "phase6e_selected_model_card.csv")
)


# Step 8: Report-ready model performance table
phase6e_model_performance_report_table <- copy(phase6d_test_auc_combined)

phase6e_model_performance_report_table[, report_model_label := fifelse(
  model_family == "logistic_regression",
  str_replace(model_id, "antibiotic_specific_logistic_", "Logistic: "),
  str_replace(model_id, "antibiotic_specific_random_forest_", "Random Forest: ")
)]

phase6e_model_performance_report_table[
  model_id == "pooled_logistic_primary",
  report_model_label := "Logistic: pooled"
]

phase6e_model_performance_report_table[
  model_id == "pooled_random_forest_primary",
  report_model_label := "Random Forest: pooled"
]

phase6e_model_performance_report_table <- phase6e_model_performance_report_table[
  ,
  .(
    phase,
    model_family,
    report_model_label,
    model_id,
    model_scope_type,
    target_scope,
    n,
    positives,
    prevalence_pct,
    auc,
    brier_score,
    log_loss,
    threshold,
    sensitivity,
    specificity,
    ppv,
    npv,
    f1
  )
][order(target_scope, model_family)]

phase6e_model_performance_report_table

fwrite(
  phase6e_model_performance_report_table,
  file.path(phase6e_dir, "phase6e_model_performance_report_table.csv")
)


# Step 9: Final model selection summary
phase6e_final_model_selection_summary <- copy(
  phase6d_model_selection_table
)

phase6e_final_model_selection_summary[, final_reporting_wording := 
                                        "Logistic regression is preferred as the primary model because Random Forest did not improve temporal test discrimination and logistic regression is simpler, more interpretable, and easier to report."
]

phase6e_final_model_selection_summary

fwrite(
  phase6e_final_model_selection_summary,
  file.path(phase6e_dir, "phase6e_final_model_selection_summary.csv")
)


# Step 10: Calibration summary for reporting
phase6e_calibration_report_table <- copy(
  phase6d_calibration_summary
)

phase6e_calibration_report_table[, calibration_note := fifelse(
  model_family == "random_forest",
  "Random Forest calibration was reviewed as benchmark behaviour; lower calibration error alone did not override model selection because temporal AUC did not improve.",
  "Logistic calibration was reviewed alongside temporal AUC and interpretability."
)]

phase6e_calibration_report_table <- phase6e_calibration_report_table[
  order(target_scope, mean_abs_calibration_error_pct)
]

phase6e_calibration_report_table

fwrite(
  phase6e_calibration_report_table,
  file.path(phase6e_dir, "phase6e_calibration_report_table.csv")
)


# Step 11: Selected model threshold report
phase6e_selected_threshold_report <- phase6d_thresholds_combined[
  model_id %in% phase6d_model_selection_table$selected_model_id
][
  order(target_scope)
]

phase6e_selected_threshold_report

fwrite(
  phase6e_selected_threshold_report,
  file.path(phase6e_dir, "phase6e_selected_threshold_report.csv")
)


# Step 12: Feature and leakage report
phase6e_feature_report <- phase6a_feature_catalog[
  ,
  .(
    variable,
    role,
    include_in_primary_model,
    rationale
  )
]

phase6e_feature_report

fwrite(
  phase6e_feature_report,
  file.path(phase6e_dir, "phase6e_feature_report.csv")
)

phase6e_leakage_report <- copy(phase6d_leakage_summary)

phase6e_leakage_report

fwrite(
  phase6e_leakage_report,
  file.path(phase6e_dir, "phase6e_leakage_report.csv")
)


# Step 13: Outcome and temporal split report
phase6e_outcome_balance_report <- copy(
  phase6a_outcome_balance_by_antibiotic
)

phase6e_temporal_split_report <- copy(
  phase6a_temporal_split_feasibility
)

fwrite(
  phase6e_outcome_balance_report,
  file.path(phase6e_dir, "phase6e_outcome_balance_report.csv")
)

fwrite(
  phase6e_temporal_split_report,
  file.path(phase6e_dir, "phase6e_temporal_split_report.csv")
)


# Step 14: Random Forest variable importance report
phase6e_rf_importance_report <- copy(phase6c_variable_importance)

phase6e_rf_importance_report[, importance_rank := frank(
  -importance,
  ties.method = "first"
), by = model_id]

phase6e_rf_importance_top5 <- phase6e_rf_importance_report[
  importance_rank <= 5
][order(model_id, importance_rank)]

phase6e_rf_importance_note <- data.table(
  note_type = c(
    "interpretation_caution",
    "reporting_position"
  ),
  note = c(
    "Random Forest impurity-based importance can be biased for categorical variables with many levels, especially country; these rankings should not be interpreted causally.",
    "Random Forest variable importance is reported as benchmark insight only, not as the basis for selecting the primary model."
  )
)

fwrite(
  phase6e_rf_importance_top5,
  file.path(phase6e_dir, "phase6e_rf_importance_top5.csv")
)

fwrite(
  phase6e_rf_importance_note,
  file.path(phase6e_dir, "phase6e_rf_importance_note.csv")
)


# Step 15: Caveats and recommendations
phase6e_caveats_and_recommendations <- data.table(
  category = c(
    "model_selection",
    "model_selection",
    "calibration",
    "validation",
    "data_unit",
    "feature_scope",
    "generalisation",
    "random_forest_importance",
    "recommended_primary_result",
    "recommended_benchmark_result"
  ),
  statement = c(
    "Do not describe Random Forest as a worse model; state that it did not improve temporal test discrimination over logistic regression.",
    "Logistic regression is preferred because it achieved comparable or slightly better temporal test AUC while being simpler, more interpretable, and easier to report.",
    "Random Forest showed lower mean absolute calibration error in several scopes, but this did not override the absence of temporal AUC improvement.",
    "The evaluation is based on retrospective temporal hold-out validation, not prospective external validation.",
    "The dataset is isolate-level, not patient-level; repeated patient-level observations cannot be assessed.",
    "MIC values, interpretation columns, resistance gene markers, State, Study, and identifiers were excluded from the primary model feature set.",
    "Country is an important predictor, so deployment to countries with no or limited historical data should be treated cautiously.",
    "Impurity-based Random Forest importance can be biased toward high-cardinality categorical variables.",
    "Primary ML result: pooled logistic regression across Cefepime, Ceftazidime, and Meropenem.",
    "Benchmark ML result: Random Forest was evaluated as a flexible benchmark but not selected as primary."
  )
)

phase6e_caveats_and_recommendations

fwrite(
  phase6e_caveats_and_recommendations,
  file.path(phase6e_dir, "phase6e_caveats_and_recommendations.csv")
)


# Step 16: Create final ML summary figure
fig_phase6e_final_auc <- ggplot(
  phase6e_model_performance_report_table,
  aes(
    x = reorder(report_model_label, auc),
    y = auc,
    shape = model_family
  )
) +
  geom_point(size = 3) +
  coord_flip() +
  labs(
    title = "Final ML model comparison",
    subtitle = "Temporal test-set AUC for logistic regression and Random Forest benchmarks",
    x = "Model",
    y = "Test AUC",
    shape = "Model family",
    caption = "Source: Vivli AMR / Pfizer ATLAS dataset, 2004–2024"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  )

fig_phase6e_final_auc

ggsave(
  filename = file.path(phase6e_dir, "phase6e_fig_final_auc_comparison.png"),
  plot = fig_phase6e_final_auc,
  width = 10,
  height = 7,
  dpi = 300
)


fig_phase6e_auc_delta <- ggplot(
  phase6d_logistic_vs_rf_comparison,
  aes(
    x = reorder(target_scope, auc_delta_rf_minus_logistic),
    y = auc_delta_rf_minus_logistic
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Random Forest did not improve temporal test AUC",
    subtitle = "AUC difference is RF minus logistic regression; negative values favour logistic regression",
    x = "Model scope",
    y = "AUC difference: RF - logistic",
    caption = "Source: Vivli AMR / Pfizer ATLAS dataset, 2004–2024"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 11)
  )

fig_phase6e_auc_delta

ggsave(
  filename = file.path(phase6e_dir, "phase6e_fig_auc_delta_rf_minus_logistic.png"),
  plot = fig_phase6e_auc_delta,
  width = 9,
  height = 6,
  dpi = 300
)


# Step 17: Copy key existing Phase 6 figures into Phase 6E package
key_figures_to_copy <- data.table(
  source_file = c(
    file.path(phase6b_dir, "phase6b_fig_test_calibration.png"),
    file.path(phase6c_dir, "phase6c_fig_test_calibration.png"),
    file.path(phase6d_dir, "phase6d_fig_auc_comparison.png"),
    file.path(phase6d_dir, "phase6d_fig_auc_delta_rf_minus_logistic.png"),
    file.path(phase6d_dir, "phase6d_fig_calibration_error.png")
  ),
  destination_file = c(
    file.path(phase6e_dir, "copied_phase6b_fig_test_calibration.png"),
    file.path(phase6e_dir, "copied_phase6c_fig_test_calibration.png"),
    file.path(phase6e_dir, "copied_phase6d_fig_auc_comparison.png"),
    file.path(phase6e_dir, "copied_phase6d_fig_auc_delta_rf_minus_logistic.png"),
    file.path(phase6e_dir, "copied_phase6d_fig_calibration_error.png")
  )
)

key_figures_to_copy[, source_exists := file.exists(source_file)]

key_figures_to_copy[, copy_success := file.copy(
  from = source_file,
  to = destination_file,
  overwrite = TRUE
)]

key_figures_to_copy

fwrite(
  key_figures_to_copy,
  file.path(phase6e_dir, "phase6e_copied_figure_check.csv")
)


# Step 18: Narrative summary
narrative_lines <- c(
  "Phase 6E ML Reporting Package",
  "Project: Vivli AMR / Pfizer ATLAS Dataset",
  "Scope: Klebsiella pneumoniae resistance to Cefepime, Ceftazidime, and Meropenem",
  "",
  "Prediction unit: isolate-antibiotic observation.",
  "Primary outcome: Resistant vs Susceptible plus Intermediate.",
  "",
  "Models compared:",
  "1. Logistic regression baseline models from Phase 6B.",
  "2. Random Forest benchmark models from Phase 6C.",
  "",
  "Final model selection statement:",
  "Random Forest did not improve temporal test discrimination compared with logistic regression, so logistic regression was selected as the primary model due to comparable or slightly better AUC, greater interpretability, and simpler reporting.",
  "",
  "Selected primary model:",
  paste0("Model ID: ", selected_model_id),
  paste0("Model family: ", selected_model_family),
  paste0("Model scope: ", selected_model_scope),
  paste0("Temporal test AUC: ", selected_test_auc),
  "",
  "Random Forest role:",
  "Random Forest is retained as a flexible benchmark and sensitivity model, not as the primary model.",
  "",
  "Calibration note:",
  "Random Forest showed lower mean absolute calibration error in some scopes, but did not improve temporal test AUC. Model selection therefore prioritised temporal discrimination, interpretability, parsimony, and reporting clarity.",
  "",
  "Leakage check:",
  "No isolate-level leakage across temporal train, validation, and test splits was detected in Phase 6B or Phase 6C.",
  "",
  "Important reporting caveat:",
  "The difference between logistic regression and Random Forest is small for pooled and Meropenem models; this should be framed as a model preference rather than overwhelming superiority.",
  ""
)

writeLines(
  narrative_lines,
  con = file.path(phase6e_dir, "phase6e_narrative_summary.txt")
)


# Step 19: Final output file list
phase6e_output_file_list <- data.table(
  file_name = list.files(phase6e_dir)
)

fwrite(
  phase6e_output_file_list,
  file.path(phase6e_dir, "phase6e_output_file_list.csv")
)

# Rewrite after output file list exists, so the list includes itself.
phase6e_output_file_list <- data.table(
  file_name = list.files(phase6e_dir)
)

fwrite(
  phase6e_output_file_list,
  file.path(phase6e_dir, "phase6e_output_file_list.csv")
)

phase6e_output_file_list

list.files(phase6e_dir)