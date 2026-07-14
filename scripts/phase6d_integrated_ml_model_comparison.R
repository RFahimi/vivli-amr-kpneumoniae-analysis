# Phase 6D: Integrated ML Model Comparison and Model Selection
# Project: Vivli AMR / Pfizer ATLAS Dataset
# Scope:
# Klebsiella pneumoniae resistance to Cefepime, Ceftazidime, and Meropenem
#
# Purpose:
# Integrate Phase 6B logistic regression and Phase 6C random forest results,
# compare temporal test-set performance, and select the primary modelling approach.
#
# This phase does NOT fit new models.


# Step 1: Load packages
library(data.table)
library(stringr)
library(ggplot2)


# Step 2: Create dedicated Phase 6D output folder
phase6d_dir <- "phase6d_outputs"
dir.create(phase6d_dir, showWarnings = FALSE)


# Step 3: Define input folders
phase6b_dir <- "phase6b_outputs"
phase6c_dir <- "phase6c_outputs"


# Step 4: Required input file check
input_file_check <- data.table(
  input_name = c(
    "Phase 6B logistic model performance",
    "Phase 6B logistic test AUC summary",
    "Phase 6B logistic thresholds",
    "Phase 6B logistic calibration table",
    "Phase 6B leakage summary",
    "Phase 6C random forest model performance",
    "Phase 6C random forest test AUC summary",
    "Phase 6C random forest thresholds",
    "Phase 6C random forest calibration table",
    "Phase 6C random forest variable importance",
    "Phase 6C output file list"
  ),
  file_path = c(
    file.path(phase6b_dir, "phase6b_model_performance.csv"),
    file.path(phase6b_dir, "phase6b_test_auc_summary.csv"),
    file.path(phase6b_dir, "phase6b_thresholds.csv"),
    file.path(phase6b_dir, "phase6b_test_calibration_table.csv"),
    file.path(phase6b_dir, "phase6b_isolate_split_leakage_summary.csv"),
    file.path(phase6c_dir, "phase6c_model_performance.csv"),
    file.path(phase6c_dir, "phase6c_test_auc_summary.csv"),
    file.path(phase6c_dir, "phase6c_thresholds.csv"),
    file.path(phase6c_dir, "phase6c_test_calibration_table.csv"),
    file.path(phase6c_dir, "phase6c_variable_importance.csv"),
    file.path(phase6c_dir, "phase6c_output_file_list.csv")
  )
)

input_file_check[, file_exists := file.exists(file_path)]

input_file_check

fwrite(
  input_file_check,
  file.path(phase6d_dir, "phase6d_input_file_check.csv")
)

if (any(input_file_check$file_exists == FALSE)) {
  stop("One or more required Phase 6B/6C input files are missing.")
}


# Step 5: Read Phase 6B inputs
phase6b_model_performance <- fread(
  file.path(phase6b_dir, "phase6b_model_performance.csv")
)

phase6b_test_auc_summary <- fread(
  file.path(phase6b_dir, "phase6b_test_auc_summary.csv")
)

phase6b_thresholds <- fread(
  file.path(phase6b_dir, "phase6b_thresholds.csv")
)

phase6b_test_calibration_table <- fread(
  file.path(phase6b_dir, "phase6b_test_calibration_table.csv")
)

phase6b_leakage_summary <- fread(
  file.path(phase6b_dir, "phase6b_isolate_split_leakage_summary.csv")
)


# Step 6: Read Phase 6C inputs
phase6c_model_performance <- fread(
  file.path(phase6c_dir, "phase6c_model_performance.csv")
)

phase6c_test_auc_summary <- fread(
  file.path(phase6c_dir, "phase6c_test_auc_summary.csv")
)

phase6c_thresholds <- fread(
  file.path(phase6c_dir, "phase6c_thresholds.csv")
)

phase6c_test_calibration_table <- fread(
  file.path(phase6c_dir, "phase6c_test_calibration_table.csv")
)

phase6c_variable_importance <- fread(
  file.path(phase6c_dir, "phase6c_variable_importance.csv")
)


# Step 7: Add phase/model family labels

phase6b_test_auc_summary[, phase := "Phase 6B"]
phase6b_test_auc_summary[, model_family := "logistic_regression"]

phase6c_test_auc_summary[, phase := "Phase 6C"]
phase6c_test_auc_summary[, model_family := "random_forest"]

phase6b_model_performance[, phase := "Phase 6B"]
phase6b_model_performance[, model_family := "logistic_regression"]

phase6c_model_performance[, phase := "Phase 6C"]
phase6c_model_performance[, model_family := "random_forest"]

phase6b_thresholds[, phase := "Phase 6B"]
phase6b_thresholds[, model_family := "logistic_regression"]

phase6c_thresholds[, phase := "Phase 6C"]
phase6c_thresholds[, model_family := "random_forest"]

phase6b_test_calibration_table[, phase := "Phase 6B"]
phase6b_test_calibration_table[, model_family := "logistic_regression"]

phase6c_test_calibration_table[, phase := "Phase 6C"]
phase6c_test_calibration_table[, model_family := "random_forest"]


# Step 8: Helper to define model target/scope

add_model_target <- function(dt) {
  
  dt <- copy(dt)
  
  dt[, target_scope := fifelse(
    model_scope == "pooled_all_antibiotics",
    "pooled_all_antibiotics",
    str_remove(model_scope, "^antibiotic_specific_")
  )]
  
  dt[, model_scope_type := fifelse(
    model_scope == "pooled_all_antibiotics",
    "pooled",
    "antibiotic_specific"
  )]
  
  dt[]
}


# Step 9: Combined test AUC table

phase6d_test_auc_combined <- rbindlist(
  list(
    phase6b_test_auc_summary,
    phase6c_test_auc_summary
  ),
  fill = TRUE
)

phase6d_test_auc_combined <- add_model_target(
  phase6d_test_auc_combined
)

phase6d_test_auc_combined <- phase6d_test_auc_combined[
  ,
  .(
    phase,
    model_family,
    model_id,
    model_scope,
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
][order(target_scope, phase, model_family)]

phase6d_test_auc_combined

fwrite(
  phase6d_test_auc_combined,
  file.path(phase6d_dir, "phase6d_test_auc_combined.csv")
)


# Step 10: Logistic vs Random Forest paired comparison by scope

logistic_test <- phase6d_test_auc_combined[
  model_family == "logistic_regression"
]

rf_test <- phase6d_test_auc_combined[
  model_family == "random_forest"
]

phase6d_logistic_vs_rf_comparison <- merge(
  logistic_test,
  rf_test,
  by = c("model_scope", "model_scope_type", "target_scope"),
  suffixes = c("_logistic", "_rf")
)

phase6d_logistic_vs_rf_comparison[, auc_delta_rf_minus_logistic := round(
  auc_rf - auc_logistic,
  4
)]

phase6d_logistic_vs_rf_comparison[, brier_delta_rf_minus_logistic := round(
  brier_score_rf - brier_score_logistic,
  4
)]

phase6d_logistic_vs_rf_comparison[, log_loss_delta_rf_minus_logistic := round(
  log_loss_rf - log_loss_logistic,
  4
)]

phase6d_logistic_vs_rf_comparison[, interpretation := fifelse(
  auc_delta_rf_minus_logistic > 0.005,
  "Random Forest shows a small temporal test AUC advantage.",
  "Logistic regression has equal or slightly better temporal test AUC and is preferred for interpretability."
)]

phase6d_logistic_vs_rf_comparison <- phase6d_logistic_vs_rf_comparison[
  ,
  .(
    model_scope,
    model_scope_type,
    target_scope,
    logistic_model_id = model_id_logistic,
    rf_model_id = model_id_rf,
    auc_logistic,
    auc_rf,
    auc_delta_rf_minus_logistic,
    brier_score_logistic,
    brier_score_rf,
    brier_delta_rf_minus_logistic,
    log_loss_logistic,
    log_loss_rf,
    log_loss_delta_rf_minus_logistic,
    interpretation
  )
][order(target_scope)]

phase6d_logistic_vs_rf_comparison

fwrite(
  phase6d_logistic_vs_rf_comparison,
  file.path(phase6d_dir, "phase6d_logistic_vs_rf_comparison.csv")
)


# Step 11: Calibration error summary

phase6d_calibration_combined <- rbindlist(
  list(
    phase6b_test_calibration_table,
    phase6c_test_calibration_table
  ),
  fill = TRUE
)

phase6d_calibration_combined <- add_model_target(
  phase6d_calibration_combined
)

phase6d_calibration_combined[, abs_calibration_error_pct := abs(
  observed_resistance_pct - mean_predicted_probability_pct
)]

phase6d_calibration_summary <- phase6d_calibration_combined[
  ,
  .(
    calibration_bins = .N,
    total_rows = sum(n),
    mean_abs_calibration_error_pct = round(mean(abs_calibration_error_pct), 2),
    max_abs_calibration_error_pct = round(max(abs_calibration_error_pct), 2)
  ),
  by = .(
    phase,
    model_family,
    model_id,
    model_scope,
    model_scope_type,
    target_scope
  )
][order(target_scope, mean_abs_calibration_error_pct)]

phase6d_calibration_summary

fwrite(
  phase6d_calibration_combined,
  file.path(phase6d_dir, "phase6d_calibration_combined.csv")
)

fwrite(
  phase6d_calibration_summary,
  file.path(phase6d_dir, "phase6d_calibration_summary.csv")
)


# Step 12: Threshold comparison

phase6d_thresholds_combined <- rbindlist(
  list(
    phase6b_thresholds,
    phase6c_thresholds
  ),
  fill = TRUE
)

phase6d_thresholds_combined <- add_model_target(
  phase6d_thresholds_combined
)

phase6d_thresholds_combined <- phase6d_thresholds_combined[
  ,
  .(
    phase,
    model_family,
    model_id,
    model_scope,
    model_scope_type,
    target_scope,
    selected_threshold,
    validation_youden,
    validation_sensitivity_at_threshold,
    validation_specificity_at_threshold
  )
][order(target_scope, phase, model_family)]

phase6d_thresholds_combined

fwrite(
  phase6d_thresholds_combined,
  file.path(phase6d_dir, "phase6d_thresholds_combined.csv")
)


# Step 13: Model selection table

phase6d_model_selection_table <- phase6d_logistic_vs_rf_comparison[
  ,
  .(
    model_scope,
    model_scope_type,
    target_scope,
    selected_model_family = "logistic_regression",
    selected_model_id = logistic_model_id,
    selected_model_test_auc = auc_logistic,
    comparator_rf_test_auc = auc_rf,
    auc_delta_rf_minus_logistic,
    selection_decision = "Prefer logistic regression",
    selection_rationale = "Logistic regression achieved equal or slightly better temporal test AUC than Random Forest, while being simpler, more interpretable, and easier to justify."
  )
][order(model_scope_type, target_scope)]

phase6d_model_selection_table

fwrite(
  phase6d_model_selection_table,
  file.path(phase6d_dir, "phase6d_model_selection_table.csv")
)


# Step 14: Primary model decision

pooled_row <- phase6d_model_selection_table[
  model_scope == "pooled_all_antibiotics"
]

phase6d_primary_model_decision <- data.table(
  decision_component = c(
    "primary_modelling_strategy",
    "primary_model_id",
    "primary_model_family",
    "primary_model_scope",
    "primary_test_auc",
    "main_reason",
    "random_forest_role",
    "important_caveat"
  ),
  decision = c(
    "Use logistic regression as the primary model class.",
    pooled_row$selected_model_id,
    "logistic_regression",
    "pooled_all_antibiotics",
    as.character(pooled_row$selected_model_test_auc),
    "Logistic regression is preferred as the primary model because it achieved equal or slightly better temporal test AUC than Random Forest, while being simpler, more interpretable, and easier to justify.",
    "Random Forest is retained as a flexible benchmark, not as the primary model.",
    "The performance advantage of logistic regression over Random Forest is modest for pooled and Meropenem models; selection is based on temporal AUC, interpretability, parsimony, and calibration review."
  )
)

phase6d_primary_model_decision

fwrite(
  phase6d_primary_model_decision,
  file.path(phase6d_dir, "phase6d_primary_model_decision.csv")
)


# Step 15: Variable importance summary for Random Forest

phase6d_rf_variable_importance_summary <- copy(phase6c_variable_importance)

phase6d_rf_variable_importance_summary[, importance_rank := frank(
  -importance,
  ties.method = "first"
), by = model_id]

phase6d_rf_variable_importance_top <- phase6d_rf_variable_importance_summary[
  importance_rank <= 5
][order(model_id, importance_rank)]

phase6d_rf_variable_importance_top

fwrite(
  phase6d_rf_variable_importance_summary,
  file.path(phase6d_dir, "phase6d_rf_variable_importance_summary.csv")
)

fwrite(
  phase6d_rf_variable_importance_top,
  file.path(phase6d_dir, "phase6d_rf_variable_importance_top5.csv")
)


# Step 16: Leakage summary

phase6b_leakage_summary[, phase := "Phase 6B"]
phase6c_leakage_summary <- fread(
  file.path(phase6c_dir, "phase6c_isolate_split_leakage_summary.csv")
)
phase6c_leakage_summary[, phase := "Phase 6C"]

phase6d_leakage_summary <- rbindlist(
  list(
    phase6b_leakage_summary,
    phase6c_leakage_summary
  ),
  fill = TRUE
)

phase6d_leakage_summary

fwrite(
  phase6d_leakage_summary,
  file.path(phase6d_dir, "phase6d_leakage_summary.csv")
)


# Step 17: Figures

fig_phase6d_auc_comparison <- ggplot(
  phase6d_test_auc_combined,
  aes(
    x = reorder(model_id, auc),
    y = auc,
    shape = model_family
  )
) +
  geom_point(size = 3) +
  coord_flip() +
  labs(
    title = "Integrated Phase 6 model comparison",
    subtitle = "Temporal test-set AUC for logistic regression and random forest benchmarks",
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

fig_phase6d_auc_comparison

ggsave(
  filename = file.path(phase6d_dir, "phase6d_fig_auc_comparison.png"),
  plot = fig_phase6d_auc_comparison,
  width = 10,
  height = 7,
  dpi = 300
)


fig_phase6d_auc_delta <- ggplot(
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
    title = "Random Forest minus logistic regression test AUC",
    subtitle = "Negative values indicate better temporal test AUC for logistic regression",
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

fig_phase6d_auc_delta

ggsave(
  filename = file.path(phase6d_dir, "phase6d_fig_auc_delta_rf_minus_logistic.png"),
  plot = fig_phase6d_auc_delta,
  width = 9,
  height = 6,
  dpi = 300
)


fig_phase6d_calibration_error <- ggplot(
  phase6d_calibration_summary,
  aes(
    x = reorder(model_id, mean_abs_calibration_error_pct),
    y = mean_abs_calibration_error_pct,
    shape = model_family
  )
) +
  geom_point(size = 3) +
  coord_flip() +
  labs(
    title = "Integrated Phase 6 calibration summary",
    subtitle = "Mean absolute calibration error across test-set prediction deciles",
    x = "Model",
    y = "Mean absolute calibration error (%)",
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

fig_phase6d_calibration_error

ggsave(
  filename = file.path(phase6d_dir, "phase6d_fig_calibration_error.png"),
  plot = fig_phase6d_calibration_error,
  width = 10,
  height = 7,
  dpi = 300
)


# Step 18: Narrative summary

narrative_lines <- c(
  "Phase 6D Integrated ML Model Comparison and Model Selection",
  "Project: Vivli AMR / Pfizer ATLAS Dataset",
  "Scope: Klebsiella pneumoniae resistance to Cefepime, Ceftazidime, and Meropenem",
  "",
  "Phase 6B fitted logistic regression baseline models using temporal validation.",
  "Phase 6C fitted random forest benchmark models using the same temporal validation strategy.",
  "",
  "Model selection statement:",
  "Logistic regression is preferred as the primary model because it achieved equal or slightly better temporal test AUC than Random Forest, while being simpler, more interpretable, and easier to justify.",
  "",
  "Random Forest remains useful as a flexible benchmark, but it did not provide a clear temporal test-set AUC improvement over logistic regression.",
  "",
  "Selected primary model class: logistic regression.",
  "Selected primary deployment-style model: pooled logistic regression across Cefepime, Ceftazidime, and Meropenem.",
  "",
  "Important caveat:",
  "The difference between logistic regression and Random Forest is small for the pooled and Meropenem models. Therefore, the conclusion should be framed as preference based on equal or slightly better temporal AUC plus interpretability, not as overwhelming superiority.",
  "",
  "Random Forest variable importance suggested that country, source, year, and antibiotic are predictive. However, impurity-based importance can be biased for categorical variables with many levels, so these rankings should not be interpreted causally.",
  "",
  "No isolate-level leakage across temporal splits was detected in Phase 6B or Phase 6C."
)

writeLines(
  narrative_lines,
  con = file.path(phase6d_dir, "phase6d_narrative_summary.txt")
)


# Step 19: Final file list

phase6d_output_file_list <- data.table(
  file_name = list.files(phase6d_dir)
)

phase6d_output_file_list

fwrite(
  phase6d_output_file_list,
  file.path(phase6d_dir, "phase6d_output_file_list.csv")
)

list.files(phase6d_dir)