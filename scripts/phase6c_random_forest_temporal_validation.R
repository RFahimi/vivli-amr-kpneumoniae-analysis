# Phase 6C: Random Forest Benchmark with Temporal Validation
# Project: Vivli AMR / Pfizer ATLAS Dataset
# Scope:
# Klebsiella pneumoniae resistance to Cefepime, Ceftazidime, and Meropenem
#
# Purpose:
# Fit flexible random forest probability models using temporal validation.
#
# Models:
# 1. Pooled random forest across all three antibiotics
# 2. Antibiotic-specific random forest models
#
# Outcome:
# resistant_primary = Resistant vs Susceptible + Intermediate
#
# This phase does not fit gradient boosting models.


# Step 1: Load packages
library(data.table)
library(stringr)
library(ggplot2)

if (!requireNamespace("ranger", quietly = TRUE)) {
  install.packages("ranger")
}

library(ranger)


# Step 2: Create dedicated Phase 6C output folder
phase6c_dir <- "phase6c_outputs"
dir.create(phase6c_dir, showWarnings = FALSE)


# Step 3: Define input file from Phase 6A
phase6a_dir <- "phase6a_outputs"

ml_primary_file <- file.path(
  phase6a_dir,
  "phase6a_ml_dataset_primary.rds"
)

file.exists(ml_primary_file)


# Step 4: Read ML-ready primary dataset
ml_dataset_primary <- readRDS(ml_primary_file)

dim(ml_dataset_primary)
names(ml_dataset_primary)

required_cols <- c(
  "isolate_id",
  "isolate_antibiotic_id",
  "antibiotic",
  "resistant_primary",
  "year_num",
  "country",
  "gender",
  "age_group",
  "speciality",
  "source"
)

missing_required_cols <- setdiff(required_cols, names(ml_dataset_primary))

missing_required_cols

if (length(missing_required_cols) > 0) {
  stop("Required columns are missing from Phase 6A ML dataset.")
}

phase6c_input_check <- data.table(
  item = c(
    "ml_primary_file_exists",
    "rows_in_ml_dataset_primary",
    "unique_isolates",
    "missing_required_columns"
  ),
  value = c(
    as.character(file.exists(ml_primary_file)),
    as.character(nrow(ml_dataset_primary)),
    as.character(uniqueN(ml_dataset_primary$isolate_id)),
    paste(missing_required_cols, collapse = "; ")
  )
)

phase6c_input_check

fwrite(
  phase6c_input_check,
  file.path(phase6c_dir, "phase6c_input_check.csv")
)


# Step 5: Define temporal split
ml_model_data <- copy(ml_dataset_primary)

ml_model_data[, temporal_split := fifelse(
  year_num <= 2018,
  "train_2004_2018",
  fifelse(
    year_num <= 2021,
    "validation_2019_2021",
    "test_2022_2024"
  )
)]

ml_model_data[, temporal_split := factor(
  temporal_split,
  levels = c(
    "train_2004_2018",
    "validation_2019_2021",
    "test_2022_2024"
  )
)]

phase6c_split_summary <- ml_model_data[
  ,
  .(
    isolate_antibiotic_rows = .N,
    unique_isolates = uniqueN(isolate_id),
    resistant_n = sum(resistant_primary),
    non_resistant_n = sum(resistant_primary == 0),
    resistance_pct = round(100 * mean(resistant_primary), 2),
    countries = uniqueN(country),
    min_year = min(year_num),
    max_year = max(year_num)
  ),
  by = .(antibiotic, temporal_split)
][order(antibiotic, temporal_split)]

phase6c_split_summary

fwrite(
  phase6c_split_summary,
  file.path(phase6c_dir, "phase6c_split_summary.csv")
)


# Step 6: Check isolate leakage across temporal splits
phase6c_isolate_split_leakage_check <- ml_model_data[
  ,
  .(
    n_splits = uniqueN(temporal_split),
    splits = paste(sort(unique(as.character(temporal_split))), collapse = "; "),
    min_year = min(year_num),
    max_year = max(year_num)
  ),
  by = isolate_id
][n_splits > 1]

phase6c_isolate_split_leakage_check

phase6c_isolate_split_leakage_summary <- data.table(
  metric = c(
    "isolates_crossing_temporal_splits",
    "unique_isolates_total"
  ),
  value = c(
    nrow(phase6c_isolate_split_leakage_check),
    uniqueN(ml_model_data$isolate_id)
  )
)

phase6c_isolate_split_leakage_summary

fwrite(
  phase6c_isolate_split_leakage_summary,
  file.path(phase6c_dir, "phase6c_isolate_split_leakage_summary.csv")
)

fwrite(
  phase6c_isolate_split_leakage_check,
  file.path(phase6c_dir, "phase6c_isolate_split_leakage_check.csv")
)


# Step 7: Utility functions

safe_div <- function(num, den) {
  ifelse(den == 0, NA_real_, num / den)
}

auc_roc <- function(y, p) {
  y <- as.integer(y)
  p <- as.numeric(p)
  ok <- complete.cases(y, p)
  y <- y[ok]
  p <- p[ok]
  
  if (length(unique(y)) < 2) {
    return(NA_real_)
  }
  
  n_pos <- as.numeric(sum(y == 1))
  n_neg <- as.numeric(sum(y == 0))
  
  ranks <- rank(p, ties.method = "average")
  
  auc <- (
    sum(ranks[y == 1]) -
      n_pos * (n_pos + 1) / 2
  ) / (n_pos * n_neg)
  
  as.numeric(auc)
}

log_loss_binary <- function(y, p) {
  y <- as.integer(y)
  p <- as.numeric(p)
  eps <- 1e-15
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

evaluate_binary_predictions <- function(y, p, threshold) {
  
  y <- as.integer(y)
  p <- as.numeric(p)
  
  pred_class <- as.integer(p >= threshold)
  
  tp <- sum(pred_class == 1 & y == 1)
  tn <- sum(pred_class == 0 & y == 0)
  fp <- sum(pred_class == 1 & y == 0)
  fn <- sum(pred_class == 0 & y == 1)
  
  sensitivity <- safe_div(tp, tp + fn)
  specificity <- safe_div(tn, tn + fp)
  ppv <- safe_div(tp, tp + fp)
  npv <- safe_div(tn, tn + fn)
  accuracy <- safe_div(tp + tn, tp + tn + fp + fn)
  f1 <- safe_div(2 * ppv * sensitivity, ppv + sensitivity)
  
  data.table(
    n = length(y),
    positives = sum(y == 1),
    negatives = sum(y == 0),
    prevalence_pct = round(100 * mean(y), 2),
    mean_predicted_probability = round(mean(p), 4),
    auc = round(auc_roc(y, p), 4),
    brier_score = round(mean((p - y)^2), 4),
    log_loss = round(log_loss_binary(y, p), 4),
    threshold = threshold,
    accuracy = round(accuracy, 4),
    sensitivity = round(sensitivity, 4),
    specificity = round(specificity, 4),
    ppv = round(ppv, 4),
    npv = round(npv, 4),
    f1 = round(f1, 4),
    tp = tp,
    tn = tn,
    fp = fp,
    fn = fn
  )
}

find_best_threshold_youden <- function(y, p) {
  
  threshold_grid <- seq(0.01, 0.99, by = 0.01)
  
  threshold_perf <- rbindlist(
    lapply(
      threshold_grid,
      function(th) {
        tmp <- evaluate_binary_predictions(y, p, threshold = th)
        tmp[, threshold := th]
        tmp[, youden := sensitivity + specificity - 1]
        tmp
      }
    ),
    fill = TRUE
  )
  
  threshold_perf[
    order(-youden, abs(threshold - 0.5))
  ][1]
}


# Step 8: Categorical preprocessing for ranger
# Rare training levels are mapped to "Other".
# Validation/test levels unseen in training are also mapped to "Other".

prepare_categorical_for_rf <- function(
    dt,
    categorical_cols,
    split_col = "temporal_split",
    train_label = "train_2004_2018",
    min_count_by_col = list()
) {
  
  dt <- copy(dt)
  level_map_list <- list()
  
  train_idx <- as.character(dt[[split_col]]) == train_label
  
  for (col in categorical_cols) {
    
    dt[[col]] <- as.character(dt[[col]])
    dt[is.na(get(col)) | get(col) == "", (col) := "Unknown"]
    
    train_vals <- dt[[col]][train_idx]
    
    counts <- data.table(level = train_vals)[
      ,
      .N,
      by = level
    ][order(-N, level)]
    
    min_count <- min_count_by_col[[col]]
    
    if (is.null(min_count)) {
      min_count <- 30L
    }
    
    keep_levels <- counts[N >= min_count, level]
    
    if (length(keep_levels) == 0) {
      keep_levels <- counts[1, level]
    }
    
    nontrain_vals <- unique(dt[[col]][!train_idx])
    nontrain_vals <- nontrain_vals[!is.na(nontrain_vals)]
    
    unseen_or_unkept_nontrain <- setdiff(nontrain_vals, keep_levels)
    train_other_count <- sum(!(train_vals %in% keep_levels))
    
    if (
      length(unseen_or_unkept_nontrain) > 0 &&
      train_other_count == 0 &&
      length(keep_levels) > 1 &&
      col != "antibiotic"
    ) {
      rarest_train_level <- counts[.N, level]
      keep_levels <- setdiff(keep_levels, rarest_train_level)
    }
    
    vec <- dt[[col]]
    vec[!(vec %in% keep_levels)] <- "Other"
    
    final_levels <- sort(unique(c(keep_levels, "Other")))
    
    dt[[col]] <- factor(vec, levels = final_levels)
    
    level_map <- counts[
      ,
      .(
        variable = col,
        level,
        train_n = N,
        retained_as_original_level = level %in% keep_levels,
        mapped_to = fifelse(level %in% keep_levels, level, "Other"),
        min_count_rule = min_count
      )
    ]
    
    level_map_list[[col]] <- level_map
  }
  
  list(
    data = dt,
    level_map = rbindlist(level_map_list, fill = TRUE)
  )
}


# Step 9: Random forest fitting helper

fit_rf_model <- function(
    dt,
    model_id,
    model_scope,
    outcome_col,
    numeric_features,
    categorical_features,
    num_trees = 300,
    min_node_size = 20,
    seed = 20260615
) {
  
  train_dt <- dt[temporal_split == "train_2004_2018"]
  validation_dt <- dt[temporal_split == "validation_2019_2021"]
  test_dt <- dt[temporal_split == "test_2022_2024"]
  
  feature_cols <- c(numeric_features, categorical_features)
  
  train_model_dt <- train_dt[
    ,
    c(outcome_col, feature_cols),
    with = FALSE
  ]
  
  train_model_dt[, (outcome_col) := factor(
    get(outcome_col),
    levels = c(0, 1),
    labels = c("non_resistant", "resistant")
  )]
  
  rf_formula <- as.formula(
    paste(
      outcome_col,
      "~",
      paste(feature_cols, collapse = " + ")
    )
  )
  
  mtry_value <- max(
    1,
    floor(sqrt(length(feature_cols)))
  )
  
  set.seed(seed)
  
  rf_fit <- ranger::ranger(
    formula = rf_formula,
    data = as.data.frame(train_model_dt),
    probability = TRUE,
    num.trees = num_trees,
    mtry = mtry_value,
    min.node.size = min_node_size,
    importance = "impurity",
    respect.unordered.factors = "order",
    classification = TRUE,
    seed = seed,
    num.threads = max(1, parallel::detectCores() - 1)
  )
  
  predict_split <- function(split_dt, split_name) {
    
    split_model_dt <- split_dt[
      ,
      feature_cols,
      with = FALSE
    ]
    
    pred <- predict(
      rf_fit,
      data = as.data.frame(split_model_dt)
    )
    
    pred_prob <- as.numeric(pred$predictions[, "resistant"])
    
    data.table(
      model_id = model_id,
      model_scope = model_scope,
      temporal_split = split_name,
      isolate_id = split_dt$isolate_id,
      isolate_antibiotic_id = split_dt$isolate_antibiotic_id,
      antibiotic = split_dt$antibiotic,
      year_num = split_dt$year_num,
      country = as.character(split_dt$country),
      resistant_primary = split_dt[[outcome_col]],
      pred_prob = pred_prob
    )
  }
  
  pred_train <- predict_split(train_dt, "train_2004_2018")
  pred_validation <- predict_split(validation_dt, "validation_2019_2021")
  pred_test <- predict_split(test_dt, "test_2022_2024")
  
  predictions <- rbindlist(
    list(
      pred_train,
      pred_validation,
      pred_test
    ),
    fill = TRUE
  )
  
  best_validation_threshold <- find_best_threshold_youden(
    y = pred_validation$resistant_primary,
    p = pred_validation$pred_prob
  )
  
  threshold_table <- data.table(
    model_id = model_id,
    model_scope = model_scope,
    selected_threshold = best_validation_threshold$threshold,
    validation_youden = best_validation_threshold$youden,
    validation_sensitivity_at_threshold = best_validation_threshold$sensitivity,
    validation_specificity_at_threshold = best_validation_threshold$specificity
  )
  
  evaluate_split_threshold <- function(pred_dt, threshold, threshold_type) {
    
    perf <- evaluate_binary_predictions(
      y = pred_dt$resistant_primary,
      p = pred_dt$pred_prob,
      threshold = threshold
    )
    
    perf[, model_id := model_id]
    perf[, model_scope := model_scope]
    perf[, temporal_split := unique(pred_dt$temporal_split)]
    perf[, threshold_type := threshold_type]
    
    perf
  }
  
  performance <- rbindlist(
    list(
      evaluate_split_threshold(pred_train, 0.5, "threshold_0.5"),
      evaluate_split_threshold(pred_validation, 0.5, "threshold_0.5"),
      evaluate_split_threshold(pred_test, 0.5, "threshold_0.5"),
      evaluate_split_threshold(pred_train, threshold_table$selected_threshold, "validation_youden_threshold"),
      evaluate_split_threshold(pred_validation, threshold_table$selected_threshold, "validation_youden_threshold"),
      evaluate_split_threshold(pred_test, threshold_table$selected_threshold, "validation_youden_threshold")
    ),
    fill = TRUE
  )
  
  performance <- performance[
    ,
    .(
      model_id,
      model_scope,
      temporal_split,
      threshold_type,
      n,
      positives,
      negatives,
      prevalence_pct,
      mean_predicted_probability,
      auc,
      brier_score,
      log_loss,
      threshold,
      accuracy,
      sensitivity,
      specificity,
      ppv,
      npv,
      f1,
      tp,
      tn,
      fp,
      fn
    )
  ]
  
  importance_dt <- data.table(
    variable = names(rf_fit$variable.importance),
    importance = as.numeric(rf_fit$variable.importance)
  )[order(-importance)]
  
  importance_dt[, model_id := model_id]
  importance_dt[, model_scope := model_scope]
  
  registry <- data.table(
    model_id = model_id,
    model_scope = model_scope,
    model_family = "random_forest_ranger",
    outcome = outcome_col,
    features = paste(feature_cols, collapse = "; "),
    num_trees = num_trees,
    mtry = mtry_value,
    min_node_size = min_node_size,
    train_rows = nrow(train_dt),
    validation_rows = nrow(validation_dt),
    test_rows = nrow(test_dt),
    train_positives = sum(train_dt[[outcome_col]]),
    validation_positives = sum(validation_dt[[outcome_col]]),
    test_positives = sum(test_dt[[outcome_col]])
  )
  
  list(
    model = rf_fit,
    predictions = predictions,
    threshold_table = threshold_table,
    performance = performance,
    registry = registry,
    importance = importance_dt
  )
}


# Step 10: Fit pooled random forest

pooled_categorical_cols <- c(
  "antibiotic",
  "country",
  "gender",
  "age_group",
  "speciality",
  "source"
)

pooled_min_count_by_col <- list(
  antibiotic = 1L,
  country = 30L,
  gender = 30L,
  age_group = 30L,
  speciality = 30L,
  source = 30L
)

pooled_prepared <- prepare_categorical_for_rf(
  dt = ml_model_data,
  categorical_cols = pooled_categorical_cols,
  split_col = "temporal_split",
  train_label = "train_2004_2018",
  min_count_by_col = pooled_min_count_by_col
)

ml_model_data_pooled <- pooled_prepared$data
phase6c_pooled_level_map <- pooled_prepared$level_map

fwrite(
  phase6c_pooled_level_map,
  file.path(phase6c_dir, "phase6c_pooled_level_map.csv")
)

pooled_rf_result <- fit_rf_model(
  dt = ml_model_data_pooled,
  model_id = "pooled_random_forest_primary",
  model_scope = "pooled_all_antibiotics",
  outcome_col = "resistant_primary",
  numeric_features = c("year_num"),
  categorical_features = pooled_categorical_cols,
  num_trees = 300,
  min_node_size = 20,
  seed = 20260615
)

gc()


# Step 11: Fit antibiotic-specific random forests

antibiotic_list <- sort(unique(ml_model_data$antibiotic))

specific_rf_results <- list()
specific_level_maps <- list()

specific_categorical_cols <- c(
  "country",
  "gender",
  "age_group",
  "speciality",
  "source"
)

specific_min_count_by_col <- list(
  country = 30L,
  gender = 30L,
  age_group = 30L,
  speciality = 30L,
  source = 30L
)

for (ab in antibiotic_list) {
  
  ab_dt <- ml_model_data[
    antibiotic == ab
  ]
  
  ab_prepared <- prepare_categorical_for_rf(
    dt = ab_dt,
    categorical_cols = specific_categorical_cols,
    split_col = "temporal_split",
    train_label = "train_2004_2018",
    min_count_by_col = specific_min_count_by_col
  )
  
  ab_model_data <- ab_prepared$data
  
  level_map <- ab_prepared$level_map
  level_map[, antibiotic := ab]
  
  specific_level_maps[[ab]] <- level_map
  
  ab_model_id <- paste0(
    "antibiotic_specific_random_forest_",
    str_replace_all(str_to_lower(ab), "[^a-z0-9]+", "_")
  )
  
  specific_rf_results[[ab]] <- fit_rf_model(
    dt = ab_model_data,
    model_id = ab_model_id,
    model_scope = paste0("antibiotic_specific_", ab),
    outcome_col = "resistant_primary",
    numeric_features = c("year_num"),
    categorical_features = specific_categorical_cols,
    num_trees = 300,
    min_node_size = 20,
    seed = 20260615
  )
  
  gc()
}

phase6c_specific_level_map <- rbindlist(
  specific_level_maps,
  fill = TRUE
)

fwrite(
  phase6c_specific_level_map,
  file.path(phase6c_dir, "phase6c_antibiotic_specific_level_map.csv")
)


# Step 12: Combine model outputs

all_rf_results <- c(
  list(pooled_random_forest_primary = pooled_rf_result),
  specific_rf_results
)

phase6c_model_registry <- rbindlist(
  lapply(all_rf_results, function(x) x$registry),
  fill = TRUE
)

phase6c_thresholds <- rbindlist(
  lapply(all_rf_results, function(x) x$threshold_table),
  fill = TRUE
)

phase6c_model_performance <- rbindlist(
  lapply(all_rf_results, function(x) x$performance),
  fill = TRUE
)

phase6c_predictions <- rbindlist(
  lapply(all_rf_results, function(x) x$predictions),
  fill = TRUE
)

phase6c_variable_importance <- rbindlist(
  lapply(all_rf_results, function(x) x$importance),
  fill = TRUE
)

phase6c_model_registry
phase6c_thresholds
phase6c_model_performance
phase6c_variable_importance

fwrite(
  phase6c_model_registry,
  file.path(phase6c_dir, "phase6c_model_registry.csv")
)

fwrite(
  phase6c_thresholds,
  file.path(phase6c_dir, "phase6c_thresholds.csv")
)

fwrite(
  phase6c_model_performance,
  file.path(phase6c_dir, "phase6c_model_performance.csv")
)

fwrite(
  phase6c_variable_importance,
  file.path(phase6c_dir, "phase6c_variable_importance.csv")
)


# Save validation and test predictions only
phase6c_validation_predictions <- phase6c_predictions[
  temporal_split == "validation_2019_2021"
]

phase6c_test_predictions <- phase6c_predictions[
  temporal_split == "test_2022_2024"
]

fwrite(
  phase6c_validation_predictions,
  file.path(phase6c_dir, "phase6c_validation_predictions.csv")
)

fwrite(
  phase6c_test_predictions,
  file.path(phase6c_dir, "phase6c_test_predictions.csv")
)


# Step 13: Save model objects

saveRDS(
  pooled_rf_result$model,
  file.path(phase6c_dir, "phase6c_pooled_random_forest_model.rds")
)

specific_rf_model_objects <- lapply(
  specific_rf_results,
  function(x) x$model
)

saveRDS(
  specific_rf_model_objects,
  file.path(phase6c_dir, "phase6c_antibiotic_specific_random_forest_models.rds")
)


# Step 14: Test-set summary table

phase6c_test_performance_summary <- phase6c_model_performance[
  temporal_split == "test_2022_2024" &
    threshold_type == "validation_youden_threshold"
][
  order(model_scope)
]

phase6c_test_performance_summary

fwrite(
  phase6c_test_performance_summary,
  file.path(phase6c_dir, "phase6c_test_performance_summary.csv")
)


# Step 15: Test AUC summary

phase6c_test_auc_summary <- phase6c_test_performance_summary[
  ,
  .(
    model_id,
    model_scope,
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
][order(-auc)]

phase6c_test_auc_summary

fwrite(
  phase6c_test_auc_summary,
  file.path(phase6c_dir, "phase6c_test_auc_summary.csv")
)


# Step 16: Calibration table for test predictions

make_calibration_table <- function(pred_dt, n_bins = 10) {
  
  dt <- copy(pred_dt)
  
  dt[, pred_bin := cut(
    pred_prob,
    breaks = unique(quantile(pred_prob, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)),
    include.lowest = TRUE
  ), by = model_id]
  
  dt[
    ,
    .(
      n = .N,
      observed_resistance_pct = round(100 * mean(resistant_primary), 2),
      mean_predicted_probability_pct = round(100 * mean(pred_prob), 2),
      min_pred_prob = min(pred_prob),
      max_pred_prob = max(pred_prob)
    ),
    by = .(model_id, model_scope, pred_bin)
  ][order(model_id, pred_bin)]
}

phase6c_test_calibration_table <- make_calibration_table(
  phase6c_test_predictions,
  n_bins = 10
)

phase6c_test_calibration_table

fwrite(
  phase6c_test_calibration_table,
  file.path(phase6c_dir, "phase6c_test_calibration_table.csv")
)


# Step 17: Optional comparison against Phase 6B logistic results
phase6b_auc_file <- file.path(
  "phase6b_outputs",
  "phase6b_test_auc_summary.csv"
)

if (file.exists(phase6b_auc_file)) {
  
  phase6b_test_auc_summary <- fread(phase6b_auc_file)
  phase6b_test_auc_summary[, phase := "Phase 6B logistic baseline"]
  phase6c_test_auc_summary[, phase := "Phase 6C random forest"]
  
  phase6bc_auc_comparison <- rbindlist(
    list(
      phase6b_test_auc_summary,
      phase6c_test_auc_summary
    ),
    fill = TRUE
  )
  
  fwrite(
    phase6bc_auc_comparison,
    file.path(phase6c_dir, "phase6c_vs_phase6b_test_auc_comparison.csv")
  )
  
} else {
  
  phase6bc_auc_comparison <- copy(phase6c_test_auc_summary)
  phase6bc_auc_comparison[, phase := "Phase 6C random forest"]
}


# Step 18: Figures

fig_phase6c_test_auc <- ggplot(
  phase6c_test_auc_summary,
  aes(
    x = reorder(model_id, auc),
    y = auc
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Phase 6C random forest model performance",
    subtitle = "Test set AUC using temporal split; threshold selected on validation set",
    x = "Model",
    y = "Test AUC",
    caption = "Source: Vivli AMR / Pfizer ATLAS dataset, 2004–2024"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 11)
  )

fig_phase6c_test_auc

ggsave(
  filename = file.path(phase6c_dir, "phase6c_fig_test_auc.png"),
  plot = fig_phase6c_test_auc,
  width = 9,
  height = 6,
  dpi = 300
)


fig_phase6c_test_calibration <- ggplot(
  phase6c_test_calibration_table,
  aes(
    x = mean_predicted_probability_pct,
    y = observed_resistance_pct
  )
) +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.4) +
  geom_point(aes(size = n), alpha = 0.75) +
  geom_line(aes(group = model_id), linewidth = 0.5) +
  facet_wrap(~ model_id, ncol = 2) +
  labs(
    title = "Phase 6C test-set calibration",
    subtitle = "Observed vs mean predicted resistance by prediction decile",
    x = "Mean predicted resistance probability (%)",
    y = "Observed resistance (%)",
    size = "Rows",
    caption = "Source: Vivli AMR / Pfizer ATLAS dataset, 2004–2024"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 9),
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  )

fig_phase6c_test_calibration

ggsave(
  filename = file.path(phase6c_dir, "phase6c_fig_test_calibration.png"),
  plot = fig_phase6c_test_calibration,
  width = 11,
  height = 8,
  dpi = 300
)


fig_phase6bc_auc_comparison <- ggplot(
  phase6bc_auc_comparison,
  aes(
    x = reorder(model_id, auc),
    y = auc,
    shape = phase
  )
) +
  geom_point(size = 3) +
  coord_flip() +
  labs(
    title = "Phase 6B vs Phase 6C test-set AUC comparison",
    subtitle = "Logistic regression baseline compared with random forest benchmark",
    x = "Model",
    y = "Test AUC",
    shape = "Phase",
    caption = "Source: Vivli AMR / Pfizer ATLAS dataset, 2004–2024"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  )

fig_phase6bc_auc_comparison

ggsave(
  filename = file.path(phase6c_dir, "phase6c_fig_phase6b_vs_phase6c_auc.png"),
  plot = fig_phase6bc_auc_comparison,
  width = 10,
  height = 7,
  dpi = 300
)


# Step 19: Narrative summary

best_rf_model_by_auc <- phase6c_test_auc_summary[
  order(-auc)
][1]

narrative_lines <- c(
  "Phase 6C Random Forest Benchmark with Temporal Validation",
  "Project: Vivli AMR / Pfizer ATLAS Dataset",
  "Scope: Klebsiella pneumoniae resistance to Cefepime, Ceftazidime, and Meropenem",
  "",
  "Outcome: Resistant vs Susceptible plus Intermediate.",
  "Prediction unit: isolate-antibiotic observation.",
  "",
  "Temporal split:",
  "Training: 2004-2018",
  "Validation: 2019-2021",
  "Testing: 2022-2024",
  "",
  "Models fitted:",
  "1. Pooled random forest across all three antibiotics.",
  "2. Antibiotic-specific random forest models.",
  "",
  paste0(
    "Best test-set AUC in this random forest phase: ",
    best_rf_model_by_auc$model_id,
    " with AUC = ",
    best_rf_model_by_auc$auc,
    "."
  ),
  "",
  "Thresholds were selected on the validation set using Youden's index.",
  "The test set was used only for final temporal hold-out evaluation.",
  "",
  "These models are flexible benchmarks. They should be compared against Phase 6B logistic baselines using test-set AUC, calibration, and threshold-dependent metrics."
)

writeLines(
  narrative_lines,
  con = file.path(phase6c_dir, "phase6c_narrative_summary.txt")
)


# Step 20: Final file list

phase6c_output_file_list <- data.table(
  file_name = list.files(phase6c_dir)
)

phase6c_output_file_list

fwrite(
  phase6c_output_file_list,
  file.path(phase6c_dir, "phase6c_output_file_list.csv")
)

list.files(phase6c_dir)