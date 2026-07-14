# Phase 6A: ML Problem Definition and Modelling Dataset Preparation
# Project: Vivli AMR / Pfizer ATLAS Dataset
# Scope:
# Klebsiella pneumoniae resistance to Cefepime, Ceftazidime, and Meropenem
#
# Purpose:
# Define the machine-learning prediction problem and prepare an isolate-antibiotic
# modelling dataset.
#
# This phase does NOT fit any model.


# Step 1: Load packages
library(data.table)
library(stringr)


# Step 2: Create dedicated Phase 6A output folder
phase6a_dir <- "phase6a_outputs"
dir.create(phase6a_dir, showWarnings = FALSE)


# Step 3: Define input file
raw_file <- "atlas_vivli_2004_2024.csv"

file.exists(raw_file)


# Step 4: Read column names first
raw_names <- names(fread(raw_file, nrows = 0))

selected_species <- "Klebsiella pneumoniae"

selected_antibiotics <- c(
  "Cefepime",
  "Ceftazidime",
  "Meropenem"
)

selected_interpretation_cols <- paste0(selected_antibiotics, "_I")

metadata_cols <- c(
  "Isolate Id",
  "Study",
  "Species",
  "Country",
  "State",
  "Gender",
  "Age Group",
  "Speciality",
  "Source",
  "Year"
)

required_cols <- c(
  metadata_cols,
  selected_interpretation_cols
)

missing_required_cols <- setdiff(required_cols, raw_names)

missing_required_cols

if (length(missing_required_cols) > 0) {
  stop("Required columns are missing from raw ATLAS file.")
}

phase6a_input_check <- data.table(
  item = c(
    "raw_file_exists",
    "n_columns_in_raw_file",
    "selected_species",
    "selected_antibiotics",
    "required_columns_missing"
  ),
  value = c(
    as.character(file.exists(raw_file)),
    as.character(length(raw_names)),
    selected_species,
    paste(selected_antibiotics, collapse = "; "),
    paste(missing_required_cols, collapse = "; ")
  )
)

phase6a_input_check

fwrite(
  phase6a_input_check,
  file.path(phase6a_dir, "phase6a_input_check.csv")
)


# Step 5: Read only required columns
# Important: use character columns to preserve IDs and MIC-related text patterns.
raw_selected <- fread(
  raw_file,
  select = required_cols,
  colClasses = "character",
  na.strings = c("", "NA", "N/A", "NULL")
)

dim(raw_selected)
names(raw_selected)


# Step 6: Helper functions
clean_text <- function(x) {
  x <- str_squish(as.character(x))
  x[x == ""] <- NA_character_
  x[str_to_upper(x) %in% c("NA", "N/A", "NULL", "UNKNOWN", "NOT AVAILABLE")] <- NA_character_
  x
}

clean_interpretation <- function(x) {
  x <- clean_text(x)
  x_lower <- str_to_lower(x)
  
  out <- fifelse(
    x_lower == "susceptible",
    "Susceptible",
    fifelse(
      x_lower == "intermediate",
      "Intermediate",
      fifelse(
        x_lower == "resistant",
        "Resistant",
        NA_character_
      )
    )
  )
  
  out
}


# Step 7: Clean metadata and selected interpretation columns
for (col in metadata_cols) {
  raw_selected[, (col) := clean_text(get(col))]
}

for (col in selected_interpretation_cols) {
  raw_selected[, (col) := clean_interpretation(get(col))]
}


# Step 8: Filter to selected species
kp_selected <- raw_selected[
  Species == selected_species
]

dim(kp_selected)

phase6a_species_scope_check <- data.table(
  metric = c(
    "rows_in_raw_selected_columns",
    "rows_for_selected_species",
    "unique_isolates_for_selected_species"
  ),
  value = c(
    nrow(raw_selected),
    nrow(kp_selected),
    uniqueN(kp_selected[["Isolate Id"]])
  )
)

phase6a_species_scope_check

fwrite(
  phase6a_species_scope_check,
  file.path(phase6a_dir, "phase6a_species_scope_check.csv")
)


# Step 9: Interpretation value check before long transformation
phase6a_interpretation_value_check <- rbindlist(
  lapply(
    selected_interpretation_cols,
    function(col) {
      kp_selected[
        ,
        .N,
        by = .(interpretation_value = get(col))
      ][
        ,
        antibiotic := str_remove(col, "_I$")
      ][
        ,
        interpretation_column := col
      ][
        order(antibiotic, interpretation_value)
      ]
    }
  ),
  fill = TRUE
)

phase6a_interpretation_value_check

fwrite(
  phase6a_interpretation_value_check,
  file.path(phase6a_dir, "phase6a_interpretation_value_check.csv")
)


# Step 10: Convert selected antibiotic interpretations to long isolate-antibiotic format
ml_long <- melt(
  kp_selected,
  id.vars = metadata_cols,
  measure.vars = selected_interpretation_cols,
  variable.name = "antibiotic_interpretation_column",
  value.name = "interpretation_clean",
  variable.factor = FALSE
)

ml_long[, antibiotic := str_remove(antibiotic_interpretation_column, "_I$")]

# Keep only valid S/I/R interpretation rows
ml_long <- ml_long[
  interpretation_clean %in% c("Susceptible", "Intermediate", "Resistant")
]

dim(ml_long)


# Step 11: Define primary and sensitivity outcomes
ml_long[, resistant_primary := as.integer(interpretation_clean == "Resistant")]

ml_long[, non_susceptible_sensitivity := as.integer(
  interpretation_clean %in% c("Intermediate", "Resistant")
)]


# Step 12: Build clean modelling dataset
ml_dataset <- ml_long[
  ,
  .(
    isolate_id = `Isolate Id`,
    isolate_antibiotic_id = paste(`Isolate Id`, antibiotic, sep = "__"),
    species = Species,
    antibiotic,
    interpretation_clean,
    resistant_primary,
    non_susceptible_sensitivity,
    year = Year,
    year_num = suppressWarnings(as.integer(Year)),
    country = Country,
    state = State,
    gender = Gender,
    age_group = `Age Group`,
    speciality = Speciality,
    source = Source,
    study = Study
  )
]

# Clean predictor columns again defensively
clean_predictor_cols <- c(
  "country",
  "state",
  "gender",
  "age_group",
  "speciality",
  "source",
  "study"
)

for (col in clean_predictor_cols) {
  ml_dataset[, (col) := clean_text(get(col))]
}


# Step 13: Basic eligibility for ML dataset
ml_dataset[, has_valid_year := !is.na(year_num)]
ml_dataset[, has_valid_country := !is.na(country)]
ml_dataset[, has_valid_primary_outcome := !is.na(resistant_primary)]
ml_dataset[, has_valid_sensitivity_outcome := !is.na(non_susceptible_sensitivity)]

ml_dataset[, ml_ready_primary := (
  has_valid_year == TRUE &
    has_valid_country == TRUE &
    has_valid_primary_outcome == TRUE
)]

ml_dataset[, ml_ready_sensitivity := (
  has_valid_year == TRUE &
    has_valid_country == TRUE &
    has_valid_sensitivity_outcome == TRUE
)]

phase6a_eligibility_summary <- data.table(
  metric = c(
    "long_rows_with_valid_interpretation",
    "unique_isolates_in_long_dataset",
    "primary_ml_ready_rows",
    "sensitivity_ml_ready_rows",
    "rows_missing_year",
    "rows_missing_country"
  ),
  value = c(
    nrow(ml_dataset),
    uniqueN(ml_dataset$isolate_id),
    nrow(ml_dataset[ml_ready_primary == TRUE]),
    nrow(ml_dataset[ml_ready_sensitivity == TRUE]),
    nrow(ml_dataset[has_valid_year == FALSE]),
    nrow(ml_dataset[has_valid_country == FALSE])
  )
)

phase6a_eligibility_summary

fwrite(
  phase6a_eligibility_summary,
  file.path(phase6a_dir, "phase6a_eligibility_summary.csv")
)


# Step 14: Create final ML-ready datasets
ml_dataset_primary <- ml_dataset[
  ml_ready_primary == TRUE
]

ml_dataset_sensitivity <- ml_dataset[
  ml_ready_sensitivity == TRUE
]

# For categorical ML algorithms, retain explicit Unknown levels for non-core covariates.
categorical_predictors <- c(
  "antibiotic",
  "country",
  "gender",
  "age_group",
  "speciality",
  "source"
)

for (col in categorical_predictors) {
  ml_dataset_primary[is.na(get(col)), (col) := "Unknown"]
  ml_dataset_sensitivity[is.na(get(col)), (col) := "Unknown"]
}

# State and study are retained as metadata but not primary predictors.
ml_dataset_primary[, state_missing := is.na(state)]
ml_dataset_sensitivity[, state_missing := is.na(state)]

ml_dataset_primary[, study_missing := is.na(study)]
ml_dataset_sensitivity[, study_missing := is.na(study)]


# Step 15: Outcome balance by antibiotic
phase6a_outcome_balance_by_antibiotic <- ml_dataset_primary[
  ,
  .(
    isolate_antibiotic_rows = .N,
    unique_isolates = uniqueN(isolate_id),
    susceptible_n = sum(interpretation_clean == "Susceptible"),
    intermediate_n = sum(interpretation_clean == "Intermediate"),
    resistant_n = sum(interpretation_clean == "Resistant"),
    resistance_pct = round(100 * mean(resistant_primary), 2),
    non_susceptible_pct = round(100 * mean(non_susceptible_sensitivity), 2),
    min_year = min(year_num, na.rm = TRUE),
    max_year = max(year_num, na.rm = TRUE),
    countries = uniqueN(country)
  ),
  by = antibiotic
][order(antibiotic)]

phase6a_outcome_balance_by_antibiotic

fwrite(
  phase6a_outcome_balance_by_antibiotic,
  file.path(phase6a_dir, "phase6a_outcome_balance_by_antibiotic.csv")
)


# Step 16: Overall outcome balance
phase6a_overall_outcome_balance <- ml_dataset_primary[
  ,
  .(
    isolate_antibiotic_rows = .N,
    unique_isolates = uniqueN(isolate_id),
    susceptible_n = sum(interpretation_clean == "Susceptible"),
    intermediate_n = sum(interpretation_clean == "Intermediate"),
    resistant_n = sum(interpretation_clean == "Resistant"),
    resistance_pct = round(100 * mean(resistant_primary), 2),
    non_susceptible_pct = round(100 * mean(non_susceptible_sensitivity), 2),
    min_year = min(year_num, na.rm = TRUE),
    max_year = max(year_num, na.rm = TRUE),
    countries = uniqueN(country)
  )
]

phase6a_overall_outcome_balance

fwrite(
  phase6a_overall_outcome_balance,
  file.path(phase6a_dir, "phase6a_overall_outcome_balance.csv")
)


# Step 17: Missingness summary before Unknown handling for key metadata
missingness_cols <- c(
  "year_num",
  "country",
  "state",
  "gender",
  "age_group",
  "speciality",
  "source",
  "study"
)

phase6a_missingness_summary <- rbindlist(
  lapply(
    missingness_cols,
    function(col) {
      data.table(
        variable = col,
        missing_n = sum(is.na(ml_dataset[[col]])),
        total_n = nrow(ml_dataset),
        missing_pct = round(100 * mean(is.na(ml_dataset[[col]])), 2)
      )
    }
  )
)

phase6a_missingness_summary

fwrite(
  phase6a_missingness_summary,
  file.path(phase6a_dir, "phase6a_missingness_summary.csv")
)


# Step 18: Feature level summary
feature_level_cols <- c(
  "antibiotic",
  "country",
  "gender",
  "age_group",
  "speciality",
  "source",
  "study",
  "state"
)

phase6a_feature_level_summary <- rbindlist(
  lapply(
    feature_level_cols,
    function(col) {
      data.table(
        variable = col,
        unique_levels = uniqueN(ml_dataset_primary[[col]], na.rm = TRUE),
        missing_n = sum(is.na(ml_dataset_primary[[col]])),
        missing_pct = round(100 * mean(is.na(ml_dataset_primary[[col]])), 2)
      )
    }
  )
)

phase6a_feature_level_summary

fwrite(
  phase6a_feature_level_summary,
  file.path(phase6a_dir, "phase6a_feature_level_summary.csv")
)


# Step 19: Proposed temporal split feasibility
# This is not final model splitting yet. It only checks whether the proposed
# time windows contain adequate data and resistant cases.

ml_dataset_primary[, proposed_temporal_split := fifelse(
  year_num <= 2018,
  "train_2004_2018",
  fifelse(
    year_num <= 2021,
    "validation_2019_2021",
    "test_2022_2024"
  )
)]

phase6a_temporal_split_feasibility <- ml_dataset_primary[
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
  by = .(antibiotic, proposed_temporal_split)
][order(antibiotic, proposed_temporal_split)]

phase6a_temporal_split_feasibility

fwrite(
  phase6a_temporal_split_feasibility,
  file.path(phase6a_dir, "phase6a_temporal_split_feasibility.csv")
)


# Step 20: Country-year sparsity check for ML dataset
# Type-safe version: force numeric/integer outputs explicitly.

phase6a_country_year_outcome_check <- ml_dataset_primary[
  ,
  .(
    isolate_antibiotic_rows = as.integer(.N),
    resistant_n = as.integer(sum(resistant_primary)),
    resistance_pct = round(100 * mean(resistant_primary), 2)
  ),
  by = .(antibiotic, country, year_num)
][order(antibiotic, country, year_num)]

phase6a_country_year_sparsity_summary <- phase6a_country_year_outcome_check[
  ,
  .(
    country_year_cells = as.integer(.N),
    cells_n_lt_30 = as.integer(sum(isolate_antibiotic_rows < 30)),
    cells_n_ge_30 = as.integer(sum(isolate_antibiotic_rows >= 30)),
    median_cell_n = as.numeric(median(isolate_antibiotic_rows)),
    min_cell_n = as.integer(min(isolate_antibiotic_rows)),
    max_cell_n = as.integer(max(isolate_antibiotic_rows))
  ),
  by = antibiotic
][order(antibiotic)]

phase6a_country_year_sparsity_summary

fwrite(
  phase6a_country_year_sparsity_summary,
  file.path(phase6a_dir, "phase6a_country_year_sparsity_summary.csv")
)


# Step 21: ML feature catalog and leakage decisions
phase6a_feature_catalog <- data.table(
  variable = c(
    "isolate_id",
    "isolate_antibiotic_id",
    "species",
    "antibiotic",
    "year_num",
    "country",
    "gender",
    "age_group",
    "speciality",
    "source",
    "state",
    "study",
    "MIC columns",
    "interpretation columns",
    "resistance gene markers"
  ),
  role = c(
    "identifier / split grouping",
    "row identifier",
    "scope variable",
    "primary predictor",
    "primary predictor",
    "primary predictor",
    "primary predictor",
    "primary predictor",
    "primary predictor",
    "primary predictor",
    "excluded from primary model",
    "metadata / possible sensitivity predictor",
    "excluded",
    "target source",
    "excluded from primary model"
  ),
  include_in_primary_model = c(
    FALSE,
    FALSE,
    FALSE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE
  ),
  rationale = c(
    "Identifier; must be used to avoid isolate-level leakage across splits.",
    "Identifier for isolate-antibiotic rows; not a predictor.",
    "Constant after scope restriction.",
    "Needed for pooled multi-antibiotic model.",
    "Known at prediction time and captures secular trend.",
    "Known at prediction time; important spatial/epidemiological predictor.",
    "Patient/isolate metadata; may be predictive.",
    "Patient/isolate metadata; may be predictive.",
    "Clinical metadata; may be predictive.",
    "Specimen/source metadata; may be predictive.",
    "High missingness; subnational analysis was not feasible.",
    "May encode dataset/program structure; excluded from primary deployable model.",
    "Direct susceptibility measurement; would leak laboratory result.",
    "Used to define target; cannot be predictor.",
    "Sparse and biologically downstream; reserved for possible separate genotype-aware analysis."
  )
)

phase6a_feature_catalog

fwrite(
  phase6a_feature_catalog,
  file.path(phase6a_dir, "phase6a_feature_catalog.csv")
)


# Step 22: ML problem definition table
phase6a_ml_problem_definition <- data.table(
  component = c(
    "prediction_unit",
    "organism_scope",
    "antibiotic_scope",
    "primary_outcome",
    "sensitivity_outcome",
    "primary_predictor_set",
    "excluded_primary_predictors",
    "recommended_validation_strategy",
    "split_grouping_unit",
    "modelling_status"
  ),
  definition = c(
    "isolate-antibiotic observation",
    selected_species,
    paste(selected_antibiotics, collapse = "; "),
    "Resistant vs Susceptible + Intermediate",
    "Resistant + Intermediate vs Susceptible",
    "antibiotic, year, country, gender, age group, speciality, source",
    "isolate IDs, MIC values, interpretation columns, State, Study, resistance gene markers",
    "temporal validation: train 2004-2018, validation 2019-2021, test 2022-2024",
    "Isolate Id; pooled models must avoid placing observations from the same isolate in different splits",
    "dataset preparation only; no model fitted in Phase 6A"
  )
)

phase6a_ml_problem_definition

fwrite(
  phase6a_ml_problem_definition,
  file.path(phase6a_dir, "phase6a_ml_problem_definition.csv")
)


# Step 23: Save ML-ready datasets
# CSV for auditability; RDS for later modelling in R.

fwrite(
  ml_dataset_primary,
  file.path(phase6a_dir, "phase6a_ml_dataset_primary.csv")
)

fwrite(
  ml_dataset_sensitivity,
  file.path(phase6a_dir, "phase6a_ml_dataset_sensitivity.csv")
)

saveRDS(
  ml_dataset_primary,
  file.path(phase6a_dir, "phase6a_ml_dataset_primary.rds")
)

saveRDS(
  ml_dataset_sensitivity,
  file.path(phase6a_dir, "phase6a_ml_dataset_sensitivity.rds")
)


# Step 24: Narrative summary
narrative_lines <- c(
  "Phase 6A ML Problem Definition and Dataset Preparation",
  "Project: Vivli AMR / Pfizer ATLAS Dataset",
  "Scope: Klebsiella pneumoniae resistance to Cefepime, Ceftazidime, and Meropenem",
  "",
  "Prediction unit: isolate-antibiotic observation.",
  "Primary outcome: Resistant vs Susceptible plus Intermediate.",
  "Sensitivity outcome: Resistant plus Intermediate vs Susceptible.",
  "",
  "The primary candidate predictors are antibiotic, year, country, gender, age group, speciality, and source.",
  "Isolate identifiers are retained for splitting and audit purposes but are not predictors.",
  "MIC columns, interpretation columns, resistance gene markers, State, and Study are excluded from the primary model feature set.",
  "",
  "A temporal validation strategy is proposed for later phases: train on 2004-2018, validate on 2019-2021, and test on 2022-2024.",
  "This phase prepares the modelling dataset only; no model is fitted."
)

writeLines(
  narrative_lines,
  con = file.path(phase6a_dir, "phase6a_narrative_summary.txt")
)


# Step 25: Final output file list
phase6a_output_file_list <- data.table(
  file_name = list.files(phase6a_dir)
)

phase6a_output_file_list

fwrite(
  phase6a_output_file_list,
  file.path(phase6a_dir, "phase6a_output_file_list.csv")
)

list.files(phase6a_dir)