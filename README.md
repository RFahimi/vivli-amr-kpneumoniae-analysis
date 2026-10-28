# Vivli AMR: Klebsiella pneumoniae Analysis

This repository contains the reproducible R analysis pipeline developed for the 2026 Vivli AMR Surveillance Data Challenge.

The project examines temporal, geographic, and predictive patterns of antimicrobial resistance in *Klebsiella pneumoniae* against three antibiotics:

- Cefepime
- Ceftazidime
- Meropenem

The analysis uses the Vivli AMR / Pfizer ATLAS surveillance dataset covering 2004–2024.

## Project objectives

The analysis was designed to:

1. Audit the structure and quality of the source dataset.
2. Identify clinically relevant species–antibiotic combinations with sufficient coverage.
3. Describe resistance patterns over time and across countries.
4. Assess global spatial autocorrelation using Moran's I.
5. Develop and compare logistic regression and Random Forest prediction models.
6. Produce an integrated reporting and submission package.

## Data access

The source Pfizer ATLAS dataset is access-controlled through the Vivli AMR Register and is not included in this repository.

Data Request DOI:

`https://doi.org/10.25934/PR00013399`

To run the analysis locally, authorised users must place the following file in the repository root:

```text
atlas_vivli_2004_2024.csv
```

The source dataset must not be committed to GitHub. It is excluded through the repository `.gitignore` file.

Generated output folders, model objects, record-level prediction files, and other potentially restricted analytical outputs are also excluded from version control.

## Repository structure

```text
vivli-amr-kpneumoniae-analysis/
├── README.md
├── .gitignore
├── scripts/
├── reproducibility/
│   └── R_sessionInfo.txt
└── documentation/
```

## Analysis workflow

The 18 R scripts should be run sequentially from the repository root.

### Phase 1: Data audit

```text
scripts/phase1_data_audit_clean.R
```

Audits the dataset dimensions, variable structure, isolate identifiers, missingness, geographic coverage, temporal coverage, and antimicrobial susceptibility variables.

### Phase 2: Species–antibiotic coverage

```text
scripts/phase2_species_antibiotic_coverage.R
```

Evaluates data availability across species and antibiotics and identifies combinations suitable for downstream analysis.

### Phase 3: Scope selection

```text
scripts/phase3_scope_country_year_check.R
```

Assesses country-year coverage and supports selection of *Klebsiella pneumoniae* with cefepime, ceftazidime, and meropenem.

### Phase 4: Descriptive analysis

```text
scripts/phase4_descriptive_kpneumoniae.R
```

Creates overall resistance summaries, country-level summaries, temporal trends, and descriptive figures.

### Phase 5: Spatial analysis

Run the following scripts in this order:

```text
scripts/phase5_spatial_feasibility_kpneumoniae.R
scripts/phase5b_spatial_visualisation_package.R
scripts/phase5c_regional_comparison_and_temporal_trends.R
scripts/phase5d_a_morans_i_feasibility_kpneumoniae.R
scripts/phase5d_b_morans_i_execution_kpneumoniae.R
scripts/phase5e_integrated_phase5_summary.R
```

These scripts:

- assess the feasibility of country-level spatial analysis;
- standardise country identifiers;
- produce geographic visualisations;
- compare regional and temporal resistance patterns;
- prepare spatial weights;
- calculate global Moran's I;
- integrate the Phase 5 results.

The primary Moran's I analysis uses symmetrised k-nearest-neighbour spatial weights with `k = 4`.

A sensitivity analysis uses symmetrised k-nearest-neighbour weights with `k = 6`.

### Phase 6: Predictive modelling

Run the following scripts in this order:

```text
scripts/phase6a_ml_problem_definition_dataset_prep.R
scripts/phase6b_baseline_logistic_temporal_validation.R
scripts/phase6c_random_forest_temporal_validation.R
scripts/phase6d_integrated_ml_model_comparison.R
scripts/phase6e_ml_reporting_package.R
```

The primary binary outcome is:

```text
Resistant versus susceptible plus intermediate
```

The prediction unit is an isolate-antibiotic observation.

Temporal validation is defined as:

```text
Training:   2004–2018
Validation: 2019–2021
Testing:    2022–2024
```

Classification thresholds are selected on the validation set using Youden's index and are then applied unchanged to the 2022–2024 temporal test set.

Logistic regression is used as the primary interpretable modelling approach. Random Forest is retained as a flexible benchmark.

### Phase 7: Integrated reporting

Run the following scripts in this order:

```text
scripts/phase7_final_integrated_reporting_package.R
scripts/phase7_patch_add_ceftazidime_choropleth.R
```

These scripts integrate the descriptive, spatial, and predictive results and apply the final ceftazidime choropleth update.

### Phase 8: Final deliverable assembly

```text
scripts/phase8_final_deliverable_assembly.R
```

Assembles the final tables, figures, summaries, and submission materials.

## Complete script order

The complete execution order is:

```text
1.  phase1_data_audit_clean.R
2.  phase2_species_antibiotic_coverage.R
3.  phase3_scope_country_year_check.R
4.  phase4_descriptive_kpneumoniae.R
5.  phase5_spatial_feasibility_kpneumoniae.R
6.  phase5b_spatial_visualisation_package.R
7.  phase5c_regional_comparison_and_temporal_trends.R
8.  phase5d_a_morans_i_feasibility_kpneumoniae.R
9.  phase5d_b_morans_i_execution_kpneumoniae.R
10. phase5e_integrated_phase5_summary.R
11. phase6a_ml_problem_definition_dataset_prep.R
12. phase6b_baseline_logistic_temporal_validation.R
13. phase6c_random_forest_temporal_validation.R
14. phase6d_integrated_ml_model_comparison.R
15. phase6e_ml_reporting_package.R
16. phase7_final_integrated_reporting_package.R
17. phase7_patch_add_ceftazidime_choropleth.R
18. phase8_final_deliverable_assembly.R
```

## Software environment

The analysis was conducted using:

```text
R version 4.5.2
```

The complete R session information is available in:

```text
reproducibility/R_sessionInfo.txt
```

Core R packages used across the pipeline include:

- `data.table`
- `stringr`
- `ggplot2`
- `countrycode`
- `sf`
- `spdep`
- `rnaturalearth`
- `rnaturalearthdata`
- `ranger`

Required packages should be installed before running the relevant scripts.

## Reproducibility notes

- Run all scripts from the repository root.
- Run the scripts in the sequence specified above.
- Earlier phases generate files required by later phases.
- The restricted source dataset is not distributed through this repository.
- Generated analytical output folders are excluded from version control.
- Model objects and record-level prediction files are excluded from version control.
- Full reproduction requires authorised access to the Vivli AMR / Pfizer ATLAS dataset.

## Project scope

**Organism**

```text
Klebsiella pneumoniae
```

**Antibiotics**

```text
Cefepime
Ceftazidime
Meropenem
```

**Study period**

```text
2004–2024
```

**Geographic unit**

```text
Country
```

**Primary modelling unit**

```text
Isolate-antibiotic observation
```

## Authors

Developed by Reza Fahimi.

## Licence

No software licence has currently been assigned.

Reuse of the scripts should be discussed with the project team and must comply with the Vivli AMR data-access and data-use conditions.
