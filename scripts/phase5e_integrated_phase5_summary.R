# Phase 5E: Integrated Phase 5 Summary
# Project: Vivli AMR / Pfizer ATLAS Dataset
# Scope:
# Klebsiella pneumoniae resistance to Cefepime, Ceftazidime, and Meropenem
#
# Purpose:
# Integrate outputs from Phase 5B, Phase 5C, Phase 5D-A, and Phase 5D-B
# into clean report-ready tables and narrative summaries.
#
# This phase does not create new inferential analyses.


# Step 1: Load packages
library(data.table)
library(stringr)
library(ggplot2)


# Step 2: Create dedicated Phase 5E output folder
phase5e_dir <- "phase5e_outputs"
dir.create(phase5e_dir, showWarnings = FALSE)


# Step 3: Define input folders
phase5b_dir <- "phase5b_outputs"
phase5c_dir <- "phase5c_outputs"
phase5d_a_dir <- "phase5d_outputs"
phase5d_b_dir <- "phase5d_outputs"


# Step 4: Define required input files
input_file_check <- data.table(
  input_name = c(
    "Phase 5B country-antibiotic choropleth-ready table",
    "Phase 5B map join summary",
    "Phase 5C region-antibiotic summary",
    "Phase 5C region temporal coverage check",
    "Phase 5D-A weights diagnostics",
    "Phase 5D-B Moran's I interpreted results",
    "Phase 5D-B Moran's I primary k4 results",
    "Phase 5D-B Moran's I sensitivity k6 results"
  ),
  file_path = c(
    file.path(phase5b_dir, "phase5b_country_antibiotic_choropleth_ready.csv"),
    file.path(phase5b_dir, "phase5b_map_join_summary.csv"),
    file.path(phase5c_dir, "phase5c_region_antibiotic_summary.csv"),
    file.path(phase5c_dir, "phase5c_region_temporal_coverage_check.csv"),
    file.path(phase5d_a_dir, "phase5d_weights_diagnostics.csv"),
    file.path(phase5d_b_dir, "phase5d_morans_i_results_interpreted.csv"),
    file.path(phase5d_b_dir, "phase5d_morans_i_primary_k4_results.csv"),
    file.path(phase5d_b_dir, "phase5d_morans_i_sensitivity_k6_results.csv")
  )
)

input_file_check[, file_exists := file.exists(file_path)]

input_file_check

fwrite(
  input_file_check,
  file.path(phase5e_dir, "phase5e_input_file_check.csv")
)

if (any(input_file_check$file_exists == FALSE)) {
  stop("One or more required input files are missing. Check phase output folders before continuing.")
}


# Step 5: Read inputs
country_antibiotic <- fread(
  file.path(phase5b_dir, "phase5b_country_antibiotic_choropleth_ready.csv")
)

map_join_summary <- fread(
  file.path(phase5b_dir, "phase5b_map_join_summary.csv")
)

region_antibiotic_summary <- fread(
  file.path(phase5c_dir, "phase5c_region_antibiotic_summary.csv")
)

region_temporal_coverage_check <- fread(
  file.path(phase5c_dir, "phase5c_region_temporal_coverage_check.csv")
)

weights_diagnostics <- fread(
  file.path(phase5d_a_dir, "phase5d_weights_diagnostics.csv")
)

moran_results_interpreted <- fread(
  file.path(phase5d_b_dir, "phase5d_morans_i_results_interpreted.csv")
)

moran_primary_k4 <- fread(
  file.path(phase5d_b_dir, "phase5d_morans_i_primary_k4_results.csv")
)

moran_sensitivity_k6 <- fread(
  file.path(phase5d_b_dir, "phase5d_morans_i_sensitivity_k6_results.csv")
)


# Step 6: Country-level spatial summary by antibiotic
phase5e_country_level_summary <- country_antibiotic[, .(
  countries = uniqueN(iso3),
  total_interpretation_n = sum(total_interpretation_n, na.rm = TRUE),
  median_country_resistance_pct = round(median(resistance_pct, na.rm = TRUE), 2),
  min_country_resistance_pct = round(min(resistance_pct, na.rm = TRUE), 2),
  max_country_resistance_pct = round(max(resistance_pct, na.rm = TRUE), 2),
  highest_resistance_country = Country[which.max(resistance_pct)],
  highest_resistance_pct = round(max(resistance_pct, na.rm = TRUE), 2),
  lowest_resistance_country = Country[which.min(resistance_pct)],
  lowest_resistance_pct = round(min(resistance_pct, na.rm = TRUE), 2)
), by = antibiotic][order(antibiotic)]

phase5e_country_level_summary

fwrite(
  phase5e_country_level_summary,
  file.path(phase5e_dir, "phase5e_country_level_summary.csv")
)


# Step 7: Top 10 countries by resistance for each antibiotic
phase5e_top10_countries_by_antibiotic <- country_antibiotic[
  order(antibiotic, -resistance_pct)
][
  ,
  .SD[1:min(.N, 10)],
  by = antibiotic
][
  ,
  .(
    antibiotic,
    Country,
    Country_clean,
    iso3,
    wb_region,
    total_interpretation_n,
    n_years_ge_30,
    resistance_pct,
    non_susceptible_pct,
    spatial_unit_note
  )
]

phase5e_top10_countries_by_antibiotic

fwrite(
  phase5e_top10_countries_by_antibiotic,
  file.path(phase5e_dir, "phase5e_top10_countries_by_antibiotic.csv")
)


# Step 8: Region-level report table
phase5e_region_report_table <- copy(region_antibiotic_summary)

phase5e_region_report_table[, region_interpretation_caveat := fifelse(
  countries == 1,
  "single-country region in spatial-ready dataset",
  fifelse(
    countries <= 3,
    "limited-country region in spatial-ready dataset",
    "multi-country region in spatial-ready dataset"
  )
)]

phase5e_region_report_table <- phase5e_region_report_table[
  order(antibiotic, -resistance_pct)
]

phase5e_region_report_table

fwrite(
  phase5e_region_report_table,
  file.path(phase5e_dir, "phase5e_region_report_table.csv")
)


# Step 9: Top region per antibiotic
phase5e_top_region_by_antibiotic <- phase5e_region_report_table[
  ,
  .SD[1],
  by = antibiotic
][
  ,
  .(
    antibiotic,
    top_region = wb_region,
    resistance_pct,
    non_susceptible_pct,
    countries,
    interpretation_n,
    region_interpretation_caveat
  )
]

phase5e_top_region_by_antibiotic

fwrite(
  phase5e_top_region_by_antibiotic,
  file.path(phase5e_dir, "phase5e_top_region_by_antibiotic.csv")
)


# Step 10: Temporal coverage report table
phase5e_temporal_coverage_report_table <- copy(region_temporal_coverage_check)

phase5e_temporal_coverage_report_table[, n500_coverage_category := fifelse(
  years_n_ge_500 >= 10,
  "strong n>=500 temporal coverage",
  fifelse(
    years_n_ge_500 >= 2,
    "moderate n>=500 temporal coverage",
    "weak n>=500 temporal coverage"
  )
)]

phase5e_temporal_coverage_report_table <- phase5e_temporal_coverage_report_table[
  order(antibiotic, wb_region)
]

phase5e_temporal_coverage_report_table

fwrite(
  phase5e_temporal_coverage_report_table,
  file.path(phase5e_dir, "phase5e_temporal_coverage_report_table.csv")
)


# Step 11: Spatial weights decision table
phase5e_weights_decision_table <- copy(weights_diagnostics)

phase5e_weights_decision_table[, methodological_decision := fifelse(
  weights_type == "queen_contiguity" &
    (zero_neighbour_units > 0 | connected_components > 1),
  "not used as primary weights: disconnected graph and zero-neighbour units",
  fifelse(
    weights_type == "symmetrised_knn_k4",
    "primary Moran's I weights",
    fifelse(
      weights_type == "symmetrised_knn_k6",
      "sensitivity Moran's I weights",
      "not classified"
    )
  )
)]

phase5e_weights_decision_table <- phase5e_weights_decision_table[
  order(antibiotic, weights_type)
]

phase5e_weights_decision_table

fwrite(
  phase5e_weights_decision_table,
  file.path(phase5e_dir, "phase5e_weights_decision_table.csv")
)


# Step 12: Moran's I report table
phase5e_morans_i_report_table <- copy(moran_results_interpreted)

phase5e_morans_i_report_table[, moran_i_rounded := round(moran_i, 3)]
phase5e_morans_i_report_table[, z_value_rounded := round(z_value, 3)]

phase5e_morans_i_report_table[, p_value_permutation_label := fifelse(
  p_value_permutation == 0,
  "p < 0.001",
  paste0("p = ", formatC(p_value_permutation, format = "f", digits = 3))
)]

phase5e_morans_i_report_table[, p_value_asymptotic_label := fifelse(
  p_value_asymptotic < 0.001,
  "p < 0.001",
  paste0("p = ", formatC(p_value_asymptotic, format = "f", digits = 3))
)]

phase5e_morans_i_report_table <- phase5e_morans_i_report_table[
  ,
  .(
    antibiotic,
    outcome,
    weights_type,
    k,
    countries,
    moran_i = moran_i_rounded,
    expected_i = round(expected_i, 3),
    z_value = z_value_rounded,
    p_value_permutation_label,
    p_value_asymptotic_label,
    evidence_label
  )
][order(antibiotic, k)]

phase5e_morans_i_report_table

fwrite(
  phase5e_morans_i_report_table,
  file.path(phase5e_dir, "phase5e_morans_i_report_table.csv")
)


# Step 13: Primary Moran's I table only
phase5e_primary_morans_i_k4_report <- phase5e_morans_i_report_table[
  k == 4
][order(antibiotic)]

phase5e_primary_morans_i_k4_report

fwrite(
  phase5e_primary_morans_i_k4_report,
  file.path(phase5e_dir, "phase5e_primary_morans_i_k4_report.csv")
)


# Step 14: Methodological decision log
phase5e_method_decision_log <- data.table(
  phase_component = c(
    "Phase 5B spatial visualisation",
    "Phase 5C regional comparison",
    "Phase 5C temporal trends",
    "Phase 5D-A spatial weights diagnostics",
    "Phase 5D-B Moran's I primary analysis",
    "Phase 5D-B Moran's I sensitivity analysis",
    "Phase 5D interpretation scope"
  ),
  decision = c(
    "Use spatial-ready country-antibiotic aggregated data for choropleth maps and country ranking.",
    "Use World Bank region as the primary regional grouping variable.",
    "Use country-year spatial-ready data joined to World Bank region; aggregate by antibiotic, region, and year.",
    "Do not use queen contiguity as primary weights because the graph is disconnected and has zero-neighbour units.",
    "Use symmetrised k-nearest neighbours with k = 4 as primary spatial weights.",
    "Use symmetrised k-nearest neighbours with k = 6 as sensitivity spatial weights.",
    "Interpret Moran's I as exploratory country-level spatial autocorrelation, not causal spatial transmission."
  ),
  rationale = c(
    "Spatial-ready countries have sufficient country-level coverage and map join success.",
    "World Bank region avoids missing regional assignment for special units such as Taiwan.",
    "Temporal trends require country-year input rather than across-year choropleth aggregates.",
    "Queen contiguity is unsuitable for island countries and geographically fragmented country samples.",
    "k = 4 creates a connected graph while remaining relatively sparse.",
    "k = 6 tests whether conclusions are robust to a denser neighbour definition.",
    "The analysis is based on aggregated surveillance data and spatial proximity weights."
  )
)

phase5e_method_decision_log

fwrite(
  phase5e_method_decision_log,
  file.path(phase5e_dir, "phase5e_method_decision_log.csv")
)


# Step 15: Compact Moran's I figure
fig_phase5e_morans_i_summary <- ggplot(
  phase5e_morans_i_report_table,
  aes(
    x = antibiotic,
    y = moran_i,
    group = weights_type,
    shape = weights_type
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.6) +
  labs(
    title = "Moran's I summary for country-level K. pneumoniae resistance",
    subtitle = "Primary analysis uses symmetrised kNN k=4; k=6 shown as sensitivity",
    x = "Antibiotic",
    y = "Moran's I",
    shape = "Spatial weights",
    caption = "Source: Vivli AMR / Pfizer ATLAS dataset, 2004–2024"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  )

fig_phase5e_morans_i_summary

ggsave(
  filename = file.path(phase5e_dir, "phase5e_fig_morans_i_summary.png"),
  plot = fig_phase5e_morans_i_summary,
  width = 9,
  height = 6,
  dpi = 300
)


# Step 16: Narrative summary text file
narrative_lines <- c(
  "Phase 5 Integrated Summary",
  "Project: Vivli AMR / Pfizer ATLAS Dataset",
  "Scope: Klebsiella pneumoniae resistance to Cefepime, Ceftazidime, and Meropenem",
  "",
  "Phase 5B established that country-level spatial visualisation was feasible for the selected organism-antibiotic scope.",
  "Choropleth-ready country-antibiotic datasets, country ranking tables, and map join checks were generated.",
  "",
  "Phase 5C compared resistance across World Bank regions and described temporal trends by region among spatial-ready countries.",
  "World Bank region was used as the primary regional grouping variable. Regions with one or few contributing countries should be interpreted as summaries of the spatial-ready dataset, not as fully representative regional estimates.",
  "",
  "Phase 5D-A evaluated candidate spatial weights. Queen contiguity was not used as the primary weights definition because it produced disconnected graphs and zero-neighbour units.",
  "Symmetrised k-nearest-neighbour weights were retained: k = 4 for the primary analysis and k = 6 for sensitivity analysis.",
  "",
  "Phase 5D-B showed positive spatial autocorrelation in country-level resistance for all three antibiotics.",
  "The evidence was strongest for Cefepime and Ceftazidime, and weaker but still present for Meropenem.",
  "",
  "Interpretation: among spatial-ready countries, countries with higher resistance tend to be geographically near countries with higher resistance, and countries with lower resistance tend to be near countries with lower resistance.",
  "This is an exploratory spatial autocorrelation analysis based on aggregated surveillance data and should not be interpreted as causal evidence of spatial transmission.",
  ""
)

writeLines(
  narrative_lines,
  con = file.path(phase5e_dir, "phase5e_integrated_summary_narrative.txt")
)


# Step 17: Final file list
phase5e_file_list <- data.table(
  file_name = list.files(phase5e_dir)
)

phase5e_file_list

fwrite(
  phase5e_file_list,
  file.path(phase5e_dir, "phase5e_output_file_list.csv")
)

list.files(phase5e_dir)

input_file_check
phase5e_country_level_summary
phase5e_top_region_by_antibiotic
phase5e_primary_morans_i_k4_report
list.files("phase5e_outputs")