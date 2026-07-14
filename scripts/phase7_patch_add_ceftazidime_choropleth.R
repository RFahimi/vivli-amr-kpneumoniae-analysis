# Phase 7 Patch: Add Ceftazidime choropleth to final figures
# Purpose:
# Complete the final reporting package by adding the Ceftazidime choropleth.
# This is not a new analysis; it only updates the Phase 7 figure package.

library(data.table)

phase7_dir <- "phase7_outputs"
phase7_figures_dir <- file.path(phase7_dir, "final_figures")

dir.create(phase7_dir, showWarnings = FALSE)
dir.create(phase7_figures_dir, showWarnings = FALSE)

# Rebuild final figure set with all three single-antibiotic choropleths:
# Cefepime, Ceftazidime, Meropenem.

key_figures_v2 <- data.table(
  section = c(
    "descriptive_results",
    "descriptive_results",
    "spatial_results",
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
    file.path("phase5b_outputs", "phase5b_fig_choropleth_ceftazidime.png"),
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
    file.path(phase7_figures_dir, "figure_05_phase5b_choropleth_ceftazidime.png"),
    file.path(phase7_figures_dir, "figure_06_phase5b_choropleth_meropenem.png"),
    file.path(phase7_figures_dir, "figure_07_phase5c_temporal_trend_by_region_n500.png"),
    file.path(phase7_figures_dir, "figure_08_phase5d_moran_scatter_k4.png"),
    file.path(phase7_figures_dir, "figure_09_phase5e_morans_i_summary.png"),
    file.path(phase7_figures_dir, "figure_10_phase6e_final_auc_comparison.png"),
    file.path(phase7_figures_dir, "figure_11_phase6e_auc_delta_rf_minus_logistic.png"),
    file.path(phase7_figures_dir, "figure_12_phase6e_calibration_error.png")
  )
)

key_figures_v2[, source_exists := file.exists(source_file)]

if (any(key_figures_v2$source_exists == FALSE)) {
  print(key_figures_v2[source_exists == FALSE])
  stop("One or more source figures are missing. Check before copying.")
}

# Remove old final figure PNGs to avoid duplicate numbering.
old_final_figures <- list.files(
  phase7_figures_dir,
  pattern = "\\.png$",
  full.names = TRUE
)

if (length(old_final_figures) > 0) {
  unlink(old_final_figures)
}

key_figures_v2[, copy_success := file.copy(
  from = source_file,
  to = destination_file,
  overwrite = TRUE
)]

key_figures_v2

fwrite(
  key_figures_v2,
  file.path(phase7_dir, "phase7_key_figures_copy_check.csv")
)

phase7_figure_inventory <- key_figures_v2[
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

# Update reporting checklist
phase7_reporting_package_checklist <- fread(
  file.path(phase7_dir, "phase7_reporting_package_checklist.csv")
)

phase7_reporting_package_checklist[
  item == "key_figures_copied",
  status := all(key_figures_v2$copy_success == TRUE)
]

phase7_reporting_package_checklist[
  item == "figure_inventory_created",
  status := file.exists(file.path(phase7_dir, "phase7_final_figures_inventory.csv"))
]

fwrite(
  phase7_reporting_package_checklist,
  file.path(phase7_dir, "phase7_reporting_package_checklist.csv")
)

# Rewrite output file list twice so phase7_output_file_list.csv includes itself.
phase7_output_file_list <- data.table(
  file_path = list.files(
    phase7_dir,
    recursive = TRUE,
    full.names = FALSE
  )
)

fwrite(
  phase7_output_file_list,
  file.path(phase7_dir, "phase7_output_file_list.csv")
)

phase7_output_file_list <- data.table(
  file_path = list.files(
    phase7_dir,
    recursive = TRUE,
    full.names = FALSE
  )
)

fwrite(
  phase7_output_file_list,
  file.path(phase7_dir, "phase7_output_file_list.csv")
)

# Final checks
key_figures_v2[, .(section, source_file, source_exists, copy_success)]
phase7_figure_inventory
phase7_reporting_package_checklist
list.files(phase7_figures_dir)
list.files(phase7_dir, recursive = TRUE)