# Phase 5C: Regional Comparison and Temporal Trends
# Project: Vivli AMR / Pfizer ATLAS Dataset
# Scope:
# Klebsiella pneumoniae resistance to Cefepime, Ceftazidime, and Meropenem
#
# Purpose:
# Compare resistance across World Bank regions and describe temporal trends by region.
# This phase does not run Moran's I and does not build ML models.


# Step 1: Load packages
library(data.table)
library(stringr)
library(ggplot2)


# Step 2: Create Phase 5C output folder
phase5c_dir <- "phase5c_outputs"
dir.create(phase5c_dir, showWarnings = FALSE)


# Step 3: Define input files
phase5_dir <- "phase5_outputs"
phase5b_dir <- "phase5b_outputs"

country_year_file <- file.path(
  phase5_dir,
  "phase5_country_year_spatial_ready.csv"
)

region_mapping_file <- file.path(
  phase5b_dir,
  "phase5b_region_mapping_table.csv"
)

country_antibiotic_file <- file.path(
  phase5b_dir,
  "phase5b_country_antibiotic_choropleth_ready.csv"
)

file.exists(country_year_file)
file.exists(region_mapping_file)
file.exists(country_antibiotic_file)


# Step 4: Read input files
country_year_spatial_ready <- fread(country_year_file)
region_mapping_table <- fread(region_mapping_file)
country_antibiotic_choropleth_ready <- fread(country_antibiotic_file)

dim(country_year_spatial_ready)
dim(region_mapping_table)
dim(country_antibiotic_choropleth_ready)

names(country_year_spatial_ready)
names(region_mapping_table)
names(country_antibiotic_choropleth_ready)

head(country_year_spatial_ready)
head(region_mapping_table)
head(country_antibiotic_choropleth_ready)


# Step 5: Prepare clean region mapping table
# Main region variable for Phase 5C is wb_region.
# This table contains only Phase 5B spatial-ready countries.

region_mapping_small <- unique(
  region_mapping_table[, .(
    Country_clean,
    iso3,
    continent,
    wb_region,
    spatial_unit_note,
    map_joined
  )]
)

dim(region_mapping_small)

region_mapping_small[is.na(wb_region)]


# Step 6: Restrict country-year data to Phase 5B spatial-ready countries
# This keeps Phase 5C consistent with the choropleth/ranking dataset.

spatial_ready_country_set <- unique(
  region_mapping_small[, .(
    Country_clean,
    iso3
  )]
)

country_year_spatial_ready_5c <- merge(
  country_year_spatial_ready,
  spatial_ready_country_set,
  by = c("Country_clean", "iso3"),
  all = FALSE
)

dim(country_year_spatial_ready)
dim(country_year_spatial_ready_5c)

uniqueN(country_year_spatial_ready$iso3)
uniqueN(country_year_spatial_ready_5c$iso3)


# Step 7: Join region information onto filtered country-year table

country_year_region <- merge(
  country_year_spatial_ready_5c,
  region_mapping_small,
  by = c("Country_clean", "iso3"),
  all.x = TRUE
)

dim(country_year_region)
head(country_year_region)


# Step 8: Check missing region after join
missing_region_check <- country_year_region[
  is.na(wb_region),
  .N,
  by = .(Country, Country_clean, iso3)
][order(Country_clean)]

missing_region_check
nrow(missing_region_check)

fwrite(
  missing_region_check,
  file.path(phase5c_dir, "phase5c_missing_region_check.csv")
)


# Step 9: Join region information onto country-antibiotic aggregate table
# This remains useful for region-level country coverage checks.

cols_to_remove_aggregate <- c(
  "continent",
  "wb_region",
  "spatial_unit_note",
  "map_joined"
)

cols_to_remove_aggregate <- cols_to_remove_aggregate[
  cols_to_remove_aggregate %in% names(country_antibiotic_choropleth_ready)
]

if (length(cols_to_remove_aggregate) > 0) {
  country_antibiotic_choropleth_ready[, (cols_to_remove_aggregate) := NULL]
}

country_antibiotic_region <- merge(
  country_antibiotic_choropleth_ready,
  region_mapping_small,
  by = c("Country_clean", "iso3"),
  all.x = TRUE
)

dim(country_antibiotic_region)
country_antibiotic_region[is.na(wb_region)]


# Step 10: Input coverage check by antibiotic and region
region_input_check <- country_year_region[, .(
  country_year_cells = .N,
  countries = uniqueN(Country),
  years = uniqueN(Year),
  total_interpretation_n = sum(interpretation_n, na.rm = TRUE),
  min_year = min(Year_num, na.rm = TRUE),
  max_year = max(Year_num, na.rm = TRUE)
), by = .(antibiotic, wb_region)][
  order(antibiotic, wb_region)
]

region_input_check

fwrite(
  region_input_check,
  file.path(phase5c_dir, "phase5c_region_input_check.csv")
)


# Step 11: Country-level regional coverage check
region_country_coverage_check <- country_antibiotic_region[, .(
  countries = uniqueN(Country),
  total_interpretation_n = sum(total_interpretation_n, na.rm = TRUE),
  median_country_resistance_pct = round(median(resistance_pct, na.rm = TRUE), 2),
  min_country_resistance_pct = round(min(resistance_pct, na.rm = TRUE), 2),
  max_country_resistance_pct = round(max(resistance_pct, na.rm = TRUE), 2)
), by = .(antibiotic, wb_region)][
  order(antibiotic, wb_region)
]

region_country_coverage_check

fwrite(
  region_country_coverage_check,
  file.path(phase5c_dir, "phase5c_region_coverage_check.csv")
)


# Step 12: Create region-antibiotic summary
# Correct aggregation: group by antibiotic and wb_region only.

region_antibiotic_summary <- country_year_region[, .(
  interpretation_n = as.integer(sum(interpretation_n, na.rm = TRUE)),
  countries = as.integer(uniqueN(Country)),
  years = as.integer(uniqueN(Year)),
  country_year_cells = as.integer(.N),
  n_susceptible = as.integer(sum(n_susceptible, na.rm = TRUE)),
  n_intermediate = as.integer(sum(n_intermediate, na.rm = TRUE)),
  n_resistant = as.integer(sum(n_resistant, na.rm = TRUE))
), by = .(antibiotic, wb_region)]

region_antibiotic_summary[, resistance_pct := round(
  100 * n_resistant / interpretation_n,
  2
)]

region_antibiotic_summary[, non_susceptible_pct := round(
  100 * (n_resistant + n_intermediate) / interpretation_n,
  2
)]

region_antibiotic_summary <- region_antibiotic_summary[
  order(antibiotic, -resistance_pct)
]

region_antibiotic_summary

fwrite(
  region_antibiotic_summary,
  file.path(phase5c_dir, "phase5c_region_antibiotic_summary.csv")
)


# Step 13: Create region-year temporal trend
# Correct aggregation: group by antibiotic, wb_region, and year only.

region_year_trend <- country_year_region[, .(
  interpretation_n = as.integer(sum(interpretation_n, na.rm = TRUE)),
  countries = as.integer(uniqueN(Country)),
  n_susceptible = as.integer(sum(n_susceptible, na.rm = TRUE)),
  n_intermediate = as.integer(sum(n_intermediate, na.rm = TRUE)),
  n_resistant = as.integer(sum(n_resistant, na.rm = TRUE))
), by = .(antibiotic, wb_region, Year, Year_num)]

region_year_trend[, resistance_pct := round(
  100 * n_resistant / interpretation_n,
  2
)]

region_year_trend[, non_susceptible_pct := round(
  100 * (n_resistant + n_intermediate) / interpretation_n,
  2
)]

region_year_trend[, enough_region_year_n_100 := interpretation_n >= 100]
region_year_trend[, enough_region_year_n_300 := interpretation_n >= 300]
region_year_trend[, enough_region_year_n_500 := interpretation_n >= 500]

region_year_trend <- region_year_trend[
  order(antibiotic, wb_region, Year_num)
]

region_year_trend

fwrite(
  region_year_trend,
  file.path(phase5c_dir, "phase5c_region_year_trend.csv")
)


# Step 14: Temporal coverage summary
region_temporal_coverage_check <- region_year_trend[, .(
  region_year_rows = .N,
  years_observed = uniqueN(Year),
  years_n_ge_100 = sum(enough_region_year_n_100),
  years_n_ge_300 = sum(enough_region_year_n_300),
  years_n_ge_500 = sum(enough_region_year_n_500),
  median_region_year_n = round(median(interpretation_n, na.rm = TRUE), 1),
  min_region_year_n = min(interpretation_n, na.rm = TRUE),
  max_region_year_n = max(interpretation_n, na.rm = TRUE)
), by = .(antibiotic, wb_region)][
  order(antibiotic, wb_region)
]

region_temporal_coverage_check

fwrite(
  region_temporal_coverage_check,
  file.path(phase5c_dir, "phase5c_region_temporal_coverage_check.csv")
)


# Step 15: Figure 1, regional comparison
fig_region_comparison <- ggplot(
  region_antibiotic_summary,
  aes(
    x = resistance_pct,
    y = reorder(wb_region, resistance_pct)
  )
) +
  geom_col() +
  facet_wrap(~ antibiotic, ncol = 1) +
  labs(
    title = "Regional K. pneumoniae resistance among spatial-ready countries",
    subtitle = "Region is defined using World Bank region; country-year rows have n ≥ 30",
    x = "Resistance (%)",
    y = "World Bank region",
    caption = "Source: Vivli AMR / Pfizer ATLAS dataset, 2004–2024"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 11),
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 11)
  )

fig_region_comparison

ggsave(
  filename = file.path(phase5c_dir, "phase5c_fig_region_comparison.png"),
  plot = fig_region_comparison,
  width = 10,
  height = 9,
  dpi = 300
)


# Step 16: Figure 2, temporal trend by region
fig_temporal_trend_by_region <- ggplot(
  region_year_trend,
  aes(
    x = Year_num,
    y = resistance_pct,
    group = wb_region
  )
) +
  geom_line() +
  geom_point(aes(size = interpretation_n), alpha = 0.7) +
  facet_grid(antibiotic ~ wb_region) +
  scale_size_continuous(
    name = "Interpretation records",
    range = c(1, 4)
  ) +
  labs(
    title = "Temporal trends in K. pneumoniae resistance by region",
    subtitle = "Region is defined using World Bank region; point size reflects region-year sample size",
    x = "Year",
    y = "Resistance (%)",
    caption = "Source: Vivli AMR / Pfizer ATLAS dataset, 2004–2024"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  )

fig_temporal_trend_by_region

ggsave(
  filename = file.path(phase5c_dir, "phase5c_fig_temporal_trend_by_region.png"),
  plot = fig_temporal_trend_by_region,
  width = 18,
  height = 9,
  dpi = 300
)


# Step 17: Figure 3, filtered temporal trend with n ≥ 500
region_year_trend_n500 <- region_year_trend[
  enough_region_year_n_500 == TRUE
]

region_year_trend_n500[, n_years_in_plot := .N, by = .(antibiotic, wb_region)]

region_year_trend_n500_plot <- region_year_trend_n500[
  n_years_in_plot >= 2
]

region_year_trend_n500_single_year <- region_year_trend_n500[
  n_years_in_plot == 1
]

fwrite(
  region_year_trend_n500_plot,
  file.path(phase5c_dir, "phase5c_region_year_trend_n500_for_plot.csv")
)

fwrite(
  region_year_trend_n500_single_year,
  file.path(phase5c_dir, "phase5c_region_year_trend_n500_single_year_groups.csv")
)

fig_temporal_trend_by_region_n500 <- ggplot(
  region_year_trend_n500_plot,
  aes(
    x = Year_num,
    y = resistance_pct,
    group = wb_region
  )
) +
  geom_line() +
  geom_point(aes(size = interpretation_n), alpha = 0.7) +
  facet_grid(antibiotic ~ wb_region) +
  scale_size_continuous(
    name = "Interpretation records",
    range = c(1, 4)
  ) +
  labs(
    title = "Temporal trends in K. pneumoniae resistance by region",
    subtitle = "Only region-year strata with ≥500 interpretation records are shown; groups need ≥2 eligible years for line plotting",
    x = "Year",
    y = "Resistance (%)",
    caption = "Source: Vivli AMR / Pfizer ATLAS dataset, 2004–2024"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  )

fig_temporal_trend_by_region_n500

ggsave(
  filename = file.path(phase5c_dir, "phase5c_fig_temporal_trend_by_region_n500.png"),
  plot = fig_temporal_trend_by_region_n500,
  width = 18,
  height = 9,
  dpi = 300
)


# Step 18: Final checks
dim(country_year_region)
missing_region_check
nrow(missing_region_check)

region_input_check
region_country_coverage_check
region_antibiotic_summary
region_temporal_coverage_check
region_year_trend_n500_single_year

list.files(phase5c_dir)