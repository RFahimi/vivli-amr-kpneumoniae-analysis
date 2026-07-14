# Phase 5B: Spatial Visualisation Package
# Project: Vivli AMR / Pfizer ATLAS Dataset
# Scope:
# Klebsiella pneumoniae resistance to Cefepime, Ceftazidime, and Meropenem
#
# Purpose:
# Build choropleth-ready datasets, country-level maps, and region mapping tables.
# This phase does not run Moran's I.

# Step 1: Load packages
library(data.table)
library(stringr)
library(countrycode)
library(ggplot2)

# These packages are needed for country maps.
if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}

if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
  install.packages("rnaturalearth")
}

if (!requireNamespace("rnaturalearthdata", quietly = TRUE)) {
  install.packages("rnaturalearthdata")
}

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


# Step 2: Create Phase 5B output folder
phase5b_dir <- "phase5b_outputs"
dir.create(phase5b_dir, showWarnings = FALSE)


# Step 3: Define input files from Phase 5
phase5_dir <- "phase5_outputs"

spatial_ready_file <- file.path(
  phase5_dir,
  "phase5_country_antibiotic_spatial_ready.csv"
)

mapping_file <- file.path(
  phase5_dir,
  "phase5_country_mapping_check.csv"
)

file.exists(spatial_ready_file)
file.exists(mapping_file)


# Step 4: Read Phase 5 outputs
country_antibiotic_spatial_ready <- fread(spatial_ready_file)
country_mapping_check <- fread(mapping_file)

dim(country_antibiotic_spatial_ready)
dim(country_mapping_check)

head(country_antibiotic_spatial_ready)
head(country_mapping_check)


# Step 5: Add region and continent fields
# These variables will be useful for Phase 5C.

country_antibiotic_spatial_ready[, continent := countrycode(
  iso3,
  origin = "iso3c",
  destination = "continent",
  warn = FALSE
)]

country_antibiotic_spatial_ready[, un_region := countrycode(
  iso3,
  origin = "iso3c",
  destination = "un.region.name",
  warn = FALSE
)]

country_antibiotic_spatial_ready[, un_subregion := countrycode(
  iso3,
  origin = "iso3c",
  destination = "un.regionsub.name",
  warn = FALSE
)]

country_antibiotic_spatial_ready[, wb_region := countrycode(
  iso3,
  origin = "iso3c",
  destination = "region",
  warn = FALSE
)]

#checking
head(country_antibiotic_spatial_ready[, .(
  Country,
  iso3,
  continent,
  un_region,
  un_subregion,
  wb_region
)])


# Step 6: Flag special spatial units
# These can be kept for choropleth maps if the map joins correctly.
# For Moran's I later, they may need separate treatment.
special_spatial_units <- c("HKG", "PRI", "TWN")

country_antibiotic_spatial_ready[, spatial_unit_note := fifelse(
  iso3 %in% special_spatial_units,
  "special_region_or_territory",
  "standard_country_unit"
)]


# Step 7: Load world map geometry
world <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

names(world)

# rnaturalearth usually has iso_a3.
# Some entries may use adm0_a3 more reliably.
world$map_iso3 <- world$iso_a3

if ("adm0_a3" %in% names(world)) {
  bad_iso <- is.na(world$map_iso3) | world$map_iso3 == "-99"
  world$map_iso3[bad_iso] <- world$adm0_a3[bad_iso]
}

# Keep only rows with usable ISO3 code
world <- world[
  !is.na(world$map_iso3) &
    world$map_iso3 != "-99",
]

unique(world$map_iso3)[1:20]


# Step 8: Check map join
map_iso3_table <- data.table(
  iso3 = unique(world$map_iso3)
)

country_antibiotic_spatial_ready[, map_joined := iso3 %in% map_iso3_table$iso3]

map_join_check <- unique(
  country_antibiotic_spatial_ready[, .(
    Country,
    Country_clean,
    iso3,
    continent,
    un_region,
    wb_region,
    spatial_unit_note,
    map_joined
  )]
)[order(map_joined, Country_clean)]

map_join_check

fwrite(
  map_join_check,
  file.path(phase5b_dir, "phase5b_map_join_check.csv")
)


# Step 9: Save choropleth-ready CSV without geometry
choropleth_ready_csv <- country_antibiotic_spatial_ready[, .(
  antibiotic,
  Country,
  Country_clean,
  iso3,
  continent,
  un_region,
  un_subregion,
  wb_region,
  spatial_unit_note,
  map_joined,
  total_interpretation_n,
  n_years_observed,
  n_years_ge_30,
  median_year_n,
  resistance_pct,
  non_susceptible_pct
)]

fwrite(
  choropleth_ready_csv,
  file.path(phase5b_dir, "phase5b_country_antibiotic_choropleth_ready.csv")
)


# Step 10: Create region mapping table
region_mapping_table <- unique(
  choropleth_ready_csv[, .(
    Country,
    Country_clean,
    iso3,
    continent,
    un_region,
    un_subregion,
    wb_region,
    spatial_unit_note,
    map_joined
  )]
)[order(continent, un_region, Country_clean)]

fwrite(
  region_mapping_table,
  file.path(phase5b_dir, "phase5b_region_mapping_table.csv")
)


# Step 11: Create country ranking table
country_ranking_table <- copy(choropleth_ready_csv)

country_ranking_table <- country_ranking_table[
  order(antibiotic, -resistance_pct)
]

country_ranking_table[, resistance_rank := seq_len(.N), by = antibiotic]

fwrite(
  country_ranking_table,
  file.path(phase5b_dir, "phase5b_country_ranking_table.csv")
)

country_ranking_table[
  resistance_rank <= 10,
  .(
    antibiotic,
    resistance_rank,
    Country,
    iso3,
    resistance_pct,
    non_susceptible_pct,
    total_interpretation_n,
    n_years_ge_30
  )
]


# Step 12: Join spatial-ready data to world geometry
# This creates an sf object with one geometry row per country-antibiotic combination.
choropleth_sf <- merge(
  world,
  as.data.frame(country_antibiotic_spatial_ready[map_joined == TRUE]),
  by.x = "map_iso3",
  by.y = "iso3",
  all = FALSE,
  sort = FALSE
)

dim(choropleth_sf)
unique(choropleth_sf$antibiotic)


# Step 13: Save geometry dataset as GeoPackage
# This is useful if you later want to open the layer in QGIS.
sf::st_write(
  choropleth_sf,
  file.path(phase5b_dir, "phase5b_choropleth_ready.gpkg"),
  delete_dsn = TRUE
)


# Step 14: Create one faceted choropleth map
# This uses the same resistance scale across all three antibiotics.
max_resistance <- max(
  choropleth_sf$resistance_pct,
  na.rm = TRUE
)

fig_all <- ggplot() +
  geom_sf(
    data = world,
    fill = "grey95",
    color = "white",
    linewidth = 0.1
  ) +
  geom_sf(
    data = choropleth_sf,
    aes(fill = resistance_pct),
    color = "grey40",
    linewidth = 0.1
  ) +
  facet_wrap(~ antibiotic, ncol = 1) +
  scale_fill_viridis_c(
    option = "magma",
    limits = c(0, max_resistance),
    name = "Resistance (%)",
    na.value = "grey90"
  ) +
  labs(
    title = "Country-level Klebsiella pneumoniae resistance",
    subtitle = "Countries shown meet: total interpretation records ≥ 500 and ≥5 years with n ≥ 30",
    caption = "Source: Vivli AMR / Pfizer ATLAS dataset, 2004–2024"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(size = 11),
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  )

fig_all

ggsave(
  filename = file.path(phase5b_dir, "phase5b_fig_choropleth_all_antibiotics.png"),
  plot = fig_all,
  width = 11,
  height = 12,
  dpi = 300
)


# Step 15: Create one map per antibiotic
for (ab in unique(choropleth_sf$antibiotic)) {
  
  p_ab <- ggplot() +
    geom_sf(
      data = world,
      fill = "grey95",
      color = "white",
      linewidth = 0.1
    ) +
    geom_sf(
      data = choropleth_sf[choropleth_sf$antibiotic == ab, ],
      aes(fill = resistance_pct),
      color = "grey40",
      linewidth = 0.1
    ) +
    scale_fill_viridis_c(
      option = "magma",
      limits = c(0, max_resistance),
      name = "Resistance (%)",
      na.value = "grey90"
    ) +
    labs(
      title = paste0("Country-level K. pneumoniae resistance to ", ab),
      subtitle = "Countries shown meet: total interpretation records ≥ 500 and ≥5 years with n ≥ 30",
      caption = "Source: Vivli AMR / Pfizer ATLAS dataset, 2004–2024"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(size = 15),
      plot.subtitle = element_text(size = 11),
      legend.position = "bottom"
    )
  
  print(p_ab)
  
  out_file <- paste0(
    "phase5b_fig_choropleth_",
    str_replace_all(str_to_lower(ab), " ", "_"),
    ".png"
  )
  
  ggsave(
    filename = file.path(phase5b_dir, out_file),
    plot = p_ab,
    width = 11,
    height = 6,
    dpi = 300
  )
}


# Step 16: Create a short map-join summary
map_join_summary <- country_antibiotic_spatial_ready[, .(
  countries_in_spatial_ready = uniqueN(iso3),
  countries_joined_to_map = uniqueN(iso3[map_joined == TRUE]),
  countries_not_joined_to_map = uniqueN(iso3[map_joined == FALSE])
), by = antibiotic][order(antibiotic)]

map_join_summary

fwrite(
  map_join_summary,
  file.path(phase5b_dir, "phase5b_map_join_summary.csv")
)


# Step 17: List Phase 5B outputs
list.files(phase5b_dir)