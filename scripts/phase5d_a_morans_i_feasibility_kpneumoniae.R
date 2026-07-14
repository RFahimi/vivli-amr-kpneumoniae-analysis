# Phase 5D-A: Moran's I Feasibility and Spatial Weights Diagnostics
# Project: Vivli AMR / Pfizer ATLAS Dataset
# Scope:
# Klebsiella pneumoniae resistance to Cefepime, Ceftazidime, and Meropenem
#
# Purpose:
# Prepare country-level geometry and evaluate candidate spatial weights.
# This step does not run Moran's I yet.


# Step 1: Load packages
library(data.table)
library(stringr)
library(ggplot2)

if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}

if (!requireNamespace("spdep", quietly = TRUE)) {
  install.packages("spdep")
}

if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
  install.packages("rnaturalearth")
}

if (!requireNamespace("rnaturalearthdata", quietly = TRUE)) {
  install.packages("rnaturalearthdata")
}

library(sf)
library(spdep)
library(rnaturalearth)
library(rnaturalearthdata)


# Step 2: Create Phase 5D output folder
phase5d_dir <- "phase5d_outputs"
dir.create(phase5d_dir, showWarnings = FALSE)


# Step 3: Define input file from Phase 5B
phase5b_dir <- "phase5b_outputs"

country_antibiotic_file <- file.path(
  phase5b_dir,
  "phase5b_country_antibiotic_choropleth_ready.csv"
)

file.exists(country_antibiotic_file)


# Step 4: Read Phase 5B country-antibiotic data
country_antibiotic_ready <- fread(country_antibiotic_file)

dim(country_antibiotic_ready)
names(country_antibiotic_ready)
head(country_antibiotic_ready)


# Step 5: Basic input checks
moran_input_check <- country_antibiotic_ready[, .(
  countries = uniqueN(iso3),
  records = sum(total_interpretation_n, na.rm = TRUE),
  min_resistance_pct = min(resistance_pct, na.rm = TRUE),
  median_resistance_pct = median(resistance_pct, na.rm = TRUE),
  max_resistance_pct = max(resistance_pct, na.rm = TRUE)
), by = antibiotic][order(antibiotic)]

moran_input_check

fwrite(
  moran_input_check,
  file.path(phase5d_dir, "phase5d_moran_input_check.csv")
)


# Step 6: Check duplicate country-antibiotic rows
duplicate_country_antibiotic_check <- country_antibiotic_ready[
  ,
  .N,
  by = .(antibiotic, iso3, Country_clean)
][N > 1]

duplicate_country_antibiotic_check

fwrite(
  duplicate_country_antibiotic_check,
  file.path(phase5d_dir, "phase5d_duplicate_country_antibiotic_check.csv")
)


# Step 7: Identify special spatial units
# These may need caution in Moran's I interpretation.
special_units_for_moran <- country_antibiotic_ready[
  spatial_unit_note == "special_region_or_territory",
  .(
    antibiotic,
    Country,
    Country_clean,
    iso3,
    resistance_pct,
    total_interpretation_n,
    n_years_ge_30
  )
][order(antibiotic, Country_clean)]

special_units_for_moran

fwrite(
  special_units_for_moran,
  file.path(phase5d_dir, "phase5d_special_units_for_moran.csv")
)


# Step 8: Load world map geometry
world <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

world$map_iso3 <- world$iso_a3

if ("adm0_a3" %in% names(world)) {
  bad_iso <- is.na(world$map_iso3) | world$map_iso3 == "-99"
  world$map_iso3[bad_iso] <- world$adm0_a3[bad_iso]
}

world <- world[
  !is.na(world$map_iso3) &
    world$map_iso3 != "-99",
]

world <- sf::st_make_valid(world)

world_small <- world[, c(
  "map_iso3",
  "name",
  "name_long",
  "continent",
  "region_un",
  "subregion",
  "region_wb",
  "geometry"
)]


# Step 9: Join AMR data to country geometry
country_antibiotic_ready[, map_joined_5d := iso3 %in% world_small$map_iso3]

join_check_5d <- country_antibiotic_ready[, .(
  countries = uniqueN(iso3),
  countries_joined = uniqueN(iso3[map_joined_5d == TRUE]),
  countries_not_joined = uniqueN(iso3[map_joined_5d == FALSE])
), by = antibiotic][order(antibiotic)]

join_check_5d

fwrite(
  join_check_5d,
  file.path(phase5d_dir, "phase5d_map_join_check.csv")
)

not_joined_5d <- country_antibiotic_ready[
  map_joined_5d == FALSE,
  .(
    antibiotic,
    Country,
    Country_clean,
    iso3
  )
]

not_joined_5d

fwrite(
  not_joined_5d,
  file.path(phase5d_dir, "phase5d_not_joined_countries.csv")
)


# Step 10: Create sf object for Moran's I preparation
moran_sf <- merge(
  world_small,
  as.data.frame(country_antibiotic_ready[map_joined_5d == TRUE]),
  by.x = "map_iso3",
  by.y = "iso3",
  all = FALSE,
  sort = FALSE
)

moran_sf <- sf::st_make_valid(moran_sf)

dim(moran_sf)
unique(moran_sf$antibiotic)


# Step 11: Save geometry-ready file
gpkg_file <- file.path(
  phase5d_dir,
  "phase5d_moran_country_geometry_ready.gpkg"
)

if (file.exists(gpkg_file)) {
  unlink(gpkg_file)
}

sf::st_write(
  moran_sf,
  gpkg_file,
  delete_dsn = TRUE
)


# Step 12: Define a function for spatial weights diagnostics
make_weights_diagnostics <- function(sf_data, antibiotic_name) {
  
  sf_ab <- sf_data[sf_data$antibiotic == antibiotic_name, ]
  sf_ab <- sf_ab[order(sf_ab$Country_clean), ]
  sf_ab <- sf::st_make_valid(sf_ab)
  
  n_countries <- nrow(sf_ab)
  
  # Queen contiguity
  queen_nb <- spdep::poly2nb(
    sf_ab,
    queen = TRUE,
    row.names = sf_ab$Country_clean
  )
  
  queen_card <- spdep::card(queen_nb)
  queen_components <- spdep::n.comp.nb(queen_nb)$nc
  
  queen_diag <- data.table(
    antibiotic = antibiotic_name,
    weights_type = "queen_contiguity",
    countries = n_countries,
    total_links = sum(queen_card),
    min_degree = min(queen_card),
    median_degree = median(queen_card),
    max_degree = max(queen_card),
    zero_neighbour_units = sum(queen_card == 0),
    connected_components = queen_components
  )
  
  # k-nearest neighbours needs coordinates.
  # Use Equal Earth projection before deriving representative points.
  sf_ab_proj <- sf::st_transform(sf_ab, 8857)
  point_geom <- sf::st_point_on_surface(sf::st_geometry(sf_ab_proj))
  coords <- sf::st_coordinates(point_geom)
  
  make_knn_diag <- function(k_value) {
    
    k_actual <- min(k_value, n_countries - 1)
    
    knn_obj <- spdep::knearneigh(
      coords,
      k = k_actual
    )
    
    knn_nb <- spdep::knn2nb(
      knn_obj,
      row.names = sf_ab$Country_clean
    )
    
    # Symmetrise kNN so neighbour relationships are mutual in the weights graph.
    knn_nb_sym <- spdep::make.sym.nb(knn_nb)
    
    knn_card <- spdep::card(knn_nb_sym)
    knn_components <- spdep::n.comp.nb(knn_nb_sym)$nc
    
    data.table(
      antibiotic = antibiotic_name,
      weights_type = paste0("symmetrised_knn_k", k_value),
      countries = n_countries,
      total_links = sum(knn_card),
      min_degree = min(knn_card),
      median_degree = median(knn_card),
      max_degree = max(knn_card),
      zero_neighbour_units = sum(knn_card == 0),
      connected_components = knn_components
    )
  }
  
  k4_diag <- make_knn_diag(4)
  k6_diag <- make_knn_diag(6)
  
  rbindlist(
    list(
      queen_diag,
      k4_diag,
      k6_diag
    ),
    fill = TRUE
  )
}


# Step 13: Run spatial weights diagnostics for each antibiotic
antibiotic_list <- sort(unique(moran_sf$antibiotic))

weights_diagnostics <- rbindlist(
  lapply(
    antibiotic_list,
    function(ab) {
      make_weights_diagnostics(moran_sf, ab)
    }
  ),
  fill = TRUE
)

weights_diagnostics

fwrite(
  weights_diagnostics,
  file.path(phase5d_dir, "phase5d_weights_diagnostics.csv")
)


# Step 14: Save country list used for Moran's I preparation
moran_country_list <- unique(
  as.data.table(sf::st_drop_geometry(moran_sf))[, .(
    antibiotic,
    Country,
    Country_clean,
    map_iso3,
    wb_region,
    spatial_unit_note,
    total_interpretation_n,
    n_years_ge_30,
    resistance_pct,
    non_susceptible_pct
  )]
)[order(antibiotic, Country_clean)]

moran_country_list

fwrite(
  moran_country_list,
  file.path(phase5d_dir, "phase5d_moran_country_list.csv")
)


# Step 15: Final checks
moran_input_check
duplicate_country_antibiotic_check
special_units_for_moran
join_check_5d
not_joined_5d
weights_diagnostics

list.files(phase5d_dir)