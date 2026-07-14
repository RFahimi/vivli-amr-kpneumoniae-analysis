# Phase 5D-B: Moran's I Execution
# Project: Vivli AMR / Pfizer ATLAS Dataset
# Scope:
# Klebsiella pneumoniae resistance to Cefepime, Ceftazidime, and Meropenem
#
# Purpose:
# Run Moran's I on aggregated country-level resistance using k-nearest-neighbour
# spatial weights.
#
# Primary weights: symmetrised kNN, k = 4
# Sensitivity weights: symmetrised kNN, k = 6
# Outcome: resistance_pct


# Step 1: Load packages
library(data.table)
library(stringr)
library(ggplot2)
library(sf)
library(spdep)


# Step 2: Define Phase 5D output folder
phase5d_dir <- "phase5d_outputs"
dir.create(phase5d_dir, showWarnings = FALSE)


# Step 3: Read geometry-ready file from Phase 5D-A
moran_gpkg_file <- file.path(
  phase5d_dir,
  "phase5d_moran_country_geometry_ready.gpkg"
)

file.exists(moran_gpkg_file)

moran_sf <- sf::st_read(moran_gpkg_file)

dim(moran_sf)
names(moran_sf)
unique(moran_sf$antibiotic)


# Step 4: Basic check
moran_execution_input_check <- as.data.table(sf::st_drop_geometry(moran_sf))[
  ,
  .(
    countries = uniqueN(map_iso3),
    min_resistance_pct = min(resistance_pct, na.rm = TRUE),
    median_resistance_pct = median(resistance_pct, na.rm = TRUE),
    max_resistance_pct = max(resistance_pct, na.rm = TRUE),
    total_interpretation_n = sum(total_interpretation_n, na.rm = TRUE)
  ),
  by = antibiotic
][order(antibiotic)]

moran_execution_input_check

fwrite(
  moran_execution_input_check,
  file.path(phase5d_dir, "phase5d_moran_execution_input_check.csv")
)


# Step 5: Function to create symmetrised kNN weights
make_knn_weights <- function(sf_ab, k_value) {
  
  sf_ab <- sf_ab[order(sf_ab$Country_clean), ]
  sf_ab <- sf::st_make_valid(sf_ab)
  
  n_countries <- nrow(sf_ab)
  k_actual <- min(k_value, n_countries - 1)
  
  # Equal Earth projection for representative points and distances
  sf_ab_proj <- sf::st_transform(sf_ab, 8857)
  
  point_geom <- sf::st_point_on_surface(sf::st_geometry(sf_ab_proj))
  coords <- sf::st_coordinates(point_geom)
  
  knn_obj <- spdep::knearneigh(
    coords,
    k = k_actual
  )
  
  knn_nb <- spdep::knn2nb(
    knn_obj,
    row.names = sf_ab$Country_clean
  )
  
  knn_nb_sym <- spdep::make.sym.nb(knn_nb)
  
  listw <- spdep::nb2listw(
    knn_nb_sym,
    style = "W",
    zero.policy = TRUE
  )
  
  list(
    sf_ab = sf_ab,
    nb = knn_nb_sym,
    listw = listw
  )
}


# Step 6: Function to run Moran's I
run_morans_i <- function(sf_data, antibiotic_name, k_value, outcome_col = "resistance_pct") {
  
  sf_ab <- sf_data[sf_data$antibiotic == antibiotic_name, ]
  sf_ab <- sf_ab[order(sf_ab$Country_clean), ]
  
  weights_obj <- make_knn_weights(
    sf_ab = sf_ab,
    k_value = k_value
  )
  
  sf_ab <- weights_obj$sf_ab
  listw <- weights_obj$listw
  nb <- weights_obj$nb
  
  x <- as.numeric(sf_ab[[outcome_col]])
  
  if (any(is.na(x))) {
    stop(paste("Missing values in outcome for", antibiotic_name, outcome_col))
  }
  
  # Asymptotic Moran's I
  moran_asym <- spdep::moran.test(
    x,
    listw,
    alternative = "two.sided",
    zero.policy = TRUE
  )
  
  # Monte Carlo permutation Moran's I
  set.seed(20260615)
  
  moran_mc <- spdep::moran.mc(
    x,
    listw,
    nsim = 999,
    alternative = "two.sided",
    zero.policy = TRUE
  )
  
  nb_card <- spdep::card(nb)
  
  result <- data.table(
    antibiotic = antibiotic_name,
    outcome = outcome_col,
    weights_type = paste0("symmetrised_knn_k", k_value),
    k = k_value,
    countries = nrow(sf_ab),
    total_links = sum(nb_card),
    min_degree = min(nb_card),
    median_degree = median(nb_card),
    max_degree = max(nb_card),
    zero_neighbour_units = sum(nb_card == 0),
    connected_components = spdep::n.comp.nb(nb)$nc,
    moran_i = as.numeric(moran_asym$estimate[["Moran I statistic"]]),
    expected_i = as.numeric(moran_asym$estimate[["Expectation"]]),
    variance = as.numeric(moran_asym$estimate[["Variance"]]),
    z_value = as.numeric(moran_asym$statistic),
    p_value_asymptotic = as.numeric(moran_asym$p.value),
    p_value_permutation = as.numeric(moran_mc$p.value),
    permutations = 999
  )
  
  # Moran scatterplot data
  z <- as.numeric(scale(x))
  
  lag_z <- spdep::lag.listw(
    listw,
    z,
    zero.policy = TRUE
  )
  
  scatter_data <- as.data.table(sf::st_drop_geometry(sf_ab))[
    ,
    .(
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
    )
  ]
  
  scatter_data[, outcome := outcome_col]
  scatter_data[, weights_type := paste0("symmetrised_knn_k", k_value)]
  scatter_data[, z_outcome := z]
  scatter_data[, spatial_lag_z_outcome := as.numeric(lag_z)]
  
  list(
    result = result,
    scatter_data = scatter_data
  )
}


# Step 7: Run Moran's I for k = 4 and k = 6
antibiotic_list <- sort(unique(moran_sf$antibiotic))
k_values <- c(4, 6)

moran_results_list <- list()
moran_scatter_list <- list()

counter <- 1

for (ab in antibiotic_list) {
  for (k_value in k_values) {
    
    tmp <- run_morans_i(
      sf_data = moran_sf,
      antibiotic_name = ab,
      k_value = k_value,
      outcome_col = "resistance_pct"
    )
    
    moran_results_list[[counter]] <- tmp$result
    moran_scatter_list[[counter]] <- tmp$scatter_data
    
    counter <- counter + 1
  }
}

moran_results <- rbindlist(moran_results_list, fill = TRUE)
moran_scatter_data <- rbindlist(moran_scatter_list, fill = TRUE)

moran_results <- moran_results[
  order(antibiotic, k)
]

moran_results

fwrite(
  moran_results,
  file.path(phase5d_dir, "phase5d_morans_i_results.csv")
)

fwrite(
  moran_scatter_data,
  file.path(phase5d_dir, "phase5d_moran_scatter_data.csv")
)


# Step 8: Create a primary-results table for k = 4
moran_results_primary_k4 <- moran_results[
  k == 4
][
  order(p_value_permutation)
]

moran_results_primary_k4

fwrite(
  moran_results_primary_k4,
  file.path(phase5d_dir, "phase5d_morans_i_primary_k4_results.csv")
)


# Step 9: Create a sensitivity-results table for k = 6
moran_results_sensitivity_k6 <- moran_results[
  k == 6
][
  order(p_value_permutation)
]

moran_results_sensitivity_k6

fwrite(
  moran_results_sensitivity_k6,
  file.path(phase5d_dir, "phase5d_morans_i_sensitivity_k6_results.csv")
)


# Step 10: Moran scatterplot for primary k = 4
scatter_k4 <- moran_scatter_data[
  weights_type == "symmetrised_knn_k4"
]

fig_moran_scatter_k4 <- ggplot(
  scatter_k4,
  aes(
    x = z_outcome,
    y = spatial_lag_z_outcome
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  geom_point(aes(size = total_interpretation_n), alpha = 0.75) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
  facet_wrap(~ antibiotic, ncol = 1) +
  scale_size_continuous(
    name = "Interpretation records",
    range = c(1.5, 5)
  ) +
  labs(
    title = "Moran scatterplots for country-level K. pneumoniae resistance",
    subtitle = "Primary spatial weights: symmetrised k-nearest neighbours, k = 4",
    x = "Standardised resistance",
    y = "Spatial lag of standardised resistance",
    caption = "Source: Vivli AMR / Pfizer ATLAS dataset, 2004–2024"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 11),
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  )

fig_moran_scatter_k4

ggsave(
  filename = file.path(phase5d_dir, "phase5d_fig_moran_scatter_k4.png"),
  plot = fig_moran_scatter_k4,
  width = 9,
  height = 11,
  dpi = 300
)


# Step 11: Moran scatterplot for sensitivity k = 6
scatter_k6 <- moran_scatter_data[
  weights_type == "symmetrised_knn_k6"
]

fig_moran_scatter_k6 <- ggplot(
  scatter_k6,
  aes(
    x = z_outcome,
    y = spatial_lag_z_outcome
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  geom_point(aes(size = total_interpretation_n), alpha = 0.75) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
  facet_wrap(~ antibiotic, ncol = 1) +
  scale_size_continuous(
    name = "Interpretation records",
    range = c(1.5, 5)
  ) +
  labs(
    title = "Moran scatterplots for country-level K. pneumoniae resistance",
    subtitle = "Sensitivity spatial weights: symmetrised k-nearest neighbours, k = 6",
    x = "Standardised resistance",
    y = "Spatial lag of standardised resistance",
    caption = "Source: Vivli AMR / Pfizer ATLAS dataset, 2004–2024"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 11),
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  )

fig_moran_scatter_k6

ggsave(
  filename = file.path(phase5d_dir, "phase5d_fig_moran_scatter_k6.png"),
  plot = fig_moran_scatter_k6,
  width = 9,
  height = 11,
  dpi = 300
)


# Step 12: Add simple interpretation labels
moran_results[, evidence_label := fifelse(
  p_value_permutation < 0.05 & moran_i > 0,
  "positive spatial autocorrelation",
  fifelse(
    p_value_permutation < 0.05 & moran_i < 0,
    "negative spatial autocorrelation",
    "no clear spatial autocorrelation"
  )
)]

moran_results_interpreted <- copy(moran_results)

moran_results_interpreted

fwrite(
  moran_results_interpreted,
  file.path(phase5d_dir, "phase5d_morans_i_results_interpreted.csv")
)


# Step 13: Final checks
moran_execution_input_check
moran_results
moran_results_primary_k4
moran_results_sensitivity_k6
moran_results_interpreted

list.files(phase5d_dir)