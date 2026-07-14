#Step 1: Save the Phase 2 file = Done!

#Step 2: Create a new R Script = Done!

#Step 3: Put a title at the top of the file

# Phase 3A: Scope Selection - Country-Year Coverage Check
# Project: Vivli AMR / Pfizer ATLAS Dataset
# Purpose:
# Evaluate whether selected Species × Antibiotic pairs have enough
# country-year coverage for descriptive trends, spatial analysis, and later ML.

#Step 4: Load the packages
library(data.table)
library(stringr)

#Step 5: Specify the path to the dataset file
file_path <- "atlas_vivli_2004_2024.csv"

if (!file.exists(file_path)) {
  file_path <- file.choose()
}

file_path

#Step 6: Create the Phase 3 output folder
phase3_dir <- "phase3_outputs"
dir.create(phase3_dir, showWarnings = FALSE)


#Step 7: Read the dataset
dt <- fread(
  file_path,
  colClasses = "character",
  na.strings = c("", "NA", "N/A", "NULL"),
  showProgress = TRUE
)

setDT(dt)

dim(dt)


#Step 8: Redefine the main columns
metadata_cols <- names(dt)[1:10]

ast_cols <- names(dt)[11:104]

mic_cols <- ast_cols[!endsWith(ast_cols, "_I")]

interpretation_cols_expected <- paste0(mic_cols, "_I")

gene_cols <- names(dt)[105:127]

#Check
length(metadata_cols)
length(mic_cols)
length(interpretation_cols_expected)
length(gene_cols)

all(interpretation_cols_expected %in% names(dt))


#Step 9: Fill in the missing interpretation blanks.
dt[, (interpretation_cols_expected) := lapply(.SD, function(x) {
  x <- str_squish(x)
  x[x == ""] <- NA_character_
  x
}), .SDcols = interpretation_cols_expected]



#Step 10: Define the selected pairs for initial review 
#We'll check these for now. If we want to add another pair later, we just add it to this table.

selected_pairs <- data.table(
  pair_id = c(
    "KP_Meropenem",
    "KP_Cefepime",
    "KP_Ceftazidime",
    "EC_Cefepime",
    "PA_Meropenem"
  ),
  Species = c(
    "Klebsiella pneumoniae",
    "Klebsiella pneumoniae",
    "Klebsiella pneumoniae",
    "Escherichia coli",
    "Pseudomonas aeruginosa"
  ),
  antibiotic = c(
    "Meropenem",
    "Cefepime",
    "Ceftazidime",
    "Cefepime",
    "Meropenem"
  )
)

selected_pairs


#Step 11: Define the initial threshold for the country-year sample size
#If we want to be stricter later, we can put 50.
min_country_year_n <- 30



#Step 12: Create a function to create the country-year table
make_pair_country_year <- function(species_name, antibiotic_name, pair_id) {
  
  interp_col <- paste0(antibiotic_name, "_I")
  
  if (!interp_col %in% names(dt)) {
    stop(paste("Interpretation column not found:", interp_col))
  }
  
  tmp <- dt[
    Species == species_name & !is.na(get(interp_col)),
    .(
      Country,
      Year,
      interpretation_raw = get(interp_col)
    )
  ]
  
  tmp[, interpretation_clean := str_to_lower(str_squish(interpretation_raw))]
  
  tmp <- tmp[
    interpretation_clean %in% c(
      "susceptible",
      "intermediate",
      "resistant"
    )
  ]
  
  out <- tmp[, .(
    interpretation_n = .N,
    n_susceptible = sum(interpretation_clean == "susceptible"),
    n_intermediate = sum(interpretation_clean == "intermediate"),
    n_resistant = sum(interpretation_clean == "resistant")
  ), by = .(Country, Year)]
  
  out[, resistance_pct := round(
    100 * n_resistant / interpretation_n,
    2
  )]
  
  out[, non_susceptible_pct := round(
    100 * (n_resistant + n_intermediate) / interpretation_n,
    2
  )]
  
  out[, enough_n_30 := interpretation_n >= min_country_year_n]
  
  out[, pair_id := pair_id]
  out[, Species := species_name]
  out[, antibiotic := antibiotic_name]
  out[, interpretation_col := interp_col]
  
  setcolorder(out, c(
    "pair_id",
    "Species",
    "antibiotic",
    "interpretation_col",
    "Country",
    "Year",
    "interpretation_n",
    "enough_n_30",
    "n_susceptible",
    "n_intermediate",
    "n_resistant",
    "resistance_pct",
    "non_susceptible_pct"
  ))
  
  out[]
}



#Step 13: Create the country-year table for 5 pairs
country_year_counts <- rbindlist(
  lapply(seq_len(nrow(selected_pairs)), function(i) {
    make_pair_country_year(
      species_name = selected_pairs$Species[i],
      antibiotic_name = selected_pairs$antibiotic[i],
      pair_id = selected_pairs$pair_id[i]
    )
  }),
  fill = TRUE
)

country_year_counts[
  order(pair_id, Country, as.integer(Year))
]

#save
fwrite(
  country_year_counts,
  file.path(phase3_dir, "phase3_country_year_counts_selected_pairs.csv")
)



# Step 14: Create a general summary for each pair
# Fixed version: force numeric outputs to avoid data.table type mismatch
overall_pair_summary <- country_year_counts[, .(
  total_interpretation_n = as.numeric(sum(interpretation_n)),
  n_countries = as.integer(uniqueN(Country)),
  n_years = as.integer(uniqueN(Year)),
  country_year_cells = as.integer(.N),
  cells_n_ge_30 = as.integer(sum(enough_n_30)),
  pct_cells_n_ge_30 = as.numeric(round(100 * mean(enough_n_30), 2)),
  median_country_year_n = as.numeric(median(interpretation_n)),
  min_country_year_n = as.numeric(min(interpretation_n)),
  max_country_year_n = as.numeric(max(interpretation_n)),
  total_susceptible = as.numeric(sum(n_susceptible)),
  total_intermediate = as.numeric(sum(n_intermediate)),
  total_resistant = as.numeric(sum(n_resistant)),
  resistance_pct = as.numeric(round(
    100 * sum(n_resistant) / sum(interpretation_n),
    2
  )),
  non_susceptible_pct = as.numeric(round(
    100 * (sum(n_resistant) + sum(n_intermediate)) / sum(interpretation_n),
    2
  ))
), by = .(pair_id, Species, antibiotic)]

overall_pair_summary[
  order(-total_interpretation_n)
]

#save
fwrite(
  overall_pair_summary,
  file.path(phase3_dir, "phase3_overall_pair_summary.csv")
)



# Step 15: Create country-level summary
country_summary <- country_year_counts[, .(
  total_interpretation_n = as.numeric(sum(interpretation_n)),
  n_years_observed = as.integer(uniqueN(Year)),
  n_years_ge_30 = as.integer(sum(enough_n_30)),
  median_year_n = as.numeric(median(interpretation_n)),
  total_resistant = as.numeric(sum(n_resistant)),
  resistance_pct = as.numeric(round(
    100 * sum(n_resistant) / sum(interpretation_n),
    2
  )),
  non_susceptible_pct = as.numeric(round(
    100 * (sum(n_resistant) + sum(n_intermediate)) / sum(interpretation_n),
    2
  ))
), by = .(pair_id, Species, antibiotic, Country)]

country_summary[
  order(pair_id, -total_interpretation_n)
]

#save
fwrite(
  country_summary,
  file.path(phase3_dir, "phase3_country_summary_selected_pairs.csv")
)



# Step 16: Create year-level summary
year_summary <- country_year_counts[, .(
  total_interpretation_n = as.numeric(sum(interpretation_n)),
  n_countries_observed = as.integer(uniqueN(Country)),
  n_countries_ge_30 = as.integer(sum(enough_n_30)),
  total_resistant = as.numeric(sum(n_resistant)),
  resistance_pct = as.numeric(round(
    100 * sum(n_resistant) / sum(interpretation_n),
    2
  )),
  non_susceptible_pct = as.numeric(round(
    100 * (sum(n_resistant) + sum(n_intermediate)) / sum(interpretation_n),
    2
  ))
), by = .(pair_id, Species, antibiotic, Year)]

year_summary[
  order(pair_id, as.integer(Year))
]


#save
fwrite(
  year_summary,
  file.path(phase3_dir, "phase3_year_summary_selected_pairs.csv")
)



# Step 17: Create scope decision table
scope_decision_table <- overall_pair_summary[
  order(-pct_cells_n_ge_30, -total_interpretation_n)
]

scope_decision_table

#save
fwrite(
  scope_decision_table,
  file.path(phase3_dir, "phase3_scope_decision_table.csv")
)




























