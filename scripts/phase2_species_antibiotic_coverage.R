# Phase 2A: Species-Antibiotic Coverage Audit
# Project: Vivli AMR / Pfizer ATLAS Dataset
# Purpose:
# Identify which Species × Antibiotic pairs have enough data
# for descriptive analysis, spatial analysis, and later ML.

#Step 1: Load the packages
library(data.table)
library(stringr)

#If it says the package is not installed, just install the package:
install.packages("data.table")
install.packages("stringr")

library(data.table)
library(stringr)

#Step 2: Specify the path to the dataset file
file_path <- "atlas_vivli_2004_2024.csv"

if (!file.exists(file_path)) {
  file_path <- file.choose()
}

file_path

#Step 3: Create the Phase 2 output folder
phase2_dir <- "phase2_outputs"
dir.create(phase2_dir, showWarnings = FALSE)


#Step 4: Read the dataset
dt <- fread(
  file_path,
  colClasses = "character",
  na.strings = c("", "NA", "N/A", "NULL"),
  showProgress = TRUE
)

setDT(dt)

dim(dt)
#if shows 1011168 127 is OK!

#Step 5: Redefine the main columns
metadata_cols <- names(dt)[1:10]

ast_cols <- names(dt)[11:104]

mic_cols <- ast_cols[!endsWith(ast_cols, "_I")]

interpretation_cols_expected <- paste0(mic_cols, "_I")

gene_cols <- names(dt)[105:127]

#then check:
length(metadata_cols)
length(mic_cols)
length(interpretation_cols_expected)
length(gene_cols)


#Then check that all interpretation columns actually exist:
all(interpretation_cols_expected %in% names(dt))


#Step 6A: Make the blank interpretation values missing.
# Step 6A: Clean blank interpretation values
# Blank strings in interpretation columns should be treated as missing,
# not as a separate outcome category.

dt[, (interpretation_cols_expected) := lapply(.SD, function(x) {
  x <- str_squish(x)
  x[x == ""] <- NA_character_
  x
}), .SDcols = interpretation_cols_expected]

#Step 6: Check the actual values of the interpretations
#Here we want to see what values we have inside the _I columns.

interpretation_values <- rbindlist(
  lapply(mic_cols, function(ab) {
    
    interp_col <- paste0(ab, "_I")
    
    if (!interp_col %in% names(dt)) {
      return(data.table(
        antibiotic = ab,
        interpretation_col = interp_col,
        interpretation_value = NA_character_,
        N = NA_integer_,
        note = "interpretation column not found"
      ))
    }
    
    tmp <- dt[
      !is.na(get(interp_col)),
      .N,
      by = .(interpretation_value = get(interp_col))
    ][order(-N)]
    
    if (nrow(tmp) == 0) {
      return(data.table(
        antibiotic = ab,
        interpretation_col = interp_col,
        interpretation_value = NA_character_,
        N = 0L,
        note = "no non-missing interpretation values"
      ))
    }
    
    tmp[, antibiotic := ab]
    tmp[, interpretation_col := interp_col]
    tmp[, note := "ok"]
    
    tmp[, .(
      antibiotic,
      interpretation_col,
      interpretation_value,
      N,
      note
    )]
  }),
  fill = TRUE
)

#Now see the summary:
interpretation_values[
  !is.na(interpretation_value),
  .N,
  by = interpretation_value
][order(-N)]

#Then save
fwrite(
  interpretation_values,
  file.path(phase2_dir, "phase2_interpretation_values.csv")
)


#Step 7: Create the main Species × Antibiotic coverage table
species_antibiotic_coverage <- rbindlist(
  lapply(mic_cols, function(ab) {
    
    interp_col <- paste0(ab, "_I")
    
    if (!interp_col %in% names(dt)) {
      return(NULL)
    }
    
    tmp <- dt[
      !is.na(get(interp_col)),
      .(
        Species,
        Country,
        Year,
        interpretation_raw = get(interp_col)
      )
    ]
    
    if (nrow(tmp) == 0) {
      return(NULL)
    }
    
    tmp[, interpretation_clean := str_to_lower(str_squish(interpretation_raw))]
    
    out <- tmp[, .(
      interpretation_n = .N,
      n_countries = uniqueN(Country),
      n_years = uniqueN(Year),
      n_susceptible = sum(interpretation_clean %in% c("susceptible", "s")),
      n_intermediate = sum(interpretation_clean %in% c("intermediate", "i")),
      n_resistant = sum(interpretation_clean %in% c("resistant", "r")),
      n_other = sum(!interpretation_clean %in% c(
        "susceptible", "s",
        "intermediate", "i",
        "resistant", "r"
      ))
    ), by = Species]
    
    out[, antibiotic := ab]
    out[, interpretation_col := interp_col]
    
    out[, resistance_pct := round(
      100 * n_resistant / interpretation_n,
      2
    )]
    
    out[, non_susceptible_pct := round(
      100 * (n_resistant + n_intermediate) / interpretation_n,
      2
    )]
    
    out[, .(
      Species,
      antibiotic,
      interpretation_col,
      interpretation_n,
      n_countries,
      n_years,
      n_susceptible,
      n_intermediate,
      n_resistant,
      n_other,
      resistance_pct,
      non_susceptible_pct
    )]
  }),
  fill = TRUE
)


#Showing 50 larger combinations
species_antibiotic_coverage[
  order(-interpretation_n)
][1:50]

#Then save
fwrite(
  species_antibiotic_coverage,
  file.path(phase2_dir, "phase2_species_antibiotic_coverage.csv")
)


#Step 8: Check if there are any strange outcomes.
species_antibiotic_coverage[
  n_other > 0
][order(-n_other)][1:50]


#Step 9: Create initial candidate pairs

#Criterias:
#At least 5,000 interpretation records
#At least 10 countries
#At least 5 years
#At least one resistant
#At least one susceptible
#No strange outcomes
candidate_pairs <- species_antibiotic_coverage[
  interpretation_n >= 5000 &
    n_countries >= 10 &
    n_years >= 5 &
    n_resistant > 0 &
    n_susceptible > 0 &
    n_other == 0
][order(-interpretation_n)]

candidate_pairs[1:50]

#Then save
fwrite(
  candidate_pairs,
  file.path(phase2_dir, "phase2_candidate_pairs.csv")
)




#Step 10: Separate the important species candidates
#For now, we're only looking at these 5 organisms, as they are the most likely for our project:

priority_species <- c(
  "Escherichia coli",
  "Klebsiella pneumoniae",
  "Pseudomonas aeruginosa",
  "Acinetobacter baumannii",
  "Staphylococcus aureus"
)

#Now their shortlist:
priority_candidate_pairs <- candidate_pairs[
  Species %in% priority_species
][order(Species, -interpretation_n)]

priority_candidate_pairs

#Then save
fwrite(
  priority_candidate_pairs,
  file.path(phase2_dir, "phase2_priority_candidate_pairs.csv")
)


























































