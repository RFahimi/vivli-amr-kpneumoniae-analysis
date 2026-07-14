#Step 1: Save the Phase 3 file = DONE!

#Step 2: Create a new R Script = Done!

#Step 3: Put a title at the top of the file
# Phase 5A: Spatial Feasibility Check
# Project: Vivli AMR / Pfizer ATLAS Dataset
# Scope:
# Klebsiella pneumoniae resistance to Cefepime, Ceftazidime, and Meropenem
#
# Purpose:
# Prepare country-level spatial analysis tables and check whether
# country-level spatial analysis is feasible before running Moran's I.


#Step 4: Load the packages
library(data.table)
library(stringr)

if (!requireNamespace("countrycode", quietly = TRUE)) {
  install.packages("countrycode")
}

library(countrycode)


#Step 5: Create the Phase 5 output folder
phase5_dir <- "phase5_outputs"
dir.create(phase5_dir, showWarnings = FALSE)



#Step 6: Define the path of the Phase 4 files
#In this phase we use the outputs of Phase 4, not the original raw file.
phase4_dir <- "phase4_outputs"

country_coverage_file <- file.path(
  phase4_dir,
  "phase4_country_coverage_summary.csv"
)

country_year_file <- file.path(
  phase4_dir,
  "phase4_country_year_summary.csv"
)

#check
file.exists(country_coverage_file)
file.exists(country_year_file)


#Step 7: Read the outputs of Phase 4
country_coverage_summary <- fread(country_coverage_file)
country_year_summary <- fread(country_year_file)

dim(country_coverage_summary)
dim(country_year_summary)


head(country_coverage_summary)
head(country_year_summary)


#Step 8: Clean up the country names
#This will only remove extra spaces.
country_coverage_summary[, Country_clean := str_squish(Country)]

country_year_summary[, Country_clean := str_squish(Country)]



#Step 9: Convert country names to ISO3 codes
#For spatial analysis, countries must have standard codes.
manual_country_match <- c(
  "Russia" = "RUS",
  "Turkey" = "TUR",
  "Vietnam" = "VNM",
  "United States" = "USA",
  "South Korea" = "KOR",
  "Czech Republic" = "CZE",
  "Taiwan" = "TWN",
  "Hong Kong" = "HKG"
)

country_coverage_summary[, iso3 := countrycode(
  Country_clean,
  origin = "country.name",
  destination = "iso3c",
  custom_match = manual_country_match,
  warn = FALSE
)]

country_year_summary[, iso3 := countrycode(
  Country_clean,
  origin = "country.name",
  destination = "iso3c",
  custom_match = manual_country_match,
  warn = FALSE
)]



#Step 10: Find the countries that didn't match
unmatched_countries <- country_coverage_summary[
  is.na(iso3),
  .N,
  by = Country_clean
][order(Country_clean)]

unmatched_countries

#saving
fwrite(
  unmatched_countries,
  file.path(phase5_dir, "phase5_unmatched_countries.csv")
)



#Step 11: Create a mapping check
#This file shows what ISO3 each country is mapped to.
country_mapping_check <- unique(
  country_coverage_summary[, .(
    Country,
    Country_clean,
    iso3
  )]
)[order(Country_clean)]

country_mapping_check


#saving
fwrite(
  country_mapping_check,
  file.path(phase5_dir, "phase5_country_mapping_check.csv")
)



#Step 12: Define coverage criteria for spatial analysis
#Currently, we are keeping the same criteria as Phase 4:
  
  #At least 500 interpretation records across the country
  #At least 5 years with n ≥ 30
  #Valid ISO3 code

min_country_total_n <- 500
min_years_ge_30 <- 5

country_coverage_summary[, spatial_eligible := (
  total_interpretation_n >= min_country_total_n &
    n_years_ge_30 >= min_years_ge_30 &
    !is.na(iso3)
)]



#Step 13: Create a country-antibiotic table ready for spatial analysis
country_antibiotic_spatial_ready <- country_coverage_summary[
  spatial_eligible == TRUE,
  .(
    antibiotic,
    Country,
    Country_clean,
    iso3,
    total_interpretation_n,
    n_years_observed,
    n_years_ge_30,
    median_year_n,
    resistance_pct,
    non_susceptible_pct
  )
][order(antibiotic, -resistance_pct)]

country_antibiotic_spatial_ready

#saving
fwrite(
  country_antibiotic_spatial_ready,
  file.path(phase5_dir, "phase5_country_antibiotic_spatial_ready.csv")
)



#Step 14: Create a feasibility summary for each antibiotic.
#This table tells us how many countries are suitable for spatial analysis for each antibiotic.
spatial_feasibility_summary <- country_coverage_summary[, .(
  total_countries_observed = uniqueN(Country_clean),
  countries_with_iso3 = uniqueN(iso3[!is.na(iso3)]),
  unmatched_countries = uniqueN(Country_clean[is.na(iso3)]),
  eligible_countries = sum(spatial_eligible),
  total_records_in_eligible_countries = as.numeric(
    sum(total_interpretation_n[spatial_eligible == TRUE])
  ),
  median_resistance_eligible = as.numeric(round(
    median(resistance_pct[spatial_eligible == TRUE], na.rm = TRUE),
    2
  )),
  min_resistance_eligible = as.numeric(round(
    min(resistance_pct[spatial_eligible == TRUE], na.rm = TRUE),
    2
  )),
  max_resistance_eligible = as.numeric(round(
    max(resistance_pct[spatial_eligible == TRUE], na.rm = TRUE),
    2
  ))
), by = antibiotic][order(antibiotic)]

spatial_feasibility_summary

#Now add a simple verdict:
spatial_feasibility_summary[, feasibility_verdict := fifelse(
  eligible_countries >= 30,
  "Feasible for exploratory country-level spatial analysis",
  "Weak: too few eligible countries for spatial analysis"
)]

spatial_feasibility_summary

#saving
fwrite(
  spatial_feasibility_summary,
  file.path(phase5_dir, "phase5_spatial_feasibility_summary.csv")
)



#Step 15: Create a country-year table ready for spatial-temporal analysis
#Here we only keep country-years that:
  #n ≥ 30
  #ISO3 valid
country_year_spatial_ready <- country_year_summary[
  enough_n_30 == TRUE & !is.na(iso3),
  .(
    antibiotic,
    Country,
    Country_clean,
    iso3,
    Year,
    Year_num,
    interpretation_n,
    n_susceptible,
    n_intermediate,
    n_resistant,
    resistance_pct,
    non_susceptible_pct,
    enough_n_30
  )
][order(antibiotic, Year_num, Country_clean)]

country_year_spatial_ready

#saving
fwrite(
  country_year_spatial_ready,
  file.path(phase5_dir, "phase5_country_year_spatial_ready.csv")
)



#Step 16: Create annual spatial coverage
#This output shows how many countries with n ≥ 30 we have for each antibiotic in each year.
annual_spatial_coverage <- country_year_spatial_ready[, .(
  countries_n_ge_30 = uniqueN(iso3),
  total_interpretation_n = as.numeric(sum(interpretation_n)),
  median_country_year_n = as.numeric(median(interpretation_n))
), by = .(antibiotic, Year, Year_num)][order(antibiotic, Year_num)]

annual_spatial_coverage

#saving
fwrite(
  annual_spatial_coverage,
  file.path(phase5_dir, "phase5_annual_spatial_coverage.csv")
)



#Step 17: Create top countries for spatial-ready table
#This is just for quick check and presentation
top_spatial_ready_countries <- country_antibiotic_spatial_ready[
  order(antibiotic, -resistance_pct)
][
  , head(.SD, 10),
  by = antibiotic
]

top_spatial_ready_countries

#saving
fwrite(
  top_spatial_ready_countries,
  file.path(phase5_dir, "phase5_top_spatial_ready_countries.csv")
)



#Step 18: Print the final outputs
unmatched_countries

spatial_feasibility_summary

annual_spatial_coverage[
  order(antibiotic, Year_num)
]

top_spatial_ready_countries

list.files(phase5_dir)













