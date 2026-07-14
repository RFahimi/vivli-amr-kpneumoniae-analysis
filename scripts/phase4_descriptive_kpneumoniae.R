#Step 1: Save the Phase 3 file = DONE!

#Step 2: Create a new R Script = Done!

#Step 3: Put a title at the top of the file
# Phase 4A: Descriptive Analysis for Selected Scope
# Project: Vivli AMR / Pfizer ATLAS Dataset
# Scope:
# Klebsiella pneumoniae resistance to Cefepime, Ceftazidime, and Meropenem
#
# Purpose:
# Create descriptive tables and preliminary figures for team discussion.


#Step 4: Load the packages
library(data.table)
library(stringr)
library(ggplot2)

#If ggplot2 is not installed:
install.packages("ggplot2")

library(ggplot2)



#Step 5: Specify the path to the dataset file
file_path <- "atlas_vivli_2004_2024.csv"

if (!file.exists(file_path)) {
  file_path <- file.choose()
}

file_path


#Step 6: Create the Phase 4 output folder
phase4_dir <- "phase4_outputs"
dir.create(phase4_dir, showWarnings = FALSE)



#Step 7: Read the dataset
dt <- fread(
  file_path,
  colClasses = "character",
  na.strings = c("", "NA", "N/A", "NULL"),
  showProgress = TRUE
)

setDT(dt)

dim(dt)



#Step 8: Define the main columns
metadata_cols <- names(dt)[1:10]

ast_cols <- names(dt)[11:104]

mic_cols <- ast_cols[!endsWith(ast_cols, "_I")]

interpretation_cols_expected <- paste0(mic_cols, "_I")

gene_cols <- names(dt)[105:127]

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



#Step 10: Define the scope of Phase 4
target_species <- "Klebsiella pneumoniae"

target_antibiotics <- c(
  "Cefepime",
  "Ceftazidime",
  "Meropenem"
)

target_species
target_antibiotics



#Step 11: Convert the data to long format for this scope
#This step is important. The original dataset is wide; that is, each isolate has a row and the antibiotics are columns. For descriptive analysis, we convert the three selected antibiotics to long format.
scope_long <- rbindlist(
  lapply(target_antibiotics, function(ab) {
    
    interp_col <- paste0(ab, "_I")
    
    tmp <- dt[
      Species == target_species & !is.na(get(interp_col)),
      .(
        `Isolate Id`,
        Species,
        Country,
        State,
        Gender,
        `Age Group`,
        Speciality,
        Source,
        Year,
        interpretation_raw = get(interp_col)
      )
    ]
    
    tmp[, antibiotic := ab]
    tmp[, interpretation_col := interp_col]
    
    tmp[]
  }),
  fill = TRUE
)

scope_long[, interpretation_clean := str_to_lower(str_squish(interpretation_raw))]

scope_long <- scope_long[
  interpretation_clean %in% c("susceptible", "intermediate", "resistant")
]

scope_long[, Year_num := as.integer(Year)]

dim(scope_long)

scope_long[1:10]



#Step 12: Define resistance outcomes
scope_long[, resistant := interpretation_clean == "resistant"]

scope_long[, non_susceptible := interpretation_clean %in% c(
  "intermediate",
  "resistant"
)]

scope_long[, .N, by = .(antibiotic, interpretation_clean)][
  order(antibiotic, interpretation_clean)
]



#Step 13: Create a general summary for the three antibiotics.
overall_antibiotic_summary <- scope_long[, .(
  interpretation_n = .N,
  n_countries = uniqueN(Country),
  n_years = uniqueN(Year),
  n_susceptible = sum(interpretation_clean == "susceptible"),
  n_intermediate = sum(interpretation_clean == "intermediate"),
  n_resistant = sum(interpretation_clean == "resistant"),
  resistance_pct = round(100 * mean(resistant), 2),
  non_susceptible_pct = round(100 * mean(non_susceptible), 2)
), by = antibiotic][order(-interpretation_n)]

overall_antibiotic_summary

#saving
fwrite(
  overall_antibiotic_summary,
  file.path(phase4_dir, "phase4_overall_antibiotic_summary.csv")
)



#Step 14: Create a yearly trend
yearly_trend <- scope_long[, .(
  interpretation_n = .N,
  n_countries = uniqueN(Country),
  n_susceptible = sum(interpretation_clean == "susceptible"),
  n_intermediate = sum(interpretation_clean == "intermediate"),
  n_resistant = sum(interpretation_clean == "resistant"),
  resistance_pct = round(100 * mean(resistant), 2),
  non_susceptible_pct = round(100 * mean(non_susceptible), 2)
), by = .(antibiotic, Year, Year_num)][order(antibiotic, Year_num)]

yearly_trend

#saving
fwrite(
  yearly_trend,
  file.path(phase4_dir, "phase4_yearly_trend.csv")
)



#Step 15: Create a resistance trend chart over time
p_yearly_resistance <- ggplot(
  yearly_trend,
  aes(x = Year_num, y = resistance_pct, group = antibiotic)
) +
  geom_line() +
  geom_point() +
  facet_wrap(~ antibiotic, ncol = 1) +
  labs(
    title = "Klebsiella pneumoniae resistance trends, 2004–2024",
    x = "Year",
    y = "Resistance (%)"
  ) +
  theme_minimal()

p_yearly_resistance

#saving
ggsave(
  filename = file.path(phase4_dir, "phase4_fig_yearly_resistance_trends.png"),
  plot = p_yearly_resistance,
  width = 8,
  height = 9,
  dpi = 300
)



#Step 16: Create a country-level summary
#Here, for each country and antibiotic, we calculate the total records and the percentage of resistance.
country_antibiotic_summary <- scope_long[, .(
  interpretation_n = .N,
  n_years = uniqueN(Year),
  n_susceptible = sum(interpretation_clean == "susceptible"),
  n_intermediate = sum(interpretation_clean == "intermediate"),
  n_resistant = sum(interpretation_clean == "resistant"),
  resistance_pct = round(100 * mean(resistant), 2),
  non_susceptible_pct = round(100 * mean(non_susceptible), 2)
), by = .(antibiotic, Country)][order(antibiotic, -interpretation_n)]

country_antibiotic_summary

#saving
fwrite(
  country_antibiotic_summary,
  file.path(phase4_dir, "phase4_country_antibiotic_summary.csv")
)



#Step 17: Create country-year summary with threshold
min_country_year_n <- 30

country_year_summary <- scope_long[, .(
  interpretation_n = .N,
  n_susceptible = sum(interpretation_clean == "susceptible"),
  n_intermediate = sum(interpretation_clean == "intermediate"),
  n_resistant = sum(interpretation_clean == "resistant"),
  resistance_pct = round(100 * mean(resistant), 2),
  non_susceptible_pct = round(100 * mean(non_susceptible), 2)
), by = .(antibiotic, Country, Year, Year_num)]

country_year_summary[, enough_n_30 := interpretation_n >= min_country_year_n]

country_year_summary[
  order(antibiotic, Country, Year_num)
]

#saving
fwrite(
  country_year_summary,
  file.path(phase4_dir, "phase4_country_year_summary.csv")
)



# Step 18: Identify countries with reliable coverage
# Fixed version: force numeric outputs to avoid data.table type mismatch
country_coverage_summary <- country_year_summary[, .(
  total_interpretation_n = as.numeric(sum(interpretation_n)),
  n_years_observed = as.integer(uniqueN(Year)),
  n_years_ge_30 = as.integer(sum(enough_n_30)),
  median_year_n = as.numeric(median(interpretation_n)),
  resistance_pct = as.numeric(round(
    100 * sum(n_resistant) / sum(interpretation_n),
    2
  )),
  non_susceptible_pct = as.numeric(round(
    100 * (sum(n_resistant) + sum(n_intermediate)) / sum(interpretation_n),
    2
  ))
), by = .(antibiotic, Country)][order(antibiotic, -total_interpretation_n)]

country_coverage_summary

#saving
fwrite(
  country_coverage_summary,
  file.path(phase4_dir, "phase4_country_coverage_summary.csv")
)



#Step 19: Create a chart of top countries for each antibiotic.
#To keep the plots defensible, we only show countries with minimal coverage; this is for the plot only, not a final removal from the entire project.
plot_country_summary <- country_coverage_summary[
  total_interpretation_n >= 500 &
    n_years_ge_30 >= 5
]

top_country_resistance <- plot_country_summary[
  order(antibiotic, -resistance_pct)
][
  , head(.SD, 15),
  by = antibiotic
]

top_country_resistance

#saving
fwrite(
  top_country_resistance,
  file.path(phase4_dir, "phase4_top_country_resistance_for_plot.csv")
)



#plot
p_country_resistance <- ggplot(
  top_country_resistance,
  aes(x = reorder(Country, resistance_pct), y = resistance_pct)
) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ antibiotic, scales = "free_y") +
  labs(
    title = "Highest country-level resistance among countries with adequate coverage",
    x = "Country",
    y = "Resistance (%)"
  ) +
  theme_minimal()

p_country_resistance

#saving
ggsave(
  filename = file.path(phase4_dir, "phase4_fig_top_country_resistance.png"),
  plot = p_country_resistance,
  width = 10,
  height = 8,
  dpi = 300
)



#Step 20: Create a short table for presentation
team_summary_table <- overall_antibiotic_summary[, .(
  antibiotic,
  interpretation_n,
  n_countries,
  n_years,
  resistance_pct,
  non_susceptible_pct
)]

team_summary_table

#saving
fwrite(
  team_summary_table,
  file.path(phase4_dir, "phase4_team_summary_table.csv")
)


# Step 21: Create filtered yearly trend figure
# We only plot antibiotic-year strata with at least 500 interpretation records.
# This prevents unstable early Ceftazidime years from dominating the visual trend.

min_yearly_n_for_plot <- 500

yearly_trend_filtered <- yearly_trend[
  interpretation_n >= min_yearly_n_for_plot
]

yearly_trend_filtered[
  order(antibiotic, Year_num)
]

#saving
fwrite(
  yearly_trend_filtered,
  file.path(phase4_dir, "phase4_yearly_trend_filtered_for_plot.csv")
)

#Now create the filtered chart:
p_yearly_resistance_filtered <- ggplot(
  yearly_trend_filtered,
  aes(x = Year_num, y = resistance_pct, group = antibiotic)
) +
  geom_line() +
  geom_point() +
  facet_wrap(~ antibiotic, ncol = 1) +
  labs(
    title = "Klebsiella pneumoniae resistance trends, 2004–2024",
    subtitle = "Only antibiotic-year strata with at least 500 interpretation records are shown",
    x = "Year",
    y = "Resistance (%)"
  ) +
  theme_minimal()

p_yearly_resistance_filtered

#saving
ggsave(
  filename = file.path(phase4_dir, "phase4_fig_yearly_resistance_trends_filtered.png"),
  plot = p_yearly_resistance_filtered,
  width = 8,
  height = 9,
  dpi = 300
)



# Step 22: Create facet-ordered top country resistance figure
# This fixes country ordering separately within each antibiotic panel.

top_country_resistance_ordered <- copy(top_country_resistance)

top_country_resistance_ordered[, country_facet_label := paste(
  Country,
  antibiotic,
  sep = "___"
)]

top_country_resistance_ordered[
  order(antibiotic, -resistance_pct)
]


#saving
fwrite(
  top_country_resistance_ordered,
  file.path(phase4_dir, "phase4_top_country_resistance_ordered_for_plot.csv")
)


#plot
p_country_resistance_ordered <- ggplot(
  top_country_resistance_ordered,
  aes(
    x = reorder(country_facet_label, resistance_pct),
    y = resistance_pct
  )
) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ antibiotic, scales = "free_y") +
  scale_x_discrete(
    labels = function(x) sub("___.*$", "", x)
  ) +
  labs(
    title = "Highest country-level resistance among countries with adequate coverage",
    subtitle = "Countries shown have at least 500 interpretation records and at least 5 years with n ≥ 30",
    x = "Country",
    y = "Resistance (%)"
  ) +
  theme_minimal()

p_country_resistance_ordered


#saving
ggsave(
  filename = file.path(phase4_dir, "phase4_fig_top_country_resistance_ordered.png"),
  plot = p_country_resistance_ordered,
  width = 10,
  height = 8,
  dpi = 300
)


#Step 23: Print some output for final check
yearly_trend_filtered[
  order(antibiotic, Year_num)
]

top_country_resistance_ordered[
  order(antibiotic, -resistance_pct)
]

list.files(phase4_dir)















