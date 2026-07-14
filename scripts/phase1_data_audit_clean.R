# Phase 1: Data Audit and Initial Quality Assessment
# Project: Vivli AMR / Pfizer ATLAS Dataset
#
# Purpose:
# Audit the structure, coverage, missingness, and unit of observation
# in the restricted Pfizer ATLAS dataset before downstream analysis.

# Step 1: Load required packages
library(data.table)
library(stringr)

# Step 2: Define the restricted source-data path
# The data/ directory is excluded from GitHub through .gitignore.
file_path <- "atlas_vivli_2004_2024.csv"

if (!file.exists(file_path)) {
  stop(
    paste0(
      "Restricted ATLAS dataset not found at: ",
      file_path,
      "\nCreate a local data/ folder in the repository root and place ",
      "atlas_vivli_2004_2024.csv inside it.",
      "\nDo not commit the source dataset to GitHub."
    )
  )
}


#Step 3: Read only the column names

#Don't read the entire dataset yet. Just read the header:

header_only <- fread(file_path, nrows = 0)

col_names <- names(header_only)

length(col_names)
col_names
## we expect 127, if so, its OK!


  
#Step 4: Read only the first 1000 rows

#Here we are reading only a small sample, not the entire file.

sample_dt <- fread(
    file_path,
    nrows = 1000,
    colClasses = "character",
    na.strings = c("", "NA", "N/A", "NULL"),
    showProgress = TRUE
  )

#the check:
dim(sample_dt)
names(sample_dt)


#Step 5: Read the entire dataset


dt <- fread(
  file_path,
  colClasses = "character",
  na.strings = c("", "NA", "N/A", "NULL"),
  showProgress = TRUE
)

setDT(dt)

#Then get the dimensions of the dataset:
dim(dt)


  
#Step 6: Specify the column structure

#This dataset has a regular structure:
  
 #Columns 1-10: metadata
 #Columns 11-104: antibiotic susceptibility
 #Columns 105-127: resistance gene markers

metadata_cols <- names(dt)[1:10]

ast_cols <- names(dt)[11:104]

mic_cols <- ast_cols[!endsWith(ast_cols, "_I")]

sir_cols <- ast_cols[endsWith(ast_cols, "_I")]

gene_cols <- names(dt)[105:127]

#now check:
length(metadata_cols)
length(mic_cols)
length(sir_cols)
length(gene_cols)

#now check the names:
metadata_cols
head(mic_cols, 20)
head(sir_cols, 20)
gene_cols


#Step 7: Memorise the number of rows and columns.
n_rows <- nrow(dt)
n_cols <- ncol(dt)

n_rows
n_cols



#Step 8: Identify key columns

metadata_cols

#Now check that they all exist:
required_metadata <- c(
  "Isolate Id",
  "Study",
  "Species",
  "Country",
  "State",
  "Gender",
  "Age Group",
  "Speciality",
  "Source",
  "Year"
)

required_metadata %in% names(dt)
#if all are TRUE, its OK!



#Step 9: Understand exactly what each row is.

row_unit_check <- dt[, .(
  rows = .N,
  unique_isolate_ids = uniqueN(`Isolate Id`),
  duplicate_isolate_ids = .N - uniqueN(`Isolate Id`)
)]

row_unit_check




#Step 10: Get the summary of Study, Year, Country, Species

dt[, .N, by = Study][order(-N)]

dt[, .N, by = Year][order(as.integer(Year))]

dt[, .N, by = Country][order(-N)][1:30]

dt[, .N, by = Species][order(-N)][1:30]




#Step 11: Get patient/sample metadata summary

dt[, .N, by = Gender][order(-N)]

dt[, .N, by = `Age Group`][order(-N)]

dt[, .N, by = Speciality][order(-N)]

dt[, .N, by = Source][order(-N)][1:30]




#Step 12: Create a simple variable catalogue
variable_catalog <- data.table(
  variable = names(dt),
  non_missing = sapply(dt, function(x) sum(!is.na(x))),
  missing = sapply(dt, function(x) sum(is.na(x))),
  missing_pct = round(sapply(dt, function(x) mean(is.na(x))) * 100, 2),
  n_unique = sapply(dt, function(x) uniqueN(x, na.rm = TRUE))
)

variable_catalog

#To see the first 30 columns:
head(variable_catalog, 30)



# Step 13: Create a summary of antibiotic columns

interpretation_cols_matched <- paste0(mic_cols, "_I")

antibiotic_summary <- data.table(
  antibiotic = mic_cols,
  mic_col = mic_cols,
  mic_non_missing = sapply(
    mic_cols,
    function(x) sum(!is.na(dt[[x]]))
  ),
  mic_missing_pct = round(
    sapply(
      mic_cols,
      function(x) mean(is.na(dt[[x]])) * 100
    ),
    2
  ),
  interpretation_col = interpretation_cols_matched,
  interpretation_col_exists = interpretation_cols_matched %in% names(dt),
  interpretation_non_missing = sapply(
    interpretation_cols_matched,
    function(x) {
      if (x %in% names(dt)) {
        sum(!is.na(dt[[x]]))
      } else {
        NA_integer_
      }
    }
  ),
  interpretation_missing_pct = round(
    sapply(
      interpretation_cols_matched,
      function(x) {
        if (x %in% names(dt)) {
          mean(is.na(dt[[x]])) * 100
        } else {
          NA_real_
        }
      }
    ),
    2
  )
)

antibiotic_summary[order(-interpretation_non_missing)]




#Step 14: Save the outputs
output_dir <- "phase1_outputs"
dir.create(output_dir, showWarnings = FALSE)

fwrite(variable_catalog,
       file.path(output_dir, "phase1_variable_catalog.csv"))

fwrite(antibiotic_summary,
       file.path(output_dir, "phase1_antibiotic_summary.csv"))

fwrite(row_unit_check,
       file.path(output_dir, "phase1_row_unit_check.csv"))

fwrite(dt[, .N, by = Study][order(-N)],
       file.path(output_dir, "phase1_counts_by_study.csv"))

fwrite(dt[, .N, by = Year][order(as.integer(Year))],
       file.path(output_dir, "phase1_counts_by_year.csv"))

fwrite(dt[, .N, by = Country][order(-N)],
       file.path(output_dir, "phase1_counts_by_country.csv"))

fwrite(dt[, .N, by = Species][order(-N)],
       file.path(output_dir, "phase1_counts_by_species.csv"))

fwrite(dt[, .N, by = Gender][order(-N)],
       file.path(output_dir, "phase1_counts_by_gender.csv"))

fwrite(dt[, .N, by = `Age Group`][order(-N)],
       file.path(output_dir, "phase1_counts_by_age_group.csv"))

fwrite(dt[, .N, by = Speciality][order(-N)],
       file.path(output_dir, "phase1_counts_by_speciality.csv"))

fwrite(dt[, .N, by = Source][order(-N)],
       file.path(output_dir, "phase1_counts_by_source.csv"))

