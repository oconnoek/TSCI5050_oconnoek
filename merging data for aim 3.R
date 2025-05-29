######### AIM 3 MERGING FILES BIOMARKERS ###########

rm(list = ls())
# ================================
# Script: consolidate_biomarkers_and_results.R
# Purpose: Combine biomarker data and merge with consolidated results
# ================================

# Load required packages
library(readxl)
library(dplyr)
library(purrr)
library(stringr)

# ------------------------
# 1. List all biomarker files
# ------------------------

biomarker_files <- list.files(path = "Data/",
                              pattern = "IR-AD-001.*\\.xlsx",
                              full.names = TRUE)

# Clean names to fix upper and lower case mixes:###

library(janitor)

read_biomarker_file <- function(file) {
  cohort <- extract_cohort(file)
  df <- read_excel(file, sheet = 1, col_types = "text") %>%
    janitor::clean_names()
  
  if (!"visit" %in% names(df)) {
    stop(paste("Missing 'visit' column in:", file))
  }
  
  time_point <- df$visit[1]
  
  df %>%
    mutate(cohort = cohort,
           time_point = time_point)
}



# ------------------------
# 2. Extract cohort from filename
# ------------------------

extract_cohort <- function(filename) {
  str_extract(filename, "C\\d+")
}

# ------------------------
# 3. Read a single file, extract cohort + visit info
# ------------------------

read_biomarker_file <- function(file) {
  cohort <- extract_cohort(file)
  df <- read_excel(file, sheet = 1)
  
  time_point <- df$Visit[1]  # Assumes all rows in file have the same visit
  
  df %>%
    mutate(cohort = cohort,
           time_point = time_point)
}


##########################
read_biomarker_file <- function(file) {
  cohort <- extract_cohort(file)
  
  df <- read_excel(file, sheet = 1, col_types = "text")  # Treat all as text
  if (!"Visit" %in% names(df)) {
    stop(paste("Missing 'Visit' column in:", file))
  }
  
  time_point <- df$Visit[1]
  
  df %>%
    mutate(cohort = cohort,
           time_point = time_point)
}

############
# ------------------------
# 4. Combine all biomarker files
# ------------------------

biomarker_data <- map_dfr(biomarker_files, read_biomarker_file) %>%
  relocate(cohort, time_point)


# ------------------------
# 5. Optional: Recode time points
# ------------------------
# 
# biomarker_data <- biomarker_data %>%
#   mutate(time_point = recode(time_point,
#                              V1 = "Baseline",
#                              V2 = "Mid",
#                              V3 = "Final"))

# ------------------------
# 6. Read consolidated results file
# ------------------------

# Replace this with your actual filename
results_data <- read_excel("Data/Master_Merge_Cohort_1-6_CAO_050725.xlsx")

# ------------------------
# 7. Merge biomarker and consolidated result data by PIN
# ------------------------
#convert PIN to character in both datasets##

# Convert PIN to character in both datasets
biomarker_data <- biomarker_data %>%
  mutate(PIN = as.character(PIN))

results_data <- results_data %>%
  mutate(PIN = as.character(PIN))


###test
#intersect(biomarker_data$PIN, results_data$PIN)

##Keep only most recent results per PIN#####

results_data_clean <- results_data %>%
  group_by(PIN) %>%
  slice_tail(n = 1) %>%
  ungroup()

### Standardize PINs ####

# Ensure both are character and pad to 3 digits with leading zeros
# biomarker_data <- biomarker_data %>%
#   mutate(PIN = str_pad(as.character(PIN), width = 3, side = "left", pad = "0"))
# 
# results_data_clean <- results_data_clean %>%
#   mutate(PIN = str_pad(as.character(PIN), width = 3, side = "left", pad = "0"))
# 

merged_data <- biomarker_data %>%
  inner_join(results_data_clean, by = "PIN")


##### convert to numeric if character ############

#merged_data %>%
# summarise(across(where(is.character), ~ sum(grepl("^[0-9.]+$", .)) / n()))

numeric_vars <- c("PIN","CD4 ABS", "CD4 %", "CD8 ABS", "CD8 %", "CD4/8 Ratio", "WBC", "RBC", "Hb g/dl", "Hct %",
                  "MCV", "MCH", "MCHC", "RDW", "Platelets", "Neutro %", "Lymph%", "Mono%", "Eosin %", "Basos%",
                  "Neut (Abs)", "Lymph (Abs)", "Mono (Abs)", "Eos (Abs)", "Baso (Abs)", "Immature Granulocytes %",
                  "Immature Grans (Abs)", "Glucose", "BUN", "Creatinine", "eGFR", "BUN/Creatinie Ratio", "Sodium",
                  "Potassium", "Chloride", "Carbon Dioxide, Total", "Calcium", "Protein, Total", "Albumin",
                  "Globulin, Total", "A/G Ratio", "Bilirubin, Total", "Alkaline Phosphatase", "AST (SGOT)",
                  "ALT (SGPT)", "Cholesterol, Total", "Triglycerides", "HDL", "VLDL", "LDL",
                  "C-Reactive Protein, Cardiac", "Vitamin D, 25-Hydroxy", "LDH", "Creatine Kinase, Total",
                  "Testosterone", "Ferritin")

merged_data <- merged_data %>%
  mutate(across(all_of(numeric_vars), ~ suppressWarnings(as.numeric(.))))



#confirm 
n_distinct(unmatched_biomarkers$PIN)


# Now join
# merged_data <- biomarker_data %>%
#   left_join(results_data, by = "PIN")
# 
# 
# 
# merged_data <- biomarker_data %>%
#   left_join(results_data, by = "PIN")  # Change "PIN" if your ID column has a different name

# ------------------------
# 8. Optional: Save merged data
# ------------------------
# 
# all_data <- all_data %>%
#   mutate(time_point = recode(time_point,
#                              V1 = "Baseline",
#                              V2 = "Mid",
#                              V3 = "Final"))

#write.xlsx(all_data, "combined_data.xlsx", row.names = FALSE)


write.xlsx(merged_data, "combined_merged_data.xlsx", rowNames = FALSE)

# ------------------------
# 9. Optional: Check unmatched PINs
# ------------------------

# Subjects in biomarkers but not in results
unmatched_biomarkers <- anti_join(biomarker_data, results_data, by = "PIN")

# Subjects in results but not in biomarkers
unmatched_results <- anti_join(results_data, biomarker_data, by = "PIN")

# You can inspect these manually:

View(unmatched_biomarkers)
View(unmatched_results)

# See unique unmatched PINs
unmatched_biomarkers %>% select(PIN) %>% distinct()

# Look at their cohorts
unmatched_biomarkers %>% count(cohort)

# Compare with results_data_clean
#anti_join(unmatched_biomarkers, results_data_clean, by = "PIN")

#confirm rows after merge to exclude "G Donor"
nrow(merged_data)
n_distinct(merged_data$PIN)

