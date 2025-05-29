################### RM MANOVA 3 Time points ############################

rm(list = ls())

library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# 1. Load the data
dta <- read_excel("Data/SWCC_Retro_MANOVA3.xlsx",  # replace with actual filename
                  col_types = c(rep("text", 4), rep("numeric", 32)))  # 9 variables × 3 time points = 27

# 2. Recode outcome
dta$consolidated_result <- as.character(dta$consolidated_result)
dta$consolidated_result <- ifelse(dta$consolidated_result == "Graduate", "graduate", "elim")
dta$consolidated_result <- factor(dta$consolidated_result, levels = c("graduate", "elim"))

# 3. Define variable base names (the 9 fitness variables)
fitness_base_vars <- c(
  "broadjump_inches",
  "agilityright_seconds",
  "deadlift_pounds",
  "pullups",
  "farmerscarry_seconds",
  "shuttle_300yd_seconds",
  "fin_swim_1500m_seconds",
  "ruck_3mile_seconds"
)

# 4. Create full variable names with 3 time points each
fitness_vars_3tp <- unlist(lapply(fitness_base_vars, function(var) {
  paste0(var, 1:3)
}))

# 5. Clean data: remove rows with missing fitness data across any of the 3 time points for the variables
dta_clean <- dta %>%
  filter(complete.cases(select(., all_of(fitness_vars_3tp), consolidated_result))) %>%
  mutate(id = row_number())

# 6. Run MANOVA for each fitness variable (3 time points together)
manova_results_list <- lapply(fitness_base_vars, function(var) {
  vars_timepoints <- paste0(var, 1:3)
  formula <- as.formula(paste("cbind(", paste(vars_timepoints, collapse = ", "), ") ~ consolidated_result"))
  
  fit <- manova(formula, data = dta_clean)
  sum_fit <- summary(fit, test = "Pillai")$stats
  
  data.frame(
    Variable = var,
    Pillai_Trace = sum_fit[1, "Pillai"],
    Df1 = sum_fit[1, "num Df"],
    Df2 = sum_fit[1, "den Df"],
    F_value = sum_fit[1, "approx F"],
    p_value = sum_fit[1, "Pr(>F)"]
  )
})

# 7. Combine into one dataframe
manova_results_df <- do.call(rbind, manova_results_list)

# 8. Export results to Excel
write_xlsx(manova_results_df, "fitness_3_timepoints_results.xlsx")
