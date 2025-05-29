#AIM 1 2-way RM MANOVA attempt complete
rm(list = ls())

library(dplyr)
library(tidyr)
library(car)
library(readxl)

# 1. Load data
##dta <- read_excel("Data/Erin_SWCC_Retro_ANOVA2.xlsx")

dta <- read_excel("Data/Erin_SWCC_Retro_ANOVA2.xlsx", 
                  col_types = c(rep("text", 4), rep("numeric", 18)))  # adjust as needed



# 2. Clean and recode consolidated_result
dta$consolidated_result <- as.character(dta$consolidated_result)
dta$consolidated_result <- ifelse(dta$consolidated_result == "Graduate", "graduate", "elim")
dta$consolidated_result <- factor(dta$consolidated_result, levels = c("graduate", "elim"))

# 3. Remove rows with missing fitness data in relevant columns (columns 5 to 22)
# your cleaned dataset with no missing values:
dta_clean <- dta[complete.cases(dta[, 5,6,9:22]), ] %>% mutate(id = row_number())

# List of all 18 fitness variable names
fitness_vars <- c(
  "broadjump_inches1", "broadjump_inches2",
  "agilityright_seconds1", "agilityright_seconds2",
  "deadlift_pounds1", "deadlift_pounds2",
  "pullups1", "pullups2",
  "farmerscarry_seconds1", "farmerscarry_seconds2",
  "shuttle_300yd_seconds1", "shuttle_300yd_seconds2",
  "fin_swim_1500m_seconds1", "fin_swim_1500m_seconds2",
  "ruck_3mile_seconds1", "ruck_3mile_seconds2"
)

# Calculate and print variance for each variable
sapply(dta[, fitness_vars], var, na.rm = TRUE)

zero_variance <- sapply(dta[, fitness_vars], function(x) var(x, na.rm = TRUE) == 0)
names(zero_variance[zero_variance])





# # 5. Pivot longer: gather fitness measures across time points into long format
# dta_long <- dta_clean %>%
#   pivot_longer(
#     cols = c("broadjump_inches1","broadjump_inches2","agilityleft_seconds1","agilityleft_seconds2",
#              "agilityright_seconds1","agilityright_seconds2","deadlift_pounds1","deadlift_pounds2",
#              "pullups1","pullups2","farmerscarry_seconds1","farmerscarry_seconds2",
#              "shuttle_300yd_seconds1","shuttle_300yd_seconds2","fin_swim_1500m_seconds1","fin_swim_1500m_seconds2",
#              "ruck_3mile_seconds1","ruck_3mile_seconds2"),
#     names_to = c("measure", "time"),
#     names_pattern = "(.*)(\\d)$",
#     values_to = "value"
#   )

# # 6. Pivot wider to get one row per subject per time, columns for each measure
# dta_long_wide <- dta_long %>%
#   pivot_wider(names_from = measure, values_from = value)

# # 7. Convert time to factor
# dta_long_wide <- dta_long_wide %>%
#   mutate(time = factor(time))
# 
# # 8. Prepare within-subject design data frame for time factor (2 levels)
# idata <- data.frame(time = factor(c(1, 2)))
# 
# # 9. Prepare multivariate response matrix for dependent variables
# # Select columns of fitness measures (same names as after pivot_wider)
# fitness_vars <- c("broadjump_inches","agilityleft_seconds","agilityright_seconds",
#                   "deadlift_pounds","pullups","farmerscarry_seconds",
#                   "shuttle_300yd_seconds","fin_swim_1500m_seconds","ruck_3mile_seconds")

Y <- as.matrix(dta_clean[, c(
  "broadjump_inches1","broadjump_inches2",
  "agilityright_seconds1","agilityright_seconds2",
  "deadlift_pounds1","deadlift_pounds2",
  "pullups1","pullups2",
  "farmerscarry_seconds1","farmerscarry_seconds2",
  "shuttle_300yd_seconds1","shuttle_300yd_seconds2",
  "fin_swim_1500m_seconds1","fin_swim_1500m_seconds2",
  "ruck_3mile_seconds1","ruck_3mile_seconds2"
)])

idata <- data.frame(
  time = factor(rep(c(1, 2), times = 8)),  # 9 measures, each with 2 time points
  measure = factor(rep(c("broadjump_inches","agilityright_seconds",
                         "deadlift_pounds","pullups","farmerscarry_seconds",
                         "shuttle_300yd_seconds","fin_swim_1500m_seconds","ruck_3mile_seconds"),
                       each = 2))
)


library(car)

fit <- Anova(
  lm(Y ~ consolidated_result, data = dta_clean),
  idata = idata,
  idesign = ~ time * measure,
  type = "III"
)

summary(fit, multivariate = TRUE)

# 
#
##################Post hoc analysis ##################################

# Example for broadjump_inches
# library(ez)
# ezANOVA(
#   data = long_df,  # long-form version of your data
#   dv = .(value),
#   wid = .(id),
#   within = .(time),
#   between = .(consolidated_result),
#   subset = (measure == "broadjump_inches"),
#   detailed = TRUE,
#   type = 3
# )
library(dplyr)

# Your variable pairs (only the variables at time 1 and time 2 for each fitness test)
fitness_vars_pairs <- list(
  broadjump = c("broadjump_inches1", "broadjump_inches2"),
  agilityright = c("agilityright_seconds1", "agilityright_seconds2"),
  deadlift = c("deadlift_pounds1", "deadlift_pounds2"),
  pullups = c("pullups1", "pullups2"),
  farmerscarry = c("farmerscarry_seconds1", "farmerscarry_seconds2"),
  shuttle = c("shuttle_300yd_seconds1", "shuttle_300yd_seconds2"),
  finswim = c("fin_swim_1500m_seconds1", "fin_swim_1500m_seconds2"),
  ruck = c("ruck_3mile_seconds1", "ruck_3mile_seconds2")
)

manova_results <- list()

for (var in names(fitness_vars_pairs)) {
  
  vars_to_test <- fitness_vars_pairs[[var]]
  
  # Prepare data subset for MANOVA
  dta_sub <- dta_clean %>%
    select(consolidated_result, all_of(vars_to_test)) %>%
    na.omit()
  
  # Create MANOVA formula: cbind(var1, var2) ~ consolidated_result
  manova_formula <- as.formula(
    paste("cbind(", paste(vars_to_test, collapse = ", "), ") ~ consolidated_result")
  )
  
  # Fit MANOVA model
  fit_manova <- manova(manova_formula, data = dta_sub)
  
  # Get summary with Pillai test
  summary_manova <- summary(fit_manova, test = "Pillai")
  
  # Extract the stats from the first row (consolidated_result)
  pillai_trace <- summary_manova$stats["consolidated_result", "Pillai"]
  f_value <- summary_manova$stats["consolidated_result", "approx F"]
  df1 <- summary_manova$stats["consolidated_result", "num Df"]
  df2 <- summary_manova$stats["consolidated_result", "den Df"]
  p_value <- summary_manova$stats["consolidated_result", "Pr(>F)"]
  
  # Store results in a list
  manova_results[[var]] <- data.frame(
    Variable = var,
    Pillai_Trace = pillai_trace,
    Df1 = df1,
    Df2 = df2,
    F_value = f_value,
    p_value = p_value
  )
}

# Combine all results into one data.frame
manova_results_df <- do.call(rbind, manova_results)

print(manova_results_df)



############univariate ANOVA for each variable at time 1 and 2 separately ##############

library(dplyr)
library(openxlsx)

# Variables base names (without 1 or 2 suffix)
base_vars <- c(
  "broadjump_inches",
  "agilityright_seconds",
  "deadlift_pounds",
  "pullups",
  "farmerscarry_seconds",
  "shuttle_300yd_seconds",
  "fin_swim_1500m_seconds",
  "ruck_3mile_seconds"
)

# Prepare empty data frame to store results
uni_anova_results <- data.frame(
  Variable = character(),
  TimePoint = character(),
  Df1 = integer(),
  Df2 = integer(),
  F_value = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each variable and time point
for (var in base_vars) {
  for (tp in 1:2) {
    var_name <- paste0(var, tp)
    formula <- as.formula(paste(var_name, "~ consolidated_result"))
    
    # Run ANOVA (linear model + anova)
    fit <- aov(formula, data = dta_clean)
    anova_res <- summary(fit)[[1]]
    
    # Extract degrees of freedom, F and p values for consolidated_result effect
    df1 <- anova_res["consolidated_result", "Df"]
    df2 <- anova_res["Residuals", "Df"]
    F_val <- anova_res["consolidated_result", "F value"]
    p_val <- anova_res["consolidated_result", "Pr(>F)"]
    
    # Add to results
    uni_anova_results <- rbind(
      uni_anova_results,
      data.frame(
        Variable = var,
        TimePoint = paste0("Time", tp),
        Df1 = df1,
        Df2 = df2,
        F_value = F_val,
        p_value = p_val,
        stringsAsFactors = FALSE
      )
    )
  }
}

# View the results
print(uni_anova_results)

########## export to excel #####

# Export to Excel file
write.xlsx(uni_anova_results, file = "univariate_anova_results.xlsx", overwrite = TRUE)

#####################visualize ###################################

library(ggplot2)
library(dplyr)
library(tidyr)

# Your selected variables without the time suffixes
vars_base <- c("broadjump_inches", "agilityright_seconds", "pullups", 
               "deadlift_pounds","fin_swim_1500m_seconds","ruck_3mile_seconds",
               "farmerscarry_seconds","shuttle_300yd_seconds")

# Prepare to reshape wide to long by extracting columns with 1 and 2 suffixes
# For each base variable, create two columns: var1 and var2 (Time1 and Time2)

# Select relevant columns (including consolidated_result for group)
cols_to_select <- c("consolidated_result", paste0(vars_base, "1"), paste0(vars_base, "2"))
plot_data_wide <- dta_clean %>% select(all_of(cols_to_select))

# Pivot longer for variables and timepoints
plot_data_long <- plot_data_wide %>%
  pivot_longer(
    cols = -consolidated_result,
    names_to = c("variable", "timepoint"),
    names_pattern = "(.*)([12])$",
    values_to = "value"
  ) %>%
  mutate(timepoint = ifelse(timepoint == "1", "Time1", "Time2"))

# Summarize means and SE by variable, timepoint, and group
summary_plot <- plot_data_long %>%
  group_by(variable, timepoint, consolidated_result) %>%
  summarize(
    mean_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    n = n(),
    se = sd_value / sqrt(n),
    .groups = "drop"
  )

# Plot
ggplot(summary_plot, aes(x = timepoint, y = mean_value, color = consolidated_result, group = consolidated_result)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  geom_line(position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.2, position = position_dodge(0.3)) +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Fitness Variable Means Over Time by Group",
       x = "Time Point",
       y = "Mean Value",
       color = "Group") +
  theme_minimal()


