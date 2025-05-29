###### AIM 3 Biomarkers ########

rm(list = ls())

# Aim 3 Biomarker Analysis Script

# Load required libraries
library(dplyr)
library(tidyr)
library(car)
library(multcomp)
library(stats)
library(nortest)
library(survival)
library(survminer)
library(readxl)

# Replace with your actual dataset
df <- read.xlsx("Data/current_data_with_clusters.xlsx")

# List of biomarkers
biomarkers <- c("CD4.ABS", "CD4.%", "CD8.ABS", "CD8.%", "CD4/8.Ratio", "WBC", "RBC", "Hb.g/dl", "Hct.%",
                "MCV", "MCH", "MCHC", "RDW", "Platelets", "Neutro.%", "Lymph%", "Mono%", "Eosin.%", "Basos%",
                "Neut.(Abs)", "Lymph.(Abs)", "Mono.(Abs)", "Eos.(Abs)", "Baso.(Abs)", "Immature.Granulocytes.%",
                "Immature.Grans.(Abs)", "Glucose", "BUN", "Creatinine", "eGFR", "BUN/Creatinie.Ratio", "Sodium",
                "Potassium", "Chloride", "Carbon.Dioxide,.Total", "Calcium", "Protein,.Total", "Albumin",
                "Globulin,.Total", "A/G.Ratio", "Bilirubin,.Total", "Alkaline.Phosphatase", "AST.(SGOT)",
                "ALT.(SGPT)", "Cholesterol,.Total", "Triglycerides", "HDL", "VLDL", "LDL",
                "C-Reactive.Protein,.Cardiac", "Vitamin.D,.25-Hydroxy", "LDH", "Creatine.Kinase,.Total",
                "Testosterone", "Ferritin")



setdiff(biomarkers, names(df))

#df_clean <- df %>% filter(complete.cases(across(all_of(biomarkers))))

threshold <- 0.5 * nrow(df)  # 50% of rows

biomarkers_filtered <- names(which(sapply(df[biomarkers], function(x) sum(is.na(x))) <= threshold))


sapply(df[biomarkers_filtered], function(x) sum(is.na(x)))


library(dplyr)

df_imputed <- df %>%
  mutate(across(all_of(biomarkers_filtered), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))


##################

# 1. Likelihood Ratio Tests with FDR

# Initialize result list

# Assuming `biomarkers_filtered` contains your chosen column names
lrt_results <- list()

for (biomarker in biomarkers_filtered) {
  print(paste("Processing:", biomarker))

  sub_df <- df_imputed %>% filter(!is.na(cluster))

  if (!is.numeric(sub_df[[biomarker]])) next

  sub_df[[biomarker]] <- log1p(sub_df[[biomarker]])

  full_model <- lm(as.formula(paste0("`", biomarker, "` ~ factor(cluster)")), data = sub_df)
  null_model <- lm(as.formula(paste0("`", biomarker, "` ~ 1")), data = sub_df)

  lrt <- anova(null_model, full_model)

  if (nrow(lrt) >= 2) {
    lrt_results[[biomarker]] <- lrt$`Pr(>F)`[2]
  }
}

##### LRT on biomarker_filtered, not imputed#########

# lrt_results <- list()
# 
# for (biomarker in biomarkers_filtered) {
#   print(paste("Processing:", biomarker))
# 
#   sub_df <- df %>% filter(!is.na(.data[[biomarker]]), !is.na(cluster))
# 
#   if (!is.numeric(sub_df[[biomarker]])) next
# 
#   sub_df[[biomarker]] <- log1p(sub_df[[biomarker]])
# 
#   full_model <- lm(as.formula(paste0("`", biomarker, "` ~ factor(cluster)")), data = sub_df)
#   null_model <- lm(as.formula(paste0("`", biomarker, "` ~ 1")), data = sub_df)
# 
#   lrt <- anova(null_model, full_model)
# 
#   if (nrow(lrt) >= 2) {
#     lrt_results[[biomarker]] <- lrt$`Pr(>F)`[2]
#   }
# }
# 


# 
# lrt_results <- list()
# 
# for (biomarker in biomarkers) {
#   print(paste("Processing:", biomarker))
#   
#   if (!is.numeric(df_clean[[biomarker]])) next
#   
#   df_clean[[biomarker]] <- log1p(df_clean[[biomarker]])
#   
#   full_model <- lm(as.formula(paste0("`", biomarker, "` ~ factor(cluster)")), data = df_clean)
#   null_model <- lm(as.formula(paste0("`", biomarker, "` ~ 1")), data = df_clean)
#   
#   lrt <- anova(null_model, full_model)
#   
#   if (nrow(lrt) >= 2) {
#     lrt_results[[biomarker]] <- lrt$`Pr(>F)`[2]
#   }
# }


lrt_fdr <- p.adjust(unlist(lrt_results), method = "fdr")
signif_biomarkers <- names(lrt_fdr[lrt_fdr < 0.05])

# 2. Tukey HSD if >2 clusters
tukey_results <- list()
if (nlevels(as.factor(df$cluster)) >= 3) {
  for (biomarker in signif_biomarkers) {
    model <- aov(as.formula(paste0("`", biomarker, "` ~ cluster")), data = df)
    tukey_results[[biomarker]] <- TukeyHSD(model)
  }
}

# 3. Multiple Linear Regression with Significant Biomarkers
if (length(signif_biomarkers) > 0) {
  mlr_formula <- as.formula(paste("cluster ~", paste(paste0("`", signif_biomarkers, "`"), collapse = " + ")))
  mlr_model <- lm(mlr_formula, data = df)
  print(summary(mlr_model))
  print(AIC(mlr_model))
  print(ad.test(residuals(mlr_model)))
}

# 4. IHG vs Completion, IHG vs Vitamin D (Fisher's Exact)

df$consolidated_result <- ifelse(df$consolidated_result %in% c("Graduate", "Select"),
                                 "Graduate",
                                 "Elim")

# Optionally, convert to a factor with defined levels
#df$consolidated_result <- factor(df$consolidated_result, levels = c("Elim", "Graduate"))

# df$vit_d_cat <- cut(df$`Vitamin.D,.25-Hydroxy`, breaks = c(-Inf, 20, 30, 40, 50, Inf),
#                     labels = c("<20", "20–29.9", "30–39.9", "40–49.9", ">=50"))

df$vit_d_cat_collapsed <- dplyr::case_when(
  df$vit_d_cat %in% c("<20", "20–29.9") ~ "Low",
  df$vit_d_cat %in% c("30–39.9", "40–49.9") ~ "Adequate",
  df$vit_d_cat %in% c(">=50") ~ "Optimal",
  TRUE ~ NA_character_
)

df$vit_d_cat_collapsed <- factor(df$vit_d_cat_collapsed, levels = c("Low", "Adequate", "Optimal"))


print("IHG" %in% colnames(df))
print("consolidated_result" %in% colnames(df))


if("IHG" %in% colnames(df) & "consolidated_result" %in% colnames(df)) {
  print(chisq.test(table(df$IHG, df$consolidated_result)))
  print(fisher.test(table(df$IHG, df$vit_d_cat), simulate.p.value = TRUE))
}

chisq.test(table(df$vit_d_cat_collapsed, df$consolidated_result))
fisher.test(table(df$vit_d_cat_collapsed, df$consolidated_result), simulate.p.value = TRUE)



# 5. Paired t-tests for T1 vs T2
# convert to wide format: columns like biomarker_T1 and biomarker_T2

library(dplyr)
library(tidyr)

# Example: your vector of biomarkers to test
#signif_biomarkers <- c("Biomarker1", "Biomarker2", "Biomarker3")  # replace with your actual biomarker names

# Step 1: Filter for visits 1 and 2 only
df_filtered <- df %>%
  filter(Visit %in% c("V1", "V2"))

# Step 2: Keep only PINs with data for both visits
valid_pins <- df_filtered %>%
  group_by(PIN) %>%
  summarize(n_visits = n_distinct(Visit)) %>%
  filter(n_visits == 2) %>%
  pull(PIN)

df_filtered <- df_filtered %>%
  filter(PIN %in% valid_pins)


### check duplicates ####

df_filtered %>%
  dplyr::group_by(PIN, Visit) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::filter(n > 1)

df_filtered <- df_filtered %>%
  group_by(PIN, Visit) %>%
  slice(1) %>%  # keep only the first row per PIN-Visit group
  ungroup()



# Step 3: Pivot wider to get columns for T1 and T2 for each biomarker
df_wide <- df_filtered %>%
  dplyr::select(PIN, Visit, all_of(biomarkers)) %>%
  tidyr::pivot_wider(
    id_cols = PIN,
    names_from = Visit,
    names_prefix = "V",
    values_from = all_of(biomarkers)
  )


# Step 4: Run paired t-tests per biomarker

paired_pvals <- sapply(biomarkers, function(bio) {
  t.test(
    df_wide[[paste0(bio, "_VV1")]],
    df_wide[[paste0(bio, "_VV2")]],
    paired = TRUE,
    na.action = na.omit
  )$p.value
})

# Output the p-values
paired_pvals

#####

paired_pvals <- sapply(biomarkers, function(bio) {
  t.test(df_wide[[paste0(bio, "_VV1")]], df_wide[[paste0(bio, "_VV2")]], paired = TRUE)$p.value
})
paired_fdr <- p.adjust(paired_pvals, method = "fdr")
signif_changes <- names(paired_fdr[paired_fdr < 0.05])

# 6. Logistic Regression for Course Completion
logit_model <- glm(df$consolidated_result ~ ., data = df[, c("consolidated_result", signif_biomarkers)], family = binomial)
print(summary(logit_model))

# 7. Injury: Logistic, KM, Cox Models
df$ferritin_cat <- cut(df$Ferritin, breaks = c(-Inf, 30, 50, 60, Inf),
                       labels = c("<30", "30–49.9", "50–59.9", ">=60"))

if("injury" %in% colnames(df)) {
  print(summary(glm(injury ~ `Vitamin D, 25-Hydroxy` + Ferritin, family = binomial, data = df)))
  surv_obj <- Surv(time = df$time_to_injury, event = df$injury)
  km_fit <- survfit(surv_obj ~ vit_d_cat, data = df)
  ggsurvplot(km_fit, data = df, pval = TRUE, risk.table = TRUE)
  
  cox_model <- coxph(surv_obj ~ vit_d_cat + ferritin_cat, data = df)
  print(summary(cox_model))
}
