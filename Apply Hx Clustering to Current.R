#####. Apply HX Clustering to Current Data ############### ADD CLUSTER C to Merged data ##########
rm(list = ls())

# 
# library(dplyr)
# library(readxl)
# library(tidyverse)  # includes ggplot2, purrr, etc.
# library(ggplot2)
# 
# # Load data
# hx_dta <- read_xlsx("Data/Erin_SWCC_Retro_TEST HCLUST.xlsx")
# current_data <- read_xlsx("Data/combined_merged_data.xlsx")
# 
# # Select fitness variables only
# fitness_vars <- c("swim_seconds", "run_seconds", "pushups", "situps", "pullups", 
#                   "age_calc", "battery_iq_overall", "asvab_afqt_percentile")
# 
# historic_data <- hx_dta %>% select(all_of(fitness_vars)) %>% as_tibble()
# 
# # Step 1: Clean historic fitness data - keep only complete cases
# historic_clean <- historic_data %>%
#   filter(complete.cases(across(all_of(fitness_vars))))
# 
# # Step 2: Scale the cleaned data
# historic_scaled <- scale(historic_clean[fitness_vars])
# 
# # Step 3: Run k-means clustering
# set.seed(123)  # reproducibility
# k <- 5         # number of clusters; adjust as needed
# kmeans_fit <- kmeans(historic_scaled, centers = k)
# 
# # Step 4: Assign clusters to cleaned data only
# historic_clean <- historic_clean %>%
#   mutate(cluster = factor(kmeans_fit$cluster))
# 
# # Step 5: Store means and SDs for future scaling of new data
# means <- attr(historic_scaled, "scaled:center")
# sds <- attr(historic_scaled, "scaled:scale")



################### 
library(dplyr)
library(readxl)
library(tidyverse)

hx_dta <- read_xlsx("Data/Erin_SWCC_Retro_TEST HCLUST.xlsx")
current_data <- read_xlsx("Data/combined_merged_data.xlsx")


historic_data <- hx_dta[,c("swim_seconds","run_seconds","pushups", "situps", "pullups",
                           "age_calc", "battery_iq_overall", "asvab_afqt_percentile")]


# ------------------------------
# STEP 1: Define fitness variables
# ------------------------------
fitness_vars <- c("swim_seconds","run_seconds","pushups", "situps", "pullups",
                  "age_calc", "battery_iq_overall", "asvab_afqt_percentile")
# ------------------------------
# STEP 2: Scale and cluster historical fitness data
# ------------------------------
historic_data <- as_tibble(historic_data)
historic_fitness <- historic_data %>% dplyr::select(all_of(fitness_vars))


# 1. Clean historic fitness data first — keep only complete cases
historic_clean <- historic_fitness %>%
  filter(complete.cases(across(all_of(fitness_vars))))

# 2. Scale the cleaned data
historic_scaled <- scale(historic_clean[fitness_vars])

# Perform k-means clustering
set.seed(123)  # for reproducibility
k <- 5  # change this based on your original clustering
kmeans_fit <- kmeans(historic_scaled, centers = k)


# # 3. Run kmeans on cleaned data
# set.seed(123)
# kmeans_fit <- kmeans(historic_scaled, centers = k)

# 4. Assign clusters to cleaned dataset only
historic_clean <- historic_clean %>%
  mutate(cluster = factor(kmeans_fit$cluster))



# Store means and SDs for applying to new data
means <- attr(historic_scaled, "scaled:center")
sds <- attr(historic_scaled, "scaled:scale")

#
# # Add cluster labels to historical data
# historic_clustered <- historic_fitness %>%
#   mutate(cluster = factor(kmeans_fit$cluster))

# ---------------------------
# Visualization of clusters using PCA
# ---------------------------

# Compute PCA on scaled data
pca <- prcomp(historic_scaled)

# Create a data frame with PCA results and clusters
pca_df <- data.frame(
  PC1 = pca$x[,1],
  PC2 = pca$x[,2],
  cluster = historic_clean$cluster
)

# Plot the first two principal components colored by cluster
ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "PCA of Historic Fitness Data with K-means Clusters",
       x = "PC1", y = "PC2", color = "Cluster") +
  theme_minimal() +
  theme(legend.position = "right")


# ------------------------------
# STEP 3: Standardize current data using same scale
# ------------------------------
current_fitness <- current_data %>% select(PIN, all_of(fitness_vars))

current_scaled <- scale(current_fitness[fitness_vars], center = means, scale = sds)

# ------------------------------
# STEP 4: Assign current data to nearest historical centroid
# ------------------------------
get_closest_cluster <- function(row, centroids) {
  distances <- apply(centroids, 1, function(centroid) sum((row - centroid)^2))
  return(which.min(distances))
}

current_cluster_ids <- apply(current_scaled, 1, get_closest_cluster, centroids = kmeans_fit$centers)

current_clustered <- current_data %>%
  mutate(fitness_cluster = factor(current_cluster_ids))

# ------------------------------
# STEP 5: Now you're ready to analyze
# ------------------------------

# Example: LRT comparing biomarkers across fitness clusters
lrt_results <- list()
biomarkers <- c("Glucose", "Bilirubin_Total", "Hemoglobin", "Albumin")  # replace with yours

for (biomarker in biomarkers) {
  current_clustered[[biomarker]] <- log1p(current_clustered[[biomarker]])
  full_model <- lm(as.formula(paste0("`", biomarker, "` ~ fitness_cluster")), data = current_clustered)
  null_model <- lm(as.formula(paste0("`", biomarker, "` ~ 1")), data = current_clustered)
  lrt <- anova(null_model, full_model)
  lrt_results[[biomarker]] <- lrt$`Pr(>F)`[2]
}

# View sorted p-values
lrt_results %>%
  unlist() %>%
  sort()
