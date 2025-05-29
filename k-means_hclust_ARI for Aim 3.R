#### Attempt 2 apply hx cluters to current data using ARI for k-means and HClust #######
rm(list = ls())


# Load required libraries
library(dplyr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(cluster)
library(factoextra)
library(mclust)  # For adjustedRandIndex

# Load data
hx_dta <- read_xlsx("Data/Erin_SWCC_Retro_TEST HCLUST.xlsx")
current_data <- read_xlsx("Data/combined_merged_data.xlsx")

# Select fitness variables
fitness_vars <- c("swim_seconds","run_seconds","pushups", "situps", "pullups", 
                  "age_calc", "battery_iq_overall", "asvab_afqt_percentile")

# Clean and prepare historical data
historic_data <- hx_dta[, fitness_vars] %>% as_tibble()
historic_clean <- historic_data %>% filter(complete.cases(.))

# Scale the data
historic_scaled <- scale(historic_clean)

# -----------------------------
# K-means clustering
# -----------------------------
set.seed(123)
k <- 5
kmeans_fit <- kmeans(historic_scaled, centers = k)
historic_kmeans <- historic_clean %>% mutate(kmeans_cluster = factor(kmeans_fit$cluster))

# Store means and SDs for applying to new data
means <- attr(historic_scaled, "scaled:center")
sds <- attr(historic_scaled, "scaled:scale")

# -----------------------------
# Hierarchical clustering (Ward's method)
# -----------------------------
dist_matrix <- dist(historic_scaled, method = "euclidean")
hclust_fit <- hclust(dist_matrix, method = "ward.D2")
hclust_clusters <- cutree(hclust_fit, k = k)
historic_hclust <- historic_clean %>% mutate(hclust_cluster = factor(hclust_clusters))

# -----------------------------
# Compare clustering results
# -----------------------------
ari <- adjustedRandIndex(kmeans_fit$cluster, hclust_clusters)
cat("Adjusted Rand Index between k-means and hierarchical clustering:", ari, "\n")

# -----------------------------
# Visualize with PCA
# -----------------------------
pca_res <- prcomp(historic_scaled)
pca_data <- as_tibble(pca_res$x[, 1:2]) %>%
  mutate(KMeans = factor(kmeans_fit$cluster),
         HClust = factor(hclust_clusters))

# Plot K-means clusters
p1 <- ggplot(pca_data, aes(x = PC1, y = PC2, color = KMeans)) +
  geom_point(alpha = 0.6) +
  labs(title = "PCA - K-means Clusters") +
  theme_minimal()

# Plot HClust clusters
p2 <- ggplot(pca_data, aes(x = PC1, y = PC2, color = HClust)) +
  geom_point(alpha = 0.6) +
  labs(title = "PCA - Hierarchical Clusters") +
  theme_minimal()

# Show plots side by side
library(patchwork)
p1 + p2


########################################

# STEP 1: Extract and clean current fitness data
fitness_vars_current <- fitness_vars
fitness_vars_current[fitness_vars_current == "age_calc"] <- "Age"

current_fitness <- current_data %>%
  dplyr::select(all_of(fitness_vars_current)) %>%
  filter(complete.cases(.))


# STEP 2: Scale current data using historic means and SDs
current_scaled <- scale(current_fitness, center = means, scale = sds)

# STEP 3: Assign to nearest K-means centroid (Euclidean distance)
get_nearest_cluster <- function(x, centers) {
  distances <- apply(centers, 1, function(centroid) sum((x - centroid)^2))
  return(which.min(distances))
}

# Apply function row-wise to all current_scaled rows
assigned_clusters <- apply(current_scaled, 1, get_nearest_cluster, centers = kmeans_fit$centers)

# STEP 4: Add cluster assignments back to current data
current_fitness_clustered <- current_fitness %>%
  mutate(cluster = factor(assigned_clusters))

# If you want to merge back with full current_data (e.g. biomarkers):
# First ensure row numbers align (only those with complete fitness data)
current_data_clustered <- current_data %>%
  filter(complete.cases(across(all_of(fitness_vars_current)))) %>%
  mutate(cluster = factor(assigned_clusters))


########### visualize ##########

# Step 1: Get just the fitness variables (complete cases)
current_fitness_complete <- current_data %>%
  filter(complete.cases(across(all_of(fitness_vars_current)))) %>%
  dplyr::select(all_of(fitness_vars_current))

# Step 2: Scale using historic means and SDs
current_scaled <- scale(current_fitness_complete, center = means, scale = sds)


# # Scale the fitness variables in current data (using historic means & sds)
# current_scaled <- scale(current_data %>% 
#                           filter(complete.cases(across(all_of(fitness_vars_current)))) %>% 
#                           select(all_of(fitness_vars_current)),
#                         center = means,
#                         scale = sds)

# Run PCA
pca_current <- prcomp(current_scaled)

# Prepare a PCA plot
library(ggplot2)

pca_df_current <- as.data.frame(pca_current$x[, 1:2]) %>%
  mutate(
    cluster = factor(assigned_clusters),
    PIN = current_data$PIN[complete.cases(current_data[, fitness_vars_current])]
  )

# Function to calculate convex hulls
get_hull <- function(df) df[chull(df$PC1, df$PC2), ]

hulls <- pca_df_current %>%
  group_by(cluster) %>%
  group_map(~ get_hull(.x)) %>%
  bind_rows()


library(ggrepel)

# ggplot(pca_df_current, aes(x = PC1, y = PC2, color = cluster)) +
#   geom_point(alpha = 0.6) +
#   geom_polygon(data = hulls, aes(group = cluster), fill = NA, color = "black", linetype = "dashed") +
#   labs(title = "PCA with Convex Hulls by Cluster", x = "PC1", y = "PC2") +
#   theme_minimal() +
#   scale_color_brewer(palette = "Set1")
# 

# 
# pca_df_current <- as.data.frame(pca_current$x[, 1:2]) %>%
#   mutate(cluster = factor(assigned_clusters))
# 
ggplot(pca_df_current, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = "PCA of Current Fitness Data with Assigned Clusters",
       x = "PC1", y = "PC2") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

current_with_cluster <- current_fitness_complete %>%
  mutate(cluster = factor(assigned_clusters)) %>%
  pivot_longer(cols = all_of(fitness_vars_current), names_to = "variable", values_to = "value")

ggplot(current_with_cluster, aes(x = variable, y = value, fill = cluster)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(), width = 0.7) +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.7), width = 0.3) +
  labs(title = "Cluster-wise Means of Fitness Variables", x = "Fitness Variable", y = "Mean Value") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



####### export ######

library(writexl)

# Export to Excel
write_xlsx(current_data_clustered, "Data/current_data_with_clusters.xlsx")

###################



