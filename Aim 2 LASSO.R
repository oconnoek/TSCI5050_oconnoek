## LASSO Predictive Model#########

library(readxl)
library(glmnet)
library(pROC)
#install.packages("writexl")
library(writexl)


# Load and prepare data
dta <- read_excel("Data/Erin_SWCC_Retro_TEST HCLUST.xlsx")

# Select predictor variables (columns used for clustering)
X <- dta[,c(5:9,25:27)]
X_scaled <- scale(X)
fil <- rowSums(is.na(X_scaled)) == 0

# Cluster assignment
hc <- hclust(dist(X_scaled[fil,], method = 'euclidean'), method = 'ward.D2')
cluster_cut <- cutree(hc, k = 5)

# Prepare full dataset
X_full <- as.data.frame(X_scaled[fil, ])
X_full$cluster <- factor(cluster_cut)

# Recode consolidated_result to binary: "Graduate" = 1, others = 0
dta$binary_result <- ifelse(dta$consolidated_result == "Graduate", 1, 0)

# Extract outcome (make sure this is aligned correctly)
y_full <- dta$binary_result[fil]


# Split into training/validation
set.seed(123)
n <- nrow(X_full)
train_idx <- sample(seq_len(n), size = floor(2/3 * n))
valid_idx <- setdiff(seq_len(n), train_idx)

X_train <- X_full[train_idx, ]
X_valid <- X_full[valid_idx, ]
y_train <- y_full[train_idx]
y_valid <- y_full[valid_idx]


######## LASSO Loop ######

selected_vars_by_cluster <- list()

for (k in 1:5) {
  cat("Running LASSO on Cluster", k, "\n")
  cluster_rows <- X_train$cluster == k
  vars_in_cluster <- cluster_vars
  
  n_cluster <- sum(cluster_rows)
  if (n_cluster < 10) {
    cat("  Skipping cluster", k, "- not enough samples.\n")
    next
  }
  
  X_sub <- as.matrix(X_train[cluster_rows, vars_in_cluster])
  y_sub <- y_train[cluster_rows]
  
  # Fit LASSO
  cv_fit <- cv.glmnet(X_sub, y_sub, family = "binomial", alpha = 1)
  coef_min <- coef(cv_fit, s = "lambda.min")
  coef_vec <- as.vector(coef_min)[-1]
  names(coef_vec) <- rownames(coef_min)[-1]
  
  # Extract and clean coefficients
  nonzero <- coef_vec != 0
  if (!any(nonzero)) {
    cat("  No nonzero coefficients for cluster", k, "\n")
    next
  }
  
  coef_table <- data.frame(
    cluster = paste0("Cluster_", k),
    variable = names(coef_vec)[nonzero],
    coefficient = coef_vec[nonzero],
    n_in_cluster = n_cluster,
    stringsAsFactors = FALSE
  )
  
  selected_vars_by_cluster[[paste0("Cluster_", k)]] <- coef_table
}


# Combine into one data frame
all_cluster_results <- do.call(rbind, selected_vars_by_cluster)
write_xlsx(all_cluster_results, "Cluster_LASSO_Results.xlsx")

selected_vars <- unique(selected_vars)

# Step 2: Final logistic model on training set
X_train_final <- as.data.frame(X_train[, selected_vars, drop = FALSE])
X_valid_final <- as.data.frame(X_valid[, selected_vars, drop = FALSE])

final_model <- glm(y_train ~ ., data = X_train_final, family = "binomial")

# Step 3: Predict on validation set
pred_valid <- predict(final_model, newdata = X_valid_final, type = "response")

# Step 4: ROC and AUC
roc_obj <- roc(y_valid, pred_valid)
auc_value <- auc(roc_obj)

cat("Validation AUC:", auc_value, "\n")

# Optional: plot ROC
plot(roc_obj, main = paste("Validation ROC Curve - AUC:", round(auc_value, 3)))

############



############### Mean AUC over k-folds ############### BEST AUC SO FAR

library(glmnet)
library(pROC)

set.seed(123)  # for reproducibility

k <- 5
folds <- sample(rep(1:k, length.out = nrow(X2)))

auc_values <- numeric(k)

for (i in 1:k) {
  # Split data
  train_idx <- which(folds != i)
  test_idx  <- which(folds == i)
  
  X_train <- as.matrix(X2[train_idx, ])
  y_train <- y[train_idx]
  
  X_test  <- as.matrix(X2[test_idx, ])
  y_test  <- y[test_idx]
  
  # Fit LASSO model
  fit <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1)
  y_prob <- as.numeric(predict(fit, s = fit$lambda.min, newx = X_test, type = "response"))
  
  # Compute AUC
  roc_obj <- roc(y_test, y_prob)
  auc_values[i] <- auc(roc_obj)



  # Store ROC curve data
  roc_df <- data.frame(
    FPR = 1 - roc_obj$specificities,
    TPR = roc_obj$sensitivities,
    Fold = paste("Fold", i, sprintf("(AUC = %.3f)", auc_values[i]))
  )
  roc_data[[i]] <- roc_df
}

# Combine all ROC data
roc_all <- do.call(rbind, roc_data)

# Plot ROC curves
ggplot(roc_all, aes(x = FPR, y = TPR, color = Fold)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(
    title = paste("ROC Curves over", k, "Folds"),
    subtitle = paste("Mean AUC:", round(mean(auc_values), 3)),
    x = "False Positive Rate",
    y = "True Positive Rate",
    color = "Fold"
  ) +
  theme_minimal()


# Report mean AUC
mean_auc <- mean(auc_values)
print(mean_auc)

