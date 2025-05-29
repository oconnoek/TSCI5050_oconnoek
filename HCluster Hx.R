rm(list = ls())

library(pROC)
library(glmnet)
library(dplyr)
#################################################################################

library(readxl)
#Erin_SWCC_Retro_TEST_HCLUST <- read_excel("Data/Erin_SWCC_Retro_TEST HCLUST.xlsx")



dta <- read_excel("Data/Erin_SWCC_Retro_TEST HCLUST_numerical.xlsx")

#attach(dta)


#############################################################################

#X <- dta[,5:12]

#clean data:
#sum(is.na(INJURY_PRIOR))
#injury_no <- ifelse(is.na(INJURY_PRIOR), "No", INJURY_PRIOR)  # Replace NAs with No
#injury_num <- ifelse(injury_no == "Yes", 1,0)
#injury_num <- as.numeric(factor(INJURY_PRIOR, levels = 
#                                  "Yes", "No"))
#gender_num <- ifelse(gender.x == "M", 1,0)
#trqi_level_num <- as.numeric(factor(trqi_level_3, levels = 
#                                      c("AF - Non Prior Service", "AF - Prior Service", 
#                                        "AF - Retrainee", "CNN0", "CC10", "RR10")))


X <- dta[,c(9:13,17:25, 29:31)]

X2 <- scale(X)

fil <- rowSums(is.na(X))==0

#HClustering
hc <- hclust(dist(X2[fil,],'euclidean'),'ward.D2')

#plot as dendrogram
plot(hc,labels=F,hang=-1)



cluster_cut <- cutree(hc, k =4)
X2_subset <- as.data.frame(X2[fil, ])
X2_subset$cluster <- factor(cluster_cut)

summary(X2_subset$cluster)
aggregate(. ~ cluster, data = X2_subset, FUN = mean)

#cluster profiles
#Convert cluster column to row names for easy handling
cluster_means <- aggregate(. ~ cluster, data = X2_subset, FUN = mean)

rownames(cluster_means) <- paste0("Cluster_", cluster_means$cluster)
cluster_means$cluster <- NULL

# Get top 3 positive and negative traits per cluster
get_profile <- function(row) {
  sorted <- sort(row, decreasing = TRUE)
  top <- names(sorted)[1:3]
  bottom <- names(sorted)[(length(sorted)-2):length(sorted)]
  list(
    strengths = top,
    weaknesses = bottom
  )
}

profiles <- apply(cluster_means, 1, get_profile)

# Print cluster profiles
for (i in seq_along(profiles)) {
  cat(paste0("\n📊 ", names(profiles)[i], "\n"))
  cat("  🔼 Top strengths: ", paste(profiles[[i]]$strengths, collapse = ", "), "\n")
  cat("  🔽 Weakest traits: ", paste(profiles[[i]]$weaknesses, collapse = ", "), "\n")
}


#invert columns that are better to be lower: BMI, PBF, BFM, swim, run:
# # Step 1: Generate cluster profiles
# cluster_means <- aggregate(. ~ cluster, data = X2_subset, FUN = mean)
# 
# # Step 2: Invert variables where lower is better
# vars_to_invert <- c(
#   "swim_seconds", 
#   "run_seconds", 
#   "BFM_Body_Fat_Mass_Initial", 
#   "PBF_Percent_Body_Fat_Initial", 
#   "BMI_Initial"
# )
# 
# # Invert selected columns
# cluster_means[vars_to_invert] <- -cluster_means[vars_to_invert]
# 
# # Step 3 (Optional): Rename to indicate reversal
# colnames(cluster_means)[colnames(cluster_means) %in% vars_to_invert] <-
#   paste0(colnames(cluster_means)[colnames(cluster_means) %in% vars_to_invert], "_reversed")
# 
# # Step 4: View output
# print(cluster_means)



##################

#heatmap

heatmap(scale(as.matrix(X2_subset[, c(1:17)])), Rowv = as.dendrogram(hc), Colv = NA)

#library(pheatmap)
# pheatmap(scale(as.matrix(X2_subset[, c(1:22)])), 
#          cluster_rows = TRUE, cluster_cols = FALSE)
# 
# # Scale and plot the filtered matrix
# pheatmap(scale(as.matrix(X2_subset[, c(1:22)])), 
#          show_colnames = TRUE,
#          cluster_rows = TRUE,
#          cluster_cols = TRUE,
#          main = "Heatmap without NA-heavy or zero-variance columns")



# 
# # Step 1: Subset and convert to data frame
# X2_subset <- as.data.frame(X2[fil, ])
# 
# # Step 2: Add cluster labels
# X2_subset$cluster <- factor(cluster_cut)
# 
# # Step 3: Define variables to plot
# vars <- c("swim_seconds", "run_seconds", "pushups", "pullups")
# 
# # Step 4: Plot boxplots
# par(mfrow = c(2, 2))  # layout for 4 plots
# for (v in vars) {
#   boxplot(as.formula(paste(v, "~ cluster")), data = X2_subset,
#           main = paste(v, "by Cluster"),
#           xlab = "Cluster", ylab = v)
# }



# X2_subset <- X2[fil, ]  # only the rows used for clustering
# X2_subset$cluster <- factor(cluster_cut)
# 
# vars <- c(5:10)
# 
# par(mfrow = c(2, 2))
# for (v in vars) {
#   boxplot(as.formula(paste(v, "~ cluster")), data = X2_subset,
#           main = paste(v, "by Cluster"),
#           xlab = "Cluster", ylab = v)
# }



plot(hc,labels=F,hang=-1)
tmp <- identify(hc)

cluster <- rep(NA,nrow(X))

for(i in 1:length(tmp)){

  cluster[fil][tmp[[i]]] <- i

}

plot(factor(cluster),X2[,7])

y <- as.numeric(consolidated_result=='Graduate')

fit <- glm(y~1+factor(cluster),binomial)

summary(fit)

cbind(exp(summary(fit)$coefficients[-1,1]),

      exp(summary(fit)$coefficients[-1,1]+qnorm(0.025)*summary(fit)$coefficients[-1,2]),

      exp(summary(fit)$coefficients[-1,1]+qnorm(0.975)*summary(fit)$coefficients[-1,2]),

      summary(fit)$coefficients[-1,4])


###########
#to make barcharts from cluster LR
# Get predicted log-odds and convert to probabilities
log_odds <- coef(fit)
log_odds

# Calculate predicted probabilities for each cluster
# Cluster 1 (reference)
p1 <- plogis(log_odds["(Intercept)"])
# Cluster 2
p2 <- plogis(log_odds["(Intercept)"] + log_odds["factor(cluster)2"])
# Cluster 3
p3 <- plogis(log_odds["(Intercept)"] + log_odds["factor(cluster)3"])
# Cluster 4
p4 <- plogis(log_odds["(Intercept)"] + log_odds["factor(cluster)4"])

# Combine into a data frame for plotting
prob_df <- data.frame(
  Cluster = factor(1:4),
  Probability = c(p1, p2, p3, p4)
)

# Plot with ggplot2 (if installed)
library(ggplot2)
ggplot(prob_df, aes(x = Cluster, y = Probability, fill = Cluster)) +
  geom_col() +
  ylim(0,1) +
  geom_text(aes(label = round(Probability, 3)), vjust = -0.5) +
  labs(
    title = "Predicted Probability of Graduation by Cluster",
    y = "Probability",
    x = "Cluster"
  ) +
  theme_minimal()


################################################################################################################################



X <- dta[,c(9:15,17:31)]

fil <- rowSums(is.na(X))==0&!is.na(consolidated_result)

X2 <- X[fil,]

y <- as.numeric(consolidated_result[fil]=='Graduate')



nrow(X2)*.6

set.seed(267)

train_indices <- sample(seq_len(nrow(X2)), size = floor(0.6 * nrow(X2)))
a <- seq_len(nrow(X2)) %in% train_indices

#a <- c(1:nrow(X2))%in%sample(c(1:nrow(X2)))[1:2383]

X2.a <- as.matrix(X2[a,])

X2.b <- X2[!a,]

y.a <- y[a]

y.b <- y[!a]



#perform k-fold cross-validation to find optimal lambda value

cv_model <- cv.glmnet(X2.a, y.a, alpha = 1, family=binomial)



#find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
# 
# best_lambda
# 
# 
# 
best_model <- glmnet(X2.a, y.a, alpha = 1, lambda = best_lambda, family=binomial)
# 
coef(best_model)
# 
x <- as.numeric(as.matrix(X2.b)%*%as.matrix(coef(best_model)[-1]))
# 
ROC <- roc(y.b,x)
# 
plot(ROC)
# 
auc(ROC)
# # 
# 
x <- as.numeric(as.matrix(X2.a)%*%as.matrix(coef(best_model)[-1]))
# 
ROC <- roc(y.a,x)
# 
plot(ROC)
# 
auc(ROC)
# 

################ Random forest ###############

# library(randomForest)
# rf_model <- randomForest(x = X2.a, y = as.factor(y.a), ntree = 500)
# rf_probs <- predict(rf_model, newdata = X2.b, type = "prob")[,2]
# roc(y.b, rf_probs)


############ LASSO ################

library(glmnet)
fit <- cv.glmnet(X2.a, y.a, family="binomial", alpha=1)
best_lambda <- fit$lambda.min
y_prob <- as.numeric(predict(fit, s=best_lambda, newx=as.matrix(X2.b), type="response"))
roc(y.b, y_prob)

##############################################

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
}

# Report mean AUC
mean_auc <- mean(auc_values)
print(mean_auc)

##########################################
################## 5 fold, 3 models per fold, LR, LASSO and Random Forest####
library(glmnet)
library(pROC)
library(randomForest)

set.seed(123)  # for reproducibility

# Assume X2 = predictor data, y = binary outcome (0/1)
# Prepare 5-fold CV
k <- 5
folds <- sample(rep(1:k, length.out = nrow(X2)))

# Store AUCs
auc_logistic <- numeric(k)
auc_lasso <- numeric(k)
auc_rf <- numeric(k)
auc_ensemble <- numeric(k)

for (i in 1:k) {
  cat("Fold", i, "\n")
  
  # Split data
  train_idx <- which(folds != i)
  test_idx  <- which(folds == i)
  
  X_train <- X2[train_idx, ]
  y_train <- y[train_idx]
  
  X_test  <- X2[test_idx, ]
  y_test  <- y[test_idx]
  
  # Logistic regression
  log_model <- glm(y_train ~ ., data = data.frame(y_train, X_train), family = "binomial")
  log_probs <- predict(log_model, newdata = X_test, type = "response")
  auc_logistic[i] <- auc(roc(y_test, log_probs))
  
  # LASSO
  lasso_model <- cv.glmnet(as.matrix(X_train), y_train, family = "binomial", alpha = 1)
  lasso_probs <- as.numeric(predict(lasso_model, s = lasso_model$lambda.min, newx = as.matrix(X_test), type = "response"))
  auc_lasso[i] <- auc(roc(y_test, lasso_probs))
  
  # Random Forest
  rf_model <- randomForest(x = X_train, y = as.factor(y_train), ntree = 500)
  rf_probs <- predict(rf_model, newdata = X_test, type = "prob")[, 2]
  auc_rf[i] <- auc(roc(y_test, rf_probs))
  
  # Ensemble
  ensemble_probs <- (log_probs + lasso_probs + rf_probs) / 3
  auc_ensemble[i] <- auc(roc(y_test, ensemble_probs))
}

# Report average AUCs
cat("\nMean AUCs over", k, "folds:\n")
cat(sprintf("Logistic:  %.4f\n", mean(auc_logistic)))
cat(sprintf("LASSO:     %.4f\n", mean(auc_lasso)))
cat(sprintf("RF:        %.4f\n", mean(auc_rf)))
cat(sprintf("Ensemble:  %.4f\n", mean(auc_ensemble)))

############ stratified 5-fold cross validation, ROC plots########### *Worse AUCs**
# 
# library(glmnet)
# library(pROC)
# library(randomForest)
# library(caret)
# library(ggplot2)
# 
# set.seed(123)
# 
# # Your predictors (X2) and binary outcome (y)
# 
# # Create stratified folds
# folds <- createFolds(y, k = 5, list = TRUE, returnTrain = FALSE)
# 
# # Initialize vectors to store AUCs
# auc_logistic <- numeric(5)
# auc_lasso <- numeric(5)
# auc_rf <- numeric(5)
# auc_ensemble <- numeric(5)
# 
# roc_data <- list()
# 
# for (i in 1:5) {
#   cat("Fold", i, "\n")
#   
#   test_idx <- folds[[i]]
#   train_idx <- setdiff(1:nrow(X2), test_idx)
#   
#   X_train <- X2[train_idx, ]
#   y_train <- y[train_idx]
#   X_test  <- X2[test_idx, ]
#   y_test  <- y[test_idx]
#   
#   # Logistic Regression
#   log_model <- glm(y_train ~ ., data = data.frame(y_train, X_train), family = "binomial")
#   log_probs <- predict(log_model, newdata = X_test, type = "response")
#   roc_log <- roc(y_test, log_probs)
#   auc_logistic[i] <- auc(roc_log)
#   
#   # LASSO
#   lasso_model <- cv.glmnet(as.matrix(X_train), y_train, family = "binomial", alpha = 1)
#   lasso_probs <- as.numeric(predict(lasso_model, s = lasso_model$lambda.min, newx = as.matrix(X_test), type = "response"))
#   roc_lasso <- roc(y_test, lasso_probs)
#   auc_lasso[i] <- auc(roc_lasso)
#   
#   # Random Forest
#   rf_model <- randomForest(x = X_train, y = as.factor(y_train), ntree = 500)
#   rf_probs <- predict(rf_model, newdata = X_test, type = "prob")[, 2]
#   roc_rf <- roc(y_test, rf_probs)
#   auc_rf[i] <- auc(roc_rf)
#   
#   # Ensemble (average probs)
#   ensemble_probs <- (log_probs + lasso_probs + rf_probs) / 3
#   roc_ensemble <- roc(y_test, ensemble_probs)
#   auc_ensemble[i] <- auc(roc_ensemble)
#   
#   # Collect ROC curve points in long format
#   roc_data[[i]] <- rbind(
#     data.frame(FPR = 1 - roc_log$specificities, TPR = roc_log$sensitivities, Model = "Logistic", Fold = paste0("Fold ", i)),
#     data.frame(FPR = 1 - roc_lasso$specificities, TPR = roc_lasso$sensitivities, Model = "LASSO", Fold = paste0("Fold ", i)),
#     data.frame(FPR = 1 - roc_rf$specificities, TPR = roc_rf$sensitivities, Model = "RandomForest", Fold = paste0("Fold ", i)),
#     data.frame(FPR = 1 - roc_ensemble$specificities, TPR = roc_ensemble$sensitivities, Model = "Ensemble", Fold = paste0("Fold ", i))
#   )
# }
# 
# # Combine ROC data for plotting
# roc_df <- do.call(rbind, roc_data)
# 
# # Plot ROC curves by fold and model
# ggplot(roc_df, aes(x = FPR, y = TPR, color = Model)) +
#   geom_line(size = 1) +
#   facet_wrap(~ Fold) +
#   labs(title = "ROC Curves by Fold", x = "1 - Specificity (FPR)", y = "Sensitivity (TPR)") +
#   theme_minimal() +
#   theme(legend.position = "bottom")
# 
# # Save AUC results to CSV
# auc_df <- data.frame(
#   Fold = 1:5,
#   Logistic = auc_logistic,
#   LASSO = auc_lasso,
#   RandomForest = auc_rf,
#   Ensemble = auc_ensemble
# )
# write.csv(auc_df, "auc_results.csv", row.names = FALSE)
# 
# # Print mean AUCs
# cat("\nMean AUCs over 5 stratified folds:\n")
# cat(sprintf("Logistic:  %.4f\n", mean(auc_logistic)))
# cat(sprintf("LASSO:     %.4f\n", mean(auc_lasso)))
# cat(sprintf("RandomForest: %.4f\n", mean(auc_rf)))
# cat(sprintf("Ensemble:  %.4f\n", mean(auc_ensemble)))
# 
