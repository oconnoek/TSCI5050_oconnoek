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

fil <- rowSums(is.na(X))==0&!is.na(elim_washback.x)

X2 <- X[fil,]

y <- as.numeric(elim_washback.x[fil]=='Graduate')



nrow(X2)*.6

set.seed(267)

a <- c(1:nrow(X2))%in%sample(c(1:nrow(X2)))[1:2383]

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
# 
# 
# 
# 
# 
x <- as.numeric(as.matrix(X2.a)%*%as.matrix(coef(best_model)[-1]))
# 
ROC <- roc(y.a,x)
# 
plot(ROC)
# 
auc(ROC)
# 

