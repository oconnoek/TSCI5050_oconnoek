rm(list = ls())

library(pROC)
library(glmnet)
library(dplyr)
library(readxl)

dta <- read_excel("Data/Erin_SWCC_Retro_TEST HCLUST.xlsx")

attach(dta)

#############################################################################

X <- dta[,c(5:9,25:27)]

X2 <- scale(X)

fil <- rowSums(is.na(X))==0

#HClustering
hc <- hclust(dist(X2[fil,],'euclidean'),'ward.D2')

#plot as dendrogram
plot(hc,labels=F,hang=-1)

cluster_cut <- cutree(hc, k =5)
X2_subset <- as.data.frame(X2[fil, ])
X2_subset$cluster <- factor(cluster_cut)

summary(X2_subset$cluster)
aggregate(. ~ cluster, data = X2_subset, FUN = mean)

##cluster profiles

#invert columns that are better to be lower: swim, run, age:
# Generate cluster profiles
cluster_means <- aggregate(. ~ cluster, data = X2_subset, FUN = mean)

# Invert variables where lower is better
vars_to_invert <- c(
"swim_seconds",
"run_seconds",
"age_calc")

# Invert selected columns
cluster_means[vars_to_invert] <- -cluster_means[vars_to_invert]

# Rename to indicate reversal
colnames(cluster_means)[colnames(cluster_means) %in% vars_to_invert] <-
  paste0(colnames(cluster_means)[colnames(cluster_means) %in% vars_to_invert], "_reversed")

# View output
print(cluster_means)

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


##################

#heatmap

heatmap(scale(as.matrix(X2_subset[, c(1:8)])), Rowv = as.dendrogram(hc), Colv = NA)



# Step 1: Subset and convert to data frame
X2_subset <- as.data.frame(X2[fil, ])

# Step 2: Add cluster labels
X2_subset$cluster <- factor(cluster_cut)

# Step 3: Define variables to plot
vars <- c("swim_seconds", "run_seconds", "pushups", "pullups")

# Step 4: Plot boxplots
par(mfrow = c(2, 2))  # layout for 4 plots
for (v in vars) {
  boxplot(as.formula(paste(v, "~ cluster")), data = X2_subset,
          main = paste(v, "by Cluster"),
          xlab = "Cluster", ylab = v)
}



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

plot(factor(cluster),X2[,3])

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
# Cluster 5
p5 <- plogis(log_odds["(Intercept)"] + log_odds["factor(cluster)5"])

# Combine into a data frame for plotting
prob_df <- data.frame(
  Cluster = factor(1:5),
  Probability = c(p1, p2, p3, p4, p5)
)

# Plot with ggplot2 (if installed)
library(ggplot2)
ggplot(prob_df, aes(x = Cluster, y = Probability, fill = Cluster)) +
  geom_col() +
  ylim(0,1) +
  geom_text(aes(label = round(Probability, 3)), size = 5, vjust = -1) +
  labs(
    title = "Predicted Probability of Graduation by Cluster",
    y = "Probability",
    x = "Cluster",
      ) +
  #theme_minimal()
theme(
  axis.title = element_text(size = 22),
  axis.text = element_text(size = 18),
  legend.title = element_text(size = 18),
  legend.text = element_text(size = 16),
  plot.title = element_text(size = 22, face = "bold")
)


################################################################################################################################

