##manova attempt

library(dplyr)
library(readxl)

dta <- read_excel("Data/Erin_SWCC_Retro_ANOVA.xlsx")

attach(dta)

X <- dta[,c(10:36)]

#X2 <- scale(X)

fil <- rowSums(is.na(X))==0
X_complete <- X[fil, ]

skim(X_complete)
describe(X_complete)

#inspecting and cleaning data
str(dta)
dta$consolidated_result <- as.factor(dta$consolidated_result)
colSums(is.na(dta[, 10:36]))  # Adjust 10:36 if needed
dta_clean <- na.omit(dta[, c(10:36, which(names(dta) == "consolidated_result"))])

#Graduate or not
# Recode consolidated_result to just two categories: Graduate and Elim
dta$consolidated_result <- ifelse(dta$consolidated_result == "Graduate", 
                                  "Graduate", 
                                  "Elim")

# Convert to factor to ensure proper levels
dta$consolidated_result <- factor(dta$consolidated_result, levels = c("Graduate", "Elim"))

# Check the distribution after recoding
table(dta$consolidated_result)


#checking levels of grouping: 
table(dta$consolidated_result)


dependent_vars <- c(10:36)
manova_model <- manova(cbind(c(10:36)
) ~ consolidated_result, data = X_complete)


#fit <- manova(as.matrix(dta_clean[, 1:27]) ~ dta_clean$consolidated_result)
#summary(fit, test = "Wilks")


# Step 1: Subset your data to include only the relevant variables
manova_data <- dta %>%
  select(consolidated_result,
         c(10:36),
        )  # example: 9 fitness variables × 3 time points = 27 columns

# Step 2: Convert course_outcome to factor
manova_data$consolidated_result <- as.factor(manova_data$consolidated_result)

# Step 3: Fit MANOVA
fit <- manova(cbind(c(10:36)) ~ consolidated_result,
              data = manova_data)

# Step 4: Summary of MANOVA with Wilks’ Lambda
summary(fit, test = "Wilks")
