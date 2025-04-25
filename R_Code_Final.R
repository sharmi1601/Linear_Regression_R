#load the libraries

library(tidyverse)
library(caret)
library(car)
library(MASS)
library(lmtest)
library(psych)
library(ggplot2)
library(fastDummies)
library(dplyr)
library(splitTools)
library(patchwork)
library(car)
library(knitr)
library(caret)


# Read the data
data <- read.csv("C:/Users/sharm/OneDrive/Desktop/MS/Semester - 2/Linear Regression and Time Series/Final_Project_Sem-2/Life-Expectancy-Data.csv")

glimpse(data)
summary(data)

#cleaning the dataset
anyNA(data)
any(is.na(data))
colSums(is.na(data))

#As there are no null values we go for dealing with categorical columns.
sapply(data,class)

#Now check whether the Year column is linea or Non-linear to know wther it should be categorical column or Numeric column
ggplot(data, aes(Year, Life_expectancy)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
# As there is approximate linear trend in between Year and Life_expectancy we can leave Year column as Numeric

# Create dummy variables for 'Region', dropping the first category to avoid multicollinearity
data <- dummy_cols(data, select_columns = "Region", remove_first_dummy = TRUE)

# Remove the original 'Region' and 'Country' column
data <- data %>% dplyr::select(-Region)
data <- data %>% dplyr::select(-Country)

#seeing the data after creating the dummy variables for economy development and Region columns
head(data)

summary(data)
ncol(data)

#split data in Train, Validation and test
set.seed(123)
inds <- partition(
  y = data$Life_expectancy,  # Response variable (atomic vector)
  p = c(train = 0.6, valid = 0.2, test = 0.2)
)
train <- data[inds$train, ]
valid <- data[inds$valid, ]
test  <- data[inds$test, ]

ncol(train)
summary(train)

#Now plotting all the variables against the target variables. which means doing EDA.
par(mfrow = c(4, 4), mar = c(4, 4, 2, 1))  # 4x4 grid, adjust as needed

y_var <- "Life_expectancy"
predictor_vars <- setdiff(names(train), y_var)

# Plot the first 16 variables
for (var in predictor_vars[1:16]) {
  plot(train[[var]], train[[y_var]],
       xlab = var, ylab = y_var,
       main = paste(y_var, "vs", var),
       pch = 20, col = "black")
  abline(lm(train[[y_var]] ~ train[[var]]), col = "red")
}

# Standardization of Numeric columns without standardizing Target Variable, Year Column and Dummy columns.
# Specify columns to exclude from standardization
exclude_cols <- c("Life_expectancy", "Year")

# Specify prefixes that identify dummy variables
dummy_prefixes <- c("Region_", "Economy_status_") 

# Function to check if a column is a dummy variable based on its name
is_dummy <- function(col_name) {
  any(startsWith(col_name, dummy_prefixes))
}

# Get names of columns to standardize
cols_to_standardize <- names(train)[
  sapply(names(train), function(col) {
    is.numeric(train[[col]]) &&             # is numeric
      !(col %in% exclude_cols) &&           # not in exclude list
      !is_dummy(col)                        # not a dummy
  })
]

# Standardize only the selected columns
train_standardized <- train
train_standardized[cols_to_standardize] <- scale(train[cols_to_standardize])


# Calculate means and standard deviations from the training data
# (We need to store these to apply to validation and test sets)
train_means <- colMeans(train[cols_to_standardize])
train_sds <- apply(train[cols_to_standardize], 2, sd)

# Standardize validation set using training means and sds
valid_standardized <- valid
for (col in cols_to_standardize) {
  valid_standardized[[col]] <- (valid[[col]] - train_means[col]) / train_sds[col]
}
summary(valid_standardized)

# Standardize test set using training means and sds
test_standardized <- test
for (col in cols_to_standardize) {
  test_standardized[[col]] <- (test[[col]] - train_means[col]) / train_sds[col]
}
summary(test_standardized)

# View the names of standardized columns
print(cols_to_standardize)
summary(train_standardized)

# To see whether the Target Variable is Right Skewed or Left Skewed
ggplot(train, aes(x = Life_expectancy)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  labs(title = "Density Plot of Life Expectancy", x = "Life Expectancy", y = "Density")

ncol(train_standardized)

# Creating Interaction Terms and Quadratic Terms
# 1. Basic Polynomial (Quadratic) Terms
train_standardized$GDP_per_capita_sq <- train_standardized$GDP_per_capita^2
train_standardized$BMI_sq <- train_standardized$BMI^2
train_standardized$Alcohol_consumption_sq <- train_standardized$Alcohol_consumption^2
train_standardized$Incidents_HIV_sq <- train_standardized$Incidents_HIV^2
train_standardized$Thinness_five_nine_years_sq <- train_standardized$Thinness_five_nine_years^2
train_standardized$Adult_mortality_sq <- train_standardized$Adult_mortality^2
train_standardized$Schooling_sq <- train_standardized$Schooling^2

# 2. Simple Interaction Terms
train_standardized$GDP_Schooling <- train_standardized$GDP_per_capita * train_standardized$Schooling
train_standardized$GDP_Adult_mortality <- train_standardized$GDP_per_capita * train_standardized$Adult_mortality
train_standardized$BMI_Developed <- train_standardized$BMI * train_standardized$Economy_status_Developed
train_standardized$HIV_GDP <- train_standardized$Incidents_HIV * train_standardized$GDP_per_capita
train_standardized$Hepatitis_Polio <- train_standardized$Hepatitis_B * train_standardized$Polio
train_standardized$Year_Adult_mortality <- train_standardized$Year * train_standardized$Adult_mortality
train_standardized$Year_GDP <- train_standardized$Year * train_standardized$GDP_per_capita
train_standardized$Thinness_Schooling <- train_standardized$Thinness_five_nine_years * train_standardized$Schooling
train_standardized$Adult_mortality_Developed <- train_standardized$Adult_mortality * train_standardized$Economy_status_Developed
train_standardized$GDP_Asia <- train_standardized$GDP_per_capita * train_standardized$Region_Asia

# 3. Complex Interaction Terms
train_standardized$GDP_sq_Developed <- train_standardized$GDP_per_capita_sq * train_standardized$Economy_status_Developed
train_standardized$BMI_sq_Asia <- train_standardized$BMI_sq * train_standardized$Region_Asia
train_standardized$Adult_mortality_sq_Schooling <- train_standardized$Adult_mortality_sq * train_standardized$Schooling
train_standardized$Alcohol_BMI_sq <- train_standardized$Alcohol_consumption * train_standardized$BMI_sq
train_standardized$Schooling_sq_GDP <- train_standardized$Schooling_sq * train_standardized$GDP_per_capita

#Total number of variables after Feature Generation
ncol(train_standardized)
summary(train_standardized)

#Now doing Feature Selection Using Forward, Backward and Step-Wise Selection Methods
# Create full and null models
full_model <- lm(Life_expectancy ~ ., data = train_standardized)
null_model <- lm(Life_expectancy ~ 1, data = train_standardized)

# 1. Forward Selection
forward_model <- step(null_model,
                      scope = list(lower = null_model, upper = full_model),
                      direction = "forward",
                      trace = 2)  # trace=2 shows detailed output

# 2. Backward Elimination
backward_model <- step(full_model,
                       direction = "backward",
                       trace = 2)

# 3. Step-wise Selection
stepwise_model <- step(null_model,
                       scope = list(lower = null_model, upper = full_model),
                       direction = "both",
                       trace = 2)

# 4. Manual Feature Selection
manual_feature_selection <- function(data, target_var) {
  # Initial setup
  predictors <- names(data)[names(data) != target_var]
  
  # Function to wrap problematic variable names
  wrap_vars <- function(vars) {
    ifelse(grepl("[^\\w]", vars, perl = TRUE), paste0("`", vars, "`"), vars)
  }
  
  # Step 0: Remove aliased variables first
  initial_formula <- as.formula(paste(target_var, "~", paste(wrap_vars(predictors), collapse = "+")))
  initial_model <- lm(initial_formula, data = data)
  
  # Check for aliased coefficients
  alias_info <- alias(initial_model)
  if(!is.null(alias_info$Complete)) {
    aliased_vars <- rownames(alias_info$Complete)
    predictors <- setdiff(predictors, aliased_vars)
    cat("Removed aliased variables:", paste(aliased_vars, collapse = ", "), "\n\n")
  }
  
  # Remove high VIF predictors iteratively
  repeat {
    safe_predictors <- wrap_vars(predictors)
    formula <- as.formula(paste(target_var, "~", paste(safe_predictors, collapse = "+")))
    model <- lm(formula, data = data)
    
    # Check for new aliases after removal
    if(any(is.na(coef(model)))) {
      new_aliases <- names(which(is.na(coef(model))))[-1]  # Exclude intercept
      predictors <- setdiff(predictors, new_aliases)
      cat("Removed newly aliased variables:", paste(new_aliases, collapse = ", "), "\n")
      next
    }
    
    vif_scores <- car::vif(model)
    if(all(vif_scores <= 10)) break
    
    worst_vif <- names(which.max(vif_scores))
    predictors <- predictors[predictors != worst_vif]
    cat("Removed", worst_vif, "(VIF =", round(max(vif_scores), 1), ")\n")
  }
  
  # Remove non-significant predictors iteratively
  repeat {
    safe_predictors <- wrap_vars(predictors)
    formula <- as.formula(paste(target_var, "~", paste(safe_predictors, collapse = "+")))
    model <- lm(formula, data = data)
    
    p_values <- summary(model)$coefficients[-1, 4]  # Exclude intercept
    if(all(p_values <= 0.05)) break
    
    worst_p <- names(which.max(p_values))
    predictors <- predictors[predictors != worst_p]
    cat("Removed", worst_p, "(p-value =", round(max(p_values), 4), ")\n")
  }
  
  return(list(
    final_model = model,
    remaining_predictors = predictors,
    vif_scores = car::vif(model),
    p_values = summary(model)$coefficients[-1, 4]
  ))
}

# Usage with your data
result <- manual_feature_selection(train_standardized, "Life_expectancy")
summary(result$final_model)


# Comparison of the all feature selection models
# Function to calculate metrics for comparison
get_model_stats <- function(model, model_name, train_data) {
  # Training metrics
  train_pred <- predict(model, newdata = train_data)
  train_rmse <- sqrt(mean((train_data$Life_expectancy - train_pred)^2))
  
  data.frame(
    Model = model_name,
    R2 = round(summary(model)$r.squared, 3),
    Adj_R2 = round(summary(model)$adj.r.squared, 3),
    Train_RMSE = round(train_rmse, 3),
    AIC = round(AIC(model), 1),
    BIC = round(BIC(model), 1),
    Predictors = length(coef(model)) - 1
  )
}

# Create comparison table
comparison_table <- rbind(
  get_model_stats(forward_model, "Forward", train_standardized),
  get_model_stats(backward_model, "Backward", train_standardized),
  get_model_stats(stepwise_model, "Stepwise", train_standardized),
  get_model_stats(result$final_model, "Manual", train_standardized)
)

# Display formatted results
kable(comparison_table, align = 'c', caption = "Model Comparison")

# Now Final Model Selection

# STEP 1: Select the best model from comparison
selected_model <- backward_model

# View initial model summary
summary(selected_model)

# STEP 2: Remove insignificant terms iteratively
refinement <- function(model) {
  continue <- TRUE
  
  while(continue) {
    model_summary <- summary(model)
    p_values <- model_summary$coefficients[-1, 4]  # Exclude intercept
    
    # Stop if all terms are significant
    if(all(p_values <= 0.05)) {
      continue <- FALSE
      cat("All terms are now significant at alpha = 0.05\n")
    } else {
      # Remove the least significant term
      worst_term <- rownames(model_summary$coefficients)[which.max(p_values) + 1]  # +1 to adjust for intercept
      
      cat("Removing", worst_term, "with p-value =", round(max(p_values), 4), "\n")
      
      # Update formula by removing the term
      formula_terms <- attr(terms(model), "term.labels")
      formula_terms <- formula_terms[formula_terms != worst_term]
      
      if(length(formula_terms) > 0) {
        new_formula <- paste("Life_expectancy ~", paste(formula_terms, collapse = " + "))
        model <- update(model, as.formula(new_formula))
      } else {
        cat("Warning: No terms left in model\n")
        break
      }
    }
  }
  return(model)
}

# Apply refinement
refined_model <- refinement(selected_model)
summary(refined_model)
length(coef(refined_model))


# STEP 3: Handle influential observations

infl <- influence.measures(refined_model)
# Calculate thresholds for high leverage and influence
n <- nrow(train_standardized)
p <- length(coef(refined_model))
leverage_threshold <- 2 * p / n
cook_threshold <- 4 / n

# Identify problematic observations
high_leverage <- which(hatvalues(refined_model) > leverage_threshold)
high_influence <- which(cooks.distance(refined_model) > cook_threshold)

cat("High leverage points:", length(high_leverage), "\n")
cat("High influence points:", length(high_influence), "\n")

# Combine both types of outliers
outliers <- unique(c(high_leverage, high_influence))
cat("Total outliers to remove:", length(outliers), "\n")

# STEP 4: Rebuild final model without outliers
# Create clean dataset
train_clean <- train_standardized[-outliers, ]
cat("Original observations:", n, "| After removing outliers:", nrow(train_clean), "\n")

# Rebuild the model on clean data
final_formula <- formula(refined_model)
final_model <- lm(final_formula, data = train_clean)

# View final model
summary(final_model)

# Get model diagnostics
final_metrics <- data.frame(
  R2 = summary(final_model)$r.squared,
  Adj_R2 = summary(final_model)$adj.r.squared,
  RMSE = sqrt(mean(residuals(final_model)^2)),
  AIC = AIC(final_model),
  BIC = BIC(final_model),
  Predictors = length(coef(final_model)) - 1,
  Observations = nrow(train_clean)
)

print(final_metrics)

# STEP 5: ANOVA test
anova_test <- anova(final_model)
print(anova_test)

# K-fold cross-validation on cleaned training data
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)
cv_model <- train(
  formula(final_model), 
  data = train_clean,
  method = "lm",
  trControl = train_control
)
# View results
print(cv_model$results)

ncol(train_clean)
nrow(train_clean)

# Get predictor names from final model (excluding intercept)
required_predictors <- names(coef(final_model))[-1]  # -1 removes intercept
cat("Required predictors (32):", required_predictors)

summary(valid_standardized)
ncol(valid_standardized)

# 1. Add required polynomial/interaction terms
valid_standardized$GDP_per_capita_sq <- valid_standardized$GDP_per_capita^2
valid_standardized$BMI_sq <- valid_standardized$BMI^2
valid_standardized$Thinness_five_nine_years_sq <- valid_standardized$Thinness_five_nine_years^2
valid_standardized$Adult_mortality_sq <- valid_standardized$Adult_mortality^2
valid_standardized$Schooling_sq <- valid_standardized$Schooling^2

valid_standardized$GDP_Schooling <- valid_standardized$GDP_per_capita * valid_standardized$Schooling
valid_standardized$GDP_Adult_mortality <- valid_standardized$GDP_per_capita * valid_standardized$Adult_mortality
valid_standardized$BMI_Developed <- valid_standardized$BMI * valid_standardized$Economy_status_Developed
valid_standardized$HIV_GDP <- valid_standardized$Incidents_HIV * valid_standardized$GDP_per_capita
valid_standardized$Year_Adult_mortality <- valid_standardized$Year * valid_standardized$Adult_mortality
valid_standardized$Year_GDP <- valid_standardized$Year * valid_standardized$GDP_per_capita
valid_standardized$Adult_mortality_Developed <- valid_standardized$Adult_mortality * valid_standardized$Economy_status_Developed
valid_standardized$Adult_mortality_sq_Schooling <- valid_standardized$Adult_mortality_sq * valid_standardized$Schooling
valid_standardized$Schooling_sq_GDP <- valid_standardized$Schooling_sq * valid_standardized$GDP_per_capita

# 2. Harmonize column names (DO THIS ONCE)
required_predictors <- gsub("`", "", names(coef(final_model))[-1])
names(valid_standardized) <- gsub("`", "", names(valid_standardized))

# 3. Subset validation data (DO THIS ONCE)
valid_final <- valid_standardized[, c("Life_expectancy", required_predictors)]

ncol(valid_final)

# 4. Predict and calculate metrics (DO THIS ONCE)
valid_pred <- predict(final_model, newdata = valid_final)
valid_rmse <- sqrt(mean((valid_final$Life_expectancy - valid_pred)^2))
valid_r2 <- cor(valid_pred, valid_final$Life_expectancy)^2

cat("Validation RMSE:", round(valid_rmse, 4), "\n")
cat("Validation R-squared:", round(valid_r2, 4), "\n")



# --------------------------
# TEST DATA PREPARATION
# --------------------------

# A. Add polynomial terms (same as training/validation)
test_standardized <- test_standardized %>%
  mutate(
    GDP_per_capita_sq = GDP_per_capita^2,
    BMI_sq = BMI^2,
    Thinness_five_nine_years_sq = Thinness_five_nine_years^2,
    Adult_mortality_sq = Adult_mortality^2,
    Schooling_sq = Schooling^2
  )

# B. Add interaction terms (same as training/validation)
test_standardized <- test_standardized %>%
  mutate(
    GDP_Schooling = GDP_per_capita * Schooling,
    GDP_Adult_mortality = GDP_per_capita * Adult_mortality,
    BMI_Developed = BMI * Economy_status_Developed,
    HIV_GDP = Incidents_HIV * GDP_per_capita,
    Year_Adult_mortality = Year * Adult_mortality,
    Year_GDP = Year * GDP_per_capita,
    Adult_mortality_Developed = Adult_mortality * Economy_status_Developed,
    Adult_mortality_sq_Schooling = Adult_mortality_sq * Schooling,
    Schooling_sq_GDP = Schooling_sq * GDP_per_capita
  )

# C. Harmonize column names (remove backticks)
names(test_standardized) <- gsub("`", "", names(test_standardized))

# D. Subset to final model's required predictors
required_predictors <- gsub("`", "", names(coef(final_model))[-1])
test_final <- test_standardized[, c("Life_expectancy", required_predictors)]

# E. Verify alignment
cat("Test dimensions:", dim(test_final), "\n")
cat("Missing predictors:", setdiff(required_predictors, names(test_final)), "\n")

# Predict
test_pred <- predict(final_model, newdata = test_final)

# Metrics
test_rmse <- sqrt(mean((test_final$Life_expectancy - test_pred)^2))
test_r2 <- cor(test_pred, test_final$Life_expectancy)^2

cat("\nFinal Test Performance:\n")
cat("RMSE:", round(test_rmse, 4), "\n")
cat("R-squared:", round(test_r2, 4), "\n")


vif(final_model)  # Values >5-10 indicate problematic multicollinearity
summary(final_model)









# Check for aliased coefficients
alias_info <- alias(final_model)
if (!is.null(alias_info$Complete)) {
  aliased_vars <- rownames(alias_info$Complete)
  cat("Aliased variables detected:", aliased_vars, "\n")
  
  # Remove aliased variables and refit model
  predictors <- setdiff(names(coef(final_model))[-1], aliased_vars)
  formula_str <- paste("Life_expectancy ~", paste(predictors, collapse = "+"))
  final_model_clean <- lm(formula_str, data = train_clean)
} else {
  cat("No aliased variables detected.\n")
}
# View cleaned model summary
summary(final_model_clean)

# Calculate VIF for the cleaned model
library(car)
vif(final_model_clean)

# Function to iteratively remove variables with VIF > threshold
reduce_vif <- function(model, threshold = 10) {
  vif_values <- car::vif(model)
  while (any(vif_values > threshold)) {
    max_vif_var <- names(which.max(vif_values))
    cat("Removing variable with high VIF:", max_vif_var, "VIF:", max(vif_values), "\n")
    predictors <- setdiff(names(coef(model))[-1], max_vif_var)
    formula_str <- paste("Life_expectancy ~", paste(predictors, collapse = "+"))
    model <- lm(formula_str, data = train_clean)
    vif_values <- car::vif(model)
  }
  return(model)
}

# Apply to cleaned model
final_model_reduced <- reduce_vif(final_model_clean)
summary(final_model_reduced)
vif(final_model_reduced)



# Polynomial terms
test_standardized$GDP_per_capita_sq <- test_standardized$GDP_per_capita^2
test_standardized$BMI_sq <- test_standardized$BMI^2
test_standardized$Thinness_five_nine_years_sq <- test_standardized$Thinness_five_nine_years^2
test_standardized$Adult_mortality_sq <- test_standardized$Adult_mortality^2
test_standardized$Schooling_sq <- test_standardized$Schooling^2

# Interaction terms
test_standardized$BMI_Developed <- test_standardized$BMI * test_standardized$Economy_status_Developed
test_standardized$Year_Adult_mortality <- test_standardized$Year * test_standardized$Adult_mortality
test_standardized$Adult_mortality_Developed <- test_standardized$Adult_mortality * test_standardized$Economy_status_Developed
test_standardized$Adult_mortality_sq_Schooling <- test_standardized$Adult_mortality_sq * test_standardized$Schooling
test_standardized$Schooling_sq_GDP <- test_standardized$Schooling_sq * test_standardized$GDP_per_capita


required_predictors <- names(coef(final_model_reduced))[-1]  # Exclude intercept
test_final <- test_standardized[, c("Life_expectancy", required_predictors)]

ncol(test_standardized)
# Get predictors from the reduced model
required_predictors <- names(coef(final_model_reduced))[-1]

# Compare with test dataset columns
missing_cols <- setdiff(required_predictors, names(test_standardized))
cat("Missing columns in test data:", missing_cols, "\n")


# --------------------------
# STEP 1: List and Compare Columns
# --------------------------

# Get column names from test data
test_cols <- names(test_standardized)
cat("Test dataset columns (", length(test_cols), "):\n", paste(test_cols, collapse = ", "), "\n\n")

# Get predictor names from final model (excluding intercept)
model_cols <- names(coef(final_model_reduced))[-1]
cat("Model predictors (", length(model_cols), "):\n", paste(model_cols, collapse = ", "), "\n\n")

# --------------------------
# STEP 2: Identify Mismatches
# --------------------------

# Find missing columns in test data
missing_cols <- setdiff(model_cols, test_cols)
cat("Missing columns in test data:\n", paste(missing_cols, collapse = ", "), "\n\n")

# Find extra columns in test data
extra_cols <- setdiff(test_cols, c("Life_expectancy", model_cols))
cat("Extra columns in test data:\n", paste(extra_cols, collapse = ", "), "\n\n")

# --------------------------
# STEP 3: Fix Column Issues
# --------------------------

# Add missing columns (set to 0 for dummy variables)
for(col in missing_cols) {
  if(grepl("Region", col)) {
    # Handle region dummy variables
    test_standardized[[col]] <- 0
    cat("Added region dummy:", col, "\n")
  } else {
    # Handle other terms (polynomials/interactions)
    test_standardized[[col]] <- test_standardized[[
      gsub("_sq|_GDP|_Schooling|_Developed|_sq_Schooling", "", col)
    ]]^ifelse(grepl("_sq", col), 2, 1)
    cat("Added derived term:", col, "\n")
  }
}

# Remove extra columns
test_standardized <- test_standardized[, !(names(test_standardized) %in% extra_cols)]

# --------------------------
# STEP 4: Create Final Test Set
# --------------------------

# Verify target variable exists
if(!"Life_expectancy" %in% names(test_standardized)) {
  stop("Life_expectancy column missing in test data!")
}

# Create final test dataset
test_final <- test_standardized[, c("Life_expectancy", model_cols)]

# --------------------------
# STEP 5: Final Verification
# --------------------------

cat("\nFinal test dataset structure:\n")
str(test_final[, 1:5])  # Show first 5 columns for preview

cat("\nMissing columns after fix:", setdiff(model_cols, names(test_final)), "\n")
cat("Extra columns after fix:", setdiff(names(test_final), c("Life_expectancy", model_cols)), "\n")


ncol(test_final)


# Print column names in test data
cat("Test columns:", names(test_final), "\n")

# Check for the problematic region column
"Region_Central America and Caribbean" %in% names(test_final)

# Remove backticks from region columns (if present)
names(test_final) <- gsub("`", "", names(test_final))

# Add missing region columns (if absent)
missing_regions <- setdiff(
  c("Region_Central America and Caribbean", 
    "Region_European Union", 
    "Region_Rest of Europe", 
    "Region_South America"),
  names(test_final)
)

if (length(missing_regions) > 0) {
  test_final[missing_regions] <- 0  # Add zero-filled columns
  cat("Added missing regions:", missing_regions, "\n")
}
test_pred <- predict(final_model_reduced, newdata = test_final)

# Metrics
test_rmse <- sqrt(mean((test_final$Life_expectancy - test_pred)^2))
test_r2 <- cor(test_pred, test_final$Life_expectancy)^2

cat("\nFinal Test Performance:\n")
cat("RMSE:", round(test_rmse, 4), "\n")
cat("R-squared:", round(test_r2, 4), "\n")

ncol(test_final)

summary(final_model_reduced)
summary(test_final)

test_final$`Region_North America` <- NULL
summary(test_final)
ncol(test_final)
