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

# View the names of standardized columns
print(cols_to_standardize)
summary(train_standardized)

# To see whether the Target Variable is Right Skewed or Left Skewed
ggplot(train, aes(x = Life_expectancy)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  labs(title = "Density Plot of Life Expectancy", x = "Life Expectancy", y = "Density")
















