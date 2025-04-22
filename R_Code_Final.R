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

#seeing the data after creating the dummy varibales for economy development and Region columns
head(data)




