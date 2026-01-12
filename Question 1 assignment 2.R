
library(readr)
library(ggplot2)
library(dplyr)
library(summarytools)
install.packages(c("readr", "ggplot2", "dplyr", "summarytools"))
west_roxbury <- read_csv("WestRoxbury.csv")
data <- read_csv("C:/Users/Vaishnavi/Downloads/WestRoxbury.csv")
head(data)
summary(data)
recent_properties <- data %>% filter(`YR BUILT` > 2000)
summary(west_roxbury)
# Load the WestRoxbury Dataset
west_roxbury <- read_csv("C:/Users/Vaishnavi/Downloads/WestRoxbury.csv")
# Check if the dataset is loaded properly
head(west_roxbury)
summary(west_roxbury)
# Check for missing data
colSums(is.na(west_roxbury))
# Boxplot for each numeric column
numeric_columns <- sapply(west_roxbury, is.numeric)
boxplot(west_roxbury[, numeric_columns], main="Boxplot for numeric columns", las=2)
# View the structure of the dataset
str(west_roxbury)
# Load the BostonHousing Dataset
boston_housing <- read_csv("C:/Users/Vaishnavi/Downloads/BostonHousing (1).csv")
# View the first few rows
head(boston_housing)
# Get a summary of the dataset
summary(boston_housing)
# Check for missing data
colSums(is.na(boston_housing))
# Boxplot for each numeric column
numeric_columns <- sapply(boston_housing, is.numeric)
boxplot(boston_housing[, numeric_columns], main="Boxplot for numeric columns", las=2)
# Histogram for each numeric column
par(mfrow=c(2, 2)) # To plot multiple histograms in one window
for(col in names(boston_housing)[numeric_columns]) {
  hist(boston_housing[[col]], main=paste("Histogram of", col), xlab=col)
}
# Scatter plot matrix
pairs(boston_housing[, numeric_columns], main="Scatterplot Matrix")
# Load the BostonHousing Dataset
boston_housing <- read.csv("C:/Users/Vaishnavi/Downloads/BostonHousing (1).csv")
# View the first few rows of the dataset
head(boston_housing)
# Get a summary of the dataset
summary(boston_housing)
# Build a linear regression model
lm_model <- lm(medv ~ ., data = boston_housing)
# List column names of the dataset
names(boston_housing)
# Build a linear regression model
lm_model <- lm(MEDV ~ ., data = boston_housing)
# View summary of the model
summary(lm_model)
# View coefficients of the model
coefficients <- coef(lm_model)
coefficients
# View R-squared and Adjusted R-squared
rsquared <- summary(lm_model)$r.squared
adj_rsquared <- summary(lm_model)$adj.r.squared
cat("R-squared:", rsquared, "\n")
cat("Adjusted R-squared:", adj_rsquared, "\n")
# Plot residuals vs. fitted values
plot(lm_model, which = 1)
# Check normality of residuals
shapiro.test(resid(lm_model))
# Make predictions on the training data
predictions <- predict(lm_model)
# Assuming you have the actual target variable values in a vector called 'actual_values'
rmse <- sqrt(mean((actual_values - predictions)^2))
mae <- mean(abs(actual_values - predictions))
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
# Step 1: Assess Model Performance
rmse <- sqrt(mean((actual_values - predictions)^2))
mae <- mean(abs(actual_values - predictions))
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
# Extract actual target variable values from the dataset
actual_values <- boston_housing$MEDV 
# Calculate RMSE and MAE
rmse <- sqrt(mean((actual_values - predictions)^2))
mae <- mean(abs(actual_values - predictions))
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
# Plot predictions vs. actual values
plot(actual_values, predictions, xlab = "Actual Values", ylab = "Predictions", main = "Predictions vs. Actual Values")
abline(0, 1, col = "red")  # Add a line of equality
# Implement k-fold cross-validation
library(caret)
set.seed(123)  # Set seed for reproducibility
train_control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation
cv_model <- train(MEDV ~ ., data = boston_housing, method = "lm", trControl = train_control)
cv_model$results 
# Implement k-fold cross-validation
library(caret)
set.seed(123)  # Set seed for reproducibility
train_control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation
cv_model <- train(MEDV ~ ., data = boston_housing, method = "lm", trControl = train_control)
cv_model$results  # View cross-validation results
install.packages("caret")
library(caret)
set.seed(123)  # Set seed for reproducibility
train_control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation
cv_model <- train(MEDV ~ ., data = boston_housing, method = "lm", trControl = train_control)
cv_model$results  # View cross-validation results
# Fine-tune the linear regression model (e.g., adjust hyperparameters)
# Example: Change the method to ridge regression
ridge_model <- train(MEDV ~ ., data = boston_housing, method = "ridge", trControl = train_control)
ridge_model$results  # View results
# Compare performance of different models
# Example: Compare linear regression and ridge regression


# Fine-tune the linear regression model or other models as needed
# Example: Change hyperparameters or try different algorithms
# (This step is optional depending on your assignment requirements)

# Step 2: Model Comparison
compare_models <- resamples(list(Linear_Regression = cv_model, Ridge_Regression = ridge_model))
summary(compare_models)
# Compare performance of different models
# Example: Compare linear regression and ridge regression

# Step 1: Fine-Tuning the Model (Optional)
# Fine-tune the linear regression model or other models as needed
# Example: Change hyperparameters or try different algorithms
# (This step is optional depending on your assignment requirements)

# Step 2: Model Comparison
compare_models <- resamples(list(Linear_Regression = cv_model, Ridge_Regression = ridge_model))
summary(compare_models)
compare_models <- resamples(list(Linear_Regression = cv_model, Ridge_Regression = ridge_model))
summary(compare_models)
compare_models <- resamples(list(Linear_Regression = cv_model, Ridge_Regression = ridge_model))
compare_models <- resamples(list(Linear_Regression = cv_model, Ridge_Regression = ridge_model))
comparison_summary <- summary(compare_models)
install.packages("corrplot")  
install.packages("rpart")
rice_data <- read.csv("C:/Users/Vaishnavi/Downloads/Rice_Cammeo_Osmancik.csv")
head(rice_data)
install.packages("corrplot")
install.packages("rpart")




















