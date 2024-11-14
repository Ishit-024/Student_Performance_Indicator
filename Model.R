# Install and load required libraries
install.packages(c("ggplot2", "tidyverse", "caret", "randomForest", "xgboost", "Metrics", "dplyr", "rpart", "glmnet", "adabag"))

# Load libraries
library(ggplot2)
library(tidyverse)
library(caret)
library(randomForest)
library(xgboost)
library(Metrics)
library(dplyr)
library(rpart)
library(glmnet)
library(adabag)
install.packages("Metrics")
install.packages("MLmetrics")  # alternative option if needed

library(Metrics)
library(MLmetrics)

# Load the data
df <- read.csv('C:/Users/Administrator/Downloads/stud.csv')

# Show the first 5 rows
head(df)

# Separate X (features) and y (target variable)
set.seed(123)  # Set seed for reproducibility
trainIndex <- createDataPartition(df$math_score, p = 0.8, list = FALSE)
train_data <- df[trainIndex, ]
test_data <- df[-trainIndex, ]

# Define target variable and features
y_train <- train_data$math_score
y_test <- test_data$math_score
X_train <- train_data[, -which(names(train_data) == "math_score")]
X_test <- test_data[, -which(names(test_data) == "math_score")]

# Standardize the features
preProcValues <- preProcess(X_train, method = c("center", "scale"))
X_train_scaled <- predict(preProcValues, X_train)
X_test_scaled <- predict(preProcValues, X_test)

# Convert scaled data back to data frames if necessary
X_train_scaled <- as.data.frame(X_train_scaled)
X_test_scaled <- as.data.frame(X_test_scaled)

# Define evaluation function
evaluate_model <- function(true, predicted) {
  mae <- MAE(true, predicted)  # Using MLmetrics::MAE function
  mse <- mean((true - predicted) ^ 2)
  rmse <- sqrt(mse)
  r2 <- if (sd(true) == 0 || sd(predicted) == 0) NA else cor(true, predicted)^2
  return(list(mae = mae, rmse = rmse, r2 = r2))
}

# Define models with their respective methods
# Ensure `lasso_grid` and `ridge_grid` have been set up if needed for tuning
lasso_grid <- expand.grid(alpha = 1, lambda = seq(0.01, 0.1, by = 0.01))
ridge_grid <- expand.grid(alpha = 0, lambda = seq(0.01, 0.1, by = 0.01))

models <- list(
  "Linear Regression" = lm(math_score ~ ., data = train_data),
  "Lasso" = train(math_score ~ ., data = train_data, method = "glmnet", tuneGrid = lasso_grid),
  "Ridge" = train(math_score ~ ., data = train_data, method = "glmnet", tuneGrid = ridge_grid),
  "K-Neighbors Regressor" = train(math_score ~ ., data = train_data, method = "knn"),
  "Decision Tree" = rpart(math_score ~ ., data = train_data),
  "Random Forest Regressor" = randomForest(math_score ~ ., data = train_data)
)

# Initialize lists to store model names and R² scores
model_list <- c()
r2_list <- c()

# Evaluate each model
for (model_name in names(models)) {
  model <- models[[model_name]]
  
  # Make predictions
  y_train_pred <- predict(model, X_train_scaled)
  y_test_pred <- predict(model, X_test_scaled)
  
  # Evaluate train and test sets
  train_results <- evaluate_model(y_train, y_train_pred)
  test_results <- evaluate_model(y_test, y_test_pred)
  
  # Print model performance for Training set
  print(paste(model_name))
  print("Model performance for Training set")
  print(paste("- Root Mean Squared Error:", round(train_results$rmse, 4)))
  print(paste("- Mean Absolute Error:", round(train_results$mae, 4)))
  
  # Handle NA in R² score
  if (!is.na(train_results$r2)) {
    print(paste("- R2 Score:", round(train_results$r2, 4)))
  } else {
    print("- R2 Score: NA (constant prediction)")
  }
  
  # Print model performance for Test set
  print("Model performance for Test set")
  print(paste("- Root Mean Squared Error:", round(test_results$rmse, 4)))
  print(paste("- Mean Absolute Error:", round(test_results$mae, 4)))
  
  if (!is.na(test_results$r2)) {
    print(paste("- R2 Score:", round(test_results$r2, 4)))
  } else {
    print("- R2 Score: NA (constant prediction)")
  }
  
  # Store model names and R² scores
  r2_list <- c(r2_list, test_results$r2)
  model_list <- c(model_list, model_name)
}

# Display results in a data frame
results_df <- data.frame(Model_Name = model_list, R2_Score = r2_list) %>%
  arrange(desc(R2_Score))
print(results_df)

# Accuracy of Linear Regression model and plotting results
lin_model <- lm(math_score ~ ., data = train_data)
y_pred <- predict(lin_model, X_test)
score <- cor(y_test, y_pred)^2 * 100
print(paste("Accuracy of the Linear Regression model is", round(score, 2)))

# Plot Actual vs Predicted values
ggplot(data = NULL, aes(x = y_test, y = y_pred)) +
  geom_point() +
  xlab('Actual') +
  ylab('Predicted') +
  geom_smooth(method = "lm", color = "red", se = FALSE)

# Display prediction difference
pred_df <- data.frame(Actual_Value = y_test, Predicted_Value = y_pred, Difference = y_test - y_pred)
print(head(pred_df))