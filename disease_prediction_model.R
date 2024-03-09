# Load necessary libraries
library(caret)
library(randomForest)

# Load the dataset
health_data <- read.csv("health.csv")

# Define the target variables for disease prediction
disease_target_variables <- c('H2a', 'H7a', 'H13a', 'H17', 'X1')

# Specify columns to exclude
columns_to_exclude <- c("H20a", "H20b", "H20c", "H20d", "H20e", "H20f")

# Exclude specified columns
predictor_columns <- setdiff(colnames(health_data), c(disease_target_variables, columns_to_exclude))

# Check and handle missing values
missing_values <- sum(is.na(health_data[, predictor_columns]))
if (missing_values > 0) {
  warning("Handling missing values in the dataset.")
  # Replace missing values with the median of each column
  health_data[, predictor_columns] <- apply(health_data[, predictor_columns], 2, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
}

# Apply one-hot encoding using caret's dummyVars
disease_dummy_vars <- dummyVars("~ .", data = health_data[, predictor_columns], fullRank = TRUE)
health_data_encoded <- predict(disease_dummy_vars, newdata = health_data[, predictor_columns])

# Combine one-hot encoded columns with the target variables
health_data_encoded <- cbind(health_data_encoded, health_data[, disease_target_variables])

# Model Training for Disease Prediction (Random Forest)
disease_rf_models <- list()

for (target_variable in disease_target_variables) {
  # Handle missing values in the target variable
  health_data_encoded_copy <- health_data_encoded[complete.cases(health_data_encoded[[target_variable]]), ]
  
  # Ensure the target variable is treated as a factor with two levels
  health_data_encoded_copy[[target_variable]] <- as.factor(health_data_encoded_copy[[target_variable]])
  
  # Ensure the target variable has two levels
  if (length(levels(health_data_encoded_copy[[target_variable]])) != 2) {
    warning(paste("Skipping", target_variable, "due to insufficient data points for binary classification."))
    next  # Skip to the next iteration
  }
  
  # Split data into training and testing sets
  set.seed(123)
  splitIndex <- createDataPartition(health_data_encoded_copy[[target_variable]], p = 0.7, list = FALSE)
  
  if (length(splitIndex) < 2) {
    warning(paste("Skipping", target_variable, "due to insufficient data points for splitting."))
    next  # Skip to the next iteration
  }
  
  train_data <- health_data_encoded_copy[splitIndex, ]
  test_data <- health_data_encoded_copy[-splitIndex, ]
  
  # Specify the predictor variables for the formula
  features <- setdiff(colnames(train_data), disease_target_variables)
  predictor_vars <- paste(features, collapse = " + ")
  
  # Construct the formula
  formula <- as.formula(paste(target_variable, "~", predictor_vars))
  
  # Use the formula and data to train the Random Forest model with probabilities
  rf_model <- randomForest(formula, data = train_data, ntree = 6000, type = "response")
  
  # Final Model Evaluation
  predictions <- predict(rf_model, newdata = test_data, type = "response")
  confusion_matrix <- table(test_data[[target_variable]], predictions)
  
  # Calculate evaluation metrics (e.g., accuracy, precision, recall, F1 score)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision <- diag(confusion_matrix) / (rowSums(confusion_matrix))
  recall <- diag(confusion_matrix) / (colSums(confusion_matrix))
  f1_score <- (2 * precision * recall) / (precision + recall)
  
  # Print evaluation metrics
  cat("Target Variable:", target_variable, "\n")
  cat("Accuracy:", accuracy, "\n")
  cat("Precision:", round(precision, 2), "\n")  # Round to 2 decimal places
  cat("Recall:", round(recall, 2), "\n")
  cat("F1 Score:", round(f1_score, 2), "\n\n")  # Round to 2 decimal places
  
  # Save the trained model and evaluation metrics
  disease_rf_models[[target_variable]] <- list(model = rf_model, evaluation = c(accuracy, precision, recall, f1_score))
}
