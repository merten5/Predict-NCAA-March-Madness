##### Prepare the data

# Load necessary libraries
library(tidyverse)
library(caret)
library(rpart)
library(randomForest)
library(neuralnet)

# Load the data
data <- read.csv("C:/Users/b3nja/OneDrive/Desktop/NCAA Tournament/analyzePast_Diff_modelWork_through2024.csv")

# Create a binary target variable for wins (1) and losses (0)
data <- data %>%
  mutate(Win = ifelse(ScoreDiff > 0, 1, 0))

# Ensure the data is in the correct format
data <- data %>% mutate(across(everything(), as.numeric))

# Check for missing values
sum(is.na(data))

# Identify columns with NAs after conversion
na_columns <- colnames(data)[colSums(is.na(data)) > 0]
print(na_columns)

# Remove non-numeric columns (if not needed)
data <- data %>% select(-all_of(na_columns))

# Remove rows where Win is NA
data <- data %>% filter(!is.na(Win))

# Check the distribution of the target variable
table(data$Win)

##### Define Predictors and Response
predictors <- c("seed_d", "ws.40_d", "aBPM_d", "TS_d", "TOV_d", "StlBlk_d", "OR_d", "PER_d", "depth_d")
response <- "Win"

##### Split Data into Training and Testing Sets
set.seed(123)
trainIndex <- createDataPartition(data[[response]], p = .8, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

##### Logistic Regression (Baseline Model)
# Fit logistic regression model
logistic_model <- glm(Win ~ ., data = train[, c(predictors, response)], family = binomial)

# Predict on test data
logistic_predictions <- predict(logistic_model, test, type = "response")
logistic_predictions <- ifelse(logistic_predictions > 0.5, 1, 0)  # Convert probabilities to binary

# Calculate accuracy
logistic_accuracy <- sum(logistic_predictions == test[[response]]) / nrow(test)
print(paste("Logistic Regression Accuracy:", logistic_accuracy))

##### Decision Tree
# Fit decision tree model
tree_model <- rpart(Win ~ ., data = train[, c(predictors, response)], method = "class")

# Predict on test data
tree_predictions <- predict(tree_model, test, type = "class")

# Calculate accuracy
tree_accuracy <- sum(tree_predictions == test[[response]]) / nrow(test)
print(paste("Decision Tree Accuracy:", tree_accuracy))

##### Random Forest
# Fit random forest model
rf_model <- randomForest(Win ~ ., data = train[, c(predictors, response)], ntree = 100)

# Predict on test data
rf_predictions <- predict(rf_model, test)

# Calculate accuracy
rf_accuracy <- sum(rf_predictions == test[[response]]) / nrow(test)
print(paste("Random Forest Accuracy:", rf_accuracy))

##### Neural Network
# Normalize data for neural network
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
data_normalized <- as.data.frame(lapply(data, normalize))

# Split normalized data
train_norm <- data_normalized[trainIndex, ]
test_norm <- data_normalized[-trainIndex, ]

# Fit neural network model
nn_model <- neuralnet(Win ~ ., data = train_norm[, c(predictors, response)], hidden = c(5, 3), linear.output = FALSE)

# Predict on test data
nn_predictions <- compute(nn_model, test_norm[, predictors])
predicted_values <- ifelse(nn_predictions$net.result > 0.5, 1, 0)  # Convert probabilities to binary

# Calculate accuracy
nn_accuracy <- sum(predicted_values == test[[response]]) / nrow(test)
print(paste("Neural Network Accuracy:", nn_accuracy))