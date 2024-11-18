# Ensure CRAN mirror is set
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Load necessary libraries
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(caret)) install.packages("caret")
if (!require(corrplot)) install.packages("corrplot")

library(tidyverse)
library(caret)
library(corrplot)

# Load the dataset
housing_data <- read.csv("/Users/shreyas/Desktop/R-Projects/corrected_boston.csv")

# Data Cleaning
cat("Number of missing values per column:\n")
print(colSums(is.na(housing_data)))

# Summary of cleaned data
cat("\nSummary of Data:\n")
print(summary(housing_data))

# Visualizations
# Histogram for Median House Value
ggplot(housing_data, aes(x = MEDV)) +
    geom_histogram(binwidth = 2, fill = "blue", color = "black") +
    labs(title = "Distribution of Median House Values", x = "Median Value ($1000)")

# Scatter plot: Rooms (RM) vs Median Value (MEDV)
ggplot(housing_data, aes(x = RM, y = MEDV)) +
    geom_point(color = "blue") +
    labs(title = "Rooms vs. Median House Value", x = "Average Number of Rooms", y = "Median Value ($1000)")

# Boxplot: Median Value by Charles River Proximity (CHAS)
ggplot(housing_data, aes(x = factor(CHAS), y = MEDV)) +
    geom_boxplot(fill = "lightblue") +
    labs(
        title = "Median House Value by Charles River Proximity",
        x = "Proximity to Charles River (1 = Near, 0 = Far)", y = "Median Value ($1000)"
    )

# Correlation Matrix and Heatmap
cor_matrix <- cor(housing_data)
corrplot(cor_matrix,
    method = "color", addCoef.col = "black", number.cex = 0.7,
    main = "Correlation Matrix of Boston Housing Data"
)

# Exploratory Analysis
cat("\nCorrelation between variables:\n")
print(cor_matrix)

# Regression Analysis: Predicting Median House Value
model <- lm(MEDV ~ RM + LSTAT + PTRATIO, data = housing_data)
cat("\nSummary of Linear Regression Model:\n")
print(summary(model))

# Predicting House Prices
predicted_values <- predict(model, housing_data)
housing_data <- housing_data %>% mutate(Predicted_MEDV = predicted_values)

# Save predictions to a new CSV
write.csv(housing_data, "/Users/shreyas/Desktop/R-Projects/housing_predictions.csv", row.names = FALSE)

# Visualize Predicted vs Actual
ggplot(housing_data, aes(x = MEDV, y = Predicted_MEDV)) +
    geom_point(color = "blue") +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(
        title = "Predicted vs. Actual Median House Value",
        x = "Actual Median Value ($1000)", y = "Predicted Median Value ($1000)"
    )

# Evaluate Model Performance
cat("\nModel Performance Metrics:\n")
model_performance <- data.frame(
    RMSE = RMSE(predicted_values, housing_data$MEDV),
    R2 = R2(predicted_values, housing_data$MEDV)
)
print(model_performance)

# Advanced Modeling: Machine Learning with Caret
set.seed(123)
train_index <- createDataPartition(housing_data$MEDV, p = 0.8, list = FALSE)
train_data <- housing_data[train_index, ]
test_data <- housing_data[-train_index, ]

# Train a Random Forest Model
if (!require(randomForest)) install.packages("randomForest")
library(randomForest)

rf_model <- randomForest(MEDV ~ ., data = train_data, importance = TRUE, ntree = 500)
cat("\nRandom Forest Model Summary:\n")
print(rf_model)

# Evaluate Random Forest Model
rf_predictions <- predict(rf_model, test_data)
rf_performance <- data.frame(
    RMSE = RMSE(rf_predictions, test_data$MEDV),
    R2 = R2(rf_predictions, test_data$MEDV)
)
cat("\nRandom Forest Performance Metrics:\n")
print(rf_performance)

# Visualize Feature Importance
importance <- importance(rf_model)
var_importance <- data.frame(Variable = rownames(importance), Importance = importance[, 1])
ggplot(var_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "blue") +
    coord_flip() +
    labs(title = "Variable Importance in Random Forest Model", x = "Variable", y = "Importance")

# Save Final Cleaned Data
write.csv(housing_data, "/Users/shreyas/Desktop/R-Projects/final_housing_data.csv", row.names = FALSE)

cat("\nAnalysis Complete. Results saved to R-Projects folder.")
