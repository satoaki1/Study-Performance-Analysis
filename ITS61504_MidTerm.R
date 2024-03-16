# Installing necessary libraries
install.packages("readxl")
install.packages("writexl")
install.packages("MASS")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("caret")
install.packages("caTools")
install.packages("rpart") 
install.packages("randomForest")
install.packages("e1071")

# Importing necessary libraries
library(readxl)
library(writexl)
library(MASS)
library(ggplot2)
library(reshape2)
library(caret)
library(caTools)
library(rpart)
library(randomForest)
library(e1071)

# |============================================================================|
# | Data Collection                                                            |
# |============================================================================|

dataset <- read.csv("absolute-path-to-the-directory/ITS61504_MidTerm.csv")

print(head(dataset))
print(dim(dataset))

# |============================================================================|
# | Data Preprocessing                                                         |
# |============================================================================|

## 1. Handling missing values

# How many missing values in the data
dataset[dataset == ""] <- NA
colSums(is.na(dataset))

# fill in missing values in the data
for(col in names(dataset)) {
  
  # If the variable is character type, fill N/A with mode value
  if(is.factor(dataset[[col]]) || is.character(dataset[[col]])) {
    mode_value <- names(which.max(table(dataset[[col]])))
    dataset[[col]][is.na(dataset[[col]])] <- mode_value
    
  # Else if the variable is numeric type, fill N/A with mean value
  } else if (is.numeric(dataset[[col]])) {
    dataset[[col]][is.na(dataset[[col]])] <- mean(dataset[[col]])
  }
}

# Now check again how many missing values in the data
colSums(is.na(dataset))

## 2. Label Encoding

# Label Encoding Categorical variables
for (col in colnames(dataset)) {
  if (is.character(dataset[[col]])) {
    dataset[[col]] <- as.numeric(factor(dataset[[col]]))
  }
}

dataset

# |============================================================================|
# | Exploratory Data Analysis (EDA)                                            |
# |============================================================================|

#'*Descriptive Analysis*

## Statistical Insights for numerical variables

# Custom function to calculate mode
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# For Numerical Variables
numerical_columns <- colnames(dataset)[c(6, 7, 8)]
numerical_stats <- list()

for (col in numerical_columns) {
  num_data <- dataset[[col]]
  num_stats <- data.frame(
    Variable = col,
    Max = max(num_data, na.rm = TRUE),
    Min = min(num_data, na.rm = TRUE),
    Mean = mean(num_data, na.rm = TRUE),
    Median = median(num_data, na.rm = TRUE),
    Mode = get_mode(num_data),
    Q1 = quantile(num_data, 0.25, na.rm = TRUE),
    Q3 = quantile(num_data, 0.75, na.rm = TRUE),
    SD = sd(num_data, na.rm = TRUE)
  )
  numerical_stats[[col]] <- num_stats
}

# Combine the list of data frames into one
numerical_stats_df <- do.call(rbind, numerical_stats)
rownames(numerical_stats_df) <- NULL  # Reset row names

numerical_stats_df

# chi-squared test for categorical columns
categorical_columns <- colnames(dataset)[c(1, 2, 3, 4, 5)]
target_columns <- colnames(dataset)[c(6, 7, 8)]

# Test between categorical variables and math score

# Ignore warnings that are not severe
suppressWarnings({
  
  # examine for each target variable
  for (target_col in target_columns) {
    
    # initialize the dataframe of chi-squared test results
    chi_squared_results <- data.frame(
      Column = character(), Chi_Squared = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)
    
    # Store results of Chi-squared Test for each categorical variable in the dataframe
    for (col in categorical_columns) {
      contingency_table <- table(dataset[[col]], dataset[[target_col]])
      test_result <- chisq.test(contingency_table)
      chi_squared_results <- rbind(chi_squared_results, data.frame(
        Column = col, Chi_Squared = test_result$statistic, P_Value = test_result$p.value))
    }
    
    # Finally, print the completed Chi-squared Test Results dataframe
    print(target_col)
    print(chi_squared_results)
  }
})

#'*Analyses with Visual Contexts*

## Bar plots for Categorical Variables

# Set up plotting area
par(mfrow=c(1,2))

for (col in categorical_columns) {
  cat_data <- dataset[[col]]
  freq_table <- prop.table(table(cat_data))
  
  # Plot bar chart with larger margins to accommodate labels
  barplot(freq_table, ylab = 'Proportion of students', cex.names = 0.8, las = 2, mar = c(5, 5, 4, 2) + 0.1)
  title(main = col)
}

## Correlation Analysis with Heatmap 

# correlation matrix for numerical columns
correlation_matrix <- cor(dataset)

# Convert the correlation matrix into a 'long' dataframe
correlation_data <- as.data.frame(as.table(correlation_matrix))

# Remove duplicate column names
colnames(correlation_matrix) <- make.unique(colnames(correlation_matrix))

# Plot Correlation Matrix Heatmap
ggplot(data = reshape2::melt(correlation_matrix), aes(x=Var1, y=Var2)) +
  geom_tile(aes(fill=value), color='white') +
  geom_text(aes(label=round(value, 2)), size=3) +
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1), space="Lab", name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=45, vjust=1, size=10, hjust=1),
        axis.text.y = element_text(size=10)) +
  coord_fixed()

## Add a new feature for total scores of math, reading and writing

# Create new feature "total_score"
dataset$total_score <- dataset$math_score + dataset$reading_score + dataset$writing_score

# correlation matrix for numerical columns
correlation_matrix <- cor(dataset)

# Convert the correlation matrix into a 'long' dataframe
correlation_data <- as.data.frame(as.table(correlation_matrix))

# Remove duplicate column names
colnames(correlation_matrix) <- make.unique(colnames(correlation_matrix))

# Plot a new Correlation Matrix Heatmap
ggplot(data = reshape2::melt(correlation_matrix), aes(x=Var1, y=Var2)) +
  geom_tile(aes(fill=value), color='white') +
  geom_text(aes(label=round(value, 2)), size=3) +
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1), space="Lab", name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=45, vjust=1, size=10, hjust=1),
        axis.text.y = element_text(size=10)) +
  coord_fixed()

# |============================================================================|
# | Model Training (Predict Total Score without three score attributes)        |
# |============================================================================|

# Drop average_score column and three score attributes
dataset_t <- dataset[-c(6, 7, 8)]

# Set total_score as 
target_column <- colnames(dataset_t)[c(6)]

# Split into Training and Testing Sets
set.seed(123)
splitIndex <- createDataPartition(dataset_t[,target_column], p = .80, list = FALSE, times = 1)
trainData <- dataset_t[-splitIndex,]
testData <- dataset_t[splitIndex,]

linearModel <- lm(formula = total_score ~ ., data = trainData)
linearPred <- predict(linearModel, newdata = testData)

dtModel <- rpart(formula = total_score ~ ., data = trainData)
dtPred <- predict(dtModel, newdata = testData)

rfModel <- randomForest(formula = total_score ~ ., data = trainData)
rfPred <- predict(rfModel, newdata = testData)

svmModel <- svm(formula = total_score ~ ., data = trainData)
svmPred <- predict(svmModel, newdata = testData)

# |============================================================================|
# | Model Evaluation                                                           |
# |============================================================================|

## Mean Squared Error
mse <- function(actual, predicted) {
  mean((actual - predicted) ^ 2)
}

## Root Mean Squared Error
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

## Mean Absolute Error
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

## R-squared
r_squared <- function(actual, predicted) {
  1 - sum((predicted - actual) ^ 2) / sum((actual - mean(actual)) ^ 2)
}

## DataFrame to store results information
reg_results <- data.frame(
  model_name = character(),
  mse_res = numeric(),
  rmse_res = numeric(),
  mae_res = numeric(),
  r2_res = numeric()
)

#'*Multiple Linear Regression evaluation*
linear_mse <- mse(testData$total_score, linearPred)
linear_rmse <- rmse(testData$total_score, linearPred)
linear_mae <- mae(testData$total_score, linearPred)
linear_r2 <- r_squared(testData$total_score, linearPred)
reg_results <- rbind(reg_results, data.frame(
  model_name = "Linear Regression", 
  mse_res = linear_mse, rmse_res = linear_rmse, mae_res = linear_mae, r2_res = linear_r2
))

#'*Decision Tree evaluation*
dt_mse <- mse(testData$total_score, dtPred)
dt_rmse <- rmse(testData$total_score, dtPred)
dt_mae <- mae(testData$total_score, dtPred)
dt_r2 <- r_squared(testData$total_score, dtPred)
reg_results <- rbind(reg_results, data.frame(
  model_name = "Decision Tree", 
  mse_res = dt_mse, rmse_res = dt_rmse, mae_res = dt_mae, r2_res = dt_r2
))

#'*Random Forest evaluation*
rf_mse <- mse(testData$total_score, rfPred)
rf_rmse <- rmse(testData$total_score, rfPred)
rf_mae <- mae(testData$total_score, rfPred)
rf_r2 <- r_squared(testData$total_score, rfPred)
reg_results <- rbind(reg_results, data.frame(
  model_name = "Random Forest", 
  mse_res = rf_mse, rmse_res = rf_rmse, mae_res = rf_mae, r2_res = rf_r2
))

#'*Support Vector Machine evaluation*
svm_mse <- mse(testData$total_score, svmPred)
svm_rmse <- rmse(testData$total_score, svmPred)
svm_mae <- mae(testData$total_score, svmPred)
svm_r2 <- r_squared(testData$total_score, svmPred)
reg_results <- rbind(reg_results, data.frame(
  model_name = "Support Vector Machine", 
  mse_res = svm_mse, rmse_res = svm_rmse, mae_res = svm_mae, r2_res = svm_r2
))

## Print overall results
reg_results

# |============================================================================|
# | Model Training with all attributes                                         |
# |============================================================================|

target_column <- colnames(dataset)[c(9)]

target_column

# Split into Training and Testing Sets
set.seed(123)
splitIndex <- createDataPartition(dataset[,target_column], p = .80, list = FALSE, times = 1)
trainData <- dataset[splitIndex,]
testData <- dataset[-splitIndex,]

linearModel <- lm(formula = total_score ~ ., data = trainData)
linearPred <- predict(linearModel, newdata = testData)

dtModel <- rpart(formula = total_score ~ ., data = trainData)
dtPred <- predict(dtModel, newdata = testData)

rfModel <- randomForest(formula = total_score ~ ., data = trainData)
rfPred <- predict(rfModel, newdata = testData)

svmModel <- svm(formula = total_score ~ ., data = trainData)
svmPred <- predict(svmModel, newdata = testData)

# |============================================================================|
# | Model Evaluation                                                           |
# |============================================================================|

## Mean Squared Error
mse <- function(actual, predicted) {
  mean((actual - predicted) ^ 2)
}

## Root Mean Squared Error
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

## Mean Absolute Error
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

## R-squared
r_squared <- function(actual, predicted) {
  1 - sum((predicted - actual) ^ 2) / sum((actual - mean(actual)) ^ 2)
}

## DataFrame to store results information
reg_results <- data.frame(
  model_name = character(),
  mse_res = numeric(),
  rmse_res = numeric(),
  mae_res = numeric(),
  r2_res = numeric()
)

#'*Multiple Linear Regression evaluation*
linear_mse <- mse(testData$total_score, linearPred)
linear_rmse <- rmse(testData$total_score, linearPred)
linear_mae <- mae(testData$total_score, linearPred)
linear_r2 <- r_squared(testData$total_score, linearPred)
reg_results <- rbind(reg_results, data.frame(
  model_name = "Linear Regression", 
  mse_res = linear_mse, rmse_res = linear_rmse, mae_res = linear_mae, r2_res = linear_r2
))

#'*Decision Tree evaluation*
dt_mse <- mse(testData$total_score, dtPred)
dt_rmse <- rmse(testData$total_score, dtPred)
dt_mae <- mae(testData$total_score, dtPred)
dt_r2 <- r_squared(testData$total_score, dtPred)
reg_results <- rbind(reg_results, data.frame(
  model_name = "Decision Tree", 
  mse_res = dt_mse, rmse_res = dt_rmse, mae_res = dt_mae, r2_res = dt_r2
))

#'*Random Forest evaluation*
rf_mse <- mse(testData$total_score, rfPred)
rf_rmse <- rmse(testData$total_score, rfPred)
rf_mae <- mae(testData$total_score, rfPred)
rf_r2 <- r_squared(testData$total_score, rfPred)
reg_results <- rbind(reg_results, data.frame(
  model_name = "Random Forest", 
  mse_res = rf_mse, rmse_res = rf_rmse, mae_res = rf_mae, r2_res = rf_r2
))

#'*Support Vector Machine evaluation*
svm_mse <- mse(testData$total_score, svmPred)
svm_rmse <- rmse(testData$total_score, svmPred)
svm_mae <- mae(testData$total_score, svmPred)
svm_r2 <- r_squared(testData$total_score, svmPred)
reg_results <- rbind(reg_results, data.frame(
  model_name = "Support Vector Machine", 
  mse_res = svm_mse, rmse_res = svm_rmse, mae_res = svm_mae, r2_res = svm_r2
))

## Print overall results
reg_results

