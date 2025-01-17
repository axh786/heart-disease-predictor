library(e1071)
library(dplyr)
library(ISLR)

#One-Hot encoding for Sex and ExercsiseAngina
heart <- cbind(
  heart,
  model.matrix(~ Sex - 1, data = heart),
  model.matrix(~ ExerciseAngina - 1, data = heart)
)
heart <- heart[, !colnames(heart) %in% c( "ExerciseAngina", "Sex")] ## one hot encoding for the nominal variables


#Label encoding for ChestPainType,RestingECG, and ST_Slope


heart$ST_Slope <- ifelse(heart$ST_Slope == "Down", -1,
                         ifelse(heart$ST_Slope == "Flat", 0, 1))
heart$ChestPainType <- ifelse(heart$ChestPainType == "NAP", 0,
                              ifelse(heart$ChestPainType == "ATA", 1,
                                     ifelse(heart$ChestPainType == "TA", 2, 3)))



heart$RestingECG <- ifelse(heart$RestingECG == "Normal", 0,
                           ifelse(heart$RestingECG == "ST", 1, 2))
#Replace any 0 values in Cholesterol with median of the rest

mean_cholesterol <- mean(heart$Cholesterol[heart$Cholesterol != 0])
heart$Cholesterol[heart$Cholesterol == 0] <- mean_cholesterol
heart$Cholesterol <- round(heart$Cholesterol)

# Identify numeric columns
numeric_cols <- c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")

# Split the data into training (80%) and test (20%) sets
set.seed(123)
split_index <- sample(1:nrow(heart), 0.8 * nrow(heart))
# Create training set
train_data <- heart[split_index, ]

# Create testing set
test_data <- heart[-split_index, ]

# Scale the numeric columns of the training set
train_scaled <- train_data
train_scaled[numeric_cols] <- scale(train_data[numeric_cols])

# Scale the numeric columns of the test set using the training set parameters
test_scaled <- test_data
test_scaled[numeric_cols] <- scale(test_data[numeric_cols], 
                                   center = colMeans(train_data[numeric_cols]),
                                   scale = apply(train_data[numeric_cols], 2, sd))

# Prepare training and testing sets
train_x <- train_scaled[, !names(train_scaled) %in% "HeartDisease"]
test_x <- test_scaled[, !names(test_scaled) %in% "HeartDisease"]
train_y <- train_scaled$HeartDisease
test_y <- test_scaled$HeartDisease



#Liner kernel
set.seed(1234)
tune_out <- tune(svm, 
                 train.x = train_x, 
                 train.y = as.factor(train_y), 
                 ranges = list(cost = c(0.001,0.01,0.1,1,5,10,100)),
                 kernel = "linear") 

print(summary(tune_out))

best_model <- tune_out$best.model

svm_best_pred <- predict(best_model, test_x)

# Calculate test error
test_error <- mean(svm_best_pred != test_y)

cat("Test Error:", test_error, "\n")

conf_matrix_tuned <- table(Predicted = svm_best_pred, Actual = test_y)
print("Confusion Matrix:")
print(conf_matrix_tuned)

accuracy_tuned <- sum(diag(conf_matrix_tuned)) / sum(conf_matrix_tuned)
cat("\nAccuracy:", round(accuracy_tuned, 4))

#Polynomial Kernel
# Set seed for reproducibility
set.seed(1234)

# Tuning SVM with polynomial kernel
tune_poly <- tune(svm, 
                  train.x = train_x, 
                  train.y = as.factor(train_y), 
                  ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100),
                                degree = c(2, 3, 4)),
                  kernel = "polynomial")

# Print summary of tuning results
print(summary(tune_poly))

# Get the best model from tuning
best_poly_model <- tune_poly$best.model

# Make predictions on the test set
svm_poly_pred <- predict(best_poly_model, test_x)

# Calculate test error for polynomial kernel
test_error_poly <- mean(svm_poly_pred != test_y)

cat("Test Error (Polynomial Kernel):", test_error_poly, "\n")

# Confusion matrix for polynomial kernel
conf_matrix_poly <- table(Predicted = svm_poly_pred, Actual = test_y)
print("Confusion Matrix (Polynomial Kernel):")
print(conf_matrix_poly)

# Calculate accuracy for polynomial kernel
accuracy_poly <- sum(diag(conf_matrix_poly)) / sum(conf_matrix_poly)
cat("\nAccuracy (Polynomial Kernel):", round(accuracy_poly, 4), "\n")

#Radial kernel

# Set seed for reproducibility
set.seed(1234)

# Tuning SVM with radial kernel (RBF)
tune_radial <- tune(svm, 
                    train.x = train_x, 
                    train.y = as.factor(train_y), 
                    ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100),
                                  gamma = c(0.001, 0.01, 0.1, 1)),
                    kernel = "radial")

# Print summary of tuning results
print(summary(tune_radial))

# Get the best model from tuning
best_radial_model <- tune_radial$best.model

# Make predictions on the test set
svm_radial_pred <- predict(best_radial_model, test_x)

# Calculate test error for radial kernel
test_error_radial <- mean(svm_radial_pred != test_y)

cat("Test Error (Radial Kernel):", test_error_radial, "\n")

# Confusion matrix for radial kernel
conf_matrix_radial <- table(Predicted = svm_radial_pred, Actual = test_y)
print("Confusion Matrix (Radial Kernel):")
print(conf_matrix_radial)

# Calculate accuracy for radial kernel
accuracy_radial <- sum(diag(conf_matrix_radial)) / sum(conf_matrix_radial)
cat("\nAccuracy (Radial Kernel):", round(accuracy_radial, 4), "\n")




