library(ISLR)
library(class)
set.seed(1234)
attach(heart) ## run lines 1-5 together
# detach(heart)


# dim(heart)
# summary(heart[,1:12]) ## run lines 9-10 together

mean_chol <- mean(Cholesterol[Cholesterol > 0]) # fill empty values for cholesterol
Cholesterol[Cholesterol == 0] <- mean_chol

# summary(Cholesterol) ## run lines 12-15 together


# ncol(heart)

heart <- cbind(
  heart,
  model.matrix(~ ChestPainType - 1, data = heart),
  model.matrix(~ RestingECG - 1, data = heart),
  model.matrix(~ ExerciseAngina - 1, data = heart),
  model.matrix(~ Sex - 1, data = heart) # One-hot encoding for Sex
)

# ncol(heart)

# str(heart)

heart <- heart[, !colnames(heart) %in% c("ChestPainType", "RestingECG", "ExerciseAngina", "Sex")] ## one hot encoding for the nominal variables
heart$ST_Slope <- as.numeric(factor(heart$ST_Slope, levels = c("Up", "Flat", "Down"))) ## label encoding for the ordinal variables
## run lines 18-33 together

# Scale Numeric Features (excluding HeartDisease)
numeric_features <- heart[, !colnames(heart) %in% c("HeartDisease")]
scaled_features <- scale(numeric_features)

# Combine scaled features with response variable
heart_scaled <- data.frame(scaled_features, HeartDisease = heart$HeartDisease) # Convert back to a data frame

set.seed(1234)
train_idx <- sample(1:nrow(heart_scaled), size = 0.8 * nrow(heart_scaled))
train_data <- heart_scaled[train_idx, ]
test_data <- heart_scaled[-train_idx, ]

# Separate Predictors and Response
train_X <- train_data[, !colnames(train_data) %in% c("HeartDisease")]
train_y <- train_data$HeartDisease
test_X <- test_data[, !colnames(test_data) %in% c("HeartDisease")]
test_y <- test_data$HeartDisease

# Run KNN
k <- 17
predicted <- knn(train = train_X, 
                 test = test_X, 
                 cl = train_y, k = k)

# Evaluate Model Performance
accuracy <- mean(predicted == test_y)
print(paste("Accuracy:", accuracy))

# Confusion Matrix
print(table(Predicted = predicted, Actual = test_y))

#####
# Helper function to plot pairs with KNN
plot_knn <- function(data, feature1, feature2, k, response) {
  # Extract features and response
  X <- data[, c(feature1, feature2)]
  y <- as.factor(data[[response]])
  levels(y) <- c("No Heart Disease", "Heart Disease") # Label adjustment for clarity
  
  # Create a grid for decision boundary
  x1_range <- seq(min(X[[feature1]]) - 1, max(X[[feature1]]) + 1, length.out = 200)
  x2_range <- seq(min(X[[feature2]]) - 1, max(X[[feature2]]) + 1, length.out = 200)
  grid <- expand.grid(x1 = x1_range, x2 = x2_range)
  colnames(grid) <- c(feature1, feature2)
  
  # Run KNN on the grid
  knn_predictions <- knn(
    train = X, 
    test = grid, 
    cl = y, 
    k = k
  )
  
  # Plotting
  plot(X[[feature1]], X[[feature2]], col = as.numeric(y) + 1, pch = 19, xlab = feature1, ylab = feature2)
  contour(x = x1_range, y = x2_range, z = matrix(as.numeric(knn_predictions), length(x1_range)), 
          levels = unique(as.numeric(y)), col = "black", lwd = 2, add = TRUE, drawlabels = FALSE)
  
  # Update legend with custom labels
  legend("topright", legend = c("No Heart Disease", "Heart Disease"), col = 2:3, pch = 19)
}

# Define pairs of variables
pairs_to_plot <- list(
  c("Age", "RestingBP")
)

# Plot each pair
par(mfrow = c(1, 1)) # Create a 1x1 grid for plots (adjust size as needed)
for (pair in pairs_to_plot) {
  plot_knn(heart_scaled, feature1 = pair[1], feature2 = pair[2], k = 17, response = "HeartDisease")
}
