install.packages("glmnet")
install.packages("dplyr")

library(glmnet)
library(dplyr)

my_data <- read.csv(file_path) #reading csv
#cleaning the data from here to replace missing values and remove variables not necessary
mean_value_of_remaining_obs <- mean(my_data$reviews_per_month, na.rm = TRUE) 
my_data$reviews_per_month[is.na(my_data$reviews_per_month)] <- mean_value_of_remaining_obs 
variable_to_remove <- c("name", "host_name", "host_id", "last_review")
my_data <- select(my_data, -all_of(variable_to_remove))

# Selecting specific features for Lasso
lasso_features <- c("room_type", "neighbourhood_group", "neighbourhood", "latitude", "longitude", "availability_365", "number_of_reviews")

# Converting categorical variables to num variables using one_hot_encoding
data <- my_data %>%
  mutate(room_type = as.factor(room_type),
         neighbourhood = as.factor(neighbourhood),
         neighbourhood_group = as.factor(neighbourhood_group)) %>%
  mutate(across(everything(), as.numeric))  # Convert all columns to numeric

# Splitting the data into training and testing sets
set.seed(123)
split <- sample.split(data$price, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Extracting predictors and response variable for selected features
X_train <- select(train_data, all_of(lasso_features))
y_train <- train_data$price

# Performing Lasso regression
lasso_model <- cv.glmnet(as.matrix(X_train), y_train, alpha = 1)  # alpha = 1 for Lasso

# Plotting the cross-validated mean squared error (cvm) as a function of log(lambda)
plot(lasso_model)

# Identifying the optimal lambda value
best_lambda <- lasso_model$lambda.min
cat("Optimal lambda:", best_lambda, "\n")

# Extracting the coefficients for the optimal lambda
lasso_coefficients <- coef(lasso_model, s = best_lambda)
print(lasso_coefficients)

# Making predictions on the test data using selected features
X_test <- select(test_data, all_of(lasso_features))
predictions <- predict(lasso_model, newx = as.matrix(X_test), s = best_lambda)

# Evaluating the model using MAE and RSME
mae <- mean(abs(predictions - test_data$price))
print(paste("MAE:", mae))

rmse <- sqrt(2 * mae^2)
print(paste("RMSE:",rmse))
