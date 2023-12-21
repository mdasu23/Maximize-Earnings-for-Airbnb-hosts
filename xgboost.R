install.packages("xgboost")
install.packages("dplyr")

library(xgboost)
library(dplyr)

my_data <- read.csv(file_path)#reading csv
#cleaning the data from here to replace missing values and remove variables not necessary
mean_value_of_remaining_obs <- mean(my_data$reviews_per_month, na.rm = TRUE) 
my_data$reviews_per_month[is.na(my_data$reviews_per_month)] <- mean_value_of_remaining_obs 
variable_to_remove <- c("name", "host_name", "host_id", "last_review")
my_data <- select(my_data, -all_of(variable_to_remove))

# Selecting specific features for XGBoost
xgboost_features <- c("room_type", "neighbourhood_group", "latitude", "longitude","minimum_nights","calculated_host_listings_count", "availability_365")

# Converting categorical variables to dummy variables
data <- my_data %>%
  mutate(room_type = as.factor(room_type),
         neighbourhood = as.factor(neighbourhood),
         neighbourhood_group = as.factor(neighbourhood_group)) %>%
  mutate(across(everything(), as.numeric))  # Convert all columns to numeric

# Split the data into training and testing sets
set.seed(123)
split <- sample.split(data$price, SplitRatio = 0.8)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Extracting predictors and response variables for selected features
X_train <- select(train_data, all_of(xgboost_features))
y_train <- train_data$price

# Ensuring all variables are numeric
X_train <- as.data.frame(lapply(X_train, as.numeric))

# Converting data to DMatrix format (XGBoost-specific)
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)

# Setting XGBoost parameters
params <- list(
  objective = "reg:squarederror",  # Objective for regression
  eval_metric = "mae",             # Evaluation metric: Mean Absolute Error
  max_depth = 6,                   # Maximum tree depth
  eta = 0.3                       # Learning rate
)

# Training the XGBoost model
nrounds <- 100  # Adjust the number of boosting rounds as needed
xgb_model <- xgb.train(params, data = dtrain, nrounds = nrounds)

# Making predictions on the test data
X_test <- select(test_data, all_of(xgboost_features))
X_test <- as.data.frame(lapply(X_test, as.numeric))
dtest <- xgb.DMatrix(data = as.matrix(X_test))
predictions <- predict(xgb_model, newdata = dtest)

# Evaluating the model using MAE and RSME
mae <- mean(abs(predictions - test_data$price))
print(paste("MAE:", mae))

rmse <- sqrt(2 * mae^2)
print(paste("RMSE:",rmse))
