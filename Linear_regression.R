
install.packages("caTools")
install.packages("dplyr")
library(caTools)
library(dplyr)

my_data<-read.csv(file_path) #reading csv
#cleaning the data from here to replace missing values and remove variables not necessary
mean_value_of_remaining_obs <- mean(my_data$reviews_per_month, na.rm=TRUE) 
my_data$reviews_per_month[is.na(my_data$reviews_per_month)] <- mean_value_of_remaining_obs 
variable_to_remove <- c("name","host_name","host_id","last_review")
my_data<-select(my_data,-all_of(variable_to_remove))

#converting categorical values to num using one-hot encoding
data <- my_data %>%
  mutate(room_type = as.factor(room_type),
         neighbourhood_group = as.factor(neighbourhood_group), neighbourhood=as.factor(neighbourhood)) %>%
  mutate(across(everything(), as.numeric))  # Convert all columns to numeric

#Splitting the data into training and testing sets by 70,30 ratio
set.seed(123)
split <- sample.split(data$price, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)


#Creating the linear regression model
lm_model <- lm(price ~ room_type + neighbourhood_group + neighbourhood + latitude + longitude+number_of_reviews+calculated_host_listings_count+availability_365+minimum_nights, data = train_data)

# Printing the summary of the model
print(coef(lm_model))
summary(lm_model)

# Making predictions on the test data
predictions <- predict(lm_model, newdata = test_data)

#Evaluating the model using MAE and RSME
mae <- mean(abs(predictions - test_data$price))
print(paste("MAE:", mae))

rmse <- sqrt(2 * mae^2)
print(paste("RMSE:",rmse))


