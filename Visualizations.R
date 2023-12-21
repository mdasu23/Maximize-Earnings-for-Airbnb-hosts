install.packages("ggplot2")
my_data <- read.csv(file_path) #reading csv
#cleaning the data from here to replace missing values and remove variables not necessary
mean_value_of_remaining_obs <- mean(my_data$reviews_per_month, na.rm = TRUE) 
my_data$reviews_per_month[is.na(my_data$reviews_per_month)] <- mean_value_of_remaining_obs 
variable_to_remove <- c("name", "host_name", "host_id", "last_review")
my_data <- select(my_data, -all_of(variable_to_remove))

ggplot(my_data,aes(x=longitude, y=latitude,color=price)) + 
  geom_point(size=3) +
  scale_color_gradient(name="Price",low="blue",high="red") + 
  labs(title="Airbnb listings color coded by price")

ggplot(my_data,aes(x=room_type,y=price)) + 
  geom_boxplot(fill="lightblue",color="blue") +
  labs(
    title = "Prices by Room Type",
    x = "Room Type",
    y = "Price"
  )

ggplot(my_data,aes(x=room_type,y=price,color=neighbourhood_group,size=neighbourhood_group)) +
  geom_point() +
  scale_color_manual(values = c("Manhattan"="skyblue", "Brooklyn"="lightgreen", "Bronx"="lightcoral","Queens"="pink","Staten Island"="yellow")) +
  scale_size_manual(values = c("Manhattan"=4, "Brooklyn"=6, "Bronx"=8,"Queens"=10,"Staten Island"=12)) +
  labs(
    title="Room type vs Price vs Neighbourhood Group",
    x="Room Type",
    y="Price",
    color="Neighbourhood Group"
  )
)

hist(my_data$price,
     main="Price Distribution",
     xlab="Price",
     col="skyblue",
     border="darkblue"
     )

ggplot(my_data, aes(x=price,y=number_of_reviews, color=neighbourhood_group)) + 
  geom_point() + 
  labs(
    title = "Price vs Number of Reviews by Neighbourhood Group",
    x="Price",
    y="Number of Reviews",
    color="Neighbourhood Group"
  )