library(geosphere)
library(lubridate)
library(dplyr)
library(caret)
library(mixlm)
library(ggplot2)
library(corrplot)
library(readr)
library(MASS)

delivery_data <- read_csv("Desktop/SEM-2 PDFS/TIME SERIES PDF /FINAL PROJECT/train.csv")
View(train)
colnames(delivery_data)
# Assuming 'delivery_data' is your dataset and 'Delivery_person_Age' is the column with age data
delivery_data <- na.omit(delivery_data)

attach(delivery_data)
delivery_data$`Time_taken(min)`
delivery_data_mask <- delivery_data
delivery_data_mask$Weatherconditions = gsub("conditions ","", delivery_data_mask$Weatherconditions)
delivery_data_mask$Cityname <- sub("RES.*", "", delivery_data_mask$Delivery_person_ID)
delivery_data_mask$Order_Date = as.Date(delivery_data_mask$Order_Date, format = "%d-%m-%Y")
delivery_data_mask$day = weekdays(delivery_data_mask$Order_Date)
delivery_data_mask$Time_taken.min. <- as.numeric(gsub("\\s*\\(min\\)", "", delivery_data_mask$`Time_taken(min)`))
delivery_data_mask <- subset(delivery_data_mask,Festival != "NaN ")
delivery_data_mask <- subset(delivery_data_mask,City != "NaN ")
delivery_data_mask <- subset(delivery_data_mask,Weatherconditions != "NaN ")
delivery_data_mask <- subset(delivery_data_mask,Road_traffic_density != "NaN ")
delivery_data_mask$ID <- delivery_data$ID

delivery_data_mask$Weatherconditions <- factor(delivery_data_mask$Weatherconditions)
delivery_data_mask$Road_traffic_density <- factor(delivery_data_mask$Road_traffic_density)
delivery_data_mask$Vehicle_condition <- factor(delivery_data_mask$Vehicle_condition)
delivery_data_mask$Type_of_order <- factor(delivery_data_mask$Type_of_order)
delivery_data_mask$Type_of_vehicle <- factor(delivery_data_mask$Type_of_vehicle)
delivery_data_mask$multiple_deliveries <- factor(delivery_data_mask$multiple_deliveries)
delivery_data_mask$Festival <- factor(delivery_data_mask$Festival)
delivery_data_mask$City <- factor(delivery_data_mask$City)
str(delivery_data_mask)


###Running the base model####
model_lr <- lm(Time_taken.min. ~ Delivery_person_Age+Delivery_person_Ratings+Weatherconditions+Road_traffic_density 
               +Vehicle_condition+Type_of_order+Type_of_vehicle+multiple_deliveries+Festival+City, data = delivery_data_mask)
summary(model_lr)

residuals_lr = resid(model_lr)
qqnorm(residuals_lr)
qqline(residuals_lr)
mse <- mean(residuals_lr^2)
mse
plot(model_lr)
####DFFITS DISTANCE####
# Define a function to identify outliers based on DFFITS
identify_outliers <- function(model_back, data) {
  # Calculate DFFITS
  dfts <- dffits(model_back)
  
  # Create dataframe with DFFITS and IDs
  results_df_back <- data.frame(Observed_Y = data$Time_taken.min., DFFITS = dfts, ID = data$ID)
  
  # Identify outliers based on DFFITS
  outliers_DFFITS <- results_df_back %>% filter(DFFITS > (2/sqrt(nrow(data))))

  # Get unique IDs of outliers
  exclude_ids <- unique(outliers_DFFITS$ID)
  
  return(exclude_ids)
}

#Finding outliers
exclude_ids <- identify_outliers(model_lr, delivery_data_mask)
exclude_ids
length(exclude_ids)
# Removing Outliers from the data
delivery_data_mask <- delivery_data_mask %>%
  filter(!ID %in% exclude_ids)
nrow(delivery_data_mask)

model_lr_after_df <- lm(Time_taken.min. ~ Delivery_person_Age+Delivery_person_Ratings+Weatherconditions+Road_traffic_density 
               +Vehicle_condition+Type_of_order+Type_of_vehicle+multiple_deliveries+Festival+City, data = delivery_data_mask)
summary(model_lr_after_df)
plot(model_lr_after_df)
####Feature engineering####

delivery_data_mask$LateDelivery = ifelse(delivery_data_mask$Time_taken.min. > 30, 1, 0)

delivery_data_mask$EmployeeEfficiency <- ifelse(
  delivery_data_mask$Delivery_person_Ratings > 4.5, 
  "Excellent",
  ifelse(
    delivery_data_mask$Delivery_person_Ratings > 3.5,
    "Good",
    ifelse(
      delivery_data_mask$Delivery_person_Ratings > 2.5,
      "Average",
      "Poor"
    )
  )
)

#### lets calculate the distance between the restaurant and Deliver location using Haversine formula####

haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert degrees to radians
  rad <- pi / 180
  lat1 <- lat1 * rad
  lon1 <- lon1 * rad
  lat2 <- lat2 * rad
  lon2 <- lon2 * rad
  
  # Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  # Earth's radius in kilometers (approx)
  R <- 6371.0
  distance <- R * c
  
  
  return(distance) # distance in kms
}

delivery_data_mask$distance <- haversine_distance(delivery_data_mask$Restaurant_latitude,delivery_data_mask$Restaurant_longitude,
                                                  delivery_data_mask$Delivery_location_latitude,delivery_data_mask$Delivery_location_longitude)



check_normality <- function(data, feature) {
  par(mfrow=c(1, 2))
  hist(data[[feature]], main=paste("Histogram of", feature), xlab=feature)
  qqnorm(data[[feature]], main=paste("Q-Q Plot of", feature))
  qqline(data[[feature]])
}

check_normality(delivery_data_mask, "distance")

delivery_data_mask <- subset(delivery_data_mask, distance < 22)

check_normality(delivery_data_mask, "distance")

delivery_data_mask$LateDelivery <- factor(delivery_data_mask$LateDelivery)
delivery_data_mask$EmployeeEfficiency <- factor(delivery_data_mask$EmployeeEfficiency)
delivery_data_mask$Cityname <- factor(delivery_data_mask$Cityname)
delivery_data_mask$day <- factor(delivery_data_mask$day)


delivery_data_mask <- delivery_data_mask %>%
  dplyr::select(Delivery_person_Age, Delivery_person_Ratings, Weatherconditions, 
                Road_traffic_density, distance, EmployeeEfficiency, Vehicle_condition, 
                Type_of_order, Type_of_vehicle, multiple_deliveries, Festival, City, 
                day, LateDelivery, `Time_taken.min.`,ID)


n = nrow(delivery_data_mask)

#to remove the id column
delivery_data_mask2 = delivery_data_mask[, -16]

full <- lm(Time_taken.min. ~., data = delivery_data_mask2)
backward(full,alpha = 0.05,full = TRUE, hierarchy = TRUE)


model_back = lm(Time_taken.min. ~ Delivery_person_Age + Delivery_person_Ratings + 
                  Weatherconditions + Road_traffic_density + Vehicle_condition + 
                  multiple_deliveries + Festival + City + distance + LateDelivery + 
                  EmployeeEfficiency, data = delivery_data_mask)

summary(model_back)
par(mfrow=c(1, 1))
##QQ plot
residuals = resid(model_back)
qqnorm(residuals)
qqline(residuals)
mse <- mean(residuals^2)
mse
plot(model_back)
#Finding outliers
exclude_ids <- identify_outliers(model_back, delivery_data_mask)
exclude_ids
length(exclude_ids)
2/sqrt(nrow(delivery_data_mask))
# Removing outliers
delivery_data_mask3 <- delivery_data_mask %>%
  filter(!ID %in% exclude_ids)

model_back2 = lm(Time_taken.min. ~ Delivery_person_Age + Delivery_person_Ratings + 
                   Weatherconditions + Road_traffic_density + Vehicle_condition + 
                   multiple_deliveries + Festival + City + distance + LateDelivery + 
                   EmployeeEfficiency, data = delivery_data_mask3)

summary(model_back2)

residuals = resid(model_back2)
qqnorm(residuals)
qqline(residuals)
mse <- mean(residuals^2)
mse
plot(model_back2)

# Create a data frame with your model statistics
model_data <- data.frame(
  Model = c("Base Model", "Backward Model", "Backward Model Post-Outliers"),
  Adjusted_R2 = c(0.5903, 0.8074, 0.8958),
  MSE = c(35.86135, 11.6725, 6.14284)
)

# Plot for Adjusted R-squared
ggplot(model_data, aes(x = Model, y = Adjusted_R2, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Adjusted R-squared for Different Models",
       x = "Model",
       y = "Adjusted R-squared") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot for MSE
ggplot(model_data, aes(x = Model, y = MSE, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean Squared Error (MSE) for Different Models",
       x = "Model",
       y = "MSE") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

