library(geosphere)
library(lubridate)
library(dplyr)
library(caret)
library(mixlm)
library(ggplot2)
library(corrplot)
library(readr)
train <- read_csv("train.csv")
View(train)

delivery_data <- read_csv("train.csv")
colnames(delivery_data)
# Assuming 'delivery_data' is your dataset and 'Delivery_person_Age' is the column with age data
age_range <- range(delivery_data$Delivery_person_Age, na.rm = TRUE)  # na.rm = TRUE to remove any NAs
age_range
rating <- range(delivery_data$Delivery_person_Ratings,na.rm=TRUE)
rating

# To print the range
print(age_range)

range(delivery_data$Delivery_person_Ratings)
####Data Cleaning####
nulls_per_column <- colSums(is.na(delivery_data))
print(nulls_per_column)
delivery_data <- na.omit(delivery_data)

unique_counts <- function(x) {
  unique_values <- unique(x)
  num_unique <- length(unique_values)
  return(list(unique_values = unique_values, num_unique = num_unique))
}

# Apply the function to each column of the dataset
unique_counts_results <- lapply(delivery_data, unique_counts)

# Optionally, if you want to print the results for each column
unique_counts_results


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
delivery_data_mask$Employee_ID <- delivery_data$ID

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
set.seed(2024)

n = nrow(delivery_data)
index = sample(1:n, 31026) #75% of training and 25% of test data
train = delivery_data_mask[index,] 
test = delivery_data_mask[-index,]

model_lr <- lm(Time_taken.min. ~ Delivery_person_Age+Delivery_person_Ratings+Weatherconditions+Road_traffic_density 
               +Vehicle_condition+Type_of_order+Type_of_vehicle+multiple_deliveries+Festival+City, data = train)
summary(model_lr)

residuals_lr = resid(model_lr)
qqnorm(residuals_lr)
qqline(residuals_lr)

##To get cycles of residual plots


plot(model_lr)



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

##Analyzing the correlation for categorical data
data <- delivery_data_mask
data <- data %>% select("Delivery_person_Age","Delivery_person_Ratings","Weatherconditions","Road_traffic_density","distance","EmployeeEfficiency","Vehicle_condition","Type_of_order","day","Type_of_vehicle","multiple_deliveries","Festival","City","LateDelivery","Time_taken.min.")

data$Weatherconditions <- as.numeric(factor(data$Weatherconditions))
data$Road_traffic_density <- as.numeric(factor(data$Road_traffic_density))
data$Vehicle_condition <- as.numeric(factor(data$Vehicle_condition))
data$Type_of_order <- as.numeric(factor(data$Type_of_order))
data$Type_of_vehicle <- as.numeric(factor(data$Type_of_vehicle))
data$multiple_deliveries <- as.numeric(factor(data$multiple_deliveries))
data$Festival <- as.numeric(factor(data$Festival))
data$City <- as.numeric(factor(data$City))
data$day <- as.numeric(factor(data$day))
data$EmployeeEfficiency <- as.numeric(factor(data$EmployeeEfficiency))
data$LateDelivery <- as.numeric(factor(data$LateDelivery))


mcor = round(cor(data),2) #Finding pair wise correlation
corrplot(mcor,type="lower")


check_normality <- function(data, feature) {
  par(mfrow=c(1, 2))
  hist(data[[feature]], main=paste("Histogram of", feature), xlab=feature)
  qqnorm(data[[feature]], main=paste("Q-Q Plot of", feature))
  qqline(data[[feature]])
}

check_normality(data, "distance")

delivery_data_mask <- subset(delivery_data_mask, distance < 22)


check_normality <- function(data, feature) {
  par(mfrow=c(1, 2))
  hist(delivery_data_mask[[feature]], main=paste("Histogram of", feature), xlab=feature)
  qqnorm(delivery_data_mask[[feature]], main=paste("Q-Q Plot of", feature))
  qqline(delivery_data_mask[[feature]])
}

check_normality(delivery_data_mask, "distance")

delivery_data_mask$LateDelivery <- factor(delivery_data_mask$LateDelivery)
delivery_data_mask$EmployeeEfficiency <- factor(delivery_data_mask$EmployeeEfficiency)
delivery_data_mask$Cityname <- factor(delivery_data_mask$Cityname)
delivery_data_mask$day <- factor(delivery_data_mask$day)


delivery_data_mask <- delivery_data_mask %>% select("Delivery_person_Age","Delivery_person_Ratings","Weatherconditions","Road_traffic_density","distance","EmployeeEfficiency","Vehicle_condition"
                        ,"Type_of_order","Type_of_vehicle","multiple_deliveries","Festival","City","day","LateDelivery","Time_taken.min.")


set.seed(2024)

n = nrow(delivery_data_mask)
#index = sample(1:n, 31026) #75% of training and 25% of test data
#train = delivery_data_mask[index,] 
#test = delivery_data_mask[-index,]


model_lr <- lm(Time_taken.min. ~Delivery_person_Age+Delivery_person_Ratings+EmployeeEfficiency+Festival+multiple_deliveries+LateDelivery, data = delivery_data_mask)
summary(model_lr)

full <- lm(Time_taken.min. ~ , data = train)
backward(full,alpha = 0.05,full = TRUE, hierarchy = TRUE)


model_back = lm(Time_taken.min. ~ Delivery_person_Age + Delivery_person_Ratings + 
                  Weatherconditions + Road_traffic_density + Vehicle_condition + 
                  multiple_deliveries + Festival + City + distance + LateDelivery + 
                  EmployeeEfficiency, data = delivery_data_mask)

summary(model_back)

##QQ plot
residuals = resid(model_back)
qqnorm(residuals)
qqline(residuals)

##To get cycles of residual plots

plot(model_back)


library(MASS)

### PROBABILITY PLOTS ######

(hii=hatvalues(model_back)   )          	## LEVERAGE POINTS 
cd=cooks.distance(model_back)  		## COOKS DISTANCE 
dfts=dffits(model_back)        		## DFFITS
e=residuals(model_back) 			## RESIDUAL
std_e=stdres(model_back)			## STANDARDIZED RESIDUAL
r=studres(model_back)   			## STUDENTIZED RESIDUAL 
t = rstudent(model_back) 			## COMPUTING R-Student

# Assuming mdl1 is your linear regression model

# Calculate hat values (leverage points)
hii <- hatvalues(model_back)

# Calculate Cook's distance
cd <- cooks.distance(model_back)

# Calculate DFFITS
dfts <- dffits(model_back)

# Calculate residuals
e <- residuals(model_back)

# Calculate standardized residuals
std_e <- stdres(model_back)

# Calculate studentized residuals
r <- studres(model_back)

# Calculate studentized residuals (t statistics)
t <- rstudent(model_back)

# Create dataframe
results_df <- data.frame(Observed_Y = delivery_data_mask$Time_taken.min., Hat_Values = hii, Cooks_Distance = cd,
                         DFFITS = dfts, Residuals = e, Standardized_Residuals = std_e,
                         Studentized_Residuals = r, T_Statistics = t)

outliers_DFFITS <- results_df %>% filter(DFFITS > (2/sqrt(nrow(delivery_data_mask))))
2/sqrt(nrow(delivery_data_mask))
outliers_DFFITS



library(dplyr)
delivery_data_mask$Employee_ID <- delivery_data$ID
# Add an identifier for outliers in your main dataset
delivery_data_mask <- delivery_data_mask %>%
  mutate(ID = row_number())  # Ensure there's an identifier for each row

# Add the same identifier to the results_df
results_df <- results_df %>%
  mutate(ID = row_number())

# Mark the outliers based on DFFITS
outliers_DFFITS <- results_df %>%
  filter(DFFITS > (2 / sqrt(nrow(delivery_data_mask)))) %>%
  select(ID) %>%
  mutate(Outlier = TRUE)

# Join this back to the main dataset
delivery_data_mask <- delivery_data_mask %>%
  left_join(outliers_DFFITS, by = "ID")

# Filter out the outliers
delivery_data_mask_clean <- delivery_data_mask %>%
  filter(is.na(Outlier))

# Re-run the regression model
model_lr_clean <- lm(Time_taken.min. ~ Delivery_person_Age + Delivery_person_Ratings + 
                       Weatherconditions + Road_traffic_density + Vehicle_condition + 
                       multiple_deliveries + Festival + City + distance + LateDelivery + 
                       EmployeeEfficiency, data = delivery_data_mask_clean)

# Summary of the new model
summary(model_lr_clean)

# Optionally, remove the extra columns (ID, Outlier) if they are no longer needed
delivery_data_mask_clean <- delivery_data_mask_clean %>%
  select(-ID, -Outlier)







