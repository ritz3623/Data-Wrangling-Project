#Load required libraries
library(dplyr)
library(chron)

#Acquire the data
tripdata_01 <- read.csv("train_tripdata_01.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE, quote = "")
tripdata_02 <- read.csv("train_tripdata_02.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE, quote = "")
train_tripdata <- dplyr::bind_rows(tripdata_01,tripdata_02)
rm(tripdata_01, tripdata_02)
head(train_tripdata, 10)

#Give the variable names
names(train_tripdata) <- c("VendorID", "pickup_time", "dropoff_time", "Passenger_count", "Trip_distance", "Pickup_longitude", "Pickup_latitude", "RateCodeID", "Store_and_fwd_flag", "Dropoff_longitude", "Dropoff_ latitude", "Payment_type", "Fare_amount", "Extra", "MTA_tax", "Improve_surcharge", "Tip_amount", "Tolls_amount", "Total_amount")

#Drop the observations which we dont required in our analysis
train_tripdata <- train_tripdata %>% filter(RateCodeID != 0, Payment_type != 0, Passenger_count >= 0, Trip_distance >= 0)

#Change the values of some variable from negative to positive
train_tripdata$Fare_amount <- abs(train_tripdata$Fare_amount)
train_tripdata$Extra <- abs(train_tripdata$Extra)
train_tripdata$MTA_tax <- abs(train_tripdata$MTA_tax)
train_tripdata$Improve_surcharge <- abs(train_tripdata$Improve_surcharge)
train_tripdata$Tip_amount <- abs(train_tripdata$Tip_amount)
train_tripdata$Tolls_amount <- abs(train_tripdata$Tolls_amount)
train_tripdata$Total_amount <- abs(train_tripdata$Total_amount)

#convert time values from char to datetime object
train_tripdata$pickup_time <- as.POSIXct(train_tripdata$pickup_time, format = "%Y-%m-%d %H:%M:%S", tz = "EST")
train_tripdata$dropoff_time <- as.POSIXct(train_tripdata$dropoff_time, format = "%Y-%m-%d %H:%M:%S", tz = "EST")

#Calculate the total time in minute of each ride and store it as separate variable in data frame
train_tripdata <- train_tripdata %>% mutate(Total_Time_min = ((dropoff_time - pickup_time)/60))
train_tripdata$Total_Time_min <- as.numeric(round(train_tripdata$Total_Time_min, digits = 2))

#Calculate the average speed of car in each ride
#Convert trip distance from miles to kilometers
train_tripdata$Trip_distance <- round(as.numeric(train_tripdata$Trip_distance / 0.6214), digits = 2)
train_tripdata <- train_tripdata %>% mutate(avg_speed = Trip_distance / Total_Time_min)
train_tripdata <- train_tripdata %>% select(1:3, Total_Time_min, avg_speed, 4:19)

#Create train dataset and test dataset


#Lets do EDA - Exploratory Data Analysis!
summary(train_tripdata$Tip_amount)

#Lets explore by ploting the tip amount values using graphs
#Histograph will give the frequency of Tip amount, in out case is 0.00 value is more frequent
hist(train_tripdata$Tip_amount)

#This plot will explore the pattern of customers who likely to give tip amount when they traveled to specific destination
plot(train_tripdata$RateCodeID, train_tripdata$Tip_amount, main = "Distribution of Tip amount based on Rate Code used", xlab = "Rate Code ID", ylab = "Tip Amount", xlim = c(1,6), ylim = c(0.00, 100.00))

#This plot will explore the pattern of customers who likely to give tip amount when they used payment method
plot(train_tripdata$Payment_type, train_tripdata$Tip_amount, main = "Distribution of Tip amount based on Payment Type used", xlab = "Payment Type", ylab = "Tip Amount", xlim = c(1,6), ylim = c(0.00, 100.00))

#How many time Dispute happended in past two months
dispute_happend <- train_tripdata %>% filter(Payment_type == 4)

pickup_time_list <- as.POSIXlt(dispute_happend$pickup_time, format = "%Y-%m-%d %H:%M:%S", tz = "EST")
dropoff_time_list <- as.POSIXlt(dispute_happend$dropoff_time, format = "%Y-%m-%d %H:%M:%S", tz = "EST")

#For which type of Rate the dispute likely to be happended 
hist(dispute_happend$RateCodeID, main = "Histogram of Dispute happended based on Rate Applied", xlab = "Rate Code", ylab = "Disputes", xlim = c(1,6))

#For how much count of passenger the dispute likely to be happended
hist(dispute_happend$Passenger_count, main = "Histogram of Dispute happended based on Passenger Count", xlab = "Passengers", ylab = "Disputes")
