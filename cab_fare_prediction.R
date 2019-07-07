rm(list=ls(all=T))

setwd("C:/Users/kiran/Desktop/python practice/Project-1")
cab_data = read.csv("train_cab.csv", header = T)

head(cab_data)
###########################################Explore the data##########################################
# dimensions of dataset
dim(cab_data)

# list types for each attribute
sapply(cab_data, class)

# summarize the features
summary(cab_data)

#install geosphere package to calculate distance from pickup and dropoff coordinates.
library(geosphere)
library(data.table)

#removing invalid latitude point
cab_data$pickup_latitude[cab_data$pickup_latitude>90]
library(dplyr)
cab_data <- filter(cab_data, pickup_latitude <=90)

setDT(cab_data)[ , dist_km := distGeo(matrix(c(pickup_longitude,pickup_latitude), ncol = 2,), 
                                matrix(c(dropoff_longitude,dropoff_latitude ), ncol = 2))/1000]
#removing invalid fare_amount
summary(cab_data)
cab_data %>%
  filter(fare_amount=='430-')
cab_data <- filter(cab_data, fare_amount!='430-')

#converting datatype of fare amount from factor to numeric
cab_data$fare_amount = as.numeric(as.character(cab_data$fare_amount))

high_fare<- filter(cab_data,fare_amount>200)
cab_data<-setdiff(cab_data, high_fare)

##################################Data Cleaning###############################################
missing_val = data.frame(apply(cab_data,2,function(x){sum(is.na(x))}))
cab_data <- na.omit(cab_data)

#considering max 6 passengers for cab in case of SUV
cab_data<-filter(cab_data,passenger_count<7)

#Checking improper coordinates, and considering cancelled rides with fare amount less than $90
false_coordinates <- filter(cab_data, dist_km==0,pickup_latitude==0)
cab_data<-setdiff(cab_data, false_coordinates)
false_fares <-filter(cab_data,dist_km==0 & fare_amount>90)
cab_data<-setdiff(cab_data, false_fares)
cab_data <- filter(cab_data, dist_km==0 | dist_km>=0.3 & fare_amount>0)

#considering rows with distance in range from 0km to 1000km
library(ggplot2)
ggplot(cab_data,aes(x=dist_km))+geom_histogram(bins=1000)+scale_x_log10()

cab_data <-filter(cab_data,between(cab_data$dist_km, 0, 1000))

# scatter plot between fare_amount and dist_km
dev.off()
ggplot(cab_data,aes(x=dist_km,y=fare_amount))+geom_point()+scale_x_log10()


#POSIXlt to seperate hour,weekday,month and year from pickup_datetime
cab_data$pickup_datetime = as.POSIXct(cab_data$pickup_datetime,format="%Y-%m-%d %H:%M:%S",tz='UTC')
data.frame(apply(cab_data,2,function(x){sum(is.na(x))}))
cab_data <- na.omit(cab_data)

str(cab_data)
cab_data$year = as.numeric(format(cab_data$pickup_datetime, "%Y"))
cab_data$month = as.numeric(format(cab_data$pickup_datetime, "%m"))
cab_data$weekday = as.numeric(format(cab_data$pickup_datetime, "%w"))
cab_data$hour = as.numeric(format(cab_data$pickup_datetime, "%H"))

cab_data <-within(cab_data, rm(pickup_datetime,pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))

##################################Training data###############################################
rm("false_coordinates")
rm("false_fares")
rm("missing_val")
rm("high_fare")
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(cab_data), size = floor(.70*nrow(cab_data)), replace = F)
train <- cab_data[sample, ]
test  <- cab_data[-sample, ]

Model_lm1=lm(fare_amount~.,data=train)
summary(Model_lm1)
lin_reg_pred=predict(Model_lm1, test)
RMSE_lin = sqrt(mean((test$fare_amount-lin_reg_pred)^2))
print(paste('RMSE of linear_regression:', RMSE_lin)) 
#RMSE_lin=5.56


library(randomForest)
require(Metrics)
RF_model = randomForest(fare_amount ~., train, importance = TRUE, ntree =1000, mtry=2)
RF_Predictions = predict(RF_model, test)
RMSE_rf = sqrt(mean((test$fare_amount-RF_Predictions)^2))
print(paste('RMSE of Random forest:', RMSE_rf)) 
#RMSE_rf=3.63

#Install Package
#install.packages("e1071")

#Load Library
library(e1071)


#Regression with SVM
modelsvm = svm(fare_amount~.,train,type="nu-regression",kernel="radial")

#Predict using SVM regression
predYsvm = predict(modelsvm,test)

RMSE_svm=rmse(predYsvm,test$fare_amount)
print(paste('RMSE of SVM:', RMSE_svm)) 
#RMSE_svm=3.67






