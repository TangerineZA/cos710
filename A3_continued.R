library(Metrics)

print("Loading in dataset...")
training_dataset <- read.csv("testing_cleaned.csv")

Duration <- c(training_dataset$Duration)
Distance <- c(training_dataset$Distance)
Pickup_longitude <- c(training_dataset$Pickup_longitude)
Pickup_latitude <- c(training_dataset$Pickup_latitude)
Haversine <- c(training_dataset$Haversine)
Pmonth <- c(training_dataset$PMonth)
Pickup_day <- c(training_dataset$Pickup_day)
Pickup_hour <- c(training_dataset$Pickup_hour)
Pickup_minute <- c(training_dataset$Pickup_minute)
Pickup_weekday <- c(training_dataset$Pickup_weekday)
Dropoff_hour <- c(training_dataset$Dropoff_hour)
Dropoff_minute <- c(training_dataset$Dropoff_minute)
Temp <- c(training_dataset$Temp)
Precip <- c(training_dataset$Precip)
Wind <- c(training_dataset$Wind)
Humid <- c(training_dataset$Humid)
Solar <- c(training_dataset$Solar)
Snow <- c(training_dataset$Snow)
Dust <- c(training_dataset$Dust)

expr <- function(x) {
   x * 3 * 3
}

pred_y_list <- lapply(Haversine, expr)
# pred_y_list <- readRDS("pred_y.rds")
pred_y <- unlist(pred_y_list)
# saveRDS(pred_y, file = "pred_y.rds")

mae_train <- mae(Duration, pred_y)
print("mae")
print(mae_train)

rmse_train <- rmse(Duration, pred_y)
print("rmse")
print(rmse_train)

rsq <- function(x, y) cor(x, y) ^ 2
r2_train <- rsq(Duration, pred_y)
print("rsq")
print(r2_train)

medae_func <- function(x, y) {
   result <- (median((abs((Duration - pred_y)))))
   return(result)
}
medae_train <- medae_func(Duration, pred_y)
print("medae")
print(medae_train)