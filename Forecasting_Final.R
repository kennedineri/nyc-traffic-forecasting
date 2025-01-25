
######################################################################################################################


#Part 2

install.packages("tidyverse")
install.packages("ggplot2")
library(forecast)
library(ggplot2)
library(tibble)

#Load in dataset
collisions <- read.table("~/Desktop/gdub/NYC_Daily_Collisions.csv", header=TRUE, sep=",")
attach(collisions)

#Plot time series
plot.ts(collisions$COLLISIONS,col="blue")

day1=c(1,2,3,4,5,6,7)
# 1 is Friday

#Create day variable
day=c(rep(day1,104),1,2,3)

#Box-plot of daily collisions
boxplot(collisions$COLLISIONS~day,col="blue")


#Create month variables for both years
month1=c(rep(1,31),rep(2,29), rep(3,31),rep(4,30),rep(5,31),rep(6,30),
         rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))

month2=c(rep(1,31),rep(2,28), rep(3,31),rep(4,30),rep(5,31),rep(6,30),
         rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))

month=c(month1,month2)

#Box-Plot of monthly collisions
boxplot(collisions$COLLISIONS~month,col="blue")

#Create holdout sample of 131 and training sample of 600
train_collisions <- head(collisions, 600)
holdout_collisions <- tail(collisions,131)

#Create time variables
train_time <- seq(1, nrow(train_collisions))
holdout_time <- seq(1, nrow(holdout_collisions))

# Periodogram for Collision Series 
library(TSA)

#removing trend
detrend<-lm(train_collisions$COLLISIONS~train_time)

#creating periodgram
prdgrm=periodogram(detrend$residuals,col="blue")

#calculate periods
period=1/prdgrm$freq

#finding fundamental index (index of frequency with max power)
fundamental_index <- which.max(prdgrm$spec)
fundamental_index

# Iterate over all indices and print them along with their values
harmonics <- seq_along(prdgrm$spec)

# Compute harmonic frequencies
#Plot periodogram
par(mfrow=c(1,2))
periodogram(detrend$residuals,col="blue")
plot(period,prdgrm$spec, type="h",col="blue",ylab="Peridogram",lwd=2)

#sort by amplitude
frequency=prdgrm$freq
amplitude=prdgrm$spec
all=cbind(harmonics,period,frequency,amplitude)
all

# Ordering by amplitude
newall=all[order(-all[,4]),]
newall[1:10,]

#Seasonal Dummies Model

#Run seasonal dummies model on both month and day dummies
nday = as.factor(day[1:600])
nmonth = as.factor(month[1:600])
holdout_nday = as.factor(day[601:731])
holdout_nmonth = as.factor(month[601:731])

#Fit daily model
dummies_model <- lm(COLLISIONS ~ train_time + nday + nmonth, data=train_collisions)
summary(dummies_model)
plot.ts(train_collisions$COLLISIONS, type="b",col="blue")
lines(exp(predict(dummies_model)),col="red")
par(mfrow = c(1, 1))
dummies_residuals <- resid(dummies_model)
acf(dummies_residuals, main="ACF of Dummies Residuals")

dummies_pred <- predict(dummies_model, data.frame(train_time=holdout_time, nday=holdout_nday, nmonth=holdout_nmonth, newdata = holdout_collisions)) 

dummies_MAPE=mean(abs(collisions$COLLISIONS[601:731]-dummies_pred)/collisions$COLLISIONS[601:731])
dummies_MAPE

days <- 1:nrow(holdout_collisions)
dummies_plot_data <- data.frame(Day = days, Actual = holdout_collisions$COLLISIONS, Predicted = dummies_pred)

ggplot(dummies_plot_data, aes(x = Day)) +
  geom_line(aes(y = Actual, colour = "Actual")) +
  geom_line(aes(y = Predicted, colour = "Predicted")) +
  labs(colour = "Legend") +
  ggtitle("Actual vs Predicted Daily Collisions Over Time") + 
  xlab("Day") +
  ylab("Number of Collisions")

#Cyclical Model

#Cosine & Sine Terms using harmonics
cos2=cos(2*pi*(86/600)*train_time)
sin2=sin(2*pi*(86/600)*train_time)
cos3=cos(2*pi*(171/600)*train_time)
sin3=sin(2*pi*(171/600)*train_time)
cos4=cos(2*pi*(172/600)*train_time)
sin4=sin(2*pi*(172/600)*train_time)
cos5=cos(2*pi*(2/600)*train_time)
sin5=sin(2*pi*(2/600)*train_time)
cos6=cos(2*pi*(85/600)*train_time)
sin6=sin(2*pi*(85/600)*train_time)
cos7=cos(2*pi*(3/600)*train_time)
sin7=sin(2*pi*(3/600)*train_time)
cos8=cos(2*pi*(1/600)*train_time)
sin8=sin(2*pi*(1/600)*train_time)

cyclical_model <-lm(COLLISIONS ~ train_time+cos2+sin2+cos3+sin3+cos4+sin4+cos5+sin5+
                      cos6+sin6+cos7+sin7+cos8+sin8, data=train_collisions)
summary(cyclical_model)
par(mfrow = c(1, 1))
plot.ts(train_collisions$COLLISIONS, type="b",col="blue",ylab="collisions")
lines(exp(predict(cyclical_model)),col="red",lwd=2)
par(mfrow = c(1, 1))
cyclical_residuals <- resid(cyclical_model)
acf(cyclical_residuals, main="ACF of Cyclical Residuals")

cos2_n=cos(2*pi*(86/600)*holdout_time)
sin2_n=sin(2*pi*(86/600)*holdout_time)
cos3_n=cos(2*pi*(171/600)*holdout_time)
sin3_n=sin(2*pi*(171/600)*holdout_time)
cos4_n=cos(2*pi*(172/600)*holdout_time)
sin4_n=sin(2*pi*(172/600)*holdout_time)
cos5_n=cos(2*pi*(2/600)*holdout_time)
sin5_n=sin(2*pi*(2/600)*holdout_time)
cos6_n=cos(2*pi*(85/600)*holdout_time)
sin6_n=sin(2*pi*(85/600)*holdout_time)
cos7_n=cos(2*pi*(3/600)*holdout_time)
sin7_n=sin(2*pi*(3/600)*holdout_time)
cos8_n=cos(2*pi*(2/600)*holdout_time)
sin8_n=sin(2*pi*(2/600)*holdout_time)

cyclical_pred=predict(cyclical_model,data.frame(train_time=holdout_time,cos2=cos2_n,sin2=sin2_n,cos3=cos3_n,sin3=sin3_n,
                                                cos4=cos4_n,sin4=sin4_n,cos5=cos5_n,sin5=sin5_n, cos6=cos6_n,sin6=sin6_n,
                                                cos7=cos7_n,sin7=sin7_n, cos8=cos8_n,sin8=sin8_n))                

cyclical_MAPE=mean(abs(collisions$COLLISIONS[601:731]-cyclical_pred)/collisions$COLLISIONS[601:731])
cyclical_MAPE

cyclical_plot_data <- data.frame(Day = days, Actual = holdout_collisions$COLLISIONS, Predicted = cyclical_pred)

ggplot(cyclical_plot_data, aes(x = Day)) +
  geom_line(aes(y = Actual, colour = "Actual")) +
  geom_line(aes(y = Predicted, colour = "Predicted")) +
  labs(colour = "Legend") +
  ggtitle("Actual vs Predicted Daily Collisions Over Time") + 
  xlab("Day") +
  ylab("Number of Collisions")

##########################################################################################

#Part 3
data <- read.csv('/Users/connorrodgers/Desktop/NYC_Daily_Collisions.csv', header = TRUE)

#3.1

# Ensure data types are correct
data$DATE <- as.Date(data$DATE, format="%m/%d/%Y")  # Ensure DATE is in Date format
data$AWND <- as.numeric(data$AWND)
data$TMAX <- as.numeric(data$TMAX)
data$TMIN <- as.numeric(data$TMIN)
data$PRCP <- as.numeric(data$PRCP)

# Plot COLLISIONS vs. AWND (Wind Speed)
ggplot(data, aes(x=AWND, y=COLLISIONS)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm", color="blue") +
  labs(title="Collisions vs. Wind Speed",
       x="Average Wind Speed (m/s)", y="Number of Collisions")

# Plot COLLISIONS vs. TMAX (Maximum Temperature)
ggplot(data, aes(x=TMAX, y=COLLISIONS)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm", color="red") +
  labs(title="Collisions vs. Maximum Temperature",
       x="Maximum Temperature (°C)", y="Number of Collisions")

# Plot COLLISIONS vs. TMIN (Minimum Temperature)
ggplot(data, aes(x=TMIN, y=COLLISIONS)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm", color="green") +
  labs(title="Collisions vs. Minimum Temperature",
       x="Minimum Temperature (°C)", y="Number of Collisions")

# Plot COLLISIONS vs. PRCP (Precipitation)
ggplot(data, aes(x=PRCP, y=COLLISIONS)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm", color="purple") +
  labs(title="Collisions vs. Precipitation",
       x="Precipitation (mm)", y="Number of Collisions")

# Categorizing the precipitation into categories
data$Rain_Category <- cut(data$PRCP,
                          breaks = c(-Inf, 0, 10, 30, 100, Inf),
                          labels = c("No Rain", "Light Rain", "Moderate Rain", "Heavy Rain", "Very Heavy Rain"),
                          right = TRUE)

# Removing the 'Very Heavy Rain' category if not needed
data$Rain_Category <- factor(data$Rain_Category, levels = levels(data$Rain_Category)[-5])

# Plotting COLLISIONS vs. Rain Category
ggplot(data, aes(x=Rain_Category, y=COLLISIONS)) +
  geom_jitter(alpha=0.5, width=0.2) +  # Using jitter to better visualize overlapping points
  geom_boxplot(outlier.shape = NA, alpha=0.3, fill = "lightblue", color = "black") +  # Overlaying a boxplot
  labs(title="Collisions vs. Rain Intensity",
       x="Rain Intensity", y="Number of Collisions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Improve x-axis labels readability

# Create the overlay plot with adjusted wind speed
ggplot() +
  geom_line(data = data, aes(x = DATE, y = COLLISIONS, color = "Collisions"), size = 1) +
  geom_line(data = data, aes(x = DATE, y = AWND * 50, color = "Wind Speed"), size = 1) +
  labs(title = "Time Series of Collisions and Wind Speed (Adjusted)",
       x = "Date", y = "Count / Adjusted Wind Speed (m/s * 50)") +
  scale_color_manual(name = "Legend", values = c("Collisions" = "blue", "Wind Speed" = "red")) +
  theme_minimal()

# Create the overlay plot with adjusted precipitation
ggplot() +
  geom_line(data = data, aes(x = DATE, y = COLLISIONS, color = "Collisions"), size = 1) +
  geom_line(data = data, aes(x = DATE, y = PRCP * 8, color = "Precipitation"), size = 1) +
  labs(title = "Time Series of Collisions and Precipitation (Adjusted)",
       x = "Date", y = "Count / Adjusted Precipitation (mm * 8)") +
  scale_color_manual(name = "Legend", values = c("Collisions" = "blue", "Precipitation" = "green")) +
  theme_minimal()

# Create the overlay plot with adjusted minimum temperature
ggplot() +
  geom_line(data = data, aes(x = DATE, y = COLLISIONS, color = "Collisions"), size = 1) +
  geom_line(data = data, aes(x = DATE, y = TMIN * 10, color = "Minimum Temperature"), size = 1) +
  labs(title = "Time Series of Collisions and Minimum Temperature (Adjusted)",
       x = "Date", y = "Count / Adjusted Minimum Temperature (°C * 10)") +
  scale_color_manual(name = "Legend", values = c("Collisions" = "blue", "Minimum Temperature" = "orange")) +
  theme_minimal()

# Create the overlay plot with adjusted maximum temperature
ggplot() +
  geom_line(data = data, aes(x = DATE, y = COLLISIONS, color = "Collisions"), size = 1) +
  geom_line(data = data, aes(x = DATE, y = TMAX * 10, color = "Maximum Temperature"), size = 1) +
  labs(title = "Time Series of Collisions and Maximum Temperature (Adjusted)",
       x = "Date", y = "Count / Adjusted Maximum Temperature (°C * 10)") +
  scale_color_manual(name = "Legend", values = c("Collisions" = "blue", "Maximum Temperature" = "red")) +
  theme_minimal()

#3.2
# Load necessary libraries
library(dplyr)

# Assuming 'data' is your full dataset
set.seed(123)  # For reproducibility
data <- data %>% mutate(DATE = as.Date(DATE, format="%m/%d/%Y"))
data$AWND[is.na(data$AWND)] <- mean(data$AWND, na.rm = TRUE)  # Handling missing data for AWND

# Splitting data into training and testing sets
training_indices <- sample(1:nrow(data), 600)
training_data <- data[training_indices, ]
testing_data <- data[-training_indices, ]

# Model with all variables
model_all <- lm(COLLISIONS ~ AWND + TMAX + TMIN + PRCP, data=training_data)

# Models with three variables
model_AWND_TMAX_TMIN <- lm(COLLISIONS ~ AWND + TMAX + TMIN, data=training_data)
model_AWND_TMAX_PRCP <- lm(COLLISIONS ~ AWND + TMAX + PRCP, data=training_data)
model_AWND_TMIN_PRCP <- lm(COLLISIONS ~ AWND + TMIN + PRCP, data=training_data)
model_TMAX_TMIN_PRCP <- lm(COLLISIONS ~ TMAX + TMIN + PRCP, data=training_data)

# Models with two variables
model_AWND_TMAX <- lm(COLLISIONS ~ AWND + TMAX, data=training_data)
model_AWND_TMIN <- lm(COLLISIONS ~ AWND + TMIN, data=training_data)
model_AWND_PRCP <- lm(COLLISIONS ~ AWND + PRCP, data=training_data)
model_TMAX_TMIN <- lm(COLLISIONS ~ TMAX + TMIN, data=training_data)
model_TMAX_PRCP <- lm(COLLISIONS ~ TMAX + PRCP, data=training_data)
model_TMIN_PRCP <- lm(COLLISIONS ~ TMIN + PRCP, data=training_data)

# Model with single variables
model_AWND <- lm(COLLISIONS ~ AWND, data=training_data)
model_TMAX <- lm(COLLISIONS ~ TMAX, data=training_data)
model_TMIN <- lm(COLLISIONS ~ TMIN, data=training_data)
model_PRCP <- lm(COLLISIONS ~ PRCP, data=training_data)

# Function to calculate Mean Squared Error (MSE)
calculate_mse <- function(model, data) {
  predictions <- predict(model, newdata = data)
  mse <- mean((data$COLLISIONS - predictions)^2)
  return(mse)
}


# Calculating MSE for each model on testing data
mse_all <- calculate_mse(model_all, testing_data)
mse_AWND_TMAX_TMIN <- calculate_mse(model_AWND_TMAX_TMIN, testing_data)
mse_AWND_TMAX_PRCP <- calculate_mse(model_AWND_TMAX_PRCP, testing_data)
mse_AWND_TMIN_PRCP <- calculate_mse(model_AWND_TMIN_PRCP, testing_data)
mse_TMAX_TMIN_PRCP <- calculate_mse(model_TMAX_TMIN_PRCP, testing_data)

mse_AWND_TMAX <- calculate_mse(model_AWND_TMAX, testing_data)
mse_AWND_TMIN <- calculate_mse(model_AWND_TMIN, testing_data)
mse_AWND_PRCP <- calculate_mse(model_AWND_PRCP, testing_data)
mse_TMAX_TMIN <- calculate_mse(model_TMAX_TMIN, testing_data)
mse_TMAX_PRCP <- calculate_mse(model_TMAX_PRCP, testing_data)
mse_TMIN_PRCP <- calculate_mse(model_TMIN_PRCP, testing_data)

mse_AWND <- calculate_mse(model_AWND, testing_data)
mse_TMAX <- calculate_mse(model_TMAX, testing_data)
mse_TMIN <- calculate_mse(model_TMIN, testing_data)
mse_PRCP <- calculate_mse(model_PRCP, testing_data)

# Print MSEs
print(list(
  mse_all = mse_all,
  mse_AWND_TMAX_TMIN = mse_AWND_TMAX_TMIN,
  mse_AWND_TMAX_PRCP = mse_AWND_TMAX_PRCP,
  mse_AWND_TMIN_PRCP = mse_AWND_TMIN_PRCP,
  mse_TMAX_TMIN_PRCP = mse_TMAX_TMIN_PRCP,
  mse_AWND_TMAX = mse_AWND_TMAX,
  mse_AWND_TMIN = mse_AWND_TMIN,
  mse_AWND_PRCP = mse_AWND_PRCP,
  mse_TMAX_TMIN = mse_TMAX_TMIN,
  mse_TMAX_PRCP = mse_TMAX_PRCP,
  mse_TMIN_PRCP = mse_TMIN_PRCP,
  mse_AWND = mse_AWND,
  mse_TMAX = mse_TMAX,
  mse_TMIN = mse_TMIN,
  mse_PRCP = mse_PRCP
))


# Load necessary libraries
library(forecast)

# Ensure DATE is a proper Date type, if not already
training_data$DATE <- as.Date(training_data$DATE, format="%m/%d/%Y")
testing_data$DATE <- as.Date(testing_data$DATE, format="%m/%d/%Y")

# Fit ARIMA model with specified order and external regressor
arima_model <- Arima(training_data$COLLISIONS,
                     order = c(0,0,0),  # Specifying the non-seasonal part as (0,0,0)
                     seasonal = list(order = c(0,0,0), period = 7),  # Specifying the seasonal component
                     xreg = training_data$TMAX)  # Including TMAX as an external regressor

# Summary of the model
summary(arima_model)

# Forecast on testing data
forecast_data <- forecast(arima_model, xreg = testing_data$TMAX)
print(forecast_data)

#Plot the forecast against actual testing data
library(ggplot2)
ggplot() +
  geom_line(aes(x = testing_data$DATE, y = testing_data$COLLISIONS), color = "blue", size = 1) +
  geom_line(aes(x = testing_data$DATE, y = forecast_data$mean), color = "red", size = 1) +
  labs(title = "ARIMA Forecast vs Actual Collisions",
       x = "Date", y = "Collisions") +
  theme_minimal()


#3.3
# Generate predictions for the testing data
predictions_TMAX <- predict(model_TMAX, newdata = testing_data)

# Calculate residuals for the testing data
residuals_TMAX <- testing_data$COLLISIONS - predictions_TMAX

# Adding residuals to the testing data frame for plotting
testing_data$residuals_TMAX <- residuals_TMAX

# Creating the residual plot using ggplot2
library(ggplot2)
ggplot(testing_data, aes(x = TMAX, y = residuals_TMAX)) +
  geom_point(alpha=0.5) +
  geom_hline(yintercept = 0, linetype="dashed", color = "red") +
  labs(title = "Residuals of Collisions Model Using TMAX on Testing Data",
       x = "Maximum Temperature (°C)", y = "Residuals") +
  theme_minimal()

# Generate predictions for the testing data
predictions_AWND_TMAX <- predict(model_AWND_TMAX, newdata = testing_data)

# Calculate residuals for the testing data
residuals_AWND_TMAX <- testing_data$COLLISIONS - predictions_AWND_TMAX

# Adding residuals to the testing data frame for plotting
testing_data$residuals_AWND_TMAX <- residuals_AWND_TMAX

# Creating the residual plot using ggplot2
library(ggplot2)
ggplot(testing_data, aes(x = TMAX, y = residuals_AWND_TMAX)) +
  geom_point(alpha=0.5) +
  geom_hline(yintercept = 0, linetype="dashed", color = "red") +
  labs(title = "Residuals of Collisions Model Using AWND and TMAX on Testing Data",
       x = "Maximum Temperature (°C)", y = "Residuals") +
  theme_minimal()

# Generate predictions for the testing data
predictions_TMAX_TMIN_PRCP <- predict(model_TMAX_TMIN_PRCP, newdata = testing_data)

# Calculate residuals for the testing data
residuals_TMAX_TMIN_PRCP <- testing_data$COLLISIONS - predictions_TMAX_TMIN_PRCP

# Adding residuals to the testing data frame for plotting
testing_data$residuals_TMAX_TMIN_PRCP <- residuals_TMAX_TMIN_PRCP

# Creating the residual plot using ggplot2
library(ggplot2)
ggplot(testing_data, aes(x = TMAX, y = residuals_TMAX_TMIN_PRCP)) +
  geom_point(alpha=0.5) +
  geom_hline(yintercept = 0, linetype="dashed", color = "red") +
  labs(title = "Residuals of Collisions Model Using TMAX, TMIN, and PRCP on Testing Data",
       x = "Maximum Temperature (°C)", y = "Residuals") +
  theme_minimal()

##########################################################################################

#Part 4

library(readr)
library(lubridate)
library(dplyr)
library(stats)

collisions_tibble <- as_tibble(collisions)
collisions_ts <- ts(collisions_tibble$COLLISIONS, frequency=365) 

# 4.1


# 4.2

# Fit the ARIMA model
regar <- Arima(collisions_ts, order = c(1, 0, 1), seasonal = list(order = c(0, 1, 1), period = 7), xreg = TMAX)

# Output the summary of the model
summary(regar)

# Residual Analysis ARIMA 
acf(regar$residuals, col="blue", lag.max = 36)
pacf(regar$residuals, col="blue", lag.max = 36)
Box.test(regar$residuals, lag = 36, type = "Ljung-Box")

# 4.3
TMAX_train <- TMAX[1:600]
regar2 <- Arima(train_collisions$COLLISIONS, order = c(1, 0, 1), seasonal = list(order = c(0, 1, 1), period = 7), xreg = TMAX_train)
summary(regar2)

# Residual Analysis ARIMA 
acf(regar2$residuals, col="blue", lag.max = 36)
pacf(regar2$residuals, col="blue", lag.max = 36)
Box.test(regar2$residuals, lag = 36, type = "Ljung-Box")