#### Preamble ####
# Purpose: Code to create model with log transformation covid dummy and ARIMA(021) errors
# This model trains on data up to 2021 noting that 2021 and 2020 are covd. The forecasts are great
# Author: Liam Wall
# Date: 16 June 2025
# Contact: liam.wall@mail.utoronto.ca

#### Workspace Setup #####
library(tidyverse)
library(astsa)
library(forecast)
library(ggplot2)
library(gridExtra)

#### Download Data ####
owid_co2_clean <- read_csv("data/cleaned/owid_co2_clean.csv")
temp <- read_csv("data/cleaned/temp_clean.csv")

# Assuming your full CO2 time series is called co2_ts
co2_ts <- ts(owid_co2_clean[201:274,]$co2, start = 1950)
population <- ts(owid_co2_clean[201:274,]$population, start = 1950)
temp_ts <- ts(temp[101:174,]$global_average_temperature_anomaly_relative_to_1861_1890, start = 1950) # ends in 2025

# Get the years covered by your full CO2 data
full_years <- time(co2_ts)

# Define your training and forecast periods
train_end_year <- 2021 # 2014
forecast_start_year <- 2022 # 2015
forecast_end_year <- 2023 # 2023

# Number of forecast steps (h = 9)
h_forecast <- forecast_end_year - forecast_start_year + 1

# Create the COVID-19 dummy for all years in your full dataset
covid_dummy_full <- numeric(length(full_years)) # Initialize with zeros

# Set to 1 for the relevant COVID-19 impact years: 2020 and 2021
covid_dummy_full[full_years == 2020] <- 1
covid_dummy_full[full_years == 2021] <- 1

# Combine all covariates into a full xreg matrix
full_xreg_matrix <- cbind(
  population = as.numeric(population), # make sure is full series
  covid = covid_dummy_full
)
  
#  as.matrix(owid_co2_clean[201:274,]$population) # 1950 to 2023


# Split your CO2 data and xreg matrix for training
co2_train <- window(co2_ts, end = train_end_year)
xreg_train <- as.matrix(full_xreg_matrix[1:72])

# Verify dimensions (important!)
print(paste("Length of co2_train:", length(co2_train)))
print(paste("Number of rows in xreg_train:", nrow(xreg_train)))


# Fit your ARIMAX model
# Using Arima from the forecast package
# Replace p,d,q with your chosen ARIMA orders (e.g., 1,0,0 as in earlier example)
# IMPORTANT: If co2_ts should be log-transformed and differenced,
# you would fit on log(co2_ts) and include d > 0 in your order.
# For example, if you chose ARIMA(1,1,0) on log-transformed data:
# co2_log_train <- log(co2_train)
# model_arimax_covid <- Arima(co2_log_train, order = c(1,1,0), xreg = xreg_train)
# And then the actual_values_for_table would need to be log-transformed when comparing.

pop_train_ts <- ts(owid_co2_clean[201:272,]$population)
# temp_train_ts <- window(temp_ts, end = train_end_year)
covid_dummy_train_ts <- covid_dummy_full[1:72]
fit_l_covid <- lm(log(co2_train) ~ pop_train_ts + covid_dummy_train_ts)
resid_ts <- ts(as.numeric(resid(fit_l_covid)))
# plot(resid_ts)
# plot(diff(resid_ts, differences = 1))
plot(diff(resid_ts, differences = 2))
acf2(diff(resid_ts, differences = 2))
auto.arima(resid_ts) # We get (021) BIC = -288.61


model_l022_covid <- Arima(log(co2_train), order = c(0,2,1), xreg = xreg_train)

summary(model_l022_covid)
checkresiduals(model_l022_covid)


# Create future values for 'time' covariate
future_years <- forecast_start_year:forecast_end_year
future_time_vals <- as.numeric(future_years)

# For the forecast period (2015-2023), slice the full series of covariates
forecast_xreg_matrix <- as.matrix(full_xreg_matrix[73:74])

# Ensure 'covid' column in forecast_xreg_matrix is set appropriately for future years:
# For 2015-2019: covid = 0
# For 2020: covid = 1
# For 2021: covid = 1
# For 2022, 2023, and beyond: covid = 0 (assuming recovery)
# The full_xreg_matrix already sets this for 2020-2023 correctly based on Step 2.
# If you forecast beyond 2023, you would need to manually extend the covid column with 0s.


# Generate forecasts with the new model and the future xreg matrix
co2_forecast_covid <- forecast(model_l022_covid, xreg = forecast_xreg_matrix, h = h_forecast)

# Plot the new forecasts
par(mfrow=c(1,1))
plot(co2_forecast_covid, main = "Log CO2 Forecast with COVID Dummy ARIMA(021)")

# Overlay actual observed values (ensure actual_observed_co2 is aligned by year)
# Assuming actual_observed_co2 is a vector for years 2015-2023
actual_years <- forecast_start_year:forecast_end_year
actual_observed_log_co2 <- log(owid_co2_clean[273:274,]$co2)
points(x = actual_years, y = actual_observed_log_co2, col = "red", pch = 1, cex = 1.2)
lines(x = actual_years, y = actual_observed_log_co2, col = "red", lwd = 2)

legend("topleft",
       legend = c("Forecast Mean", "Actual Values"),
       col = c("blue", "red"),
       lty = c(1, 1),
       pch = c(NA, 19),
       lwd = c(2, 2))

# Print the new forecast table
exp(data.frame(co2_forecast_covid))
print(owid_co2_clean[273:274,]$co2)
