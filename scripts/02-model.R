#### Preamble ####
# Purpose: Code to create models used in the paper
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
black_carbon <- read_csv("data/cleaned/black_carbon_clean.csv")
temp <- read_csv("data/cleaned/temp_clean.csv")

#### Model ####
# edit to the right time indexes. they end at 2019 because I have values up to 2022 and will use 2020, 2021, and 2022 for forecasting
# edited to end in 2019. starts in 1950 and is differenced twice
owid_co2_per_capita_ts <- ts(owid_co2_clean[201:270,]$co2_per_capita, start = 1950) 
# ends in 2019 and starts in 1951 because only 1 difference
black_carbon_ts <- ts(black_carbon[203:270,]$black_carbon_emissions_from_industry, start = 1952) 
# ends in 2019 and starts in 1950 only gets logged
temp_ts <- ts(temp[103:170,]$global_average_temperature_anomaly_relative_to_1861_1890, start = 1952) 


par(mfrow=c(2,1))
plot(diff(owid_co2_per_capita_ts, differences = 1))
plot(diff(owid_co2_per_capita_ts, differences = 2))

mean(diff(owid_co2_per_capita_ts, differences = 1))
mean(diff(owid_co2_per_capita_ts, differences = 2))

plot(black_carbon_ts)
plot(diff(log(black_carbon_ts)))
plot(diff(log(black_carbon_ts))**2)


ddco2_capita = diff(owid_co2_per_capita_ts, differences = 2)
time = time(ddco2_capita)
lblack_carbon = log(black_carbon_ts)
dlblack_carbon = diff(log(black_carbon_ts))
dlblack_carbon2 = diff(log(black_carbon_ts))**2

par(mfrow=c(2,2))
plot(black_carbon_ts)
plot(lblack_carbon)
plot(dlblack_carbon)
plot(dlblack_carbon2)


avg_temp_ts = temp_ts - mean(temp_ts)
avg_temp_ts2 = avg_temp_ts**2

par(mfrow=c(2,2))
plot(ddco2_capita)
plot(temp_ts)
plot(avg_temp_ts)
plot(avg_temp_ts2)

# In the model
par(mfrow=c(2,2))
plot(co2_capita)
plot(avg_temp_ts)
plot(avg_temp_ts2)
plot(lblack_carbon)

co2_capita <- ts(owid_co2_clean[203:270,]$co2_per_capita, start = 1952)
fit_lm <- lm(co2_capita ~ time + avg_temp_ts + avg_temp_ts2 + lblack_carbon, na.action=NULL)

par(mfrow=c(2,1))
plot(co2_capita)
lines(fit_lm$fitted.values, col = "red")
plot(resid(fit_lm))

acf2(resid(fit_lm))

co2_ar_error <- sarima(co2_capita, 1,0,0, xreg=cbind(time,avg_temp_ts,avg_temp_ts2,lblack_carbon))
co2_ar_error$fit

fitted_values <- (co2_capita - co2_ar_error$fit$residuals)

plot(co2_capita, lwd = 2)
lines(fitted_values, col = "red", lwd = 2)
lines(fit_lm$fitted.values, col = "blue", lty = 2, lwd = 1)




#### another way
# time = time, avg_temp = avg_temp_ts, avg_temp2 = avg_temp_ts2, lblack_carbon = lblack_carbon
# From correlation matrix 
xreg_matrix <- cbind(time = time, avg_temp = avg_temp_ts, avg_temp2 = avg_temp_ts2, lblack_carbon = lblack_carbon)

# Fit the ARIMA(1,0,0) model with xreg
# If you intended a seasonal component (SARIMA), you would add seasonal = c(P,D,Q) and period
# For a purely non-seasonal ARIMA(1,0,0) as in your sarima call:
model_arimax <- Arima(co2_capita, order = c(1, 0, 0), 
                      xreg = cbind(time = time, avg_temp = avg_temp_ts, avg_temp2 = avg_temp_ts2, lblack_carbon = lblack_carbon))

# View model summary
summary(model_arimax)
checkresiduals(model_arimax)

# Example: Creating dummy future xreg values (replace with your actual forecasts/data)
# Let's say you want to forecast 10 years (h=10)
h <- 3
last_year <- end(co2_capita)[1] # Last year in your observed data

future_time_vals <- (last_year + 1):(last_year + h)

forecast_temps <- temp[171:173,]$global_average_temperature_anomaly_relative_to_1861_1890
forecast_avg_temp_vals <- forecast_temps - mean(temp_ts)
forecast_avg_temp2_vals <- forecast_avg_temp_vals^2 
forecast_lblack_carbon_vals <- log(black_carbon[271:273,]$black_carbon_emissions_from_industry)


# Combine into a matrix for forecasting
forecast_xreg_matrix <- cbind(time = future_time_vals, avg_temp = forecast_avg_temp_vals,
                              avg_temp2 = forecast_avg_temp2_vals, lblack_carbon = forecast_lblack_carbon_vals)

# Generate forecasts using the model and future xreg values
co2_forecast <- forecast(model_arimax, xreg = forecast_xreg_matrix, h = h)

# Plot the forecast
actual_forecast_co2_capita <- owid_co2_clean[271:273,]$co2_per_capita
years <- c(2020, 2021, 2022)
par(mfrow=c(1,1))
plot(co2_forecast)
points(x = years, y = actual_forecast_co2_capita, col = "red", pch = 19)

# Print the forecast values and prediction intervals
print(co2_forecast)

forecast_table <- as.data.frame(co2_forecast)
forecast_table$Year <- as.numeric(rownames(forecast_table))
actuals_df <- data.frame(
  Year = years,
  Actual = actual_forecast_co2_capita
)
combined_forecast_table <- merge(
  forecast_table,
  actuals_df,
  by = "Year",
  all.x = TRUE # Keep all forecast rows, add actuals where years match
)
print(combined_forecast_table, row.names = FALSE, digits = 4)

# Assuming your covariates are in a data frame or matrix 'my_covariates_df'
my_covariates_df <- cbind(co2_capita, lblack_carbon, log(co2_capita))
cor(my_covariates_df) # Pearson correlation matrix

# For two specific covariates, e.g., avg_temp_ts and lblack_carbon_ts
ccf((co2_capita), time)