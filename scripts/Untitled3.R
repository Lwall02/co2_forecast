# Assuming your full CO2 per capita time series is called co2_capita_ts
co2_capita_ts <- ts(owid_co2_clean[201:274,]$co2, start = 1950, frequency = 1)

# Get the years covered by your full CO2 data
full_years <- time(co2_capita_ts)

# Define your training and forecast periods
train_end_year <- 2014
forecast_start_year <- 2015
forecast_end_year <- 2023 # or the last year of your actual_observed_co2

# Number of forecast steps (h)
h_forecast <- forecast_end_year - forecast_start_year + 1

# Create the COVID-19 dummy for all years in your full dataset
covid_dummy_full <- numeric(length(full_years)) # Initialize with zeros

# Set to 1 for the relevant COVID-19 impact years
# Common choice: 2020 and 2021
covid_dummy_full[full_years == 2020] <- 1
covid_dummy_full[full_years == 2021] <- 1
# You might consider 2022 as well if you believe a lingering effect was present,
# but for the main shock, 2020-2021 is usually sufficient for annual data.
# covid_dummy_full[full_years == 2022] <- 1

# Combine all covariates into a full xreg matrix
# Ensure avg_temp_ts2 is correctly derived if it's avg_temp_ts^2, etc.
full_xreg_matrix <- as.matrix(ts(owid_co2_clean[201:265,]$population, start = 1950)) # 1950 to 2014


# Split your CO2 data and xreg matrix for training
co2_train <- window(co2_capita_ts, end = train_end_year)
xreg_train <- window(full_xreg_matrix, end = train_end_year)

# Verify dimensions (important!)
print(paste("Length of co2_train:", length(co2_train)))
print(paste("Number of rows in xreg_train:", nrow(xreg_train)))


# Fit your ARIMAX model
# Using Arima from the forecast package
library(forecast)

# Replace p,d,q with your chosen ARIMA orders (e.g., 1,0,0 as in earlier example)
# IMPORTANT: If co2_capita_ts should be log-transformed and differenced,
# you would fit on log(co2_capita_ts) and include d > 0 in your order.
# For example, if you chose ARIMA(1,1,0) on log-transformed data:
# co2_log_train <- log(co2_train)
# model_arimax_covid <- Arima(co2_log_train, order = c(1,1,0), xreg = xreg_train)
# And then the actual_values_for_table would need to be log-transformed when comparing.

# Based on your last model description (co2_capita not log-transformed, d=0 implied)
model_arimax_covid <- Arima(co2_train, order = c(1,0,1), xreg = xreg_train)

summary(model_arimax_covid)
checkresiduals(model_arimax_covid)


# Create future values for 'time' covariate
future_years <- forecast_start_year:forecast_end_year
future_time_vals <- as.numeric(future_years)

# You need future values for avg_temp_ts, avg_temp_ts2, and lblack_carbon_ts
# These should come from external forecasts or univariate models (as discussed before)
# For demonstration, I'll use placeholders, but you MUST replace these:
# You'd need to extend your existing avg_temp_ts, avg_temp_ts2, lblack_carbon_ts to cover these years.
# E.g., if you have data up to 2023, you would slice it. If forecasting beyond 2023, you need projections.

# For the forecast period (2015-2023), slice the full series of covariates
# Or extend them if forecasting beyond the observed range of full_xreg_matrix
full_xreg_matrix2 <- as.matrix(ts(owid_co2_clean[266:274,]$population, start = 2015)) # 1950 to 2014
forecast_xreg_matrix <- window(full_xreg_matrix2)

# Ensure 'covid' column in forecast_xreg_matrix is set appropriately for future years:
# For 2015-2019: covid = 0
# For 2020: covid = 1
# For 2021: covid = 1
# For 2022, 2023, and beyond: covid = 0 (assuming recovery)
# The full_xreg_matrix already sets this for 2020-2023 correctly based on Step 2.
# If you forecast beyond 2023, you would need to manually extend the covid column with 0s.


# Generate forecasts with the new model and the future xreg matrix
co2_forecast_covid <- forecast(model_arimax_covid, xreg = forecast_xreg_matrix, h = h_forecast)

# Plot the new forecasts
plot(co2_forecast_covid, main = "CO2 per capita Forecast with COVID Dummy")

# Overlay actual observed values (ensure actual_observed_co2 is aligned by year)
# Assuming actual_observed_co2 is a vector for years 2015-2023
actual_years <- forecast_start_year:forecast_end_year
actual_observed_co2 <- owid_co2_clean[266:274,]$co2
points(x = actual_years, y = actual_observed_co2, col = "red", pch = 19, cex = 1.2)
lines(x = actual_years, y = actual_observed_co2, col = "red", lwd = 2)

legend("topleft",
       legend = c("Forecast Mean", "Actual Values"),
       col = c("blue", "red"),
       lty = c(1, 1),
       pch = c(NA, 19),
       lwd = c(2, 2))

# Print the new forecast table
print(co2_forecast_covid)
