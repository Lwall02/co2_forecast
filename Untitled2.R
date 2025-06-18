# Linear Regression
fit2 <- lm(log(owid_co2_ts) ~ population, na.action=NULL)

# Plot the CO2 data with the Fitted Regression Values and the Residuals
par(mfrow=c(2,1))
plot(log(owid_co2_ts))
lines(fit2$fitted.values, col = "red")
plot(resid(fit2))

# ACF and PACF plot of residuals
acf2(resid(fit2))

# Run Auto Arima and confirm choice of ARIMA model for errors
auto.arima(resid(fit2))
model2 <- Arima(log(owid_co2_ts), order = c(0, 2, 2), xreg = population, include.mean = FALSE)

# Plot the CO2 data with FRV and with new model with ARIMA errors and Residuals
plot(log(owid_co2_ts), lwd = 1)
lines(model2$fitted, col = "red", lwd = 1)
lines(fit2$fitted.values, col = "blue", lty = 2, lwd = 1)
plot(resid(model2))

# ACF and PACF plot of residuals of new ARIMA errors model
acf2(resid(model2))

# Forecast
h <- 9
last_year <- end(owid_co2_ts)[1] # Last year in your observed data

future_time_vals <- (last_year + 1):(last_year + h)

forecast_population_vals <- owid_co2_clean[266:274,]$population

co2_forecast2 <- forecast(model2, xreg = forecast_population_vals, h = h)

# Plot the forecast
actual_forecast_co2_2 <- log(owid_co2_clean[266:274,]$co2)
years <- c(2015:2023)
par(mfrow=c(1,1))
plot(co2_forecast2)
points(x = years, y = actual_forecast_co2_2, col = "red", pch = 19)

# Print the forecast values and prediction intervals
print(co2_forecast2)

exp(data.frame(co2_forecast2))

merge(data.frame(co2_forecast)[1], data.frame(co2_forecast2)[1])
data.frame(co2_forecast2)[1]
