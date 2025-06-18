library(forecast)

owid_co2_clean <- read_csv("data/cleaned/owid_co2_clean.csv")
black_carbon <- read_csv("data/cleaned/black_carbon_clean.csv")
temp <- read_csv("data/cleaned/temp_clean.csv")

owid_co2_ts <- ts(owid_co2_clean[201:265,]$co2, start = 1950) # 1950 to 2014
owid_co2_per_capita_ts <- ts(owid_co2_clean[201:265,]$co2_per_capita, start = 1950) # 1950 to 2019
black_carbon_ts <- ts(black_carbon[201:270,]$black_carbon_emissions_from_industry, start = 1950) # 1952 to 2019
temp_ts <- ts(temp[101:170,]$global_average_temperature_anomaly_relative_to_1861_1890, start = 1950) # 1952 to 2019
population <- ts(owid_co2_clean[201:265,]$population, start = 1950) # 1950 to 2014

par(mfrow=c(2,2))
plot(owid_co2_ts)
plot(owid_co2_per_capita_ts)
plot(black_carbon_ts)
plot(temp_ts)

fit1 <- lm(owid_co2_ts ~ population, na.action=NULL)

par(mfrow=c(2,1))
plot(owid_co2_ts)
lines(fit1$fitted.values, col = "red")
plot(resid(fit1))

acf2(resid(fit1))

auto.arima(resid(fit1))

model1 <- Arima(owid_co2_ts, order = c(1, 0, 1), xreg = population, include.mean = FALSE)

plot(owid_co2_ts, lwd = 2)
lines(model1$fitted, col = "red", lwd = 2)
lines(fit1$fitted.values, col = "blue", lty = 2, lwd = 1)
plot(resid(model1))
acf2(resid(model1))

h <- 9
last_year <- end(owid_co2_ts)[1] # Last year in your observed data

future_time_vals <- (last_year + 1):(last_year + h)

forecast_population_vals <- owid_co2_clean[266:274,]$population

co2_forecast <- forecast(model1, xreg = forecast_population_vals, h = h)

# Plot the forecast
actual_forecast_co2 <- owid_co2_clean[266:274,]$co2
years <- c(2015:2023)
par(mfrow=c(1,1))
plot(co2_forecast)
points(x = years, y = actual_forecast_co2, col = "red", pch = 19)

# Print the forecast values and prediction intervals
print(co2_forecast)
