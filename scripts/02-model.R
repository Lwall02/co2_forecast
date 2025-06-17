#### Preamble ####
# Purpose: Code to create models used in the paper
# Author: Liam Wall
# Date: 16 June 2025
# Contact: liam.wall@mail.utoronto.ca

#### Workspace Setup #####
library(tidyverse)
library(astsa) # the data are stored in the package
library(forecast)
library(ggplot2)
library(gridExtra)

#### Download Data ####
owid_co2_clean <- read_csv("data/cleaned/owid_co2_clean.csv")
black_carbon <- read_csv("data/cleaned/black_carbon_clean.csv")
temp <- read_csv("data/cleaned/temp_clean.csv")

#### Model ####
# edit to the right time indexes
# edited to end in 2022. starts in 1950 and is differenced twice
owid_co2_per_capita_ts <- ts(owid_co2_clean[201:273,]$co2_per_capita, start = 1950) 
# ends in 2022 and starts in 1951 because only 1 difference
black_carbon_ts <- ts(black_carbon[203:273,]$black_carbon_emissions_from_industry, start = 1952) 
# ends in 2022 and starts in 1950 only gets logged
temp_ts <- ts(temp[103:173,]$global_average_temperature_anomaly_relative_to_1861_1890, start = 1952) 


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
plot(co2_capita)
plot(time)
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
plot(dlblack_carbon)

co2_capita <- ts(owid_co2_clean[203:273,]$co2_per_capita, start = 1952)
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

