#### Preamble ####
# Purpose: Downloads and saves the raw datasets
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
co2_raw <- read_csv(here::here("data/raw/owid_co2_data.csv"))
black_carbon <- read_csv(here::here("data/raw/black-carbon-emissions-from-industry.csv"))
temp <- read_csv(here::here("data/raw/global-warming-annual-temperature-anomaly.csv"))

#### Clean Data ####
owid_co2_clean <- co2_raw |> 
  select(country, year, iso_code, population, gdp, co2, co2_per_capita, co2_per_gdp) |>
  filter(country == "World")
black_carbon <- black_carbon |> 
  clean_names() |>
  filter(entity == "World")
temp <- temp |> clean_names()

#### Save Data ####
# write_csv(owid_co2_clean, "data/cleaned/owid_co2_clean.csv")
# write_csv(black_carbon, "data/cleaned/black_carbon_clean.csv")
# write_csv(temp, "data/cleaned/temp_clean.csv")

#### Make Time Series #### The default ones
owid_co2_ts <- ts(owid_co2_clean$co2, start = 1750) # ends 2023
owid_co2_per_capita_ts <- ts(owid_co2_clean[201:274,]$co2_per_capita, start = 1950) # ends 2023
black_carbon_ts <- ts(black_carbon$black_carbon_emissions_from_industry, start = 1750) # ends in 2022
temp_ts <- ts(temp$global_average_temperature_anomaly_relative_to_1861_1890, start = 1850) # ends in 2025

#### Plot Time Series ####
par(mfrow=c(2,1))
plot(owid_co2_ts)
plot(owid_co2_per_capita_ts)

#### Linear Regression on Time Series ####
co2_gr = diff(log(owid_co2_ts))
log_co2 = log(owid_co2_ts)
trend_log = trend(log_co2)
trend_log2 = trend_log**2
trend = trend(co2_gr)
time = time(co2_gr)
fit_lm = lm(log_co2 ~ trend_log + trend_log2, na.action=NULL)
plot(log_co2)
lines(fit_lm$fitted.values, col = "red")
plot(resid(fit_lm))


plot(cmort)
trend = time(cmort)
temp = tempr - mean(tempr)
temp2 = temp^2

par(mfrow=c(2,2))
plot(cmort)
plot(part)
plot(temp)
plot(temp2)

summary(fit <- lm(cmort~trend + temp + temp2 + part, na.action=NULL))

par(mfrow=c(2,1))
plot(cmort)
lines(fit$fitted.values, col = "red")
plot(resid(fit))

acf2(resid(fit), 52)
sarima(cmort, 2,0,0, xreg=cbind(trend,temp,temp2,part))
