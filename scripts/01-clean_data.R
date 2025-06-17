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

#### Clean Data ####
owid_co2_clean <- co2_raw |> 
  select(country, year, iso_code, population, gdp, co2, co2_per_capita, co2_per_gdp) |>
  filter(country == "World")

#### Make Time Series ####
owid_co2_ts <- ts(owid_co2_clean$co2, start = 1750)
owid_co2_per_capita_ts <- ts(owid_co2_clean[201:274,]$co2_per_capita, start = 1950)

#### Plot Time Series ####
par(mfrow=c(2,1))
plot(owid_co2_ts)
plot(owid_co2_per_capita_ts)

#### Linear Regression on Time Series ####
co2_gr <- diff(log(owid_co2_ts))
trend = time(co2_gr)
lm_model_1 <- lm(co2_gr ~ trend, na.action=NULL)

summary(lm_model_1)

acf2(resid(lm_model_1), 100)

plot(log(owid_co2_ts))
lines(lm_model_1$fitted.values, color = "red")

ets_model <- ets(log(owid_co2_ts))  # Log to stabilize exponential growth
summary(ets_model)
autoplot(forecast(ets_model, h = 20))  # Forecast next 20 years
plot(ets_model$fitted)
plot(log(owid_co2_ts))
