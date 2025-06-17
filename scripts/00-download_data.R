#### Preamble ####
# Purpose: Downloads and saves the raw datasets
# Author: Liam Wall
# Date: 16 June 2025
# Contact: liam.wall@mail.utoronto.ca

#### Workspace Setup #####
library(tidyverse)

#### Download Data ####
co2_raw <- read_csv(file = "https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

#### Save Data ####
# write_csv(co2_raw, here::here("data/raw/owid_co2_data.csv"))