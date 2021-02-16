# This is a Monte Carlo simulation built from scratch

# Packages used:
library(tidyverse)
library(tidyquant)
library(openxlsx)

# Pulling data using quantmod (package within tidyquant)
tickers <- readxl::read_excel(file.choose(), sheet = "Tickers")
weights <- readxl::read_excel(file.choose())

symbols <- tickers$Ticker
weights <- current$