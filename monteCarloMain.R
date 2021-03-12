library(tidyquant)
library(tidyverse)
library(timetk)
library(openxlsx)
library(broom)
library(Quandl)

quandl_api_key("mRJDZwn3giwAm1kowtFr")

symbols <- readxl::read_excel("tickers.xlsx", sheet = "Tickers")
modified_tickers <- sprintf("EOD/%s", symbols$Ticker)
stock_prices <- tq_get(modified_tickers, get = "quandl", from = Sys.Date()-(365*13), to = Sys.Date()-(365*3))
