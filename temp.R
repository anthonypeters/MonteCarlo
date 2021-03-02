library(tidyverse)
library(tidyquant)
library(openxlsx)

# Pulling data using quantmod (package within tidyquant)
# Run after running re-balance script

current <- readxl::read_excel("weights.xlsx")

symbols <- current$Ticker
weights <- current$Weight

# Get prices using 'quantmod' from 'tidyquant'
# The next eight lines are from monte_carlo_ex.R
prices <- 
  getSymbols(symbols, src = 'yahoo', 
             from = "2012-12-31",
             to = "2017-12-31",
             auto.assign = TRUE) %>% 
  map(~Ad(get(.))) %>%
  reduce(merge) %>% 
  `colnames<-`(symbols)

prices <- data.frame(prices)
prices_sub <- prices[,!sapply(prices, function(x) any(is.na(x)))]
colnames(symbols_updated) <- colnames(prices_sub)
################################################################################
# Quandl API Key for our EOD stock prices subscription
quandl_api_key("mRJDZwn3giwAm1kowtFr")

# Pull tickers and groups from Excel file and order alphabetically by group
xl_tickers <- readxl::read_excel("tickers.xlsx", sheet = "Tickers")
xl_tickers <- xl_tickers[order(xl_tickers$Group),]
xl_groups <- readxl::read_excel("tickers.xlsx", sheet = "Groups")
xl_groups <- xl_groups[order(xl_groups$Group),]

# Split the tickers into a list of lists by their groups and create a numeric list of lists (via for loop, inefficient solution sorry)
split_groups = split(rownames(xl_tickers), xl_tickers$Group)
groups <- list()
counter <- 1
for (i in split_groups) {
  groups[[counter]] <- as.numeric(i)
  counter <- counter + 1
}

# Add EOD prefix to tickers so they pull end of day prices from Quandl
modified_tickers <- sprintf("EOD/%s", xl_tickers$Ticker)

# Pull EOD stock prices from Quandl (must be at least 3 years for it to work properly)
stock_prices <- tq_get(modified_tickers, get = "quandl", from = Sys.Date()-(365*3), to = Sys.Date())

# Calculate returns on the stocks over the time period
stock_returns <- stock_prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adj_close,
               mutate_fun = periodReturn,
               period = "monthly",
               type = "log",
               col_rename = "returns") %>%
  pivot_wider(names_from = symbol, values_from = returns)

# Coerce the stock returns into a xts time series
stock_returns <- stock_returns %>% tk_xts(silent = TRUE)