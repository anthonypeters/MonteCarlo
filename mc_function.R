library(tidyquant)
library(tidyverse)
library(openxlsx)
library(timetk)
library(broom)
library(highcharter)
library(mvtnorm)

# Read in current portfolio tickers & weights
weights_file <- read.xlsx("weights.xlsx", sheet = "Weights")
t <- weights_file$Tickers
w <- weights_file$Weights 

# NONTE CARLO SIMULATION FUNCTION #
mc.simulate <- function(symbols, weights, from, to, days_pred = 90){
  # Pull prices from the web
  prices <- 
    getSymbols(symbols, src = 'yahoo', 
               from = from,
               to = to,
               auto.assign = TRUE, warnings = FALSE) %>% 
    map(~Ad(get(.))) %>%
    reduce(merge) %>% 
    `colnames<-`(symbols)
  
  # Calculate long-term returns
  asset_returns_long <-  
    prices %>% 
    to.daily(OHLC = FALSE) %>% 
    tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
    gather(asset, returns, -date) %>% 
    group_by(asset) %>%  
    mutate(returns = (log(returns) - log(lag(returns)))) %>% 
    na.omit()
  
  # Isolating returns of individual assets
  returns <- data.frame(matrix(nrow = NROW(asset_returns_long)/length(symbols), 
                               ncol = length(symbols)))
  for (i in 1:length(symbols)){
    returns[,i] <- subset(asset_returns_long, subset = (asset == symbols[i]),
                          select = c(returns), drop = FALSE)
  }
  
  colnames(returns) <- symbols
  rownames(returns) <- asset_returns_long$date[1:NROW(returns)]
  
  # Create Simulated daily returns for specified number of days using mean and std
  simulated_daily_returns <- rmvnorm(n = days_pred, 
                                     mean = colMeans(returns), sigma = cov(returns), 
                                     method = "eigen")
  
  # Combine simulated returns into portfolio
  ## Multiply each column by corresponding portfolio weights
  portfolio_sim_returns <- scale(simulated_daily_returns, center = FALSE, scale = weights) %>%
    rowSums()
  
  return(portfolio_sim_returns)
}

# Testing MC sim function:
test_simulation <- mc.simulate(symbols = t, weights = w, from = "2015-12-31", to = "2018-12-31", days_pred = 60)

# CAGR FUNCTION #


# CHARTS FUNCTION #