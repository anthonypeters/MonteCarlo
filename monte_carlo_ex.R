#Packages used
library(tidyquant)
library(tidyverse)
library(timetk)
library(broom)

# Assets of interest
symbols <- c("SPY","EFA", "IJS", "EEM","AGG")

# Get prices
prices <- 
  getSymbols(symbols, src = 'yahoo', 
             from = "2012-12-31",
             to = "2017-12-31",
             auto.assign = TRUE, warnings = FALSE) %>% 
  map(~Ad(get(.))) %>%
  reduce(merge) %>% 
  `colnames<-`(symbols)

# Set portfolio weights
w <- c(0.25, 0.25, 0.20, 0.20, 0.10)

# Convert data for use
# NOTE: "return" is not rate, is dollar amount
asset_returns_long <-  
  prices %>% 
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>% 
  na.omit()

portfolio_returns_tq_rebalanced_monthly <- 
  asset_returns_long %>%
  tq_portfolio(assets_col  = asset, 
               returns_col = returns,
               weights     = w,
               col_rename  = "returns",
               rebalance_on = "months")

# Obtain mean from data
mean_port_return <- 
  mean(portfolio_returns_tq_rebalanced_monthly$returns)

# Obtain standard deviation from data
stddev_port_return <- 
  sd(portfolio_returns_tq_rebalanced_monthly$returns)

# Simulate returns (SIMULATION PART)
# NSIM: 1
simulated_monthly_returns <- rnorm(120, 
                                   mean_port_return, 
                                   stddev_port_return)

# Look at simulated data
head(simulated_monthly_returns)
tail(simulated_monthly_returns)

# Incorporate baseline $1 return
simulated_returns_add_1 <- 
  tibble(c(1, 1 + simulated_monthly_returns)) %>% 
  `colnames<-`("returns")

# Look at return + 1
head(simulated_returns_add_1)

# Convert to cumulative growth
simulated_growth <- 
  simulated_returns_add_1 %>%
  mutate(growth1 = accumulate(returns, function(x, y) x * y),
         growth2 = accumulate(returns, `*`),
         growth3 = cumprod(returns)) %>% 
  select(-returns)

# Look at converted data
tail(simulated_growth)

# Calculate CAGR
cagr <- ((simulated_growth$growth1[nrow(simulated_growth)]^(1/10)) - 1) * 100
cagr <- round(cagr, 2)
print(c("CAGR:", cagr))