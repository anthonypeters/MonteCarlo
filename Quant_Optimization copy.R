library(tidyquant)
library(tidyverse)
library(timetk)
#### Calculate long-term returns
asset_returns_long <-  
  prices %>% 
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>% 
  na.omit()
####

#### Read in data for each ticker
w <- c(0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.015,0.0428,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.1,0.15,0.1622)
####

#### Read In Symbols
symbols <- c("FDX","CORE", "BLK","LMT","ORCL","NTRS","SJM","MDLZ","JNJ","TPH","AXP","CNC","MDT","PFE","SYY","NRZ","OHI","VIRT","WMT","TSM","REGI","V","MSFT","BX","LDOS","MRK","NKE","AMZN","GLTR", "TLT")
####

#### Read In Price Data from 2009-2019
prices <- 
  getSymbols(symbols, src = 'yahoo', 
             from = "2009-12-31",
             to = "2019-12-31",
             auto.assign = TRUE, warnings = FALSE) %>% 
  map(~Ad(get(.))) %>%
  reduce(merge) %>% 
  `colnames<-`(symbols)

#### Calculate long-term returns
asset_returns_long <-  
  prices %>% 
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>% 
  na.omit()
####

FDX <- subset(asset_returns_long, subset = (asset == "FDX"),
              select = returns, drop = TRUE)
MSFT<- subset(asset_returns_long, subset = (asset == "MSFT"),
              select = returns, drop = TRUE)


#### Calculate Monthly Returns
portfolio_returns_tq_rebalanced_monthly <- 
  asset_returns_long %>%
  tq_portfolio(assets_col  = asset, 
               returns_col = returns,
               weights     = w,
               col_rename  = "returns",
               rebalance_on = "months")
####

#### Mean and STD of Portfolio returns
mean_port_return <- 
  mean(portfolio_returns_tq_rebalanced_monthly$returns)

stddev_port_return <- 
  sd(portfolio_returns_tq_rebalanced_monthly$returns)
####

#### Create Simulated monthly returns for 10 years using mean and std
simulated_monthly_returns <- rnorm(120, 
                                   mean_port_return, 
                                   stddev_port_return)

simulated_monthly_returns