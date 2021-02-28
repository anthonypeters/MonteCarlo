library(tidyquant)
library(tidyverse)
library(timetk)
library(broom)

symbols <- c("FDX","CORE", "BLK","LMT","ORCL","NTRS","SJM","MDLZ","JNJ","TPH","AXP","CNC","MDT","PFE","SYY","NRZ","OHI","VIRT","WMT","TSM","REGI","V","MSFT","BX","LDOS","MRK","NKE","AMZN","GLTR", "TLT")

prices <- 
  getSymbols(symbols, src = 'yahoo', 
             from = "2009-12-31",
             to = "2019-12-31",
             auto.assign = TRUE, warnings = FALSE) %>% 
  map(~Ad(get(.))) %>%
  reduce(merge) %>% 
  `colnames<-`(symbols)

w <- c(0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.015,0.0428,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.1,0.15,0.1622)

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

mean_port_return <- 
  mean(portfolio_returns_tq_rebalanced_monthly$returns)

stddev_port_return <- 
  sd(portfolio_returns_tq_rebalanced_monthly$returns)

simulated_monthly_returns <- rnorm(120, 
                                   mean_port_return, 
                                   stddev_port_return)

simulated_returns_add_1 <- 
  tibble(c(1, 1 + simulated_monthly_returns)) %>% 
  `colnames<-`("returns")


simulated_growth <- 
  simulated_returns_add_1 %>%
  mutate(growth1 = accumulate(returns, function(x, y) x * y),
         growth2 = accumulate(returns, `*`),
         growth3 = cumprod(returns)) %>% 
  select(-returns)

cagr <- 
  ((simulated_growth$growth1[nrow(simulated_growth)]^
      (1/10)) - 1) * 100

cagr <- round(cagr, 2)

simulation_accum_1 <- function(init_value, N, mean, stdev) {
  tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>% 
    `colnames<-`("returns") %>%
    mutate(growth = 
             accumulate(returns, 
                        function(x, y) x * y)) %>% 
    select(growth)
}

simulation_accum_2 <- function(init_value, N, mean, stdev) {
  tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>% 
    `colnames<-`("returns") %>%
    mutate(growth = accumulate(returns, `*`)) %>% 
    select(growth)
}

simulation_cumprod <- function(init_value, N, mean, stdev) {
  tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>% 
    `colnames<-`("returns") %>%
    mutate(growth = cumprod(returns)) %>% 
    select(growth)
}

simulation_confirm_all <- function(init_value, N, mean, stdev) {
  tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>% 
    `colnames<-`("returns") %>%
    mutate(growth1 = accumulate(returns, function(x, y) x * y),
           growth2 = accumulate(returns, `*`),
           growth3 = cumprod(returns)) %>% 
    select(-returns)
}

simulation_confirm_all_test <- 
  simulation_confirm_all(1, 120, 
                         mean_port_return, stddev_port_return)


