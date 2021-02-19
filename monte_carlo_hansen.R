# This is a Monte Carlo simulation built from scratch

# Packages used:
library(tidyverse)
library(tidyquant)
library(openxlsx)

# Pulling data using quantmod (package within tidyquant)
# Run after running re-balance script

current <- readxl::read_excel(file.choose())

symbols <- current$Ticker
weights <- current$Weight

# Get prices using 'quantmod' from 'tidyquant'
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

# Calculate untransformed returns (dollar amount)
returns <- data.frame()
for (j in 1:ncol(prices_sub)){
  for (i in 1:(nrow(prices_sub) - 1)){
    returns[i,j] <- prices_sub[i+1,j] - prices_sub[i,j]
  }
}
colnames(returns) <- colnames(prices_sub)

# Histogram(s)
# ONY RUN IF YOU WANT TO SEE LOTS OF HISTOGRAMS
mapply(FUN = hist, returns, 
       main = colnames(prices_sub), 
       xlab = "Returns ($)")

# MC Sim Function
## Inputs:
###   x = single symbol
###   dist = sampling distribution (default = normal)
###   period_type = return period (default = daily)
###   period_count = number of periods (default = 365 (days))
###   nsim = number of simulations

mc.sim <- function(x, dist = "normal", 
                   period_type = 'daily', 
                   period_count = "356", 
                   nsim = 100)
{
  for (j in 1:ncol(x)){
    if (dist = "normal"){
      if (period = 'daily'){
        mean[j] <- mean(x[,j])
        stdev <- sd(x[,j])
        sim_return <- 
      }
    }
  }
}







