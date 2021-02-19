# This is a Monte Carlo simulation built (mostly) from scratch

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

# Calculate non-transformed returns (dollar amount)
returns <- data.frame()
for (j in 1:ncol(prices_sub)){
  for (i in 1:(nrow(prices_sub) - 1)){
    returns[i,j] <- prices_sub[i+1,j] - prices_sub[i,j]
  }
}
colnames(returns) <- colnames(prices_sub)

# Cleaning up i and j for later
rm(i)
rm(j)

# Histogram(s)
# ONY RUN IF YOU WANT TO SEE LOTS OF HISTOGRAMS
# mapply(FUN = hist, returns, 
#       main = colnames(prices_sub), 
#       xlab = "Returns ($)")

# MC Sim Function
## Inputs:
###   x = single symbol
###   dist = sampling distribution (default = normal) (not yet implemented)
###   period_type = return period (default = daily) (not yet implemented)
###   period_count = number of periods (default = 365 (days))
###   nsim = number of simulations
## *Can use with mapply() for different distributions, periods, etc.* 

mc.sim <- function(x, dist = "normal", period_type = 'daily', period_count = "365", nsim = 1000){
  cumulative <- data.frame()
  mean <- c()
  stdev <- c()
  sim_return <- matrix(nrow = strtoi(period_count, 10L), ncol = NCOL(x))
  sim_out <- list()
  for (m in 1:nsim){
    for (j in 1:ncol(x)){
      if (dist == "normal"){
        if (period_type == 'daily'){
          mean[j] <- mean(x[,j])
          stdev[j] <- sd(x[,j])
          sim_return[,j] <- rnorm(strtoi(period_count, 10L),
                                  mean[j],
                                  stdev[j])
        }
      }
    }
    colnames(sim_return) <- colnames(x)
    sim_out[[m]] <- sim_return
  }
  return(sim_out)
}







