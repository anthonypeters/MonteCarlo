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
colnames(symbols_updated) <- colnames(prices_sub)

# Calculate non-transformed returns (dollar amount)
returns <- data.frame()
for (j in 1:ncol(prices_sub)){
  for (i in 1:(nrow(prices_sub) - 1)){
    returns[i,j] <- prices_sub[i+1,j] - prices_sub[i,j]
  }
}
colnames(returns) <- colnames(symbols_updated)


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

# Making distribution vector for each security
distribution <- c()


mc.sim <- function(x, 
                   dist = "normal", 
                   period_type = 'daily', 
                   period_count = "365",
                   init_invest = 0,
                   nsim = 1000){
  cumulative <- data.frame()
  mean <- c()
  stdev <- c()
  sim_return <- matrix(nrow = strtoi(period_count, 10L), ncol = NCOL(x))
  sim_accum <- matrix(nrow = strtoi(period_count, 10L), ncol = NCOL(x))
  sim_accum[1,] <- init_invest
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
          for (i in 2:nrow(sim_return)){
          sim_accum[i,j] <- sim_accum[i-1,j] + sim_return[i,j]
          }
            }
      }
    }
    colnames(sim_accum) <- colnames(x)
    sim_out[[m]] <- sim_accum
  }
  return(sim_out)
}

# One year of daily returns example
one_year <- mc.sim(returns)

# Narrowing example down to TLT
TLT_sims <- matrix(nrow = 365, ncol = length(one_year))
for (s in 1:length(one_year)){
  TLT_sims[,s] <- unlist(one_year[[s]][,"TLT"])
}

# Plotting Accumulated Returns
# MAY TAKE A LONG TIME (< 30 minutes), DO NOT RUN IF NOT NECESSARY!
TLT_sims <- data.frame(TLT_sims)
index = seq(1,365,1)

set.seed(1456)
plot(TLT_sims$X1 ~ index,
     type = "l",
     main = "TLT Cumulative Return Simulation",
     ylab = "Cumulative Return (USD)",
     ylim = c(-75, 75),
     xlab = "Day",
     col = "red")
for (p in 2:length(TLT_sims)){
  lines(TLT_sims[,p] ~ index, col = sample(colors(), replace = F))
}
