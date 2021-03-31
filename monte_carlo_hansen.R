# Packages used & setup:
library(tidyverse)
library(tidyquant)
library(openxlsx)

quandl_api_key("mRJDZwn3giwAm1kowtFr")
# av_api_key("DPHG14C1C908V9GE")

# Pulling data using quantmod (package within tidyquant)
# Run after running re-balance script

current <- readxl::read_excel("weights.xlsx")

symbols <- current$Tickers
weights <- current$Weights

# Get prices
symbols_mod <- as.character(mapply("EOD/", current$Tickers, FUN = paste, sep = ""))
stock_prices <- tq_get(symbols_mod, get = "quandl", from = Sys.Date()-(365*3), to = Sys.Date())

# Remove "EOD/" prefix from symbols
stock_prices$symbol <- substr(stock_prices$symbol, 5, 9)

# Calculate DAILY returns (dollar amount)
stock_returns_daily <- stock_prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adj_close,
               mutate_fun = periodReturn,
               period = "daily",
               # type = "arithmetic",
               type = "log",
               col_rename = "returns") %>%
  pivot_wider(names_from = symbol, values_from = returns) %>%
  data.frame()

# Calculate MONTHLY returns (dollar amount)
stock_returns_monthly <- stock_prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adj_close,
               mutate_fun = periodReturn,
               period = "monthly",
               # type = "arithmetic",
               type = "log",
               col_rename = "returns") %>%
  pivot_wider(names_from = symbol, values_from = returns) %>%
  data.frame()

## Histogram(s)
# ONY RUN IF YOU WANT TO SEE LOTS OF HISTOGRAMS
# mapply(FUN = hist, returns, 
#       main = colnames(prices_sub), 
#       xlab = "Returns ($)")

# Correlation Matrices
## Correlating monthly returns
return_cor_mat <- round(cor(stock_returns_monthly[, -1]), 4)


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

# Function for running Monte Carlo simulation
mc.sim <- function(x, 
                   dist = "normal", 
                   period_count = 36, # 3-month time line
                   init_invest = 1,
                   nsim = 100){
  cumulative <- data.frame()
  mean <- c()
  stdev <- c()
  sim_return <- matrix(nrow = period_count, ncol = NCOL(x))
  sim_accum <- matrix(nrow = period_count, ncol = NCOL(x))
  sim_accum[1,] <- init_invest
  sim_out <- list()
  for (m in 1:nsim){
    for (j in 1:ncol(x)){
      if (dist == "normal"){
        mean[j] <- mean(x[,j])
        stdev[j] <- sd(x[,j])
        sim_return[,j] <- 1 + rnorm(period_count,
                                mean[j],
                                stdev[j])
        for (i in 2:nrow(sim_return)){
          sim_accum[i,j] <- sim_accum[i-1,j] * sim_return[i,j]
        }
      }
    }
    colnames(sim_accum) <- colnames(x)
    sim_out[[m]] <- sim_accum
  }
  return(sim_out)
}

# One year of daily returns example
three_months <- mc.sim(stock_returns_monthly, init_invest = 100000)

# Narrowing example down to TLT
TLT_sims <- matrix(nrow = 36, ncol = length(three_months))
for (s in 1:length(three_months)){
  TLT_sims[,s] <- unlist(three_months[[s]][,"TLT"])
}

# Plotting Accumulated Returns
TLT_sims <- data.frame(TLT_sims)
index = seq(1,36,1)

set.seed(1456)
plot(TLT_sims$X1 ~ index,
     type = "l",
     main = "TLT Cumulative Return Simulation",
     ylab = "Cumulative Return (USD)",
     ylim = c(min(TLT_sims[1:length(TLT_sims$X1),]), 
              max(TLT_sims[1:length(TLT_sims$X1),])),
     xlab = "Month",
     col = "red")
for (p in 2:length(TLT_sims)){
  lines(TLT_sims[,p] ~ index, col = sample(colors(), replace = F))
}


