library(tidyquant)
library(tidyverse)
library(openxlsx)
library(timetk)
library(broom)
library(highcharter)
library(mvtnorm)

set.seed(1456)
#### PART 1 ####

# Read in current portfolio tickers & weights
weights <- read.xlsx("weights.xlsx", sheet = "Weights") # portfolio data frame
symbols <- weights$Tickers # symbols
w <- weights$Weights # tickers

# Pulling prices from yahoo & reformatting
prices <- 
  tq_get(symbols, get= "stock.prices", 
         from = "2016-12-31",
         to = "2019-12-31",
         complete_cases = TRUE) 

prices_mod <- data.frame(matrix(nrow = NROW(prices)/length(symbols), 
                                ncol = length(symbols)))
for (i in 1:length(symbols)){
  prices_mod[,i] <- subset(prices, subset = (symbol == symbols[i]),
                           select = c(adjusted), drop = FALSE)
}
colnames(prices_mod) <- symbols
rownames(prices_mod) <- prices$date[1:NROW(prices_mod)]

#### Calculate daily log returns
asset_returns_long <-  
  prices_mod %>% 
  to.daily(OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>% 
  na.omit()

#### Isolating returns of individual assets (reformatting to be user-friendly)
returns <- data.frame(matrix(nrow = NROW(asset_returns_long)/length(symbols), 
                             ncol = length(symbols)))
for (i in 1:length(symbols)){
  returns[,i] <- subset(asset_returns_long, subset = (asset == symbols[i]),
                        select = c(returns), drop = FALSE)
}

colnames(returns) <- symbols
rownames(returns) <- asset_returns_long$date[1:NROW(returns)]
####

#### Combining asset returns into portfolio returns
portfolio_obs_returns <- scale(returns, center = FALSE, scale = 1/w) %>%
  rowSums()
#### 

#### Adding a baseline investment ($1)
observed_returns_add_1 <- 
  tibble(c(10000, 1 + portfolio_obs_returns)) %>% 
  `colnames<-`("returns")
summary(observed_returns_add_1)
####

#### Simulated Growth function using accumulate ()
accumulated_growth <- unlist(accumulate(observed_returns_add_1$returns, `*`))
summary(accumulated_growth)
hist(accumulated_growth)
####

plot(1:length(accumulated_growth), accumulated_growth, type = 'l')
