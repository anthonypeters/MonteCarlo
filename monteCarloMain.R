library(tidyquant)
library(tidyverse)
library(openxlsx)
library(timetk)
library(broom)
library(highcharter)
library(mvtnorm)


#### PART 1 ####

#### Read in current portfolio tickers & weights
weights <- read.xlsx("weights.xlsx", sheet = "Weights")

####
  
#### Read In Symbols
symbols <- weights$Tickers
####

#### Read In Price Data from 2009-2019
prices <- 
  getSymbols(symbols, src = 'yahoo', 
             from = "2016-12-31",
             to = "2019-12-31",
             auto.assign = TRUE, warnings = FALSE) %>% 
  map(~Ad(get(.))) %>%
  reduce(merge) %>% 
  `colnames<-`(symbols)

####

#### Read in data for each ticker
w <- weights$Weights 
####

#### Calculate long-term returns
asset_returns_long <-  
  prices %>% 
  to.daily(OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>% 
  na.omit()
####

#### Isolating returns of individual assets
returns <- data.frame(matrix(nrow = NROW(asset_returns_long)/length(symbols), 
                             ncol = length(symbols)))
for (i in 1:length(symbols)){
  returns[,i] <- subset(asset_returns_long, subset = (asset == symbols[i]),
                        select = c(returns), drop = FALSE)
}

colnames(returns) <- symbols
rownames(returns) <- asset_returns_long$date[1:NROW(returns)]
####

#### Creating correlation matrix of returns
return_cor <- cor(returns, method = "kendall") %>%
  round(digits = 4)
write.xlsx(return_cor, file = "returns_correlation_matrix.xlsx", row.names = TRUE)
####

#### Create Simulated daily returns for 90 days (~3 months) using mean and std
simulated_daily_returns <- rmvnorm(n = 90, mean = colMeans(returns), sigma = cov(returns), method = "eigen")
####

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

#### Create the simulated daily returns based on 1 US Dollar
simulated_returns_add_1 <- 
  tibble(c(1, 1 + simulated_daily_returns)) %>% 
  `colnames<-`("returns")
####


#### Compute and round cagr
cagr <- ((simulated_growth$growth1[nrow(simulated_growth)]^(1/10)) - 1) * 100
cagr <- round(cagr, 2)
#### 


#### Simulated Growth function using accumulate ()
simulation_accum_2 <- function(init_value, N, mean, stdev) {
  tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>% 
    `colnames<-`("returns") %>%
    mutate(growth = accumulate(returns, `*`)) %>% 
    select(growth)
}
####


#### Test for the functions
simulation_confirm_all_test <- 
  simulation_confirm_all(1, 120, 
                         mean_port_return, stddev_port_return)
####

#### PART 2 ####

#### Create 51 simulations
sims <- 51
starts <- 
  rep(1, sims) %>%
  set_names(paste("sim", 1:sims, sep = ""))

#### Monte Carlo the 51 simulations based on portfolio
monte_carlo_sim_51 <- 
  map_dfc(starts, 
          simulation_accum_1, 
          N = 120, 
          mean = mean_port_return, 
          stdev = stddev_port_return)
####

#### Adds month column
monte_carlo_sim_51 <- 
  monte_carlo_sim_51 %>% 
  mutate(month = seq(1:nrow(.))) %>% 
  select(month, everything()) %>% 
  `colnames<-`(c("month", names(starts))) %>% 
  mutate_all(funs(round(., 2))) 
####

#### Rerun function to rerun 5 times
monte_carlo_rerun_5 <-  
  rerun(.n = 5, 
        simulation_accum_1(1, 
                           120,
                           mean_port_return, 
                           stddev_port_return))
####

#### Create 51 reruns function which should run the simulation 51 times
reruns <- 51

monte_carlo_rerun_51 <- 
  rerun(.n = reruns, 
        simulation_accum_1(1, 
                           120,
                           mean_port_return, 
                           stddev_port_return)) %>%
  simplify_all() %>% 
  `names<-`(paste("sim", 1:reruns, sep = " ")) %>%
  as_tibble() %>% 
  mutate(month = seq(1:nrow(.))) %>% 
  select(month, everything())
####

#### Plots the simulation
monte_carlo_sim_51 %>% 
  gather(sim, growth, -month) %>% 
  group_by(sim) %>% 
  ggplot(aes(x = month, y = growth, color = sim)) + 
  geom_line() +
  theme(legend.position="none")
####

#### Summarizes the data from the simulation
sim_summary <- 
  monte_carlo_sim_51 %>% 
  gather(sim, growth, -month) %>% 
  group_by(sim) %>% 
  summarise(final = last(growth)) %>% 
  summarise(
    max = max(final), 
    min = min(final),
    median = median(final))
sim_summary
####

#### Plots only min, max, and median
monte_carlo_sim_51 %>% 
  gather(sim, growth, -month) %>% 
  group_by(sim) %>%
  filter(
    last(growth) == sim_summary$max || 
      last(growth) == sim_summary$median ||
      last(growth) == sim_summary$min) %>% 
  ggplot(aes(x = month, y = growth)) + 
  geom_line(aes(color = sim)) 
####

#### Converts data from wide to long tidy format
mc_gathered <- 
  monte_carlo_sim_51 %>% 
  gather(sim, growth, -month) %>% 
  group_by(sim)
####

#### Plots hcChart with all simulations
hchart(mc_gathered, 
       type = 'line', 
       hcaes(y = growth,
             x = month,
             group = sim)) %>% 
  hc_title(text = "51 Simulations") %>%
  hc_xAxis(title = list(text = "months")) %>%
  hc_yAxis(title = list(text = "dollar growth"),
           labels = list(format = "${value}")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE) %>% 
  hc_legend(enabled = FALSE)
####

#### Filters data for min, max, and median
mc_max_med_min <- 
  mc_gathered %>%
  filter(
    last(growth) == sim_summary$max || 
      last(growth) == sim_summary$median ||
      last(growth) == sim_summary$min) %>% 
  group_by(sim)
####

#### Plots min, max, and median using hcChart
hchart(mc_max_med_min, 
       type = 'line', 
       hcaes(y = growth,
             x = month,
             group = sim)) %>% 
  hc_title(text = "Min, Max, Median Simulations") %>%
  hc_xAxis(title = list(text = "months")) %>%
  hc_yAxis(title = list(text = "dollar growth"),
           labels = list(format = "${value}")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE) %>% 
  hc_legend(enabled = FALSE)
####
