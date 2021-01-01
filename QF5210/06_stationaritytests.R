library(dplyr)
library(purrr)
library(tidyr)
library(quantmod)
library(xts)
library(tseries)

raw = readRDS("data/sp500.rds")
ignore = c("AAL", "AWK", "AMP", "AVGO", "BR", "CF", "CHTR", "CMG", "CXO",
           "DAL", "DFS", "DISCA", "DISCK", "DG", "EXPE", "FTNT", "HBI", "ICE", "IPGP",
           "LDOS", "LYV", "LYB", "MA", "MSCI", "PM", "TMUS", "TEL", "TDG", "ULTA",
           "UAA", "UAL", "VRSK", "V", "WU", "^GSPC")

ADF_tests = c()

for (ticker in raw$ticker){
  if (!(ticker %in% ignore)){
    data = readRDS("data/sp500.rds") %>% 
      filter(ticker == ticker) %>% 
      select(prices) %>% 
      pluck(1, 1) %>%
      xts(x=.$close, order.by=.[[1]])
    returns = (lag.xts(data, -20, na.pad=FALSE)/data)^(252/20) - 1
    ADF_tests = c(ADF_tests, adf.test(returns)$p.value)
  }
}

boxplot(ADF_tests, main = "ADF Tests", ylab ="p-value", xlab ="All Stock Returns")



