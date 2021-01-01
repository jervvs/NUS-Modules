# DESCRIPTIVE STATISTICS S&P500
# =============================
#
# Produces descriptive statistics of the S&P500 index with particular attention
# to index performance in the past couple of years.

library(dplyr)
library(purrr)
library(tidyr)
library(quantmod)
library(xts)

# get only SP500
data = readRDS("data/sp500.rds") %>% 
    filter(ticker == "^GSPC") %>% 
    select(prices) %>% 
    pluck(1, 1) %>%
    xts(x=.$close, order.by=.[[1]])

png(file="sp500_historical.png", width=853, height=480)
plot(data)
dev.off()

daily = (lag.xts(data, -1, na.pad=TRUE)/data)^(252/1) - 1
weekly = (lag.xts(data, -5, na.pad=TRUE)/data)^(252/5) - 1
monthly = (lag.xts(data, -20, na.pad=TRUE)/data)^(252/20) - 1
yearly = (lag.xts(data, -252, na.pad=TRUE)/data)^(252/252) - 1
five_yearly = (lag.xts(data, -252*5, na.pad=TRUE)/data)^(1/5) - 1
ten_yearly = (lag.xts(data, -252*10, na.pad=TRUE)/data)^(1/10) - 1

returns = merge(daily, weekly, monthly, yearly, five_yearly, ten_yearly)
summary(returns*100)

png(file="sp500_returns.png", width=853, height=480)
par(mfrow=c(3, 1))
plot(yearly*100)
plot(five_yearly*100)
plot(ten_yearly*100)
dev.off()

# best return
data = readRDS("data/sp500.rds") %>% 
    filter(ticker!="^GSPC") %>%
    select(-prices) %>%
    unnest(logreturns)

maxreturns = data %>% 
    group_by(date) %>%
    arrange(desc(logreturn)) %>%
    mutate(row_number = row_number()) %>%
    filter(row_number == 1) %>%
    select(-row_number) %>%
    ungroup() %>%
    arrange(date) %>%
    mutate(logreturn=ifelse(logreturn < 0, 0, logreturn)) %>%
    xts(x=.$logreturn, order.by=.$date)

annual_maxreturns = ((exp(maxreturns) %>% cumprod) ^ (12/1:nrow(maxreturns))) - 1
annual_maxreturns %>% plot

# worst return
minreturns = data %>% 
    group_by(date) %>%
    arrange(logreturn) %>%
    mutate(row_number = row_number()) %>%
    filter(row_number == 1) %>%
    select(-row_number) %>%
    ungroup() %>%
    arrange(date) %>%
    xts(x=.$logreturn, order.by=.$date)

annual_minreturns = ((exp(minreturns) %>% cumprod) ^ (12/1:nrow(minreturns))) - 1
annual_minreturns %>% plot
