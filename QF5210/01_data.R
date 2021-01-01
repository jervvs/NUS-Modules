# S&P500 DATA
# ===========
#
# Prepare the data used in this project, namely S&P500 monthly log returns.
# 
# First, it downloads all of the S&P500 tickers from Wikipedia. For each ticker,
# the corresponding historical stock price from 1995-01-01 to 2020-10-01
# is downloaded and finally the monthly log returns (end of month only) are calculated. 
# 
# The dataset is structured as `tibble` with 8 columns: 
#
# * ticker: the security symbol.
# * security: the name of the coporation issuing the security.
# * gics_sector: the Global Industry Classification Standard (GICS) sector.
# * gics_subindustry: the Global Industry Classification Standard (GICS) sub-industry.
# * first_added: the first date which the security was added to S&P500.
# * prices: a tibble with the historical prices.
# * logreturns: a tibble with historical monthly log returns (end of month only).
# * trade_days: number of trading days.
#
# This code was heavily inspired by
# <https://github.com/mdancho84/quantitative_stock_analysis_tutorial/blob/master/sp500_analysis_tutorial.R>

options("getSymbols.warning4.0"=FALSE)

library(quantmod)
library(xts)
library(tidyverse)
library(rvest)
library(lubridate)

get_prices = function(ticker, ...) {

    cat("Downloading ", ticker, "...\n", sep="")

    prices = getSymbols(Symbols=ticker, auto.assign=FALSE, ...)
    names(prices) = c("open", "high", "low", "close", "volume", "adjusted")

    prices = prices %>% 
        as_tibble(rownames="date") %>%
        mutate(date=ymd(date))

    return(prices)

}

get_returns = function(stock) {

    stock = xts(stock[, -1], order.by=stock$date)
    returns = monthlyReturn(stock, type="log")
    names(returns) = "logreturn"

    returns = returns %>% 
        as_tibble(rownames="date") %>%
        mutate(date=ymd(date))

    return(returns)
}

tickers = read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
    html_node("table.wikitable") %>%
    html_table() %>%
    select(ticker=`Symbol`, security=Security, gics_sector=`GICS Sector`, gics_subindustry=`GICS Sub-Industry`, first_added=`Date first added`) %>%
    as_tibble() %>%
    mutate(ticker=gsub("\\.", "-", ticker)) %>%
    # Vontier was added to S&P500 only in Oct/2020 as the result of a corporate
    # spin-off executed by Fortive, thus it is not included in our analysis
    filter(ticker != "VNT") %>%
    # adds the S&P500 index ticker which will eventually serve as our baseline
    bind_rows(tibble(
        ticker="^GSPC", 
        security="S&P500", 
        gics_sector="N/A",
        gics_subindustry="N/A",
        first_added="1957-03-04"
    ))

from = "1995-01-01"
to = "2020-10-01"
throttle = 5

if (!file.exists("sp500.rds")) {
    sp500 = tibble(
        ticker=character(),
        security=character(),
        gics_sector=character(),
        gics_subindustry=character(),
        first_added=character(),
        prices=list(),
        logreturns=list(),
        trade_days=double()
    )
} else {
    sp500 = readRDS("data/sp500.rds")
    tickers = tickers %>% filter(!(ticker %in% sp500$ticker))
}

for (i in 1:nrow(tickers)) {

    t = tickers[[i, "ticker"]]

    cat("Processing ", t, " [", i, "/", nrow(tickers), "]...\n", sep="")

    prices = tryCatch(
        get_prices(t, from=from, to=to),
        error=function(e) {
            cat("Failed downloading ", t, ".\n", sep="")
            wait = 60 + runif(1, -10, 10)
            cat("Throttling for ", wait, "...\n", sep="")
            Sys.sleep(wait)
        }
    )

    if (is.null(prices)) {
        cat("\n")
        next
    }

    cat("Downloaded ", t, ".\n", sep="")
    returns = get_returns(prices)
    trade_days = nrow(prices)

    row = tibble(
        ticker=tickers[[i, "ticker"]],
        security=tickers[[i, "security"]],
        gics_sector=tickers[[i, "gics_sector"]],
        gics_subindustry=tickers[[i, "gics_subindustry"]],
        first_added=tickers[[i, "first_added"]],
        prices=list(prices),
        logreturns=list(returns),
        trade_days=trade_days
    )

    sp500 = sp500 %>% bind_rows(row)

    cat("Saving updated S&P500...\n")
    saveRDS(sp500, "data/sp500.rds")

    wait = max(0, throttle + runif(1, -1, 1))
    cat("Throttling for ", wait, "...\n", sep="")
    Sys.sleep(wait)

    cat("\n")

}
