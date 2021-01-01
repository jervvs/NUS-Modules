# PORTFOLIOS
# ==========
#
# Computes the tangent mean-variance and the equal weights portfolios making
# use of our previous logreturn estimates.
#

library(fPortfolio)
library(xts)
library(dplyr)
library(purrr)
library(tidyr)

#' Available Ticker: get the tickers available on a given date.
#'
#' @param .data, tibble with our logreturn estimates
#' @param .pred, column name with point estimates.
#' @param .se, column name with standard error/volatility estimates.
#' @param .date, target date.
#'
#' @return vector of available tickers.
#'
availableTicker = function(.data, .pred, .se, .date) {

    .pred = enquo(.pred)
    .se = enquo(.se)

    tickers = .data %>%
        filter(date == .date) %>%
        select(ticker, logreturn, !!.pred, !!.se) %>%
        filter(!(is.na(logreturn) | is.na(!!.pred) | is.na(!!.se))) %>%
        pull(ticker)

    return(tickers)
}

#' Conditional covariance: computes the conditional covariance
#'
#' The strategy for computing the covariance matrix is to compute the estimate
#' errors accross all dates. For each security pair, we compute their
#' cross-correlation given the available data between both. 
#' 
#' The diagonal of our covariance matrix comes from our estimated conditional
#' volatility which were estimated independently for each security. Therefore,
#' only the diagonal of the conditional covariance changes accross time.
#'
#' Finally, the conditional expectation is given by the point estimate.
#'
#' @param .data, tibble with our logreturn estimates
#' @param .pred, column name with point estimates.
#' @param .se, column name with standard error/volatility estimates.
#' @param .date, target date.
#'
#' @return list with the conditional expectation and conditional covariance
#' matrix.
#'
condCov = function(.data, .pred, .se, .date) {

    .pred = enquo(.pred)
    .se = enquo(.se)

    available = .data %>% availableTicker(!!.pred, !!.se, .date)

    errors = .data %>%
        filter(ticker %in% available) %>%
        select(ticker, date, logreturn, !!.pred, !!.se) %>%
        mutate(error=logreturn - !!.pred) %>%
        select(ticker, date, error) %>%
        pivot_wider(names_from=ticker, values_from=error) %>%
        select(-date) %>%
        as.matrix

    std = .data %>%
        filter(date == .date) %>%
        filter(ticker %in% available) %>%
        pull(!!.se)

    Sigma = cov(errors, use="pairwise.complete.obs")
    diag(Sigma) = std

    mu = .data %>%
        filter(date == .date) %>%
        filter(ticker %in% available) %>%
        select(ticker, !!.pred) %>%
        pivot_wider(names_from=ticker, values_from=!!.pred) %>%
        as.matrix

    # return(out)
    return(list(mu=mu, Sigma=Sigma))

}

#' Tangency Portfolio Weights: obtain the weights of the portfolio maximizing
#' the Sharpe-ratio.
#'
#' @param .data, tibble with our logreturn estimates
#' @param .pred, column name with point estimates.
#' @param .se, column name with standard error/volatility estimates.
#' @param .date, target date.
#'
#' @return the vector of portfolio weights.
#'
tangencyPortfolioWeights = function(.data, .pred, .se, .date) {

    .pred = enquo(.pred)
    .se = enquo(.se)

    available = estimates %>% availableTicker(!!.pred, !!.se, .date)

    data = estimates %>%
        filter(date==.date) %>%
        filter(ticker %in% available) %>%
        select(ticker, !!.pred) %>%
        pivot_wider(names_from=ticker, values_from=!!.pred) %>%
        xts(order.by=as.Date(.date)) %>%
        as.timeSeries

    covt = estimates %>% condCov(!!.pred, !!.se, .date)
    covtEstimator = function(x, spec = NULL, ...) { return(covt) }
    # the portfolioSpec expects that our covariance function is defined in the
    # global environment.
    assign("covtEstimator", covtEstimator, envir=globalenv())

    spec = portfolioSpec()
    setEstimator(spec) = "covtEstimator"

    p = efficientPortfolio(data=data, spec=spec, constraints="LongOnly")

    w = getWeights(p)

    # we make sure to clean the created function from the global environment to
    # avoid unexpected behaviour.
    rm("covtEstimator", envir=globalenv())

    return(w)

}

#' Portfolio return: compute the log return given the portfolio weights.
#'
#' @param .data, tibble containing the realised returns, filtered with only the
#' date of interest (ie. there should be only one return per ticker).
#' @param .weights, a numeric vector of portfolio weights.
#'
#' @return the portfolio log return.
#'
portfolioReturn = function(.data, .weights) {

    logreturns = .data %>%
        select(ticker, logreturn) %>%
        pivot_wider(names_from=ticker, values_from=logreturn) %>%
        as.matrix

    return(log(sum(exp(logreturns) * .weights)))

}

#' All returns: compute the realised log return for all the dates in the dataset of the
#' tanget portfolio.
#'
#' @param .data, tibble with our logreturn estimates
#' @param .pred, column name with point estimates.
#' @param .se, column name with standard error/volatility estimates.
#'
#' @return a tibble with dates and logreturns.
#'
allReturns = function(.data, .pred, .se) {

    .pred = enquo(.pred)
    .se = enquo(.se)

    returns = c()
    dates = .data %>% distinct(date) %>% pull

    for (i in 1:length(dates)) {

        d = dates[[i]]

        cat("Processing ", i, "/", length(dates), ".\n", sep="")

        available = .data %>% availableTicker(!!.pred, !!.se, d)
        w = .data %>% tangencyPortfolioWeights(!!.pred, !!.se, d)

        if (all(w == 0)) {
            returns = c(returns, 0)
        } else {
            r = .data %>%
                filter(date==d) %>%
                filter(ticker %in% available) %>%
                portfolioReturn(w)
            returns = c(returns, r)
        }

    }

    out = tibble(date=dates, logreturn=returns)

    return(out)

}

#' Equal weight portfolio return: compute the realised logreturn for all the dates in
#' the dataset.
#'
#' @param .data, tibble with our logreturn estimates
#'
#' @return a tibble with dates and logreturns.
#'
equalWeightReturn = function(.data) {

    returns = c()
    dates = .data %>% distinct(date) %>% pull

    for (i in 1:length(dates)) {

        d = dates[[i]]

        cat("Processing ", i, "/", length(dates), ".\n", sep="")

        available = .data %>%
            filter(date == d) %>%
            select(ticker, logreturn) %>%
            filter(!is.na(logreturn)) %>%
            pull(ticker)

        w = rep(1/length(available), length(available))
        names(w) = available

        r = .data %>%
            filter(date==d) %>%
            filter(ticker %in% available) %>%
            portfolioReturn(w)
        returns = c(returns, r)

    }

    out = tibble(date=dates, logreturn=returns)

    return(out)

}

# main

# load our baseline which is the S&P500 index
sp500 = readRDS("data/sp500.rds") %>%
    filter(ticker=="^GSPC") %>%
    select(-prices) %>%
    unnest(logreturns) %>%
    filter(date >= "2005-01-01") %>%
    select(date, logreturn)


# load all our logreturn estimates

# we ignore these stocks, because for some unkown reason a set of them is causing the optimizer to fail
# these are stocks that traded for less than the total amount of days available for training
ignore = c("AAL", "AWK", "AMP", "AVGO", "BR", "CF", "CHTR", "CMG", "CXO",
   "DAL", "DFS", "DISCA", "DISCK", "DG", "EXPE", "FTNT", "HBI", "ICE", "IPGP",
   "LDOS", "LYV", "LYB", "MA", "MSCI", "PM", "TMUS", "TEL", "TDG", "ULTA",
   "UAA", "UAL", "VRSK", "V", "WU")

estimates = readRDS("data/estimates.rds") %>%
    filter(ticker!="^GSPC") %>%
    filter(!(ticker %in% ignore)) %>%
    select(-prices) %>%
    filter(trade_days >= 252*10) %>%
    unnest(logreturns) %>%
    filter(date >= "2005-01-01")


# Compute portfolio allocations

cat("Computing ARIMA portfolio.\n", sep="")
arima_performance = estimates %>% allReturns(arima_pred, arima_se) %>% rename(arima_logreturn=logreturn)

cat("Computing GARCH portfolio.\n", sep="")
garch_performance = estimates  %>% allReturns(garch_pred, garch_se) %>% rename(garch_logreturn=logreturn)

cat("Computing MSGARCH portfolio.\n", sep="")
msgarch_performance = estimates  %>% allReturns(msgarch_pred, msgarch_se) %>% rename(msgarch_logreturn=logreturn)

cat("Computing equal weight portfolio return.\n", sep="")
ew_performance = estimates %>% equalWeightReturn %>% rename(ew_logreturn=logreturn)

# join all the computed performance
performance = sp500 %>%
    full_join(arima_performance, by="date") %>%
    full_join(garch_performance, by="date") %>%
    full_join(msgarch_performance, by="date") %>%
    full_join(ew_performance, by="date") %>%
    rename(sp500_logreturn=logreturn)

saveRDS(performance, "data/portfolio_performance.rds")

# compute the cumulative performance for plotting
cumperformance = performance %>% 
    mutate_at(vars(ends_with("logreturn")), ~ cumprod(exp(.))) %>%
    rename_all(~gsub("logreturn", "cumreturn", .)) %>%
    xts(x=.[2:6], order.by=.$date)

png(file="portfolio_performance.png", width=853, height=480)
plot(cumperformance, legend.loc="top")
dev.off()

yearly = (lag.xts(cumperformance, -12, na.pad=TRUE)/cumperformance)^(12/12) - 1
five_yearly = (lag.xts(cumperformance, -12*5, na.pad=TRUE)/cumperformance)^(1/5) - 1
ten_yearly = (lag.xts(cumperformance, -12*10, na.pad=TRUE)/cumperformance)^(1/10) - 1

summary(yearly, five_yearly, ten_yearly)
