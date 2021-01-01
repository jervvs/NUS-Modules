# FORECASTING MODELS
# ==================
#
# Estimate 3 stock forecasting models, ARIMA, GARCH and MSGARCH.
#
# The models are estimated on a rolling window basis. For each security `s` and
# time period `t`, we use the previous 10 years of data (`(t-119):t`) to
# estimate the one-month return. We use this model to forecast the security's
# one-month ahead return, `t+1`.
#
# We only estimate models for security-period pairs that have 10 years of
# historical data, otherwise we skip model estimation. For instance, if stock A
# entered S&P500 in March 2007, then we would only estimate its return after
# February 2017. Which means we would only consider including stock A in our
# portfolio after it has withstood the test of time by remaining at S&P500 for
# a minimum of 10 years.
#
# We estimate 3 classes of models, with increasing order of complexity:
#
# 1. ARIMA: `auto.arima` is used to determine the optimal model specification. We
# assume stationarity of returns and we allow for seasonal variations.
#
# 2. GARCH: GARCH(1, 1) with conditional volatility distributed
# according to a Student-t distribution.
#
# 3. MSGARCH: 2 state Markov-switching GARCH(1,1) model, assuming 2 volatility
# states. In one state volatility follows a normal distribution while in the
# other state it follows a Student t-distribution.

library(purrr)
library(forecast)
library(tidyverse)
library(quantmod)
library(fGarch)
library(MSGARCH)

#' Rolling ARIMA: estimate 10-years rolling-window \code{\link{auto.arima}}
#'
#' @param logreturns, tibble with 2 columns, \code{date} and \code{logreturn}.
#' Assume that it contains sorted end-of-month returns.
#' @param ... additional parameters to be passed to \code{\link{auto.arima}}
#'
#' @return tibble. A copy of logreturns with 3 additional columns,
#' \code{arima}, \code{arima_pred} and \code{arima_se}, containing the arima
#' model object, point estimate and conditional volatility respectively for
#' each time period. For periods without enough historical data, we assign
#' \code{NA} to these 3 columns.
#'
rollarima = function(logreturns, ...) {

    # 120 months (= 10 years * 12 months)
    window = 120

    pred = rep(NA, length=nrow(logreturns))
    se = rep(NA, length=nrow(logreturns))

    logreturns = logreturns %>% select(!matches("^arima"))

    if (nrow(logreturns) < window) {
        out = tibble(arima_pred=pred, arima_se=se)
        out = bind_cols(logreturns, out)
        return(out)
    }

    for (i in 120:nrow(logreturns)) {

        y = logreturns$logreturn[(i-window+1):i]

        m = auto.arima(
            y,
            x=y,
            ...
        )

        p = predict(m)

        if (i < nrow(logreturns)) {
            pred[[i+1]] = p$pred[[1]]
            se[[i+1]] = p$se[[1]]
        }

    }

    out = tibble(arima_pred=pred, arima_se=se)
    out = bind_cols(logreturns, out)

    return(out)

}

#' Rolling GARCH: estimate 10-years rolling-window \code{\link{garchFit}}
#'
#' @param logreturns, tibble with 2 columns, \code{date} and \code{logreturn}.
#' Assume that it contains sorted end-of-month returns.
#' @param ... additional parameters to be passed to \code{\link{auto.arima}}
#'
#' @return tibble. A copy of logreturns with 3 additional columns,
#' \code{garch}, \code{garch_pred} and \code{garch_se}, containing the GARCH
#' model object, point estimate and conditional volatility respectively for
#' each time period. For periods without enough historical data, we assign
#' \code{NA} to these 3 columns.
#'
rollgarch = function(logreturns, ...) {

    # 120 months (= 10 years * 12 months)
    window = 120

    pred = rep(NA, length=nrow(logreturns))
    se = rep(NA, length=nrow(logreturns))

    logreturns = logreturns %>% select(!matches("^garch"))

    if (nrow(logreturns) < window) {
        out = tibble(garch_pred=pred, garch_se=se)
        out = bind_cols(logreturns, out)
        return(out)
    }

    for (i in 120:nrow(logreturns)) {

        m = tryCatch(
            garchFit(
                data=logreturns$logreturn[(i-window+1):i],
                ...
            ),
            error=function(e) {
                cat("Failed GARCH fit index=", i, ".\n", sep="")
            }
        )

        if (is.null(m)) { next }

        p = tryCatch(
            predict(m, 1),
            error=function(e) {
                cat("Failed GARCH prediction index=", i, ".\n", sep="")
            }
        )

        if (is.null(p)) { next }

        if (i < nrow(logreturns)) {
            pred[[i+1]] = p[1, "meanForecast"]
            se[[i+1]] = p[1, "standardDeviation"]
        }

    }

    out = tibble(garch_pred=pred, garch_se=se)
    out = bind_cols(logreturns, out)

    return(out)
}

#' Rolling MSGARCH: estimate 10-years rolling-window \code{\link{FitML}}
#'
#' @param logreturns, tibble with 2 columns, \code{date} and \code{logreturn}.
#' Assume that it contains sorted end-of-month returns.
#' @param ... additional parameters to be passed to \code{\link{auto.arima}}
#'
#' @return tibble. A copy of logreturns with 3 additional columns,
#' \code{msgarch}, \code{msgarch_pred} and \code{msgarch_se}, containing the MSGARCH
#' model object, point estimate and conditional volatility respectively for
#' each time period. For periods without enough historical data, we assign
#' \code{NA} to these 3 columns.
#'
rollmsgarch = function(logreturns, spec, ...) {

    # 120 months (= 10 years * 12 months)
    window = 120

    pred = rep(NA, length=nrow(logreturns))
    se = rep(NA, length=nrow(logreturns))

    logreturns = logreturns %>% select(!matches("^msgarch"))

    if (nrow(logreturns) < window) {
        out = tibble(msgarch_pred=pred, msgarch_se=se)
        out = bind_cols(logreturns, out)
        return(out)
    }

    for (i in 120:nrow(logreturns)) {

        m = tryCatch(
            FitML(
                spec=spec,
                data=logreturns$logreturn[(i-window+1):i],
                ...
            ),
            error=function(e) {
                cat("Failed MSGARCH fit index=", i, ".\n", sep="")
            }
        )

        if (is.null(m)) { next }

        # we compute one-month expected return as the sample mean from 500
        # draws, as MSGARCH::predict only returns the forecast volatility
        p = tryCatch(
            mean(simulate(m, nsim=500)$draw),
            error=function(e) {
                cat("Failed MSGARCH mean prediction index=", i, ".\n", sep="")
            }
        )

        if (is.null(p)) { next }

        s = tryCatch(
            predict(m, nahead=1)$vol[[1]],
            error=function(e) {
                cat("Failed MSGARCH volatility prediction index=", i, ".\n", sep="")
            }
        )

        if (is.null(s)) { next }

        if (i < nrow(logreturns)) {
            pred[[i+1]] = p
            se[[i+1]] = s
        }

    }

    out = tibble(msgarch_pred=pred, msgarch_se=se)
    out = bind_cols(logreturns, out)

    return(out)

}

# main

data = readRDS("data/sp500.rds")

cat("ARIMA\n", sep="")

counter = 0
total = nrow(data)

data = data %>%
    mutate(
        logreturns=map(
            logreturns,
            function(.x) { 
                counter <<- counter + 1
                cat("Processing [", counter, "/", total, "]...\n", sep="")
                rollarima(
                    .x,
                    # assume stationarity and seasonality
                    stationary=TRUE,
                    seasonal=TRUE,
                    approximation=TRUE,
                    trace=FALSE,
                )
            }
        )
)

saveRDS(data, "data/estimates.rds")

cat("GARCH\n", sep="")

counter = 0
total = nrow(data)

data = data %>%
    mutate(
        logreturns=map(
            logreturns,
            function(.x) {
                counter <<- counter + 1
                cat("Processing [", counter, "/", total, "]...\n", sep="")
                # assume Student-t distribution
                rollgarch(.x, cond.dist="std", trace=FALSE)
            }
        )
    )

saveRDS(data, "data/estimates.rds")

cat("MSGARCH\n", sep="")

# two-state Markov switching GARCH model. The volatility of one state follows a
# normal distribution, while the other follows a Student-t distribution.
spec = CreateSpec(
    variance.spec = list(model = c("sGARCH", "sGARCH")),
    distribution.spec=list(distribution = c("norm", "std")),
)

counter = 0
total = nrow(data)

data = data %>%
    mutate(
        logreturns=map(
            logreturns,
            function(.x) {
                counter <<- counter + 1
                cat("Processing [", counter, "/", total, "]...\n", sep="")
                rollmsgarch(.x, spec, ctr=list(do.se=FALSE, do.sort=FALSE))
            }
        )
    )

saveRDS(data, "data/estimates.rds")
