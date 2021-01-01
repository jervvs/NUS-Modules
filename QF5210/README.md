# QF5210 Project: Beating the S&P500

The objective of this project is to determine the extent to which different
statistical modelling frameworks, with focus on stochastic and regime switching
volatility  models, impact the performance of an optimal portfolio allocation
strategy for stocks of S&P500.

# Data

The data used in this project is located in `data/s&p500.rds` all of which is
sourced from [Yahoo! Finance](https://finance.yahoo.com/) using the R library
[`quantmod`](https://www.quantmod.com/), check the source code `01_data.R`
for more information.

The data consists of all S&P500 securities (except for Voltier which was only
recently added as the result of a corporate spin off) and the S&P500 index itself. 
The data includes historic prices and log monthly returns (end of
month only) from 1995-01-01 to 2020-10-01.

The dataset contains 505 rows with 8 columns:

* ticker: the security symbol.
* security: the name of the coporation issuing the security.
* gics_sector: the Global Industry Classification Standard (GICS) sector.
* gics_subindustry: the Global Industry Classification Standard (GICS) sub-industry.
* first_added: the first date which the security was added to S&P500.
* prices: a tibble with the historical prices.
* logreturns: a tibble with historical monthly log returns (end of month only).
* trade_days: number of trading days.

Additional data files in `data/` consist of: (1) model estimate outputs, `data/estimates.rds`, and, (2) realised portfolio logreturns, `data/portfolio_performance.rds`.

# Scripts

This project employed R to conduct all of the computational analysis. The
scripts are ordered in the logical order which they were used.

`01_data.R`
> to obtain all of the S&P500 stock market data for the past 25 years. See
> previous section for more details on the data.

`02_sp500_descriptive.R`
> some descriptive analysis of the S&P500 index. Timing of investiment and the
> ideal portfolio.

`03_models.R`
> forecasting models for each individual stock in the S&P500.

`04_models_comparison.R`
> compare the fit of the different models given the realised data.

`05_portfolios.R`
> portfolio allocation optimization according to modern portfolio theory, using
> our conditional expectation and volatility estimated in the previous script.

`06_statuinaritytests.R`
> conduct stationarity tests to check whether securities returns in the
> S&P500 are indeed stationary.
