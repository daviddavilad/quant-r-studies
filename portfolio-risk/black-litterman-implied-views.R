# ==============================================================================
# Black-Litterman Implied Views (Reverse Optimization)
# ------------------------------------------------------------------------------
# Computes the equilibrium excess returns implied by an equal-weighted market
# portfolio via reverse optimization: Pi = delta * Sigma * w_mkt. Adjusts for
# the risk-free rate using the 3M T-bill and compares the implied excess
# returns to the realized S&P 500 annualized return as a sanity check.
#
# Starting point for the Black-Litterman framework: rather than using noisy
# historical means as the expected return prior, use the reverse-optimized
# returns that are consistent with observed market weights.
#
# No external CSV required. Pulls prices via quantmod from Yahoo and FRED.
# Produces: implied returns barplot, risk-free-adjusted implied returns,
#           and the benchmark annualized return.
# ==============================================================================

library(quantmod)
library(MASS)

# ---- Universe and date range -------------------------------------------------
tickers    <- c("AAPL", "MSFT", "GOOG", "AMZN", "TSLA")
start_date <- "2020-01-01"
end_date   <- "2023-01-01"

getSymbols(tickers, src = "yahoo", from = start_date, to = end_date)

# ---- Daily returns and covariance --------------------------------------------
calculate_daily_returns <- function(ticker) dailyReturn(Cl(get(ticker)))
returns    <- do.call(merge, lapply(tickers, calculate_daily_returns))
cov_matrix <- cov(returns, use = "complete.obs")

# ---- Reverse optimization: Pi = delta * Sigma * w ----------------------------
# Equal weights as a simple proxy for the market portfolio.
market_weights <- rep(1 / length(tickers), length(tickers))

# Risk aversion coefficient. 2.5 is a standard textbook value; in practice
# this is calibrated to the market Sharpe ratio.
delta <- 2.5

implied_returns        <- delta * cov_matrix %*% market_weights
implied_returns_vector <- as.vector(implied_returns)
colnames(implied_returns) <- "Implied Returns"

print(implied_returns)

barplot(implied_returns_vector,
        main      = "Implied Excess Equilibrium Returns",
        xlab      = "Stocks",
        ylab      = "Implied Returns",
        col       = "steelblue",
        names.arg = tickers)

# ---- Risk-free rate adjustment -----------------------------------------------
# Use the 3-month Treasury bill rate from FRED as the risk-free proxy.
getSymbols("DTB3", src = "FRED")
risk_free_rate <- mean(na.omit(DTB3[paste0(start_date, "/", end_date)])) / 100

implied_excess_returns <- implied_returns_vector - risk_free_rate

# ---- Benchmark sanity check --------------------------------------------------
getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date)
benchmark_returns       <- dailyReturn(Cl(GSPC))
benchmark_annual_return <- mean(benchmark_returns) * 252

cat("Risk-Free Rate (3M T-bill mean):      ", round(risk_free_rate, 4), "\n")
cat("Benchmark Annual Return (S&P 500):    ", round(benchmark_annual_return, 4), "\n")
cat("Implied Excess Equilibrium Returns:\n")
print(implied_excess_returns)

barplot(implied_excess_returns,
        main      = "Implied Excess Equilibrium Returns (Adjusted for Rf)",
        xlab      = "Stocks",
        ylab      = "Implied Excess Returns",
        col       = "steelblue",
        names.arg = tickers)
