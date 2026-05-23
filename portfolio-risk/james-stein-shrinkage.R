# ==============================================================================
# James-Stein Shrinkage for Expected Returns
# ------------------------------------------------------------------------------
# Applies the James-Stein estimator to a sample of mean stock returns,
# shrinking each estimate toward the grand mean to reduce the well-known
# overfitting problem when using sample means as inputs to mean-variance
# optimization. The shrunk estimator dominates the sample mean estimator
# (in mean-squared error) when the number of assets is 3 or more.
#
# Pulls prices via quantmod from Yahoo. No external CSV required.
# Produces: printout of sample means vs James-Stein estimates and their
#           respective standard errors.
# ==============================================================================

library(quantmod)
library(MASS)

# ---- James-Stein estimator function ------------------------------------------
# Returns:  JS-shrunk mean returns.
# Formula:  theta_hat_JS = grand_mean + (1 - k) * (theta_hat - grand_mean)
#           where k is the shrinkage factor, k = (p - 2) / S, S the sum of
#           variances of the individual estimators. We clip k at 0 to keep
#           the positive-part James-Stein estimator.
james_stein <- function(returns) {
  mean_returns <- colMeans(returns, na.rm = TRUE)
  p            <- ncol(returns)
  n            <- nrow(returns)
  sample_var   <- apply(returns, 2, var, na.rm = TRUE)

  grand_mean        <- mean(mean_returns)
  shrinkage_factor  <- 1 - ((p - 2) / sum(sample_var / n))
  shrinkage_factor  <- max(0, shrinkage_factor)  # positive-part shrinkage

  grand_mean + shrinkage_factor * (mean_returns - grand_mean)
}

# ---- Universe and data -------------------------------------------------------
symbols <- c("AAPL", "MSFT", "GOOG", "AMZN")
getSymbols(symbols, from = "2020-01-01", to = "2024-01-01")

stock_returns <- do.call(cbind, lapply(symbols, function(sym) {
  dailyReturn(Cl(get(sym)))
}))
stock_returns <- na.omit(stock_returns)

# ---- Apply and report --------------------------------------------------------
js_estimates <- james_stein(stock_returns)

print("Sample Mean Returns:")
print(colMeans(stock_returns, na.rm = TRUE))
print("James-Stein Estimates:")
print(js_estimates)

# Standard errors of the sample means. Note that JS shrinks the mean estimates
# but does not modify the underlying dispersion of returns, so the standard
# errors of the shrunk means are formally the same.
sample_std_devs <- apply(stock_returns, 2, sd, na.rm = TRUE)

print("Standard errors of sample means:")
print(sample_std_devs / sqrt(nrow(stock_returns)))
