# ==============================================================================
# Style ETF Correlation Across Windows
# ------------------------------------------------------------------------------
# Computes pairwise return correlations across seven style-factor ETFs (large
# growth, large value, mid growth, mid value, small growth, small value, and
# international) over three time windows:
#
#   - 3-year window: recent regime, dominated by 2022-2024 dynamics.
#   - 5-year window: includes the COVID dislocation and recovery.
#   - Annual breakdown: one correlation matrix per calendar year.
#
# The point is to show that "diversification" across style factors is a
# regime-dependent claim. Pairs that look uncorrelated in a 3-year window can
# look highly correlated in specific years, and vice versa. The annual view
# is the most useful diagnostic for understanding when style diversification
# was real and when it wasn't.
#
# Pulls prices via tidyquant from Yahoo. No external CSV required.
# Produces: three correlation matrices printed to console (3y, 5y, and annual).
# ==============================================================================

library(tidyquant)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrr)
library(lubridate)

# ---- Universe ----------------------------------------------------------------
# VUG: large growth | VTV: large value | IWP: mid growth | IWS: mid value
# IWO: small growth | IWN: small value | VXUS: international ex-US
etf_symbols <- c("VUG", "VTV", "IWP", "IWS", "IWO", "IWN", "VXUS")

start_date <- Sys.Date() - (10 * 365)
end_date   <- Sys.Date()

etf_prices <- tq_get(etf_symbols, from = start_date,
                      to = end_date, get = "stock.prices")

# ---- Helper: daily log returns over a configurable window --------------------
log_returns_window <- function(prices_df, years_back) {
  prices_df %>%
    group_by(symbol) %>%
    arrange(date) %>%
    mutate(daily_return = log(adjusted / lag(adjusted))) %>%
    filter(date >= Sys.Date() - (years_back * 365)) %>%
    select(date, symbol, daily_return) %>%
    na.omit()
}

# ---- 3-year correlation ------------------------------------------------------
returns_3y <- log_returns_window(etf_prices, years_back = 3) %>%
  pivot_wider(names_from = symbol, values_from = daily_return)

cor_3y <- cor(returns_3y %>% select(-date), use = "complete.obs")
cat("3-year correlation matrix:\n")
print(round(cor_3y, 3))

# ---- 5-year correlation ------------------------------------------------------
returns_5y <- log_returns_window(etf_prices, years_back = 5) %>%
  pivot_wider(names_from = symbol, values_from = daily_return)

cor_5y <- cor(returns_5y %>% select(-date), use = "complete.obs")
cat("\n5-year correlation matrix:\n")
print(round(cor_5y, 3))

# ---- Annual correlations -----------------------------------------------------
# Aggregate daily log returns to annual sums (log returns are additive over
# time, so this is the correct aggregation), then correlate cross-sectionally.
annual_returns <- etf_prices %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(daily_return = log(adjusted / lag(adjusted))) %>%
  na.omit() %>%
  mutate(year = year(date)) %>%
  group_by(symbol, year) %>%
  summarise(annual_return = sum(daily_return, na.rm = TRUE),
            .groups = "drop")

returns_annual_wide <- annual_returns %>%
  pivot_wider(names_from = symbol, values_from = annual_return)

cor_annual <- cor(returns_annual_wide %>% select(-year),
                   use = "complete.obs")
cat("\nAnnual-frequency correlation matrix:\n")
print(round(cor_annual, 3))
