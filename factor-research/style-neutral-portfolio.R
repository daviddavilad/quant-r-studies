# ==============================================================================
# Style-Neutral Portfolio Construction
# ------------------------------------------------------------------------------
# Constructs a value/growth style-neutral portfolio over a configurable date
# range by going long an equal-weighted value basket (JPM, XOM, PG) and short
# an equal-weighted growth basket (AAPL, MSFT, GOOGL, AMZN, TSLA, NVDA), then
# plots the cumulative returns of all three series for comparison.
#
# The style-neutral return is the daily long-short spread. When value
# outperforms growth, the line rises; when growth outperforms, it falls. The
# resulting return stream is roughly market-neutral by construction (long and
# short legs share systematic market exposure), so the time series is closer
# to a pure style factor than to a directional bet.
#
# Pulls prices via quantmod from Yahoo. No external CSV required.
# Produces: cumulative-return plot for value, growth, and style-neutral
#           portfolios over the chosen window.
# ==============================================================================

library(quantmod)
library(ggplot2)
library(dplyr)
library(tidyr)
library(PerformanceAnalytics)
library(scales)

plot_style_neutral_portfolio <- function(start_date, end_date) {
  symbols <- c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA",
                "JPM", "XOM", "PG", "NVDA")

  # ---- Download and build tidy returns ---------------------------------------
  stock_list <- lapply(symbols, function(sym) {
    xt <- tryCatch(
      getSymbols(sym, src = "yahoo",
                 from = start_date, to = end_date,
                 auto.assign = FALSE),
      error = function(e) NULL
    )
    if (is.null(xt) || nrow(xt) == 0) return(NULL)

    r <- Return.calculate(Cl(xt), method = "discrete")
    r <- r[-1, , drop = FALSE]
    tibble::tibble(
      date    = as.Date(zoo::index(r)),
      returns = as.numeric(zoo::coredata(r)),
      symbol  = sym
    )
  })

  stock_df <- bind_rows(Filter(Negate(is.null), stock_list)) %>% drop_na()
  if (nrow(stock_df) == 0) {
    stop("No data downloaded for the requested period.")
  }

  # ---- Style groups (adjust as needed) ---------------------------------------
  value_stocks  <- c("JPM", "XOM", "PG")
  growth_stocks <- c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA", "NVDA")

  have          <- unique(stock_df$symbol)
  value_stocks  <- intersect(value_stocks,  have)
  growth_stocks <- intersect(growth_stocks, have)
  if (length(value_stocks) == 0 || length(growth_stocks) == 0) {
    stop("One of the groups is empty after downloads.")
  }

  # ---- Equal-weighted style baskets ------------------------------------------
  value_returns <- stock_df %>%
    filter(symbol %in% value_stocks) %>%
    group_by(date) %>%
    summarise(value_return = mean(returns), .groups = "drop")

  growth_returns <- stock_df %>%
    filter(symbol %in% growth_stocks) %>%
    group_by(date) %>%
    summarise(growth_return = mean(returns), .groups = "drop")

  # ---- Long value / short growth = style-neutral -----------------------------
  returns_combined <- inner_join(value_returns, growth_returns,
                                  by = "date") %>%
    mutate(
      style_neutral_return = value_return - growth_return,
      cum_value            = cumprod(1 + value_return)         - 1,
      cum_growth           = cumprod(1 + growth_return)        - 1,
      cum_style_neutral    = cumprod(1 + style_neutral_return) - 1
    )

  # ---- Plot ------------------------------------------------------------------
  plot_df <- returns_combined %>%
    select(date, cum_value, cum_growth, cum_style_neutral) %>%
    pivot_longer(-date, names_to = "Strategy", values_to = "CumulativeReturn") %>%
    mutate(Strategy = recode(Strategy,
                              "cum_value"         = "Value",
                              "cum_growth"        = "Growth",
                              "cum_style_neutral" = "Style-Neutral"))

  ggplot(plot_df, aes(x = date, y = CumulativeReturn, color = Strategy)) +
    geom_line(linewidth = 1) +
    labs(title = "Cumulative Returns: Value vs Growth vs Style-Neutral",
         x = "Date", y = "Cumulative Return", color = "Portfolio") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    theme_minimal(base_size = 13)
}

# ---- Example call ------------------------------------------------------------
plot_style_neutral_portfolio("2023-01-01", "2024-01-01")
