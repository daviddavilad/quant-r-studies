# ==============================================================================
# Sector Rotation Strategy
# ------------------------------------------------------------------------------
# Monthly long-only sector rotation across the nine SPDR sector ETFs. Each
# month, rank sectors by their trailing N-month return and hold the top-K
# equally weighted for the following month. Cumulative performance is
# compared against the S&P 500 over the full sample.
#
# Default parameters: 3-month lookback, top-3 sectors. Both are easy to vary
# at the top of the script. The strategy is intentionally simple: no
# transaction costs, no risk overlay, no execution slippage. It is the
# standard textbook setup against which more careful implementations can be
# benchmarked.
#
# Pulls prices via tidyquant from Yahoo. No external CSV required.
# Produces: cumulative return plot of the rotation strategy vs S&P 500.
# ==============================================================================

library(tidyquant)
library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)

# ---- Parameters --------------------------------------------------------------
sector_etfs <- c("XLK", "XLF", "XLV", "XLE", "XLY",
                  "XLI", "XLP", "XLU", "XLB")
look_back   <- 3      # months
top_n       <- 3      # number of sectors to hold

start_date <- "2010-01-01"

# ---- Sector monthly returns --------------------------------------------------
sector_data <- tq_get(sector_etfs, from = start_date,
                       to = Sys.Date(), get = "stock.prices")

monthly_returns <- sector_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, mutate_fun = monthlyReturn) %>%
  ungroup()

returns_wide <- monthly_returns %>%
  pivot_wider(names_from = symbol, values_from = monthly.returns) %>%
  arrange(date) %>%
  drop_na()

dates <- returns_wide$date

# ---- Backtest ----------------------------------------------------------------
# At time i, use the past `look_back` months to rank sectors, then hold the
# top-N equally weighted at time i+1.
portfolio_ret <- numeric(length(dates) - look_back)

for (i in look_back:(length(dates) - 1)) {
  lookback_slice <- returns_wide[(i - look_back + 1):i, -1]
  forward_return <- returns_wide[i + 1, -1]

  cumret      <- apply(1 + as.matrix(lookback_slice), 2, prod) - 1
  top_sectors <- names(sort(cumret, decreasing = TRUE))[1:top_n]

  portfolio_ret[i - look_back + 1] <- mean(as.numeric(forward_return[top_sectors]))
}

portfolio_returns <- tibble(
  date              = dates[(look_back + 1):length(dates)],
  portfolio_return  = portfolio_ret,
  cumulative_return = cumprod(1 + portfolio_ret) - 1
)

# ---- Benchmark ---------------------------------------------------------------
benchmark <- tq_get("^GSPC", from = start_date,
                     to = Sys.Date(), get = "stock.prices") %>%
  tq_transmute(select = adjusted, mutate_fun = monthlyReturn) %>%
  rename(benchmark_return = monthly.returns)

# ---- Robust month-end alignment ----------------------------------------------
port_df <- portfolio_returns %>%
  mutate(date_ym  = as.yearmon(date),
         date_eom = as.Date(date_ym, frac = 1)) %>%
  select(date = date_eom, date_ym, portfolio_return, cumulative_return)

bench_df <- benchmark %>%
  mutate(date_ym  = as.yearmon(date),
         date_eom = as.Date(date_ym, frac = 1)) %>%
  select(date_bench = date_eom, date_ym, benchmark_return)

plot_data <- inner_join(port_df, bench_df, by = "date_ym") %>%
  arrange(date_ym) %>%
  mutate(benchmark_cum = cumprod(1 + benchmark_return) - 1)

# ---- Plot --------------------------------------------------------------------
ggplot(plot_data, aes(x = date)) +
  geom_line(aes(y = cumulative_return,
                color = "Sector Rotation Portfolio"),
            linewidth = 1.1) +
  geom_line(aes(y = benchmark_cum,
                color = "S&P 500 Benchmark"),
            linewidth = 1.1) +
  labs(title    = "Sector Rotation Strategy vs. S&P 500",
       subtitle = paste("Top", top_n, "sectors; lookback =",
                         look_back, "months"),
       x = "Date", y = "Cumulative Return", color = "Series") +
  theme_minimal(base_size = 13) +
  scale_color_manual(values = c("Sector Rotation Portfolio" = "steelblue",
                                  "S&P 500 Benchmark"        = "tomato")) +
  theme(plot.title = element_text(face = "bold", size = 16))
