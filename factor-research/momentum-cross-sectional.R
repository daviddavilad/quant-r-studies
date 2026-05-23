# ==============================================================================
# Cross-Sectional Momentum: 12-1 and 6-1 Sorts on US Equities
# ------------------------------------------------------------------------------
# Implements the classic cross-sectional momentum strategy of Jegadeesh and
# Titman (1993): each month, rank stocks by past returns (skipping the most
# recent month to avoid the well-documented short-term reversal), form
# equal-weighted decile portfolios, and study the spread between the top and
# bottom deciles ("winners minus losers", WML).
#
# Two formation windows: 12-1 (twelve-month return with one-month skip) and
# 6-1 (six-month return with one-month skip). Both are evaluated on a
# universe of large- and mid-cap US equities.
#
# The 12-1 sort is the academic standard. Documented decay since publication
# is a stylized fact (McLean and Pontiff 2016) and is visible in the rolling
# returns of the WML portfolio.
#
# Pulls prices via quantmod from Yahoo. No external CSV required.
# Produces: decile-spread bar charts for both signals and a cumulative
#           return plot of WML 12-1 vs WML 6-1 vs the equal-weighted universe.
# ==============================================================================

library(quantmod)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)

# ---- Universe ----------------------------------------------------------------
# A broad large/mid-cap universe. Survivorship is a known limitation; this is
# a study, not a live signal.
tickers <- c(
  "AAPL", "MSFT", "GOOGL", "AMZN", "NVDA", "META", "TSLA",
  "JPM", "BAC", "GS", "WFC", "MS", "C",
  "JNJ", "UNH", "PFE", "ABBV", "MRK", "LLY",
  "XOM", "CVX", "COP", "SLB",
  "WMT", "HD", "MCD", "KO", "PG", "PEP", "COST", "NKE",
  "CAT", "HON", "BA", "GE", "DE",
  "NEE", "DUK", "SO",
  "T", "VZ", "DIS", "CMCSA",
  "INTC", "AMD", "CSCO", "ORCL", "IBM", "CRM", "ADBE"
)

start_date <- "2015-01-01"
end_date   <- Sys.Date()

# ---- Download and assemble monthly returns -----------------------------------
get_monthly_returns <- function(sym) {
  xt <- tryCatch(
    getSymbols(sym, src = "yahoo",
               from = start_date, to = end_date,
               auto.assign = FALSE),
    error = function(e) NULL
  )
  if (is.null(xt) || nrow(xt) == 0) return(NULL)
  m <- to.monthly(Ad(xt), indexAt = "lastof", OHLC = FALSE)
  r <- monthlyReturn(m)
  colnames(r) <- sym
  r
}

return_list <- lapply(tickers, get_monthly_returns)
return_list <- Filter(Negate(is.null), return_list)
monthly_returns_xts <- do.call(merge, return_list)

# Tidy long-format frame: date | ticker | ret
monthly_long <- data.frame(
  date = index(monthly_returns_xts),
  coredata(monthly_returns_xts)
) %>%
  pivot_longer(-date, names_to = "ticker", values_to = "ret") %>%
  arrange(ticker, date)

cat("Universe size:", length(unique(monthly_long$ticker)), "tickers\n")
cat("Months:       ", length(unique(monthly_long$date)), "\n\n")

# ---- Signal construction: J-month return, skip most recent month -------------
build_momentum_signal <- function(df, lookback, skip = 1) {
  df %>%
    group_by(ticker) %>%
    arrange(date) %>%
    mutate(
      # rolling product of (1 + ret) over the lookback window, lagged by `skip`
      momentum = rollapplyr(ret, width = lookback,
                             FUN   = function(x) prod(1 + x) - 1,
                             fill  = NA, align = "right"),
      momentum = lag(momentum, skip)
    ) %>%
    ungroup()
}

mom_12_1 <- build_momentum_signal(monthly_long, lookback = 11, skip = 1)
mom_6_1  <- build_momentum_signal(monthly_long, lookback = 5,  skip = 1)

# ---- Decile sort and WML portfolio --------------------------------------------
# Each month, sort cross-sectionally into deciles, then equal-weight within
# each decile. WML = decile 10 (winners) minus decile 1 (losers).
decile_returns <- function(df_with_signal, n_buckets = 10) {
  df_with_signal %>%
    filter(!is.na(momentum)) %>%
    group_by(date) %>%
    filter(n() >= n_buckets) %>%  # need at least one stock per decile
    mutate(decile = ntile(momentum, n_buckets)) %>%
    group_by(date, decile) %>%
    summarise(port_ret = mean(ret, na.rm = TRUE), .groups = "drop")
}

dec_12_1 <- decile_returns(mom_12_1)
dec_6_1  <- decile_returns(mom_6_1)

# Average return per decile, annualized
decile_summary <- function(dec_df, label) {
  dec_df %>%
    group_by(decile) %>%
    summarise(mean_monthly = mean(port_ret),
              annualized   = (1 + mean(port_ret))^12 - 1,
              .groups = "drop") %>%
    mutate(signal = label)
}

summary_12_1 <- decile_summary(dec_12_1, "12-1")
summary_6_1  <- decile_summary(dec_6_1,  "6-1")
all_summary  <- rbind(summary_12_1, summary_6_1)

cat("12-1 decile annualized returns:\n")
print(summary_12_1[, c("decile", "annualized")])
cat("\n6-1 decile annualized returns:\n")
print(summary_6_1[, c("decile", "annualized")])

# ---- WML time series ---------------------------------------------------------
wml_series <- function(dec_df) {
  dec_df %>%
    filter(decile %in% c(1, 10)) %>%
    pivot_wider(names_from = decile, values_from = port_ret,
                names_prefix = "d") %>%
    mutate(wml = d10 - d1) %>%
    select(date, wml)
}

wml_12_1 <- wml_series(dec_12_1) %>% mutate(signal = "WML 12-1")
wml_6_1  <- wml_series(dec_6_1)  %>% mutate(signal = "WML 6-1")

# Universe equal-weighted return for context
universe_ew <- monthly_long %>%
  group_by(date) %>%
  summarise(wml = mean(ret, na.rm = TRUE), .groups = "drop") %>%
  mutate(signal = "Equal-Weighted Universe")

all_series <- rbind(wml_12_1, wml_6_1, universe_ew) %>%
  group_by(signal) %>%
  arrange(date) %>%
  mutate(cum_ret = cumprod(1 + replace_na(wml, 0)) - 1) %>%
  ungroup()

# ---- Plots -------------------------------------------------------------------
ggplot(all_summary, aes(x = factor(decile), y = annualized * 100,
                         fill = signal)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  scale_fill_manual(values = c("12-1" = "steelblue", "6-1" = "darkgreen")) +
  labs(title    = "Cross-Sectional Momentum: Annualized Return by Decile",
       subtitle = "Decile 1 = past losers; Decile 10 = past winners",
       x = "Decile", y = "Annualized Return (%)", fill = "Signal") +
  theme_minimal()

ggplot(all_series, aes(x = date, y = cum_ret, color = signal)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  scale_color_manual(values = c("WML 12-1" = "steelblue",
                                  "WML 6-1"  = "darkgreen",
                                  "Equal-Weighted Universe" = "gray50")) +
  labs(title    = "Cumulative Return: WML 12-1 vs WML 6-1 vs Universe",
       subtitle = "WML returns shown gross of transaction costs",
       x = "Date", y = "Cumulative Return", color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
