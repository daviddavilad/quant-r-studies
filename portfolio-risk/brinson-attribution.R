# ==============================================================================
# Brinson-Hood-Beebower Performance Attribution
# ------------------------------------------------------------------------------
# Security-level Brinson attribution at the sector level. For each sector,
# decomposes the active return (portfolio - benchmark) into three components:
#
#   Allocation effect:  (w_p - w_b) * r_b_sector
#       Did the manager over- or underweight sectors that performed well?
#   Selection effect:    w_b * (r_p_sector - r_b_sector)
#       Within each sector, did the manager pick the right securities?
#   Interaction effect: (w_p - w_b) * (r_p_sector - r_b_sector)
#       Joint effect of allocation and selection together.
#
# The three effects sum to the total active return. Useful for evaluating
# whether outperformance came from sector tilts or stock selection.
#
# Pulls prices via quantmod from Yahoo. No external CSV required.
# Produces: console printouts of total returns and per-sector effects, plus
#           an Excel workbook (security-level table, sector-level Brinson
#           decomposition, and summary).
# ==============================================================================

library(quantmod)
library(dplyr)
library(tidyr)
library(writexl)
library(lubridate)

# ---- Portfolio and benchmark structure ---------------------------------------
securities <- tribble(
  ~AssetClass, ~Sector,       ~Ticker, ~w_p,     ~w_b,
  # Equities
  "Equity",    "Tech",        "AAPL",  0.041667, 0.08,
  "Equity",    "Tech",        "MSFT",  0.041667, 0.07,
  "Equity",    "Tech",        "NVDA",  0.041667, 0.05,
  "Equity",    "Financials",  "JPM",   0.041667, 0.05,
  "Equity",    "Financials",  "BAC",   0.041667, 0.05,
  "Equity",    "Financials",  "GS",    0.041667, 0.05,
  "Equity",    "Healthcare",  "JNJ",   0.041667, 0.06,
  "Equity",    "Healthcare",  "UNH",   0.041667, 0.05,
  "Equity",    "Healthcare",  "PFE",   0.041667, 0.04,
  "Equity",    "Industrials", "CAT",   0.041667, 0.04,
  "Equity",    "Industrials", "HON",   0.041667, 0.03,
  "Equity",    "Industrials", "RTX",   0.041667, 0.03,
  # Bonds
  "Bond",      "Govt",        "SHY",   0.041667, 0.05,
  "Bond",      "Govt",        "IEF",   0.041667, 0.07,
  "Bond",      "Govt",        "TLT",   0.041667, 0.08,
  "Bond",      "Corp IG",     "LQD",   0.0625,   0.06,
  "Bond",      "Corp IG",     "VCIT",  0.0625,   0.04,
  "Bond",      "High Yield",  "HYG",   0.0625,   0.03,
  "Bond",      "High Yield",  "JNK",   0.0625,   0.02,
  "Bond",      "Muni",        "MUB",   0.0625,   0.03,
  "Bond",      "Muni",        "VTEB",  0.0625,   0.02
)

# Normalize weights to ensure they each sum to exactly 1
securities <- securities %>%
  mutate(w_p = w_p / sum(w_p),
         w_b = w_b / sum(w_b))

# ---- Period and price downloads ----------------------------------------------
end_date   <- Sys.Date()
start_date <- end_date - 365

tickers <- unique(securities$Ticker)

get_adj <- function(tkr) {
  Ad(getSymbols(tkr, src = "yahoo",
                 from = start_date, to = end_date,
                 auto.assign = FALSE))
}

prices_xts <- do.call(merge, lapply(tickers, get_adj))
colnames(prices_xts) <- tickers
prices_xts <- prices_xts[complete.cases(prices_xts), ]

# Simple period return: last / first - 1
first_prices <- as.numeric(first(prices_xts))
last_prices  <- as.numeric(last(prices_xts))
sec_returns  <- (last_prices / first_prices) - 1
names(sec_returns) <- colnames(prices_xts)

returns_df <- tibble(Ticker = names(sec_returns),
                     r_sec  = as.numeric(sec_returns))

securities_ret <- securities %>%
  left_join(returns_df, by = "Ticker")

# ---- Total returns -----------------------------------------------------------
port_total_return   <- sum(securities_ret$w_p * securities_ret$r_sec)
bench_total_return  <- sum(securities_ret$w_b * securities_ret$r_sec)
active_p_minus_b    <- port_total_return - bench_total_return

cat("Portfolio total return:   ", round(port_total_return, 4), "\n")
cat("Benchmark total return:   ", round(bench_total_return, 4), "\n")
cat("Active return (P - B):    ", round(active_p_minus_b, 4), "\n\n")

# ---- Brinson decomposition at the sector level -------------------------------
sector_level <- securities_ret %>%
  group_by(Sector) %>%
  summarise(
    w_p_sector = sum(w_p),
    w_b_sector = sum(w_b),
    r_p_sector = sum(w_p * r_sec) / w_p_sector,
    r_b_sector = sum(w_b * r_sec) / w_b_sector,
    .groups    = "drop"
  )

sector_brinson <- sector_level %>%
  mutate(
    alloc_effect = (w_p_sector - w_b_sector) * r_b_sector,
    sel_effect   = w_b_sector                * (r_p_sector - r_b_sector),
    inter_effect = (w_p_sector - w_b_sector) * (r_p_sector - r_b_sector)
  )

total_alloc <- sum(sector_brinson$alloc_effect)
total_sel   <- sum(sector_brinson$sel_effect)
total_inter <- sum(sector_brinson$inter_effect)
total_active_brinson <- total_alloc + total_sel + total_inter

cat("Brinson allocation effect: ", round(total_alloc, 4), "\n")
cat("Brinson selection effect:  ", round(total_sel, 4), "\n")
cat("Brinson interaction effect:", round(total_inter, 4), "\n")
cat("Active (Brinson sum):      ", round(total_active_brinson, 4), "\n")

# ---- Excel export ------------------------------------------------------------
security_table <- securities_ret %>%
  select(AssetClass, Sector, Ticker, w_p, w_b, r_sec)

sector_table <- sector_brinson %>%
  select(Sector, w_p_sector, w_b_sector,
         r_p_sector, r_b_sector,
         alloc_effect, sel_effect, inter_effect)

summary_table <- tibble(
  Metric = c("Portfolio total return",
             "Benchmark total return",
             "Active (P - B)",
             "Total allocation effect",
             "Total selection effect",
             "Total interaction effect",
             "Active (Brinson sum)"),
  Value  = c(port_total_return, bench_total_return, active_p_minus_b,
             total_alloc, total_sel, total_inter, total_active_brinson)
)

output_list <- list(
  "Securities"     = security_table,
  "Sector_Brinson" = sector_table,
  "Summary"        = summary_table
)

write_xlsx(output_list, "Brinson_Attribution.xlsx")
cat("\nExcel file written to: Brinson_Attribution.xlsx\n")
