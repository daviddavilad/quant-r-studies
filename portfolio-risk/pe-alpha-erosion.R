# ==============================================================================
# Private Equity Alpha Erosion
# ------------------------------------------------------------------------------
# Tests the hypothesis that the historical PE return premium has compressed
# as the asset class has scaled, using publicly listed PE proxy ETFs (PSP,
# PEX) as the empirical anchor. Liquid PE proxies are imperfect (they trade
# at NAV and exclude private-fund features such as carry waterfalls and
# lock-ups), but they sidestep the well-known smoothing bias in reported
# private NAVs and allow a like-for-like risk-adjusted comparison against
# public equities and bonds.
#
# Three views:
#   (1) Rolling 1-year Sharpe across asset classes, with crisis regimes shaded.
#   (2) PE-only rolling Sharpe with a LOESS trend - the alpha-erosion signal.
#   (3) Period-by-period Sharpe across four documented regimes.
#
# Pulls prices via quantmod from Yahoo. No external CSV required.
# Produces: three ggplot panels plus annualized return tables for the full
#           period and the 2022-2023 rate-hike cycle.
# ==============================================================================

library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(scales)
library(xts)
library(zoo)

# ---- Data --------------------------------------------------------------------
US.tickers    <- c("SPY", "DIA", "QQQ", "MDY", "IJR", "IWC")
Bonds.tickers <- c("TLT", "BND", "TIP", "PHB", "BWX")
PE.tickers    <- c("PSP", "PEX")

getSymbols(c(US.tickers, Bonds.tickers, PE.tickers),
           from = "2014-01-01", periodicity = "daily")

make_port <- function(prices_xts, name) {
  r <- Return.calculate(prices_xts, method = "log")[-1]
  w <- rep(1 / ncol(r), ncol(r))
  p <- Return.portfolio(r, weights = w, rebalance_on = "years")
  colnames(p) <- name
  p
}

US.Port   <- make_port(merge(Ad(SPY), Ad(DIA), Ad(QQQ),
                              Ad(MDY), Ad(IJR), Ad(IWC)), "US.Equities")
Bond.Port <- make_port(merge(Ad(TLT), Ad(BND), Ad(TIP),
                              Ad(PHB), Ad(BWX)),         "Bonds")
PE.Port   <- make_port(merge(Ad(PSP), Ad(PEX)),          "PE.ETFs")

Rf_daily <- 0.0303 / 252  # 3.03% annualized risk-free, daily equivalent

# ---- Rolling Sharpe ----------------------------------------------------------
roll_sharpe <- function(port_xts, window = 252) {
  rs <- rollapply(port_xts, width = window,
                  FUN   = function(x) mean(x - Rf_daily) / sd(x) * sqrt(252),
                  align = "right")
  data.frame(date   = index(rs),
             sharpe = as.numeric(rs),
             Asset  = colnames(port_xts))
}

df_us   <- roll_sharpe(US.Port)
df_bond <- roll_sharpe(Bond.Port)
df_pe   <- roll_sharpe(PE.Port)
df_all  <- rbind(df_us, df_bond, df_pe)

# ---- Regime shading helper ---------------------------------------------------
add_regimes <- function(p) {
  p +
    annotate("rect", xmin = as.Date("2020-02-01"),
             xmax = as.Date("2020-06-01"),
             ymin = -Inf, ymax = Inf, alpha = 0.10, fill = "steelblue") +
    annotate("rect", xmin = as.Date("2022-01-01"),
             xmax = as.Date("2023-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.10, fill = "orange") +
    annotate("text", x = as.Date("2020-04-01"), y = 3.8,
             label = "COVID", size = 3, color = "steelblue4") +
    annotate("text", x = as.Date("2022-10-01"), y = 3.8,
             label = "Rate Hike\nCycle", size = 3, color = "darkorange")
}

# ---- Plot 1: All asset classes -----------------------------------------------
p1 <- add_regimes(
  ggplot(df_all, aes(x = date, y = sharpe, color = Asset)) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    scale_color_manual(values = c("US.Equities" = "steelblue",
                                    "Bonds"       = "tomato",
                                    "PE.ETFs"     = "darkgreen")) +
    labs(title    = "Rolling 1-Year Sharpe: All Asset Classes",
         subtitle = "Declining PE ETF trend = alpha erosion over time",
         x = "Date", y = "Sharpe Ratio", color = "",
         caption  = "Rf = 3.03% annualized | Window = 252 trading days") +
    theme_classic() +
    theme(legend.position = "bottom")
)
print(p1)

# ---- Plot 2: PE Sharpe with LOESS trend (the alpha erosion signal) -----------
p2 <- add_regimes(
  ggplot(df_pe, aes(x = date, y = sharpe)) +
    geom_line(color = "darkgreen", linewidth = 0.8, alpha = 0.6) +
    geom_smooth(method = "loess", se = TRUE,
                color = "black", fill = "gray80",
                linetype = "dashed", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title    = "PE ETF Rolling Sharpe with LOESS Trend",
         subtitle = "Downward trend = alpha erosion; band = 95% CI",
         x = "Date", y = "Sharpe Ratio",
         caption  = "LOESS trend reveals long-run direction") +
    theme_classic()
)
print(p2)

# ---- Plot 3: Period-by-period decomposition ----------------------------------
periods <- list(
  "Pre-COVID\n(2014-2019)"     = c("2014-01-01", "2019-12-31"),
  "COVID Crash\n(2020)"        = c("2020-01-01", "2020-12-31"),
  "Recovery\n(2021)"           = c("2021-01-01", "2021-12-31"),
  "Rate Hike Cycle\n(2022-23)" = c("2022-01-01", "2023-12-31")
)
port_list <- list(US.Port, Bond.Port, PE.Port)

period_sharpe <- do.call(rbind, lapply(names(periods), function(p_name) {
  start_d <- as.Date(periods[[p_name]][1])
  end_d   <- as.Date(periods[[p_name]][2])
  do.call(rbind, lapply(port_list, function(port) {
    w  <- window(port, start = start_d, end = end_d)
    r  <- as.numeric(na.omit(w))
    sr <- (mean(r - Rf_daily) / sd(r)) * sqrt(252)
    data.frame(Period = p_name,
               Asset  = colnames(port),
               Sharpe = round(sr, 3))
  }))
}))

p3 <- ggplot(period_sharpe, aes(x = Period, y = Sharpe, fill = Asset)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("US.Equities" = "steelblue",
                                 "Bonds"       = "tomato",
                                 "PE.ETFs"     = "darkgreen")) +
  geom_text(aes(label = round(Sharpe, 2)),
            position = position_dodge(width = 0.9),
            vjust    = ifelse(period_sharpe$Sharpe >= 0, -0.4, 1.2),
            size     = 3) +
  labs(title    = "Sharpe Ratio by Market Regime",
       subtitle = "PE ETF alpha erodes most sharply during rate hike cycle",
       x = NULL, y = "Sharpe Ratio", fill = "",
       caption  = "Rf = 3.03% annualized") +
  theme_classic() +
  theme(legend.position = "bottom")
print(p3)

# ---- Annualized stats tables -------------------------------------------------
cat("\n====== Full Period Annualized Statistics ======\n")
all_ports <- merge(US.Port, Bond.Port, PE.Port, join = "inner")
table.AnnualizedReturns(all_ports, Rf = Rf_daily, digits = 4)

cat("\n====== 2022 Rate Hike Cycle Only ======\n")
crisis_ports <- window(all_ports,
                        start = as.Date("2022-01-01"),
                        end   = as.Date("2023-12-31"))
table.AnnualizedReturns(crisis_ports, Rf = Rf_daily, digits = 4)
