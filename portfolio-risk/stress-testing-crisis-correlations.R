# ==============================================================================
# Stress Testing and Crisis Correlations
# ------------------------------------------------------------------------------
# Two complementary risk analyses on a diversified portfolio:
#
#   (1) Stress testing. Decomposes portfolio beta against three risk factors
#       (S&P 500 market, Fed Funds rate, technology sector), then applies
#       a -25% market shock to estimate the corresponding portfolio return.
#       The inverse calculation answers the question "what market move would
#       produce a 50% portfolio drawdown?"
#
#   (2) Crisis correlations. Computes rolling 60-day correlations between
#       major asset classes (US equities, bonds, PE ETFs, commodities)
#       through documented stress episodes (COVID-19 crash, 2022 rate-hike
#       cycle), illustrating where the classic 60/40 diversification
#       assumption fails. The 2022 episode is particularly informative:
#       stock-bond correlation flipped positive, breaking the diversification
#       benefit that has anchored institutional asset allocation for decades.
#
# Pulls prices via quantmod from Yahoo and FRED. No external CSV required.
# Produces: risk factor regression table, +/- shock scenarios, rolling
#           correlation plots, 2022 cumulative-return divergence chart,
#           and pre-2022 vs 2022 correlation heatmaps.
# ==============================================================================

library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(scales)
library(xts)
library(zoo)
library(gridExtra)

# ==============================================================================
# PART 1: STRESS TESTING VIA FACTOR REGRESSION
# ==============================================================================

# ---- Build an equal-weighted portfolio ---------------------------------------
tickers <- c("VB", "TGT", "IVV", "FNSBX")
e <- new.env()
getSymbols(tickers, env = e, from = "2024-01-01")
e <- eapply(e, to.monthly)
port_prices <- do.call(merge, lapply(e, Ad))

port <- ROC(port_prices, type = "discrete")
port <- port[apply(port, 1, function(x) all(!is.na(x))), ]
port <- reclass(coredata(port) %*% c(rep(1 / ncol(port), ncol(port))),
                match.to = port)
colnames(port) <- "port"
m.idx <- index(port)

rm(e, tickers)

# ---- Factor regressions: market, rates, sector -------------------------------
get_port_risk <- function(port) {
  dat <- new.env()
  ii <- 1

  run_regression <- function(risk_xts, label) {
    risk_xts <- risk_xts[m.idx]
    tmp      <- merge(port, risk_xts)
    tmp[is.na(tmp)] <- 0
    fit      <- lm(tmp[, 1] ~ tmp[, 2])

    alpha <- round(as.numeric(coef(fit)[1] * 12), 4)
    beta  <- round(as.numeric(coef(fit)[2]), 4)
    corr  <- round(as.numeric(cor(tmp[, 1], tmp[, 2])), 4)
    pvals <- round(summary(fit)$coefficients[, 4], 2)

    data.frame(BETA = beta, COR = corr, ALPHA = alpha,
               pval.ALPHA = pvals[1], pval.BETA = pvals[2],
               BM = label, RISK = label,
               stringsAsFactors = FALSE)
  }

  # Market: S&P 500
  spx <- ROC(Ad(to.monthly(getSymbols("^GSPC", from = "2024-01-01",
                                       auto.assign = FALSE),
                            name = "GSPC")), type = "discrete")
  assign(paste0("RISK", ii), run_regression(spx, "Market (^GSPC)"),
         envir = dat); ii <- ii + 1

  # Rates: Fed Funds
  ff <- getSymbols.FRED("FEDFUNDS",
                         env = .GlobalEnv, auto.assign = FALSE)
  assign(paste0("RISK", ii), run_regression(ff, "Fed Funds Rate"),
         envir = dat); ii <- ii + 1

  # Sector: Tech (XLK)
  xlk <- ROC(Ad(to.monthly(getSymbols("XLK", from = "2024-01-01",
                                       auto.assign = FALSE),
                            name = "XLK")), type = "discrete")
  assign(paste0("RISK", ii), run_regression(xlk, "Technology Sector"),
         envir = dat)

  do.call(rbind, mget(names(dat), envir = dat))
}

ALL <- get_port_risk(port)
ALL <- ALL[order(ALL$ALPHA, decreasing = TRUE), ]
print(ALL)

# ---- Stress scenarios --------------------------------------------------------
# Forward shock: what would a -25% market move imply for the portfolio?
exp_return <- function(market_ret, rf, beta) {
  round(rf + beta * (market_ret - rf), 4)
}

ALL$shock_down <- exp_return(-0.25, rf = 0.0075,
                              beta = as.numeric(ALL$BETA))

# Inverse: what market move corresponds to a -50% portfolio return?
implied_market <- function(target_port_ret, rf, beta) {
  round(((beta - 1) * rf + target_port_ret) / beta, 4)
}

ALL$implied_50pct_loss <- implied_market(-0.5, rf = 0.0075,
                                          beta = as.numeric(ALL$BETA))

cat("\nStress scenarios:\n")
print(ALL[, c("RISK", "BETA", "shock_down", "implied_50pct_loss")])


# ==============================================================================
# PART 2: CRISIS CORRELATIONS ACROSS ASSET CLASSES
# ==============================================================================

US.tickers    <- c("SPY", "DIA", "QQQ", "MDY", "IJR", "IWC")
Bonds.tickers <- c("TLT", "BND", "TIP", "PHB", "BWX")
PE.tickers    <- c("PSP", "PEX")
Comm.tickers  <- c("DJP", "GSG", "PDBC")

getSymbols(c(US.tickers, Bonds.tickers, PE.tickers, Comm.tickers),
           from = "2014-01-01", periodicity = "daily")

# ---- Build equal-weighted asset-class portfolios -----------------------------
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
Comm.Port <- make_port(merge(Ad(DJP), Ad(GSG), Ad(PDBC)), "Commodities")

all.ports <- na.omit(merge(US.Port, Bond.Port, PE.Port, Comm.Port,
                            join = "inner"))

# ---- Crisis band shading -----------------------------------------------------
add_crisis_bands <- function(p) {
  p +
    annotate("rect", xmin = as.Date("2020-02-01"),
             xmax = as.Date("2020-06-01"),
             ymin = -Inf, ymax = Inf, alpha = 0.10, fill = "steelblue") +
    annotate("rect", xmin = as.Date("2022-01-01"),
             xmax = as.Date("2022-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "orange") +
    annotate("text", x = as.Date("2020-04-01"), y = 0.92,
             label = "COVID\nCrash", size = 3, color = "steelblue4") +
    annotate("text", x = as.Date("2022-07-01"), y = 0.92,
             label = "2022 Rate\nHike", size = 3, color = "darkorange")
}

# ---- Rolling 60-day correlation ----------------------------------------------
roll_cor <- function(port1, port2, width = 60) {
  combined <- merge(port1, port2, join = "inner")
  rc <- rollapply(combined,
                  width     = width,
                  FUN       = function(x) cor(x[, 1], x[, 2]),
                  by.column = FALSE, align = "right")
  data.frame(date = index(rc), correlation = as.numeric(rc))
}

df_eq_bond <- roll_cor(US.Port, Bond.Port)
df_eq_pe   <- roll_cor(US.Port, PE.Port)
df_eq_comm <- roll_cor(US.Port, Comm.Port)

p1 <- add_crisis_bands(
  ggplot(df_eq_bond, aes(x = date, y = correlation)) +
    geom_line(color = "steelblue", linewidth = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(title    = "US Equities vs. Bonds",
         subtitle = "Positive in 2022 - diversification failed",
         x = NULL, y = "Rolling 60-Day Correlation") +
    theme_classic()
)

p2 <- add_crisis_bands(
  ggplot(df_eq_pe, aes(x = date, y = correlation)) +
    geom_line(color = "darkgreen", linewidth = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(title    = "US Equities vs. PE ETFs",
         subtitle = "PE ETFs are equity-like - diversification benefit is small",
         x = NULL, y = "Rolling 60-Day Correlation") +
    theme_classic()
)

p3 <- add_crisis_bands(
  ggplot(df_eq_comm, aes(x = date, y = correlation)) +
    geom_line(color = "tomato", linewidth = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(title    = "US Equities vs. Commodities",
         subtitle = "Commodities surged in 2022 - negative correlation = true hedge",
         x = "Date", y = "Rolling 60-Day Correlation") +
    theme_classic()
)

grid.arrange(p1, p2, p3, ncol = 1,
             top = "Rolling Correlations: Crisis Regimes vs. Normal Periods")

# ---- 2022 cumulative returns -------------------------------------------------
crisis_2022 <- window(all.ports,
                       start = as.Date("2022-01-01"),
                       end   = as.Date("2022-12-31"))

cum_2022 <- as.data.frame(
  do.call(merge, lapply(1:ncol(crisis_2022), function(i) {
    cumprod(1 + crisis_2022[, i]) - 1
  }))
)
colnames(cum_2022) <- colnames(crisis_2022)
cum_2022$date      <- index(crisis_2022)

cum_long <- reshape(cum_2022,
                    varying   = colnames(crisis_2022),
                    v.names   = "cum_ret",
                    timevar   = "Asset",
                    times     = colnames(crisis_2022),
                    direction = "long")

ggplot(cum_long, aes(x = date, y = cum_ret, color = Asset)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = c("US.Equities" = "steelblue",
                                  "Bonds"       = "tomato",
                                  "PE.ETFs"     = "darkgreen",
                                  "Commodities" = "orange")) +
  labs(title    = "2022 Crisis: Cumulative Returns by Asset Class",
       subtitle = "Stocks and bonds both fell; commodities rose. 60/40 broke down.",
       x = "Date", y = "Cumulative Return", color = "",
       caption  = "Data: Yahoo Finance") +
  theme_classic() +
  theme(legend.position = "bottom")

# ---- Pre-2022 vs 2022 correlation heatmaps -----------------------------------
pre_2022 <- window(all.ports,
                    start = as.Date("2014-01-01"),
                    end   = as.Date("2021-12-31"))
dur_2022 <- window(all.ports,
                    start = as.Date("2022-01-01"),
                    end   = as.Date("2022-12-31"))

cor_to_df <- function(cor_mat, period_label) {
  df        <- as.data.frame(as.table(cor_mat))
  df$Period <- period_label
  colnames(df)[1:3] <- c("Asset1", "Asset2", "Correlation")
  df
}

cor_all <- rbind(
  cor_to_df(cor(pre_2022, use = "complete.obs"), "Pre-2022 (2014-2021)"),
  cor_to_df(cor(dur_2022, use = "complete.obs"), "During 2022")
)

ggplot(cor_all, aes(x = Asset1, y = Asset2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 3.5) +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "tomato",
                       midpoint = 0, limits = c(-1, 1)) +
  facet_wrap(~ Period) +
  labs(title    = "Correlation Heatmap: Pre-2022 vs During 2022",
       subtitle = "Red = high positive correlation = diversification benefit lost",
       x = NULL, y = NULL,
       caption  = "Data: Yahoo Finance") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
