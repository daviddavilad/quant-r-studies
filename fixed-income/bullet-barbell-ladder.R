# ==============================================================================
# Bullet, Barbell, and Ladder Bond Portfolios
# ------------------------------------------------------------------------------
# Constructs the three standard maturity-structure portfolios from a universe of
# bonds, computes duration and convexity for each, and prices them across a
# range of yields. A +50bp parallel shock is then applied to compare the three
# strategies' sensitivity to rate moves.
#
# Reads:  data/BondInformation.csv  (Issuer, Coupon, YTM, Maturity, Price,
#         Frequency)
# Produces: portfolio statistics table, price-vs-YTM plot, +50bp shock plot.
# ==============================================================================

library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)

# ---- Load data ---------------------------------------------------------------
bond_data <- read.csv("../data/BondInformation.csv")
bond_data$Maturity <- as.Date(bond_data$Maturity, format = "%m/%d/%Y")
today <- Sys.Date()
bond_data$YearsToMaturity <- as.numeric(difftime(bond_data$Maturity, today,
                                                  units = "days")) / 365

# ---- Duration and convexity --------------------------------------------------
calculate_metrics <- function(price, coupon, ytm, maturity_years, freq = 2) {
  periods        <- round(maturity_years * freq)
  ytm_per_period <- ytm / freq
  coupon_payment <- coupon / freq * 100
  times          <- 1:periods
  cash_flows     <- rep(coupon_payment, periods)
  cash_flows[length(cash_flows)] <- cash_flows[length(cash_flows)] + 100

  pv_factors    <- 1 / (1 + ytm_per_period) ^ times
  pv_cash_flows <- cash_flows * pv_factors

  duration  <- sum(times * pv_cash_flows) / sum(pv_cash_flows) / freq
  convexity <- sum(pv_cash_flows * times * (times + 1)) /
               ((1 + ytm_per_period)^2 * sum(pv_cash_flows)) / freq^2

  c(Duration = duration, Convexity = convexity)
}

metrics <- mapply(calculate_metrics,
                  price          = bond_data$Price,
                  coupon         = bond_data$Coupon,
                  ytm            = bond_data$YTM,
                  maturity_years = bond_data$YearsToMaturity,
                  freq           = bond_data$Frequency)

bond_data$Duration  <- metrics["Duration", ]
bond_data$Convexity <- metrics["Convexity", ]

# ---- Portfolio construction --------------------------------------------------
# Bullet: bonds concentrated around the median maturity
median_maturity <- median(bond_data$YearsToMaturity)
bullet <- bond_data %>% filter(abs(YearsToMaturity - median_maturity) < 2)

# Barbell: short- and long-maturity ends, nothing in the middle
barbell <- bond_data %>%
  filter(YearsToMaturity < quantile(YearsToMaturity, 0.3) |
         YearsToMaturity > quantile(YearsToMaturity, 0.7))

# Ladder: equally spaced maturities across the universe
ladder <- bond_data %>% arrange(YearsToMaturity)
ladder <- ladder[round(seq(1, nrow(ladder), length.out = 5)), ]

portfolio_stats <- function(portfolio) {
  weights   <- rep(1 / nrow(portfolio), nrow(portfolio))
  duration  <- sum(weights * portfolio$Duration)
  convexity <- sum(weights * portfolio$Convexity)
  c(Duration = duration, Convexity = convexity)
}

portfolio_results <- data.frame(
  Portfolio = c("Bullet", "Barbell", "Ladder"),
  Duration  = c(portfolio_stats(bullet)["Duration"],
                portfolio_stats(barbell)["Duration"],
                portfolio_stats(ladder)["Duration"]),
  Convexity = c(portfolio_stats(bullet)["Convexity"],
                portfolio_stats(barbell)["Convexity"],
                portfolio_stats(ladder)["Convexity"])
)
print("Portfolio Statistics:")
print(portfolio_results)

# ---- Price vs yield ----------------------------------------------------------
price_bond <- function(coupon, maturity, ytm, freq = 2) {
  periods        <- round(maturity * freq)
  ytm_per_period <- ytm / freq
  coupon_payment <- coupon / freq * 100
  times          <- 1:periods
  cash_flows     <- rep(coupon_payment, periods)
  cash_flows[length(cash_flows)] <- cash_flows[length(cash_flows)] + 100
  pv_factors <- 1 / (1 + ytm_per_period) ^ times
  sum(cash_flows * pv_factors)
}

portfolio_price <- function(portfolio, new_ytm) {
  prices <- mapply(price_bond,
                   coupon   = portfolio$Coupon,
                   maturity = portfolio$YearsToMaturity,
                   ytm      = new_ytm,
                   freq     = portfolio$Frequency)
  mean(prices)
}

ytm_range   <- seq(0.01, 0.10, by = 0.0025)
price_curve <- data.frame(
  YTM     = ytm_range,
  Bullet  = sapply(ytm_range, function(y) portfolio_price(bullet,  y)),
  Barbell = sapply(ytm_range, function(y) portfolio_price(barbell, y)),
  Ladder  = sapply(ytm_range, function(y) portfolio_price(ladder,  y))
)

price_curve_long <- melt(price_curve, id.vars = "YTM")

ggplot(price_curve_long, aes(x = YTM, y = value, color = variable)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Price vs Yield Curve for Bond Portfolios",
       x = "Yield to Maturity (YTM)",
       y = "Average Portfolio Price",
       color = "Portfolio") +
  theme_minimal() +
  theme(text = element_text(size = 14))

# ---- +50bp parallel shock ----------------------------------------------------
shock_up    <- 0.005
current_ytm <- mean(bond_data$YTM)

price_impact <- data.frame(
  Portfolio    = c("Bullet", "Barbell", "Ladder"),
  Price_Before = c(portfolio_price(bullet,  current_ytm),
                   portfolio_price(barbell, current_ytm),
                   portfolio_price(ladder,  current_ytm)),
  Price_After  = c(portfolio_price(bullet,  current_ytm + shock_up),
                   portfolio_price(barbell, current_ytm + shock_up),
                   portfolio_price(ladder,  current_ytm + shock_up))
)

price_impact$Pct_Change <- 100 *
  (price_impact$Price_After - price_impact$Price_Before) /
  price_impact$Price_Before

print("Price Impact from +50bps Yield Shock:")
print(price_impact)

ggplot(price_impact, aes(x = Portfolio, y = Pct_Change, fill = Portfolio)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = sprintf("%.2f%%", Pct_Change)), vjust = -0.5, size = 5) +
  labs(title = "Impact of +50 bps Yield Increase on Bond Portfolios",
       x = "Portfolio Type", y = "Percentage Change in Price") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  scale_fill_brewer(palette = "Set2", name = "Portfolio")
