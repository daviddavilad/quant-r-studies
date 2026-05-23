# ==============================================================================
# Cashflow Matching and Immunization
# ------------------------------------------------------------------------------
# Given a stream of future liabilities, constructs a bond portfolio that is
# both PV- and duration-matched to those liabilities via quadratic programming
# (Redington immunization). The fitted portfolio's cashflows are then compared
# year-by-year against the liability schedule to show the cashflow match.
#
# Reads:  data/BondInformation.csv  (Issuer, Coupon, YTM, Maturity, Price,
#         Frequency)
# Produces: liability/portfolio duration printouts, allocation table,
#           cashflow comparison table, and a bar plot of CF vs liabilities.
# ==============================================================================

library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(tidyr)
library(quadprog)
options(scipen = 999)

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

# ---- Liability schedule ------------------------------------------------------
liabilities <- data.frame(
  Year   = c(3, 4, 5, 11),
  Amount = c(200000, 150000, 100000, 150000)
)

discount_rate <- mean(bond_data$YTM)
liabilities$PV       <- liabilities$Amount / (1 + discount_rate) ^ liabilities$Year
liabilities$Duration <- liabilities$Year * liabilities$PV

total_pv           <- sum(liabilities$PV)
liability_duration <- sum(liabilities$Duration) / total_pv

cat("Liability weighted average duration:", round(liability_duration, 4), "\n")

# ---- Bond selection: nearest 10 by duration ----------------------------------
bond_subset <- bond_data %>%
  mutate(Distance = abs(Duration - liability_duration)) %>%
  arrange(Distance) %>%
  head(10)

# ---- Quadratic program for immunization --------------------------------------
# Minimize sum(weights^2) subject to:
#   PV(weights * prices) = PV(liabilities)
#   $-duration(assets)  = $-duration(liabilities)
#   weights >= 0  (no shorting)
Dmat <- diag(nrow(bond_subset))
dvec <- rep(0, nrow(bond_subset))
Amat <- rbind(bond_subset$Price,
              bond_subset$Price * bond_subset$Duration)
bvec <- c(total_pv, total_pv * liability_duration)

res <- solve.QP(Dmat, dvec, t(Amat), bvec, meq = 2)

bond_subset$Weight         <- res$solution
bond_subset$AllocatedValue <- bond_subset$Weight * bond_subset$Price

portfolio_value    <- sum(bond_subset$AllocatedValue)
portfolio_duration <- sum(bond_subset$Weight * bond_subset$Duration *
                          bond_subset$Price) / portfolio_value

cat("Immunized portfolio value:   ", round(portfolio_value, 2), "\n")
cat("Immunized portfolio duration:", round(portfolio_duration, 4), "\n")
print(bond_subset[, c("Issuer", "Price", "Duration", "Weight", "AllocatedValue")])

# ---- Cashflow projection and comparison --------------------------------------
generate_cash_flows <- function(coupon, maturity, price, freq, weight,
                                 issue_date = today) {
  total_value     <- price * weight
  n_periods       <- round(maturity * freq)
  payment_dates   <- seq.Date(from       = issue_date + months(12 / freq),
                              by         = paste0(12 / freq, " months"),
                              length.out = n_periods)
  payment_years   <- year(payment_dates) - year(today)
  coupon_payment  <- (coupon / freq) * 100
  cf              <- rep(coupon_payment, n_periods)
  cf[length(cf)]  <- cf[length(cf)] + 100  # face at maturity
  cf_scaled       <- cf * total_value / 100

  data.frame(Year = payment_years, CashFlow = cf_scaled)
}

all_cash_flows <- data.frame()
for (i in 1:nrow(bond_subset)) {
  bond <- bond_subset[i, ]
  cf_table <- generate_cash_flows(coupon = bond$Coupon,
                                   maturity = bond$YearsToMaturity,
                                   price = bond$Price,
                                   freq = bond$Frequency,
                                   weight = bond$Weight)
  all_cash_flows <- bind_rows(all_cash_flows, cf_table)
}

portfolio_cf_by_year <- all_cash_flows %>%
  group_by(Year) %>%
  summarise(CashFlow = sum(CashFlow)) %>%
  arrange(Year)

liabilities$Year <- as.numeric(liabilities$Year)
cash_flow_comparison <- full_join(portfolio_cf_by_year, liabilities,
                                  by = "Year") %>%
  rename(Portfolio_CashFlow = CashFlow, Liability = Amount) %>%
  mutate(Portfolio_CashFlow = replace_na(Portfolio_CashFlow, 0),
         Liability          = replace_na(Liability, 0),
         Surplus            = Portfolio_CashFlow - Liability)

print("Cash Flow Matching (by Year):")
print(cash_flow_comparison)

# ---- Plot --------------------------------------------------------------------
cf_long <- pivot_longer(cash_flow_comparison,
                         cols      = c("Portfolio_CashFlow", "Liability"),
                         names_to  = "Type",
                         values_to = "Amount")

ggplot(cf_long, aes(x = Year, y = Amount, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Portfolio Cash Flows vs. Liabilities",
       x = "Year", y = "Amount ($)", fill = "Type") +
  theme_minimal() +
  theme(text = element_text(size = 14))
