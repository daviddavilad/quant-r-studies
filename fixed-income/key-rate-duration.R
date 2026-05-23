# ==============================================================================
# Key Rate Duration
# ------------------------------------------------------------------------------
# Decomposes a bond's interest-rate sensitivity into key-rate durations at the
# 2Y, 5Y, and 10Y points on the yield curve. The yield curve is linearly
# interpolated between key rates, each key rate is bumped independently, and
# the bond is repriced to produce the KRD. The KRDs are then used to estimate
# the bond's price change under a non-parallel curve shock.
#
# Parameterized bond: 10Y, 5% coupon, $100 face. No external data required.
# Produces: KRD printout, estimated price change under a shock scenario, and
#           a plot of the interpolated yield curve with key rates marked.
# ==============================================================================

library(ggplot2)

# ---- Bond setup --------------------------------------------------------------
face_value        <- 100
coupon_rate       <- 0.05
years_to_maturity <- 10
times             <- seq(1, years_to_maturity)
coupon            <- coupon_rate * face_value

# ---- Key-rate curve setup ----------------------------------------------------
key_rates   <- c(2, 5, 10)            # key maturities (years)
base_yields <- c(0.02, 0.025, 0.03)   # corresponding base yields

# ---- Linear interpolation of the yield curve ---------------------------------
get_yield_curve <- function(times, key_rates, yields) {
  approx(x = key_rates, y = yields, xout = times, rule = 2)$y
}

# ---- Bond pricing ------------------------------------------------------------
price_bond <- function(yields, times, coupon, face) {
  cash_flows <- rep(coupon, length(times))
  cash_flows[length(cash_flows)] <- cash_flows[length(cash_flows)] + face
  discount_factors <- (1 + yields) ^ (-times)
  sum(cash_flows * discount_factors)
}

base_curve <- get_yield_curve(times, key_rates, base_yields)
price_0    <- price_bond(base_curve, times, coupon, face_value)

# ---- Compute key rate durations ----------------------------------------------
compute_krd <- function(idx, key_rates, yields, bump = 0.0001) {
  bumped_yields      <- yields
  bumped_yields[idx] <- bumped_yields[idx] + bump
  bumped_curve       <- get_yield_curve(times, key_rates, bumped_yields)
  price_bumped       <- price_bond(bumped_curve, times, coupon, face_value)
  - (price_bumped - price_0) / (price_0 * bump)
}

krd_values <- sapply(1:length(key_rates), compute_krd,
                     key_rates = key_rates, yields = base_yields)
names(krd_values) <- paste0("KRD_", key_rates, "Y")

cat("Key Rate Durations:\n")
print(krd_values)
cat("Sum (should be close to modified duration):", round(sum(krd_values), 4), "\n")

# ---- Non-parallel curve shock scenario ---------------------------------------
# Example: +10bps to 2Y, -5bps to 5Y, +2bps to 10Y
yield_shocks <- c(0.0010, -0.0005, 0.0002)

price_change_pct <- sum(krd_values * yield_shocks)
estimated_price  <- price_0 * (1 + price_change_pct)

cat("\nYield Shocks (in bps):           ", yield_shocks * 10000, "\n")
cat("Estimated % Change in Price:     ", round(price_change_pct * 100, 4), "%\n")
cat("Estimated New Price:             ", round(estimated_price, 4), "\n")

# ---- Plot the yield curve ----------------------------------------------------
plot_data <- data.frame(Year = times,
                        Yield = get_yield_curve(times, key_rates, base_yields))

ggplot(plot_data, aes(x = Year, y = Yield)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  geom_point(data = data.frame(Year = key_rates, Yield = base_yields),
             color = "tomato", size = 3) +
  labs(title = "Interpolated Yield Curve with Key Rates",
       y = "Yield", x = "Year") +
  theme_minimal()
