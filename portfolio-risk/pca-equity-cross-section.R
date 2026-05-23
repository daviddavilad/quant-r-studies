# ==============================================================================
# PCA on the US Equity Cross-Section
# ------------------------------------------------------------------------------
# Runs a principal component analysis on the daily returns of a broad
# cross-section of US equities. The first principal component typically picks
# up the market factor; subsequent components correspond to statistical
# factors that proxy for sector or style exposures. Reports the variance
# explained by each component and the loadings of the first few components,
# then plots the cumulative variance explained.
#
# The exercise illustrates two practical points: (1) a small number of
# components captures most of the cross-sectional variance in equity returns,
# and (2) statistical factors are unlabeled - interpretation requires
# inspecting the loadings.
#
# Pulls prices via quantmod from Yahoo. No external CSV required.
# Produces: scree plot, cumulative variance plot, loadings table for PC1-PC3.
# ==============================================================================

library(quantmod)
library(ggplot2)
library(reshape2)

# ---- Universe: large, mid, sector representatives ----------------------------
tickers <- c(
  # Large-cap tech
  "AAPL", "MSFT", "GOOGL", "AMZN", "NVDA", "META",
  # Financials
  "JPM", "BAC", "GS", "WFC",
  # Healthcare
  "JNJ", "UNH", "PFE", "ABBV",
  # Energy
  "XOM", "CVX", "COP",
  # Consumer
  "WMT", "HD", "MCD", "KO", "PG",
  # Industrials
  "CAT", "HON", "BA",
  # Utilities / staples
  "NEE", "DUK",
  # Communications
  "T", "VZ", "DIS"
)

start_date <- "2020-01-01"
end_date   <- "2024-12-31"

# ---- Download and build the returns matrix -----------------------------------
get_returns <- function(sym) {
  xt <- tryCatch(
    getSymbols(sym, src = "yahoo",
               from = start_date, to = end_date,
               auto.assign = FALSE),
    error = function(e) NULL
  )
  if (is.null(xt) || nrow(xt) == 0) return(NULL)
  r <- dailyReturn(Cl(xt))
  colnames(r) <- sym
  r
}

return_list <- lapply(tickers, get_returns)
return_list <- Filter(Negate(is.null), return_list)
returns_xts <- do.call(merge, return_list)
returns_mat <- as.matrix(na.omit(returns_xts))

cat("Returns matrix:", nrow(returns_mat), "days x",
    ncol(returns_mat), "stocks\n\n")

# ---- PCA on the returns matrix -----------------------------------------------
# scale. = TRUE standardizes each stock's returns so the variance of every
# series is 1, otherwise high-volatility names would dominate the loadings.
pca <- prcomp(returns_mat, center = TRUE, scale. = TRUE)

# ---- Variance explained ------------------------------------------------------
var_explained     <- pca$sdev^2 / sum(pca$sdev^2)
cum_var_explained <- cumsum(var_explained)

cat("Variance explained by first 5 components:\n")
print(round(var_explained[1:5], 4))
cat("\nCumulative variance explained:\n")
print(round(cum_var_explained[1:5], 4))

# ---- Scree plot --------------------------------------------------------------
n_components <- min(15, length(var_explained))
scree_df <- data.frame(
  Component = factor(paste0("PC", 1:n_components),
                     levels = paste0("PC", 1:n_components)),
  Variance  = var_explained[1:n_components] * 100
)

ggplot(scree_df, aes(x = Component, y = Variance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", Variance)),
            vjust = -0.4, size = 3.2) +
  labs(title = "Scree Plot: Variance Explained by Principal Component",
       subtitle = paste("US equity cross-section, daily returns,",
                        start_date, "to", end_date),
       x = NULL, y = "Variance Explained (%)") +
  theme_minimal()

# ---- Cumulative variance plot ------------------------------------------------
cum_df <- data.frame(
  Component   = 1:n_components,
  Cumulative  = cum_var_explained[1:n_components] * 100
)

ggplot(cum_df, aes(x = Component, y = Cumulative)) +
  geom_line(linewidth = 1.1, color = "steelblue") +
  geom_point(size = 2.4, color = "steelblue") +
  geom_hline(yintercept = c(80, 90), linetype = "dashed", color = "gray60") +
  labs(title = "Cumulative Variance Explained",
       x = "Number of Components", y = "Cumulative Variance (%)") +
  scale_x_continuous(breaks = 1:n_components) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal()

# ---- Loadings of the first three components ----------------------------------
# Sign convention: prcomp returns loadings up to a sign flip. PC1 typically
# loads positively on every stock (the market factor); we'll flip if needed.
loadings <- pca$rotation[, 1:3]
if (mean(loadings[, 1]) < 0) loadings[, 1] <- -loadings[, 1]

cat("\nLoadings on PC1-PC3 (signs flipped so PC1 is market-like):\n")
print(round(loadings, 3))

# ---- Plot loadings -----------------------------------------------------------
loadings_df <- as.data.frame(loadings)
loadings_df$Ticker <- rownames(loadings_df)
loadings_long <- melt(loadings_df, id.vars = "Ticker",
                      variable.name = "Component", value.name = "Loading")

ggplot(loadings_long, aes(x = reorder(Ticker, Loading), y = Loading,
                          fill = Loading > 0)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Component, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato"),
                    guide = "none") +
  coord_flip() +
  labs(title = "Loadings of Principal Components 1-3",
       x = NULL, y = "Loading") +
  theme_minimal(base_size = 11)
