# Quant R Studies

A working repository of empirical studies in fixed income, portfolio construction, risk decomposition, and factor research, written in R. The studies began as coursework for *MGMT 472/579 — Portfolio Management Practicum* (Dr. Subramanian Iyer, University of New Mexico) and have been cleaned and standalone-ized for public reading.

The point of this repo is not to demonstrate novel methodology — these are textbook techniques, applied carefully — but to make the implementations transparent and reproducible. Where the original homework code took shortcuts (hardcoded paths, in-script package installation, copy-followed structure), I have rewritten for clarity. Where the original method was simply wrong relative to what its filename suggested (an early script labeled "PCA" did univariate variance decomposition rather than principal component analysis), the code has been rebuilt to match what the title claims.

## Structure

```
quant-r-studies/
├── fixed-income/
│   ├── bullet-barbell-ladder.R           # Three maturity-structure portfolios
│   ├── cashflow-immunization.R            # Liability-driven QP immunization
│   └── key-rate-duration.R                # KRD decomposition + curve shock
├── portfolio-risk/
│   ├── black-litterman-implied-views.R    # Reverse optimization of returns
│   ├── james-stein-shrinkage.R            # Shrinkage estimator vs sample mean
│   ├── pca-equity-cross-section.R         # PCA on a broad equity universe
│   ├── stress-testing-crisis-correlations.R  # Factor stress + crisis correlations
│   ├── brinson-attribution.R              # Allocation / selection / interaction
│   └── pe-alpha-erosion.R                 # Rolling Sharpe of PE proxies
├── factor-research/
│   ├── momentum-cross-sectional.R         # 12-1 and 6-1 decile sorts
│   ├── sector-rotation.R                  # Top-N sector rotation backtest
│   ├── style-correlation.R                # Style ETF correlation windows
│   └── style-neutral-portfolio.R          # Long value / short growth
└── data/
    └── BondInformation.csv                # Bond universe for fixed-income scripts
```

## Running the scripts

All scripts are designed to run standalone from their containing folder. Most pull data directly from Yahoo Finance via `quantmod` or `tidyquant` and require no external CSV files; the exceptions are the two fixed-income scripts, which read `data/BondInformation.csv` (provided in the repo).

To run any individual script:

```r
setwd("path/to/quant-r-studies/portfolio-risk")
source("brinson-attribution.R")
```

### Dependencies

Package installation is **not** inlined in the scripts — install once and keep them quiet:

```r
install.packages(c(
  "dplyr", "tidyr", "ggplot2", "reshape2", "lubridate",
  "quantmod", "tidyquant", "zoo", "xts",
  "PerformanceAnalytics", "PortfolioAnalytics",
  "writexl", "quadprog", "corrr", "scales", "gridExtra", "MASS"
))
```

Most scripts touch only a subset; the union is listed above for convenience. Yahoo data downloads occasionally fail or return incomplete histories — re-running usually resolves it.

## What each section is for

### Fixed income

Three scripts on the mechanics of bond portfolio construction. `bullet-barbell-ladder` builds and compares the three standard maturity-structure portfolios under parallel yield-curve shifts. `cashflow-immunization` solves a quadratic program to construct a bond portfolio that is both PV- and duration-matched to a stream of liabilities, then projects and verifies the resulting cashflow match. `key-rate-duration` decomposes a bond's interest-rate exposure across the 2Y, 5Y, and 10Y points on the yield curve and applies a non-parallel shock scenario.

### Portfolio and risk

Six scripts on portfolio construction inputs and risk decomposition. `black-litterman-implied-views` runs the reverse-optimization step (Pi = delta * Sigma * w) that anchors the Black-Litterman framework. `james-stein-shrinkage` applies the positive-part James-Stein estimator to the well-known overfitting problem of using sample means as MVO inputs. `pca-equity-cross-section` runs principal component analysis on the daily returns of a broad US equity universe and reports the variance explained by the first several components. `stress-testing-crisis-correlations` combines a factor-regression stress test with a rolling-correlation study of how diversification breaks down in documented crisis episodes (the 2022 rate-hike cycle in particular). `brinson-attribution` implements the Brinson-Hood-Beebower decomposition (allocation / selection / interaction effects) at the sector level. `pe-alpha-erosion` uses publicly listed PE proxy ETFs to study whether the historical PE risk-adjusted return premium has compressed over time.

### Factor research

Four scripts on factor construction. `momentum-cross-sectional` implements the canonical Jegadeesh-Titman (1993) cross-sectional momentum sort with 12-1 and 6-1 formation windows, producing decile spreads and a winners-minus-losers time series. `sector-rotation` is a simple top-N momentum rotation across the nine SPDR sector ETFs, benchmarked against the S&P 500. `style-correlation` reports pairwise correlations across style-factor ETFs over multiple time windows, making the regime-dependence of "diversification" visible. `style-neutral-portfolio` constructs a long-value / short-growth portfolio and compares its cumulative returns against the two legs individually.

## What you won't find here

Survivorship-bias corrections, transaction costs, borrow costs, or any other realistic-execution overlays. These are research scripts for learning the mechanics, not deployable strategies. The companion repository [axiom-fund](https://github.com/daviddavilad/axiom-fund) shows what these techniques look like when treated as production research artifacts.

Acknowledgements: the underlying material — coursework structure, bond data, and the choice of methods — came from Dr. Subramanian Iyer's MGMT 472/579 sequence. The implementations and writeups are mine.

## License

MIT. See `LICENSE`.
