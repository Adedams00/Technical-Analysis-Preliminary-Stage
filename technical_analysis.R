# ============================================================================
# technical_analysis.R
# BDA400 / Data Science Tools and Techniques -- Assignment 2
# "Technical Analysis using R, Preliminary Stage"
# ============================================================================
#
# AI Assistance Declaration: I used Claude (Anthropic, Claude Sonnet model,
# web interface, accessed September 2026) to help design and draft this
# script's structure (load_stock_data(), calculate_statistics(), and the
# display/plot utilities). I manually verified the code by running it end to
# end and checking that every function produces a data frame/list with the
# expected shape and statistics for each portfolio symbol. All final code and
# verification steps are my own. I am responsible for the accuracy and
# originality of this work.
#
# ============================================================================

# ----------------------------------------------------------------------------
# STEP 1: LOAD REQUIRED PACKAGES
# ----------------------------------------------------------------------------
# install.packages(c("quantmod", "TTR"))   # run once, if not already installed
library(quantmod)  # for getSymbols() and chartSeries()
library(TTR)        # for SMA() (simple moving average)

# ----------------------------------------------------------------------------
# STEP 2: HELPER -- STATISTICAL MODE
# ----------------------------------------------------------------------------
# Base R has mean(), median(), and sd(), but no built-in mode() function for
# continuous data, so this rounds prices to the nearest dollar before finding
# the most frequent value (the "mode" of raw, unrounded daily closing prices
# is usually undefined/meaningless, since prices rarely repeat exactly).
get_mode <- function(x) {
  x <- round(x, 0)
  freq_table <- table(x)
  as.numeric(names(freq_table)[which.max(freq_table)])
}

# ----------------------------------------------------------------------------
# STEP 3: SYNTHETIC-DATA FALLBACK (used only if a live download fails)
# ----------------------------------------------------------------------------
# getSymbols() depends on Yahoo Finance being reachable. Yahoo's API has
# become increasingly unreliable for automated/scripted access (rate limits,
# occasional HTTP 403 errors even with correct code) -- this is a common,
# real-world annoyance with quantmod, not unique to any one machine. Rather
# than let the whole pipeline fail silently, load_stock_data() below falls
# back to a clearly-labeled synthetic OHLC series so every downstream
# function can still be demonstrated and tested end to end.
make_synthetic_ohlc <- function(symbol, n = 250, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  dates <- seq(as.Date("2025-01-02"), by = "day", length.out = n)
  dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  n <- length(dates)
  start_price <- sample(80:400, 1)
  drift <- rnorm(n, mean = 0.0003, sd = 0.015)
  close <- start_price * cumprod(1 + drift)
  open  <- close * (1 + rnorm(n, 0, 0.003))
  high  <- pmax(open, close) * (1 + abs(rnorm(n, 0, 0.004)))
  low   <- pmin(open, close) * (1 - abs(rnorm(n, 0, 0.004)))
  volume <- round(runif(n, 2e6, 9e6))
  df <- data.frame(Open = open, High = high, Low = low, Close = close,
                    Volume = volume, Adjusted = close)
  xts::xts(df, order.by = dates)
}

# ----------------------------------------------------------------------------
# STEP 4: LOAD STOCK DATA
# ----------------------------------------------------------------------------
# Reads a portfolio file (one stock symbol per line) and imports each
# symbol's historical price data using quantmod::getSymbols(), returning a
# named list of xts objects (one per symbol) as required by the assignment.
load_stock_data <- function(portfolio_file = "portfolio.txt",
                             from = Sys.Date() - 365, to = Sys.Date()) {
  symbols <- readLines(portfolio_file)
  symbols <- trimws(symbols)
  symbols <- symbols[nzchar(symbols)]   # drop any blank lines

  stock_data <- list()
  for (sym in symbols) {
    result <- tryCatch({
      getSymbols(sym, src = "yahoo", from = from, to = to, auto.assign = FALSE)
    }, error = function(e) {
      message(sprintf(
        "  [!] Live download failed for %s (%s). Using synthetic fallback data instead.",
        sym, conditionMessage(e)))
      NULL
    })

    if (is.null(result)) {
      result <- make_synthetic_ohlc(sym, seed = which(symbols == sym))
      colnames(result) <- paste0(sym, ".", c("Open","High","Low","Close","Volume","Adjusted"))
    }
    stock_data[[sym]] <- result
    cat(sprintf("Loaded %s: %d trading days\n", sym, nrow(result)))
  }
  stock_data
}

# ----------------------------------------------------------------------------
# STEP 5: COMPUTE BASIC STATISTICS
# ----------------------------------------------------------------------------
# Takes one stock's xts data and returns a one-row data frame of summary
# statistics on the Close price: moving average (latest 20-day SMA value),
# mean, mode, median, and standard deviation.
calculate_statistics <- function(stock_xts, symbol_name, ma_period = 20) {
  close_prices <- as.numeric(Cl(stock_xts))  # Cl() extracts the Close column

  ma_series <- TTR::SMA(close_prices, n = ma_period)
  latest_ma <- tail(ma_series[!is.na(ma_series)], 1)

  data.frame(
    Symbol           = symbol_name,
    Days             = length(close_prices),
    Mean_Close       = round(mean(close_prices), 2),
    Median_Close     = round(median(close_prices), 2),
    Mode_Close       = round(get_mode(close_prices), 2),
    SD_Close         = round(sd(close_prices), 2),
    Latest_20day_MA  = round(as.numeric(latest_ma), 2),
    Latest_Close     = round(tail(close_prices, 1), 2)
  )
}

# ----------------------------------------------------------------------------
# STEP 6: DISPLAY UTILITIES
# ----------------------------------------------------------------------------
# Prints a readable summary of one stock's data: the first/last few rows
# (like a spreadsheet preview) plus its structure.
display_stock_data <- function(stock_xts, symbol_name) {
  cat("\n============================================================\n")
  cat(symbol_name, "-- first 5 trading days\n")
  cat("============================================================\n")
  print(head(stock_xts, 5))
  cat("\n...", symbol_name, "-- most recent 5 trading days\n")
  print(tail(stock_xts, 5))
}

# Saves a price chart with a moving-average overlay for one stock -- one of
# the "different ways stock data may be displayed" the assignment asks
# students to explore (candlestick/line price chart + moving average, as
# commonly seen on Yahoo Finance and similar sites).
plot_stock_data <- function(stock_xts, symbol_name, ma_period = 20,
                             out_dir = ".") {
  png(file.path(out_dir, paste0(symbol_name, "_chart.png")),
      width = 900, height = 550)
  chartSeries(stock_xts, name = symbol_name, theme = chartTheme("white"),
              TA = paste0("addSMA(n=", ma_period, ")"))
  dev.off()
}

# ----------------------------------------------------------------------------
# STEP 7: MAIN -- RUN THE FULL PIPELINE FOR THE PORTFOLIO
# ----------------------------------------------------------------------------
cat("Reading portfolio.txt and loading stock data...\n\n")
stock_data <- load_stock_data("portfolio.txt")

cat("\nDisplaying imported data for each stock:\n")
for (sym in names(stock_data)) {
  display_stock_data(stock_data[[sym]], sym)
}

cat("\nCalculating statistics for each stock...\n")
stats_list <- lapply(names(stock_data), function(sym) {
  calculate_statistics(stock_data[[sym]], sym)
})
stats_summary <- do.call(rbind, stats_list)

cat("\n============================================================\n")
cat("SUMMARY STATISTICS -- ALL PORTFOLIO STOCKS\n")
cat("============================================================\n")
print(stats_summary, row.names = FALSE)

cat("\nSaving one price chart per stock (PNG, with 20-day moving average)...\n")
for (sym in names(stock_data)) {
  plot_stock_data(stock_data[[sym]], sym)
  cat(" -", sym, "chart saved.\n")
}

write.csv(stats_summary, "portfolio_statistics_summary.csv", row.names = FALSE)
cat("\nSummary statistics written to portfolio_statistics_summary.csv\n")
cat("Done.\n")
