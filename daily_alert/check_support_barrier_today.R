library(quantmod)
library(dplyr)
library(tibble)
library(readr)

options(warn = -1)

# ── Parameters (must match support_barrier_straddle.R) ────────────────────────
symbols        <- c("PETR4.SA", "VALE3.SA", "ITUB4.SA", "BBDC4.SA", "ABEV3.SA")
pivot_n        <- 10
level_tol      <- 0.015   # 1.5%
cluster_pct    <- 0.02    # 2%
min_trade_days <- 15
lookback_days  <- 180     # calendar days used to identify S/R levels


# ── Helpers (identical to backtest) ───────────────────────────────────────────
find_pivot_highs <- function(high_vec, n = 10) {
  len <- length(high_vec)
  is_pivot <- rep(FALSE, len)
  for (i in (n + 1):(len - n)) {
    window <- high_vec[(i - n):(i + n)]
    if (!anyNA(window) && high_vec[i] == max(window))
      is_pivot[i] <- TRUE
  }
  is_pivot
}

find_pivot_lows <- function(low_vec, n = 10) {
  len <- length(low_vec)
  is_pivot <- rep(FALSE, len)
  for (i in (n + 1):(len - n)) {
    window <- low_vec[(i - n):(i + n)]
    if (!anyNA(window) && low_vec[i] == min(window))
      is_pivot[i] <- TRUE
  }
  is_pivot
}

cluster_levels <- function(levels, cpct = 0.02) {
  if (length(levels) == 0) return(numeric(0))
  lvl <- sort(unique(levels))
  groups <- list()
  current_group <- lvl[1]
  for (i in seq_along(lvl)) {
    if (i == 1) { current_group <- lvl[i]; next }
    if (abs(lvl[i] - median(current_group)) / median(current_group) <= cpct) {
      current_group <- c(current_group, lvl[i])
    } else {
      groups[[length(groups) + 1]] <- median(current_group)
      current_group <- lvl[i]
    }
  }
  groups[[length(groups) + 1]] <- median(current_group)
  unlist(groups)
}

get_next_monthly_expiry <- function(current_date) {
  for (offset in 1:90) {
    td <- current_date + offset
    if (as.numeric(format(td, "%m")) != as.numeric(format(current_date, "%m")) ||
        as.numeric(format(td, "%Y")) != as.numeric(format(current_date, "%Y"))) {
      yr  <- as.numeric(format(td, "%Y"))
      mon <- as.numeric(format(td, "%m"))
      first <- as.Date(paste(yr, mon, "01", sep = "-"))
      fri_count <- 0
      for (d in 0:30) {
        dc <- first + d
        if (as.numeric(format(dc, "%m")) != mon) break
        if (as.numeric(format(dc, "%w")) == 5) {
          fri_count <- fri_count + 1
          if (fri_count == 3) return(dc)
        }
      }
    }
  }
  current_date + 30
}


# ── Per-symbol check ───────────────────────────────────────────────────────────
check_symbol_sb <- function(sym) {
  raw <- tryCatch(
    getSymbols(sym,
               from = Sys.Date() - lookback_days - pivot_n * 2,
               to   = Sys.Date(),
               auto.assign = FALSE),
    error = function(e) NULL
  )

  if (is.null(raw) || nrow(raw) < pivot_n * 2 + 5) {
    return(tibble(
      symbol      = sym,
      last_date   = NA_character_,
      last_close  = NA_real_,
      nearest_level     = NA_real_,
      nearest_level_type = NA_character_,
      dist_pct    = NA_real_,
      days_to_exp = NA_integer_,
      is_alert    = FALSE,
      status      = "download_error"
    ))
  }

  ohlcv <- tibble(
    date  = as.Date(index(raw)),
    high  = as.numeric(Hi(raw)),
    low   = as.numeric(Lo(raw)),
    close = as.numeric(Cl(raw))
  )

  today      <- ohlcv$date[nrow(ohlcv)]
  last_close <- ohlcv$close[nrow(ohlcv)]
  expiry     <- get_next_monthly_expiry(today)
  dtexp      <- as.integer(expiry - today)

  # Identify pivot levels in the lookback window (exclude today's bar)
  hist <- ohlcv %>%
    filter(date >= today - lookback_days & date < today)

  if (nrow(hist) < pivot_n * 2 + 1) {
    return(tibble(
      symbol             = sym,
      last_date          = as.character(today),
      last_close         = round(last_close, 2),
      nearest_level      = NA_real_,
      nearest_level_type = NA_character_,
      dist_pct           = NA_real_,
      days_to_exp        = dtexp,
      is_alert           = FALSE,
      status             = "insufficient_data"
    ))
  }

  hist <- hist %>%
    mutate(
      is_ph = find_pivot_highs(high, pivot_n),
      is_pl = find_pivot_lows(low,  pivot_n)
    )

  # Only use pivots confirmed at least pivot_n days before today.
  # This eliminates lookahead: the symmetric window [i-n, i+n] requires n future
  # candles to confirm a pivot, so a pivot at date d is only safe to use from
  # date d + pivot_n onwards.
  confirmed   <- hist %>% filter(date <= today - pivot_n)
  resistances <- confirmed$high[confirmed$is_ph]
  supports    <- confirmed$low[confirmed$is_pl]
  all_levels  <- c(resistances, supports)

  if (length(all_levels) < 2) {
    return(tibble(
      symbol             = sym,
      last_date          = as.character(today),
      last_close         = round(last_close, 2),
      nearest_level      = NA_real_,
      nearest_level_type = NA_character_,
      dist_pct           = NA_real_,
      days_to_exp        = dtexp,
      is_alert           = FALSE,
      status             = "no_levels"
    ))
  }

  clustered <- cluster_levels(all_levels, cluster_pct)
  rel_dists <- abs(clustered - last_close) / last_close
  nearest_idx   <- which.min(rel_dists)
  nearest_level <- clustered[nearest_idx]
  nearest_dist  <- rel_dists[nearest_idx]
  nearest_type  <- ifelse(nearest_level < last_close, "support", "resistance")

  is_alert <- (nearest_dist <= level_tol) && (dtexp >= min_trade_days)

  tibble(
    symbol             = sym,
    last_date          = as.character(today),
    last_close         = round(last_close, 2),
    nearest_level      = round(nearest_level, 4),
    nearest_level_type = nearest_type,
    dist_pct           = round(nearest_dist * 100, 3),
    days_to_exp        = dtexp,
    is_alert           = is_alert,
    status             = "ok"
  )
}


# ── Run all and save ───────────────────────────────────────────────────────────
rows <- lapply(symbols, check_symbol_sb)
out  <- bind_rows(rows) %>% arrange(desc(is_alert), dist_pct)

daily_stamp <- format(Sys.Date(), "%Y-%m-%d")
file_today  <- paste0("support_barrier_alert_", daily_stamp, ".csv")

write_csv(out, file_today)
write_csv(out, "support_barrier_alert_latest.csv")

cat("\n=== SUPPORT / BARRIER DAILY CHECK ===\n")
cat("Date:", daily_stamp, "\n\n")
print(out)

alerts <- out %>% filter(is_alert)
cat("\nAlerts (price within", level_tol * 100, "% of S/R level):\n")
if (nrow(alerts) == 0) {
  cat("  None\n")
} else {
  print(alerts[, c("symbol", "last_date", "last_close",
                   "nearest_level", "nearest_level_type", "dist_pct", "days_to_exp")])
}

cat("\nSaved files:\n")
cat("  -", file_today, "\n")
cat("  - support_barrier_alert_latest.csv\n")
