library(quantmod)
library(dplyr)
library(tibble)
library(readr)
library(ggplot2)

# ── Parameters ─────────────────────────────────────────────────────────────────
symbols        <- c("PETR4.SA", "VALE3.SA", "ITUB4.SA", "BBDC4.SA", "ABEV3.SA")
start_date     <- as.Date("2023-01-02")
end_date       <- Sys.Date()
r_annual       <- 0.08   # risk-free rate
target_pct     <- 20     # exit straddle at +20% gain
pivot_n        <- 10     # candles each side to qualify as pivot high/low
level_tol      <- 0.015  # 1.5%  – price must be within this of a level to trigger
cluster_pct    <- 0.02   # 2%    – merge nearby levels into one cluster
lookback_days  <- 180    # calendar days of history used to build S/R levels
min_trade_days <- 15     # skip alert if fewer calendar days remain to expiry


# ── Option pricing ─────────────────────────────────────────────────────────────
black_scholes <- function(S, K, T, r, sigma, type = "call") {
  if (T <= 0 || sigma <= 0) return(0)
  d1  <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2  <- d1 - sigma * sqrt(T)
  val <- if (type == "call") S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
         else                K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
  max(val, 0)
}

straddle_price <- function(S, K, T, r, sigma) {
  black_scholes(S, K, T, r, sigma, "call") +
    black_scholes(S, K, T, r, sigma, "put")
}

straddle_price_v <- Vectorize(straddle_price)   # vectorised wrapper


# ── Monthly expiry (3rd Friday) ────────────────────────────────────────────────
get_next_monthly_expiry <- function(d) {
  for (offset in 1:90) {
    td <- d + offset
    if (format(td, "%m") != format(d, "%m") || format(td, "%Y") != format(d, "%Y")) {
      yr  <- as.integer(format(td, "%Y"))
      mon <- as.integer(format(td, "%m"))
      first <- as.Date(sprintf("%04d-%02d-01", yr, mon))
      fri_n <- 0L
      for (dd in 0:30) {
        dc <- first + dd
        if (format(dc, "%m") != format(first, "%m")) break
        if (as.integer(format(dc, "%w")) == 5L) {
          fri_n <- fri_n + 1L
          if (fri_n == 3L) return(dc)
        }
      }
    }
  }
  d + 30
}
get_next_monthly_expiry_v <- Vectorize(get_next_monthly_expiry)


# ── Rolling 20-day volatility (vectorised, no inner loop) ─────────────────────
calc_rolling_vol <- function(ret_vec, window = 20L) {
  n   <- length(ret_vec)
  out <- rep(NA_real_, n)
  if (n < window) return(out)
  for (i in window:n)
    out[i] <- sd(ret_vec[(i - window + 1L):i], na.rm = TRUE)
  out
}


# ── Pivot detection (vectorised over index, no per-day rescan) ─────────────────
find_pivots <- function(vec, n, type = c("high", "low")) {
  type   <- match.arg(type)
  len    <- length(vec)
  result <- rep(FALSE, len)
  if (len < 2 * n + 1) return(result)
  for (i in (n + 1L):(len - n)) {
    win <- vec[(i - n):(i + n)]
    if (anyNA(win)) next
    result[i] <- if (type == "high") vec[i] == max(win) else vec[i] == min(win)
  }
  result
}


# ── Level clustering ───────────────────────────────────────────────────────────
cluster_levels <- function(lvls, cpct = 0.02) {
  if (length(lvls) == 0L) return(numeric(0))
  lvls <- sort(unique(lvls))
  groups <- list(); cur <- lvls[1]
  for (v in lvls[-1]) {
    if (abs(v - median(cur)) / median(cur) <= cpct) cur <- c(cur, v)
    else { groups[[length(groups) + 1L]] <- median(cur); cur <- v }
  }
  groups[[length(groups) + 1L]] <- median(cur)
  unlist(groups)
}


# ── Per-symbol simulation ──────────────────────────────────────────────────────
simulate_symbol_sb <- function(symbol) {
  cat("Processing", symbol, "...\n")

  raw <- tryCatch(
    getSymbols(symbol, from = start_date, to = end_date, auto.assign = FALSE),
    error = function(e) NULL
  )
  if (is.null(raw) || nrow(raw) < pivot_n * 2 + 20) {
    cat("  Skipped – insufficient data\n"); return(NULL)
  }

  # ── Build OHLCV frame with rolling vol ──────────────────────────────────────
  close_vec <- as.numeric(Cl(raw))
  lr_vec    <- c(NA_real_, diff(log(close_vec)))
  rv_vec    <- calc_rolling_vol(ifelse(is.na(lr_vec), 0, lr_vec), 20L)

  ohlcv <- tibble(
    date        = as.Date(index(raw)),
    open        = as.numeric(Op(raw)),
    high        = as.numeric(Hi(raw)),
    low         = as.numeric(Lo(raw)),
    close       = close_vec,
    rolling_vol = rv_vec,
    is_ph       = find_pivots(as.numeric(Hi(raw)), pivot_n, "high"),
    is_pl       = find_pivots(as.numeric(Lo(raw)),  pivot_n, "low")
  )

  # Pre-compute expiry for every date (slow once, not in alert loop)
  ohlcv$expiry    <- as.Date(get_next_monthly_expiry_v(ohlcv$date))
  ohlcv$days_exp  <- as.integer(ohlcv$expiry - ohlcv$date)

  # ── Build alert events ───────────────────────────────────────────────────────
  # For each day i, S/R levels come from pivot highs/lows in [date-lookback, date-1].
  # We iterate day by day but ALL pivot work is pre-computed above.
  start_i <- max(pivot_n + 1L, which(ohlcv$days_exp >= min_trade_days)[1])
  if (is.na(start_i)) { cat("  No days with sufficient expiry\n"); return(NULL) }

  events <- vector("list", nrow(ohlcv))
  ev_idx <- 0L

  for (i in start_i:nrow(ohlcv)) {
    price <- ohlcv$close[i];      if (is.na(price)) next
    sigma <- ohlcv$rolling_vol[i]; if (is.na(sigma) || sigma <= 0) next
    if (ohlcv$days_exp[i] < min_trade_days) next

    today    <- ohlcv$date[i]
    lb_start <- today - lookback_days

    # Subset pivots in lookback window (before today).
    # Only accept pivots at least pivot_n days old: their right-side confirmation
    # window has fully elapsed, so they are causal and usable in live trading.
    mask <- ohlcv$date < today & ohlcv$date >= lb_start
    conf <- ohlcv$date <= today - pivot_n   # confirmation guard – no lookahead
    res  <- ohlcv$high[mask & conf & ohlcv$is_ph]
    sup  <- ohlcv$low[mask  & conf & ohlcv$is_pl]
    all_lvls <- c(res, sup)
    if (length(all_lvls) < 2L) next

    cl  <- cluster_levels(all_lvls, cluster_pct)
    rd  <- abs(cl - price) / price
    hit <- which(rd <= level_tol)
    if (length(hit) == 0L) next

    best <- hit[which.min(rd[hit])]
    ev_idx <- ev_idx + 1L
    events[[ev_idx]] <- list(
      alert_date  = today,
      alert_price = price,
      alert_vol   = sigma,
      level       = round(cl[best], 4),
      level_type  = if (cl[best] < price) "support" else "resistance",
      dist_pct    = round(rd[best] * 100, 3),
      expiry      = ohlcv$expiry[i],
      days_to_exp = ohlcv$days_exp[i]
    )
  }

  if (ev_idx == 0L) { cat("  No alerts found\n"); return(NULL) }

  alerts_raw <- bind_rows(events[1:ev_idx])

  # De-duplicate: drop rows where level is same cluster and within 5 days of prev
  alerts_df <- alerts_raw %>%
    arrange(alert_date) %>%
    mutate(
      lvl_r    = round(level, 0),
      prev_lvl = lag(lvl_r, default = -Inf),
      prev_dt  = lag(alert_date, default = alert_date[1] - 10L),
      new_ev   = lvl_r != prev_lvl | as.integer(alert_date - prev_dt) > 5L
    ) %>%
    filter(new_ev) %>%
    mutate(symbol = symbol) %>%
    select(symbol, alert_date, alert_price, alert_vol,
           level, level_type, dist_pct, expiry, days_to_exp)

  cat("  Alerts:", nrow(alerts_df), "\n")

  # ── Straddle trades ──────────────────────────────────────────────────────────
  trades <- vector("list", nrow(alerts_df))
  tr_idx <- 0L

  for (i in seq_len(nrow(alerts_df))) {
    adate  <- alerts_df$alert_date[i]
    aprice <- alerts_df$alert_price[i]
    avol   <- alerts_df$alert_vol[i]
    expiry <- alerts_df$expiry[i]

    fwd <- ohlcv %>%
      filter(date >= adate, date <= expiry, !is.na(close), !is.na(rolling_vol))
    if (nrow(fwd) < min_trade_days) next

    strike  <- round(aprice, 2)
    T_entry <- as.numeric(expiry - adate) / 365
    cost    <- straddle_price(aprice, strike, T_entry, r_annual, avol)
    if (is.na(cost) || cost <= 0) next

    pos <- fwd %>%
      mutate(
        T_rem   = as.numeric(expiry - date) / 365,
        sv      = pmax(0, straddle_price_v(close, strike, T_rem, r_annual, rolling_vol)),
        pnl_pct = (sv - cost) / cost * 100
      ) %>%
      filter(!is.na(sv))
    if (nrow(pos) == 0L) next

    exit_hit <- pos %>% filter(pnl_pct >= target_pct) %>% slice(1)
    exit_row <- if (nrow(exit_hit) > 0) exit_hit else pos %>% slice(n())
    exit_rsn <- if (nrow(exit_hit) > 0) paste0(target_pct, "% target") else "Expiry"

    tr_idx <- tr_idx + 1L
    trades[[tr_idx]] <- tibble(
      symbol         = symbol,
      trade_id       = tr_idx,
      alert_date     = adate,
      level          = alerts_df$level[i],
      level_type     = alerts_df$level_type[i],
      dist_pct       = alerts_df$dist_pct[i],
      alert_price    = round(aprice, 2),
      alert_vol      = round(avol,   4),
      strike         = strike,
      expiry_date    = expiry,
      days_to_expiry = as.integer(expiry - adate),
      straddle_cost  = round(cost,                  2),
      exit_date      = exit_row$date[1],
      exit_sv        = round(exit_row$sv[1],        2),
      exit_pnl       = round(exit_row$sv[1] - cost, 2),
      exit_pnl_pct   = round(exit_row$pnl_pct[1],  2),
      exit_reason    = exit_rsn,
      days_held      = as.integer(exit_row$date[1] - adate)
    )
  }

  if (tr_idx == 0L) { cat("  No valid trades\n"); return(NULL) }

  trades_df <- bind_rows(trades[1:tr_idx]) %>%
    mutate(
      scale_factor = 1000 / straddle_cost,
      scaled_cost  = 1000,
      scaled_exit  = exit_sv * scale_factor,
      scaled_pnl   = scaled_exit - 1000
    )

  cat("  Trades:", nrow(trades_df), "\n")

  # ── Per-symbol summary ───────────────────────────────────────────────────────
  capital         <- nrow(trades_df) * 1000
  total_pnl       <- sum(trades_df$scaled_pnl,  na.rm = TRUE)
  win_rate        <- mean(trades_df$scaled_pnl > 0, na.rm = TRUE)
  period_days     <- as.numeric(max(trades_df$exit_date) - min(trades_df$alert_date))
  cdi_pnl         <- capital * ((1 + r_annual)^(period_days / 365) - 1)

  list(
    summary = tibble(
      symbol           = symbol,
      alerts           = nrow(alerts_df),
      trades           = nrow(trades_df),
      capital          = capital,
      total_pnl        = round(total_pnl,                          2),
      total_return_pct = round(100 * total_pnl / capital,          2),
      win_rate_pct     = round(100 * win_rate,                     2),
      avg_hold_days    = round(mean(trades_df$days_held, na.rm=TRUE), 1),
      avg_pnl_trade    = round(mean(trades_df$scaled_pnl, na.rm=TRUE), 2),
      cdi_return_pct   = round(100 * cdi_pnl / capital,            2),
      excess_vs_cdi    = round(total_pnl - cdi_pnl,                2),
      outperf_x        = ifelse(cdi_pnl > 0, round(total_pnl / cdi_pnl, 2), NA_real_)
    ),
    trades = trades_df,
    alerts = alerts_df
  )
}


# ── Run ────────────────────────────────────────────────────────────────────────
results <- lapply(symbols, simulate_symbol_sb)
results <- Filter(Negate(is.null), results)
if (length(results) == 0) stop("No results obtained.")

summary_all <- bind_rows(lapply(results, `[[`, "summary"))
trades_all  <- bind_rows(lapply(results, `[[`, "trades"))
alerts_all  <- bind_rows(lapply(results, `[[`, "alerts"))

# ── Portfolio totals ───────────────────────────────────────────────────────────
port_cap     <- sum(summary_all$capital,   na.rm = TRUE)
port_pnl     <- sum(summary_all$total_pnl, na.rm = TRUE)
port_days    <- as.numeric(max(trades_all$exit_date) - min(trades_all$alert_date))
port_cdi_pnl <- port_cap * ((1 + r_annual)^(port_days / 365) - 1)

portfolio_summary <- tibble(
  symbols_used     = sum(summary_all$trades > 0),
  total_alerts     = nrow(alerts_all),
  total_trades     = nrow(trades_all),
  capital          = port_cap,
  total_pnl        = round(port_pnl,                           2),
  total_return_pct = round(100 * port_pnl / port_cap,          2),
  avg_hold_days    = round(mean(trades_all$days_held, na.rm=TRUE), 1),
  win_rate_pct     = round(100 * mean(trades_all$scaled_pnl > 0, na.rm=TRUE), 2),
  cdi_pnl          = round(port_cdi_pnl,                       2),
  cdi_return_pct   = round(100 * port_cdi_pnl / port_cap,      2),
  excess_vs_cdi    = round(port_pnl - port_cdi_pnl,            2),
  outperf_x        = ifelse(port_cdi_pnl > 0,
                            round(port_pnl / port_cdi_pnl, 2), NA_real_)
)

# ── Save CSVs ──────────────────────────────────────────────────────────────────
write_csv(summary_all,       "sb_straddle_summary_by_symbol.csv")
write_csv(trades_all,        "sb_straddle_trades.csv")
write_csv(alerts_all,        "sb_straddle_alerts.csv")
write_csv(portfolio_summary, "sb_straddle_portfolio_summary.csv")

# ── Plots ──────────────────────────────────────────────────────────────────────
bar_plot <- ggplot(summary_all,
                   aes(reorder(symbol, total_return_pct), total_return_pct, fill = symbol)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(round(total_return_pct, 1), "%")), hjust = -0.1, size = 3.5) +
  coord_flip() + theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold")) +
  labs(title    = "Support/Barrier Straddle – Return by Symbol",
       subtitle = paste0("20% target | R$ 1,000 per trade | tol=", level_tol*100, "%"),
       x = "Symbol", y = "Return on deployed capital (%)")
ggsave("sb_straddle_return_by_symbol.png", bar_plot, width = 10, height = 6, dpi = 120)

equity_curve <- trades_all %>% arrange(alert_date) %>%
  mutate(cum_pnl = cumsum(scaled_pnl))

equity_plot <- ggplot(equity_curve, aes(alert_date, cum_pnl, color = symbol)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.4) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom") +
  labs(title    = "Support/Barrier Straddle – Cumulative P&L",
       subtitle = "Triggered at support/resistance touches",
       x = "Alert date", y = "Cumulative P&L (R$)", color = "Symbol")
ggsave("sb_straddle_equity_curve.png", equity_plot, width = 12, height = 6, dpi = 120)

type_summary <- trades_all %>%
  group_by(symbol, level_type) %>%
  summarise(n = n(), avg_pnl = mean(scaled_pnl, na.rm=TRUE),
            win_rate = 100 * mean(scaled_pnl > 0, na.rm=TRUE), .groups = "drop")

type_plot <- ggplot(type_summary, aes(symbol, avg_pnl, fill = level_type)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  labs(title = "Avg Trade P&L by Level Type",
       x = "Symbol", y = "Avg P&L per trade (R$, scaled R$1k)", fill = "Level type")
ggsave("sb_straddle_by_level_type.png", type_plot, width = 10, height = 6, dpi = 120)

cat("\n=== SUMMARY BY SYMBOL ===\n"); print(summary_all)
cat("\n=== PORTFOLIO SUMMARY ===\n");  print(portfolio_summary)
