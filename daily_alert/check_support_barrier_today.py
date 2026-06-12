#!/usr/bin/env python3
"""
check_support_barrier_today.py
Daily support/barrier proximity check for B3 straddle strategy.
Pure Python equivalent of check_support_barrier_today.R (no R dependency).

Logic mirrors support_barrier_straddle.R exactly:
  - pivot_n  = 10  candles each side (only use pivots ≥ pivot_n days old → no lookahead)
  - level_tol = 1.5%  distance to trigger alert
  - cluster_pct = 2%  to merge nearby levels
  - lookback_days = 180  calendar days of history to build S/R levels
  - min_trade_days = 15  minimum calendar days to next monthly expiry
"""

import csv
import socket
import time
from datetime import date, timedelta
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import yfinance as yf

PLOT_DAYS = 90   # calendar days of price history shown in the chart

# ── Parameters ─────────────────────────────────────────────────────────────────
SYMBOLS        = ["PETR4.SA", "VALE3.SA", "ITUB4.SA", "BBDC4.SA", "ABEV3.SA"]
PIVOT_N        = 10
LEVEL_TOL      = 0.015   # 1.5%
CLUSTER_PCT    = 0.02    # 2%
LOOKBACK_DAYS  = 180
MIN_TRADE_DAYS = 15
WORKDIR        = Path(__file__).resolve().parent


# ── Monthly expiry: 3rd Friday of next month ───────────────────────────────────
def next_monthly_expiry(d: date) -> date:
    # Advance to next calendar month
    if d.month == 12:
        yr, mo = d.year + 1, 1
    else:
        yr, mo = d.year, d.month + 1
    first = date(yr, mo, 1)
    fri_count = 0
    for offset in range(31):
        dc = first + timedelta(days=offset)
        if dc.month != mo:
            break
        if dc.weekday() == 4:   # Friday
            fri_count += 1
            if fri_count == 3:
                return dc
    return d + timedelta(days=30)   # fallback


# ── Pivot detection ────────────────────────────────────────────────────────────
def find_pivots(series: np.ndarray, n: int, kind: str) -> np.ndarray:
    """
    Returns boolean array: True where series[i] is a local max/min
    over the symmetric window [i-n, i+n].
    """
    result = np.zeros(len(series), dtype=bool)
    for i in range(n, len(series) - n):
        window = series[i - n: i + n + 1]
        if np.any(np.isnan(window)):
            continue
        if kind == "high":
            result[i] = series[i] == np.max(window)
        else:
            result[i] = series[i] == np.min(window)
    return result


# ── Level clustering ───────────────────────────────────────────────────────────
def cluster_levels(levels: np.ndarray, cpct: float = CLUSTER_PCT) -> np.ndarray:
    if len(levels) == 0:
        return np.array([])
    lvls = np.sort(np.unique(levels))
    groups = []
    cur = [lvls[0]]
    for v in lvls[1:]:
        if abs(v - np.median(cur)) / np.median(cur) <= cpct:
            cur.append(v)
        else:
            groups.append(float(np.median(cur)))
            cur = [v]
    groups.append(float(np.median(cur)))
    return np.array(groups)


# ── Chart helpers ─────────────────────────────────────────────────────────────
def _safe_sym(sym: str) -> str:
    return sym.replace(".", "_").replace("/", "_")


def generate_price_plot(
    sym: str,
    price_df: pd.DataFrame,
    nearest_level: float,
    level_type: str,
    output_dir: Path,
) -> Path:
    """90-day candlestick-style close price chart with S/R horizontal line."""
    df = price_df.tail(PLOT_DAYS).copy()
    df["MM20"] = (
       df["close"]
        .rolling(20)
        .mean()
    )

    df["MM50"] = (
        df["close"]
        .rolling(50)
        .mean()
    )
    dates = pd.to_datetime(df["date"])
    closes = df["close"].values

    color_line = "#e74c3c" if level_type == "resistance" else "#27ae60"
    label_level = f"{level_type.capitalize()} {nearest_level:.2f}"

    fig, ax = plt.subplots(figsize=(11, 4))
    ax.plot(dates, closes, color="steelblue", linewidth=1.4, label="Fechamento")
    ax.plot(
        dates,
        df["MM20"],
        linewidth=1.2,
        label="MM20"
    )
    
    ax.plot(
        dates,
        df["MM50"],
        linewidth=1.2,
        label="MM50"
    )
    ax.axhline(
        y=nearest_level,
        color=color_line,
        linestyle="--",
        linewidth=1.5,
        label=label_level,
    )
    ax.plot(dates.iloc[-1], closes[-1], marker="o", color="steelblue",
            markersize=7, zorder=5)
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%d/%b"))
    ax.xaxis.set_major_locator(mdates.WeekdayLocator(interval=2))
    fig.autofmt_xdate()
    ax.set_title(f"{sym} — Cotação (últimos {PLOT_DAYS} dias)")
    ax.set_xlabel("Data")
    ax.set_ylabel("Preço (BRL)")
    ax.legend(fontsize=9)
    ax.grid(True, alpha=0.3)
    plt.tight_layout()

    plot_path = output_dir / f"{_safe_sym(sym)}_price.png"
    fig.savefig(plot_path, format="png", dpi=110, bbox_inches="tight")
    plt.close(fig)
    return plot_path


# ── Per-symbol check ───────────────────────────────────────────────────────────
def check_symbol(sym: str, max_retries: int = 3) -> dict:
    base = dict(
        symbol=sym,
        last_date="",
        last_close=float("nan"),

        support_level=float("nan"),
        resistance_level=float("nan"),

        support_dist_pct=float("nan"),
        resistance_dist_pct=float("nan"),

        market_zone="",

        nearest_level=float("nan"),
        nearest_level_type="",
        dist_pct=float("nan"),

        days_to_exp="",
        is_alert=False,
        status="ok",
        price_history=None,
    )

    # Download with retry
    raw = None
    fetch_days = LOOKBACK_DAYS + PIVOT_N * 2 + 10
    start = (date.today() - timedelta(days=fetch_days)).isoformat()
    for attempt in range(max_retries):
        try:
            raw = yf.download(sym, start=start, auto_adjust=True, progress=False)
            break
        except (socket.gaierror, OSError) as e:
            wait = min(2 ** attempt, 30)
            print(f"  Network error {sym} (attempt {attempt+1}): {e}")
            if attempt < max_retries - 1:
                time.sleep(wait)
            else:
                base["status"] = f"download_error: {e}"
                return base
        except Exception as e:
            base["status"] = f"download_error: {e}"
            return base

    if raw is None or raw.empty or len(raw) < PIVOT_N * 2 + 5:
        base["status"] = "insufficient_data"
        return base

    # Build OHLC frame
    df = pd.DataFrame({
        "date":  pd.to_datetime(raw.index).date,
        "high":  raw["High"].squeeze().values,
        "low":   raw["Low"].squeeze().values,
        "close": raw["Close"].squeeze().values,
    }).dropna().reset_index(drop=True)

    # Store full price history for chart (assigned after level detection)
    base["price_history"] = df[["date", "close"]].copy()

    today      = df["date"].iloc[-1]
    last_close = float(df["close"].iloc[-1])
    expiry     = next_monthly_expiry(today)
    dtexp      = (expiry - today).days

    base["last_date"]   = str(today)
    base["last_close"]  = round(last_close, 2)
    base["days_to_exp"] = dtexp

    if dtexp < MIN_TRADE_DAYS:
        base["status"]   = "too_close_to_expiry"
        base["is_alert"] = False
        return base

    # History window: last LOOKBACK_DAYS calendar days, excluding today
    lb_start = today - timedelta(days=LOOKBACK_DAYS)
    hist = df[(df["date"] >= lb_start) & (df["date"] < today)].copy().reset_index(drop=True)

    if len(hist) < PIVOT_N * 2 + 1:
        base["status"] = "insufficient_data"
        return base

    # Detect pivots on full history window
    hist["is_ph"] = find_pivots(hist["high"].values, PIVOT_N, "high")
    hist["is_pl"] = find_pivots(hist["low"].values,  PIVOT_N, "low")

    # ── No-lookahead guard: only use pivots confirmed ≥ PIVOT_N days ago ──────
    confirmed = hist[hist["date"] <= today - timedelta(days=PIVOT_N)]

    resistances = confirmed.loc[confirmed["is_ph"], "high"].values
    supports    = confirmed.loc[confirmed["is_pl"], "low"].values
    all_levels  = np.concatenate([resistances, supports])

    if len(all_levels) < 2:
        base["status"] = "no_levels"
        return base

    clustered = cluster_levels(all_levels)

    supports_clustered = clustered[clustered < last_close]
    resistances_clustered = clustered[clustered > last_close]

    support_level = (
        float(np.max(supports_clustered))
        if len(supports_clustered)
        else np.nan
    )

    resistance_level = (
        float(np.min(resistances_clustered))
        if len(resistances_clustered)
        else np.nan
    )

    rel_dists = np.abs(clustered - last_close) / last_close
    nearest_i = int(np.argmin(rel_dists))
    nearest = float(clustered[nearest_i])
    dist = float(rel_dists[nearest_i])
    
    level_type = (
        "support"
        if nearest < last_close
        else "resistance"
    )

    support_dist_pct = (
        (last_close - support_level)
        / last_close
        * 100
        if not np.isnan(support_level)
        else np.nan
    )
    
    resistance_dist_pct = (
        (resistance_level - last_close)
        / last_close
        * 100
        if not np.isnan(resistance_level)
        else np.nan
    )

    if (
        not np.isnan(support_dist_pct)
        and support_dist_pct <= 2
    ):
        market_zone = "Próximo do suporte"
    
    elif (
        not np.isnan(resistance_dist_pct)
        and resistance_dist_pct <= 2
    ):
        market_zone = "Próximo da resistência"
    
    else:
        market_zone = "Região neutra"
    
    base["support_level"] = (
        round(support_level, 4)
        if not np.isnan(support_level)
        else np.nan
    )  

    base["resistance_level"] = (
        round(resistance_level, 4)
        if not np.isnan(resistance_level)
        else np.nan
    )

    base["support_dist_pct"] = (
        round(support_dist_pct, 2)
        if not np.isnan(support_dist_pct)
        else np.nan
    )
    
    base["resistance_dist_pct"] = (
        round(resistance_dist_pct, 2)
        if not np.isnan(resistance_dist_pct)
        else np.nan
    )

    base["market_zone"] = market_zone

    base["nearest_level"] = round(nearest, 4)
    base["nearest_level_type"] = level_type
    base["dist_pct"] = round(dist * 100, 3)

    base["is_alert"] = dist <= LEVEL_TOL
    return base


# ── Generate all charts ────────────────────────────────────────────────────────
def generate_all_charts(rows: list, output_dir: Path) -> dict[str, Path]:
    """Generate one price chart per symbol. Returns {symbol: Path}."""
    charts = {}
    for r in rows:
        ph = r.get("price_history")
        lvl = r.get("nearest_level")
        ltype = r.get("nearest_level_type", "")
        if ph is None or not isinstance(lvl, float) or np.isnan(lvl):
            continue
        try:
            path = generate_price_plot(r["symbol"], ph, lvl, ltype, output_dir)
            charts[r["symbol"]] = path
        except Exception as exc:
            print(f"  Chart error {r['symbol']}: {exc}")
    return charts


# ── Run all and save ───────────────────────────────────────────────────────────
def main():
    rows = [check_symbol(sym) for sym in SYMBOLS]
    rows.sort(key=lambda r: (not r["is_alert"], r.get("dist_pct") or 99))

    today_str   = date.today().isoformat()
    file_today  = WORKDIR / f"support_barrier_alert_{today_str}.csv"
    file_latest = WORKDIR / "support_barrier_alert_latest.csv"

    fields = [
        "symbol",
        "last_date",
        "last_close",

        "support_level",
        "resistance_level",

        "support_dist_pct",
        "resistance_dist_pct",

        "market_zone",

        "nearest_level",
        "nearest_level_type",
        "dist_pct",

        "days_to_exp",
        "is_alert",
        "status"
    ]

    for path in (file_today, file_latest):
        with path.open("w", newline="", encoding="utf-8") as f:
            w = csv.DictWriter(f, fieldnames=fields, extrasaction="ignore")
            w.writeheader()
            w.writerows(rows)

    # Generate price charts (saved as PNG files alongside the CSVs)
    charts = generate_all_charts(rows, WORKDIR)
    if charts:
        print(f"\nGráficos gerados: {', '.join(charts.keys())}")
        for sym, path in charts.items():
            print(f"  {sym}: {path.name}")

    # Console summary
    print(f"\n=== SUPPORT / BARRIER DAILY CHECK ===")
    print(f"Date: {today_str}\n")
    hdr = f"{'symbol':<12} {'last_date':<12} {'last_close':>10} {'nearest_level':>14} {'type':<12} {'dist%':>7} {'days_exp':>9} {'alert?':>7}  status"
    print(hdr)
    print("-" * len(hdr))
    for r in rows:
        print(
            f"{r['symbol']:<12} {r['last_date']:<12} {str(r['last_close']):>10} "
            f"{str(r['nearest_level']):>14} {r['nearest_level_type']:<12} "
            f"{str(r['dist_pct']):>7} {str(r['days_to_exp']):>9} "
            f"{str(r['is_alert']):>7}  {r['status']}"
        )

    alerts = [r for r in rows if r["is_alert"]]
    print(f"\nAlertas hoje: {len(alerts)}")
    for r in alerts:
        print(f"  {r['symbol']}: preço={r['last_close']}  nível={r['nearest_level']} "
              f"({r['nearest_level_type']})  dist={r['dist_pct']}%  dias_exp={r['days_to_exp']}")


if __name__ == "__main__":
    main()
