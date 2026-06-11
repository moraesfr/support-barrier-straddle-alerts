#!/usr/bin/env bash
# Daily check: support/barrier proximity alert + straddle e-mail
set -euo pipefail

cd "$(dirname "$0")"

if [[ -f ".env" ]]; then
    set -a
    # shellcheck disable=SC1091
    source .env
    set +a
fi

Rscript check_support_barrier_today.R
python3 send_support_barrier_email.py
