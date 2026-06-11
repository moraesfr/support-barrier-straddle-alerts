#!/usr/bin/env python3
"""Send a daily e-mail summarising support/barrier straddle alerts."""
import csv
import os
import sys
import smtplib
from datetime import date
from email.message import EmailMessage
from pathlib import Path

WORKDIR    = Path(__file__).resolve().parent
LATEST_CSV = WORKDIR / "support_barrier_alert_latest.csv"


def read_rows(csv_path: Path):
    if not csv_path.exists():
        raise FileNotFoundError(f"Arquivo nao encontrado: {csv_path}")
    with csv_path.open(newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def build_summary(rows):
    alerts     = [r for r in rows if str(r.get("is_alert", "")).strip().upper() == "TRUE"]
    near_level = sorted(rows, key=lambda r: float(r.get("dist_pct") or 99))

    lines = []
    lines.append(f"Relatorio diario – Suporte/Barreira + Straddle – {date.today().isoformat()}")
    lines.append("")
    lines.append("Resumo geral:")
    lines.append(f"- Acoes monitoradas: {len(rows)}")
    lines.append(f"- Alertas hoje (dentro da tolerancia): {len(alerts)}")
    lines.append("")
    lines.append("Tabela completa:")
    header = "symbol | last_date | last_close | nearest_level | level_type | dist_pct | days_to_exp | is_alert | status"
    lines.append(header)
    lines.append("-" * len(header))
    for r in rows:
        lines.append(
            f"{r.get('symbol','')} | {r.get('last_date','')} | {r.get('last_close','')} | "
            f"{r.get('nearest_level','')} | {r.get('nearest_level_type','')} | "
            f"{r.get('dist_pct','')} | {r.get('days_to_exp','')} | "
            f"{r.get('is_alert','')} | {r.get('status','')}"
        )

    lines.append("")
    if alerts:
        lines.append("Alertas hoje:")
        for r in alerts:
            lines.append(
                f"- {r.get('symbol')} em {r.get('last_date')}: "
                f"preco={r.get('last_close')} toca {r.get('nearest_level_type')} "
                f"{r.get('nearest_level')} (dist={r.get('dist_pct')}%, {r.get('days_to_exp')} dias p/ venc.)"
            )
    else:
        lines.append("Alertas hoje: nenhum")

    lines.append("")
    lines.append("Nivel mais proximo por ativo:")
    for r in near_level:
        lines.append(
            f"  {r.get('symbol')}: {r.get('nearest_level_type','?')} {r.get('nearest_level','?')} "
            f"({r.get('dist_pct','?')}% de distancia)"
        )

    subject_prefix = "ALERTA S/R" if alerts else "SEM ALERTA"
    subject = f"[{subject_prefix}] Suporte/Barreira Straddle – {date.today().isoformat()}"
    body = "\n".join(lines)
    return subject, body


def get_env(name, required=True, default=None):
    value = os.getenv(name, default)
    if required and (value is None or value == ""):
        raise RuntimeError(f"Variavel de ambiente obrigatoria ausente: {name}")
    return value


def send_email(subject, body, attachment_path: Path):
    smtp_host = get_env("ALERT_SMTP_HOST")
    smtp_port = int(get_env("ALERT_SMTP_PORT", required=False, default="587"))
    smtp_user = get_env("ALERT_SMTP_USER")
    smtp_pass = get_env("ALERT_SMTP_PASS")
    smtp_from = get_env("ALERT_EMAIL_FROM")
    smtp_to   = get_env("ALERT_EMAIL_TO")
    use_tls   = get_env("ALERT_SMTP_USE_TLS", required=False, default="true").lower() == "true"

    recipients = [x.strip() for x in smtp_to.split(",") if x.strip()]
    if not recipients:
        raise RuntimeError("ALERT_EMAIL_TO precisa conter ao menos 1 destinatario")

    msg = EmailMessage()
    msg["Subject"] = subject
    msg["From"]    = smtp_from
    msg["To"]      = ", ".join(recipients)
    msg.set_content(body)

    with attachment_path.open("rb") as f:
        data = f.read()
    msg.add_attachment(data, maintype="text", subtype="csv",
                       filename=attachment_path.name)

    with smtplib.SMTP(smtp_host, smtp_port, timeout=30) as server:
        if use_tls:
            server.starttls()
        server.login(smtp_user, smtp_pass)
        server.send_message(msg)


def main():
    dry_run = "--dry-run" in sys.argv
    rows    = read_rows(LATEST_CSV)
    subject, body = build_summary(rows)

    if dry_run:
        print(subject)
        print()
        print(body)
        return

    send_email(subject, body, LATEST_CSV)
    print("Email enviado com sucesso.")


if __name__ == "__main__":
    main()
