#!/usr/bin/env bash
# Shared bounded validator entry point, also exercised by transport/watchdog controls.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/../checks/lib/bounded.sh"
bounded_run -i /dev/null -r "${RULES_CHECK_RETRY_SECONDS:-240}" "${RULES_CHECK_SECONDS:-120}" -- python3 -I "$(dirname "${BASH_SOURCE[0]}")/rules.py" "$@" || {
  rc=$?
  if bounded_is_incomplete "$rc"; then bounded_summary; echo 'INCOMPLETE: rule validator produced no completed observation' >&2; fi
  exit "$rc"
}
bounded_summary
