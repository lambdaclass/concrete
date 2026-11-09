#!/usr/bin/env bash
# Scheduler explicitly selects the shared specification; generic bounded users do not.
source "$(dirname "${BASH_SOURCE[0]}")/bounded.sh"
SCHEDULED_PROCESS="$(dirname "$BOUNDED_SH")/scheduled_process.py"
bounded_run() {
  local retry="" secs role="gate"
  if [ "${1:-}" = -r ]; then retry="$2"; shift 2; fi
  secs="$1"; shift
  if [ "${1:-}" = --role ]; then role="$2"; shift 2; fi
  [ "${1:-}" != -- ] || shift
  [ -n "$retry" ] || retry="$(_bounded_mul "$secs" "$BOUNDED_RETRY_FACTOR")"
  python3 -I "$SCHEDULED_PROCESS" --cwd "$PWD" --timeout "$secs" --role "$role" --retry "$retry" --status "$BOUNDED_OUTCOME_FILE" -- "$@"
}
