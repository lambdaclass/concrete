#!/usr/bin/env bash
# Scheduler explicitly selects the shared specification; generic bounded users do not.
source "$(dirname "${BASH_SOURCE[0]}")/bounded.sh"
SCHEDULED_PROCESS="$(dirname "$BOUNDED_SH")/scheduled_process.py"
bounded_run() {
  local retry="" secs
  if [ "${1:-}" = -r ]; then retry="$2"; shift 2; fi
  secs="$1"; shift
  [ "${1:-}" != -- ] || shift
  [ -n "$retry" ] || retry="$(_bounded_mul "$secs" "$BOUNDED_RETRY_FACTOR")"
  python3 -I "$SCHEDULED_PROCESS" --cwd "$PWD" --timeout "$secs" --retry "$retry" --status "$BOUNDED_OUTCOME_FILE" -- "$@"
}
