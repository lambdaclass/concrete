#!/usr/bin/env bash
# All orchestration libraries come from this trusted controller, never replayed code.
set -uo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
if [ "${1:-}" != --scheduled-context ]; then
  exec python3 -I "$ROOT/scripts/checks/scheduled/suite_budget.py" --slot "${BASH_SOURCE[0]}" "$@"
fi
shift
source "$ROOT/scripts/checks/lib/gate_env.sh" || exit 125
source "$ROOT/scripts/checks/lib/scheduled_process.sh" || exit 125
source "$ROOT/scripts/checks/lib/heavy_slots.sh" || exit 125
heavy_slot_acquire || exit 125
trap 'heavy_slot_release; rm -rf "$BOUNDED_DIR"' EXIT
budget="$1"; shift
BOUNDED_GATE_OUTCOME=1 bounded_run "$budget" -- "$@"; rc=$?
if bounded_is_incomplete "$rc"; then
  echo 'INCOMPLETE scheduled gate: nested watchdog or gate incomplete' >&2
  exit 125
fi
exit "$rc"
