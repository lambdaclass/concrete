#!/usr/bin/env bash
# All orchestration libraries come from this trusted controller, never replayed code.
set -uo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
source "$ROOT/scripts/checks/lib/gate_env.sh" || exit 125
source "$ROOT/scripts/checks/lib/bounded.sh" || exit 125
source "$ROOT/scripts/checks/lib/heavy_slots.sh" || exit 125
heavy_slot_acquire || exit 125
SCHEDULED_OUTCOME_FILE="$(mktemp)"
SCHEDULED_OUTCOME_HELPER="$ROOT/scripts/checks/lib/gate_outcome.py"
export SCHEDULED_OUTCOME_FILE SCHEDULED_OUTCOME_HELPER
trap 'heavy_slot_release; rm -f "$SCHEDULED_OUTCOME_FILE"; rm -rf "$BOUNDED_DIR"' EXIT
budget="$1"; shift
bounded_run "$budget" -- "$@"; rc=$?
python3 "$SCHEDULED_OUTCOME_HELPER" result "$SCHEDULED_OUTCOME_FILE" "$rc" "${SELECTED_SHA:-}"
exit "$?"
