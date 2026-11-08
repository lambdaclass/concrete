#!/usr/bin/env bash
# All orchestration libraries come from this trusted controller, never replayed code.
set -uo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
source "$ROOT/scripts/checks/lib/gate_env.sh" || exit 125
source "$ROOT/scripts/checks/lib/bounded.sh" || exit 125
source "$ROOT/scripts/checks/lib/heavy_slots.sh" || exit 125
heavy_slot_acquire || exit 125
trap 'heavy_slot_release' EXIT
budget="$1"; shift
bounded_run "$budget" -- "$@"; rc=$?
if bounded_is_incomplete "$rc"; then exit 125; fi
# Reserve 125 for orchestration incompleteness; a completed command exiting 125 fails.
[ "$rc" != 125 ] || exit 1
exit "$rc"
