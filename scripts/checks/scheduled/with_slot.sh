#!/usr/bin/env bash
# The shared (non-legacy) helper must coordinate landing/repro and every runner user.
set -euo pipefail
source scripts/checks/lib/gate_env.sh
source scripts/checks/lib/bounded.sh
source scripts/checks/lib/heavy_slots.sh
heavy_slot_acquire
trap 'heavy_slot_release' EXIT
budget="$1"; shift
bounded_run "$budget" -- "$@"
