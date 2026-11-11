#!/usr/bin/env bash
# Fresh isolated source-snapshot census and dependency-order judgment (plan piece P).
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
BOUNDED_PARENT="$TMP"
source scripts/checks/lib/bounded.sh
bounded_run 1500 -- python3 scripts/spec/landing_manifest.py "$@"
bounded_summary
