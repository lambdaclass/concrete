#!/usr/bin/env bash
# N2: no source edits, no stale oleans; all elaboration happens in a private copy.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/../checks/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT_DIR/scripts/checks/lib/bounded.sh"
bounded_run 900 -- python3 "$ROOT_DIR/scripts/proofs/vacuity_driver.py" "$@"
rc=$?
if bounded_is_incomplete "$rc"; then echo "INCOMPLETE command-budget"; rc=2; fi
bounded_gate_status || rc=2
exit "$rc"
