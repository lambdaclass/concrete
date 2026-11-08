#!/usr/bin/env bash
# Shared ID ownership and adversarial controls. Offline is INCOMPLETE, never a pass.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR" || exit 2
source scripts/checks/lib/bounded.sh
rc=0
bounded_run 180 -- python3 -I scripts/ids/controls.py || rc=$?
echo "id reservation controls: exit $rc"
[ "$rc" = 0 ] || exit "$rc"
bounded_run 240 -- python3 -I scripts/ids/allocate.py check
rc=$?
echo "id reservation live check: exit $rc"
bounded_summary
exit "$rc"
