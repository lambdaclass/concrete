#!/usr/bin/env bash
# Fresh isolated source-snapshot census and dependency-order judgment (plan piece P).
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/../checks/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
source scripts/checks/lib/git_sandbox.sh
git_sandbox
export PYTHONDONTWRITEBYTECODE=1
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
BOUNDED_PARENT="$TMP"
source scripts/checks/lib/bounded.sh
rc=0
for seed in 0 1 2 3; do
  export PYTHONHASHSEED="$seed"
  python3 scripts/spec/test_landing_drift.py || exit $?
  python3 scripts/spec/test_landing_pins.py || exit $?
  python3 scripts/spec/test_landing_relocations.py || exit $?
  bounded_run 900 -- python3 scripts/spec/test_landing_progress.py || rc=$?
  if [ "$rc" -ne 0 ]; then break; fi
done
if [ "$rc" -eq 0 ]; then
  bounded_run 1500 -- python3 scripts/spec/landing_manifest.py "$@" || rc=$?
fi
bounded_summary
if bounded_is_incomplete "$rc"; then
  echo 'landing inventory INCOMPLETE: bounded census did not finish' >&2
fi
exit "$rc"
