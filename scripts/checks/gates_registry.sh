#!/usr/bin/env bash
# Registry/workflow drift and schema controls; no gate campaigns.
set -uo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT/scripts/checks/lib/gate_env.sh" || exit 2
source "$ROOT/scripts/checks/lib/bounded.sh" || exit 2
bounded_run 20 -- python3 "$ROOT/scripts/checks/lib/gate_manifest.py" check-workflow; rc=$?
if bounded_is_incomplete "$rc"; then echo 'INCOMPLETE gates registry validation'; exit 1; fi
exit "$rc"
