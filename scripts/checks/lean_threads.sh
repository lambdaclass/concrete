#!/usr/bin/env bash
# The shared Lean worker ceiling must hold independently of host core count.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
limits="$(cd "$(dirname "${BASH_SOURCE[0]}")/lib" && pwd)/limits.sh"
# Each probe has a fresh environment; the parent guard must not satisfy the control.
env -u LEAN_NUM_THREADS bash -c 'source "$1"; [ "$LEAN_NUM_THREADS" = 4 ]' _ "$limits"
LEAN_NUM_THREADS=2 bash -c 'source "$1"; [ "$LEAN_NUM_THREADS" = 2 ]' _ "$limits"
LEAN_NUM_THREADS=256 bash -c 'source "$1"; [ "$LEAN_NUM_THREADS" = 4 ]' _ "$limits"
LEAN_NUM_THREADS=999999999999999999999999 bash -c 'source "$1"; [ "$LEAN_NUM_THREADS" = 4 ]' _ "$limits"
for invalid in 0 00 invalid; do
  rc=0
  LEAN_NUM_THREADS="$invalid" bash -c 'source "$1"' _ "$limits" || rc=$?
  [ "$rc" = 2 ] || { echo "FAIL invalid thread count $invalid: exit $rc"; exit 1; }
done
echo "PASS lean_threads"
