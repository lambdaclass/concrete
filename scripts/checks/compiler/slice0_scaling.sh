#!/usr/bin/env bash
# R-0550: native, same-process scaling measurements for the slice-0 compiler.
# Usage (inside nix develop): slice0_scaling.sh [REPORT_DIR]
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."
report=${1:-.lake/slice0-scaling-report}
mkdir -p "$report"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
BOUNDED_PARENT="$work"
# shellcheck source=scripts/checks/lib/bounded.sh
source scripts/checks/lib/bounded.sh
# shellcheck source=scripts/checks/lib/limits.sh
source scripts/checks/lib/limits.sh
gate_limits 64 40000

run() {
  local rc=0
  bounded_run -i /dev/null "$@" || rc=$?
  if bounded_is_incomplete "$rc"; then
    bounded_summary >&2
    return 3
  fi
  return "$rc"
}

# Clear previous output before building, including when reusing a local report directory.
: > "$report/measurements.tsv"
: > "$report/identity.txt"
: > "$report/run.log"
run 300 -- lake build slice0-scaling > "$report/build.log" 2>&1 || {
  rc=$?
  cat "$report/build.log" >&2
  exit "$rc"
}
{
  bash scripts/checks/compiler/build_identity.sh
  sha256sum scripts/checks/compiler/slice0_scaling.lean scripts/checks/compiler/slice0_scaling.sh
} > "$report/identity.txt"

rc=0
TIMEFORMAT='measurement_wall_seconds=%3R'
{ time run 60 -- .lake/build/bin/slice0-scaling > "$report/measurements.tsv"; } 2> "$report/run.log" || rc=$?
cat "$report/measurements.tsv"
cat "$report/run.log" >&2
exit "$rc"
