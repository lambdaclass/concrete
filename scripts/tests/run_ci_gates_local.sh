#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Run every gate command the CI workflow runs, locally (ROADMAP #34a).
#
# The fast suite is NOT the CI gate set: golden baselines, proof gates, and
# per-feature check_*.sh gates run only in CI, and twice (2026-06, 2026-07)
# CI stayed red for 14-15 pushes while local fast-suite green was treated as
# done. This script extracts the gate commands FROM the workflow file itself
# (so the list cannot drift) and runs them sequentially, printing only
# failures. Run it before pushing anything that touches the compiler or the
# gates; a full pass takes a while (proof/oracle gates are slow) — that is
# still ~3x faster than one CI round-trip.
#
# Usage: scripts/tests/run_ci_gates_local.sh [filter-substring]

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
WORKFLOW=".github/workflows/lean_action_ci.yml"
[ -x ".lake/build/bin/concrete" ] || { echo "error: build first" >&2; exit 2; }
FILTER="${1:-}"

PASS=0; FAIL=0; FAILED=""
while read -r cmd; do
  # normalize ./ prefix; give check_ll1.py its argument
  cmd="${cmd/bash .\//bash }"
  [ "$cmd" = "python3 scripts/check_ll1.py" ] && cmd="python3 scripts/check_ll1.py grammar/concrete.ebnf"
  if [ -n "$FILTER" ] && [[ "$cmd" != *"$FILTER"* ]]; then continue; fi
  if eval "$cmd" >/dev/null 2>&1; then
    PASS=$((PASS+1))
  else
    FAIL=$((FAIL+1)); FAILED="$FAILED\n  FAIL $cmd"
    echo "  FAIL $cmd"
  fi
done < <(grep -oE "(bash|python3) [^ ]*scripts[^ ]*\.(sh|py)" "$WORKFLOW" | sort -u)

echo
echo "CI-GATES-LOCAL: PASS=$PASS FAIL=$FAIL"
[ -n "$FAILED" ] && echo -e "Failures:$FAILED"
[ "$FAIL" -eq 0 ]
