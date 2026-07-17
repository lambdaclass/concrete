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
# Extraction contract: a workflow line invoking
#   [VAR=value ...] [bash|python3|sh] [./]scripts/....(sh|py) [args...]
# is a gate; the full argument list is preserved (fuzz seeds/counts,
# --trust-gate, the grammar path), as are VAR=value env prefixes.
# EXCLUDED on purpose: check_gate_mutation_coverage.sh — a nightly-only gate
# that mutates compiler source files to verify gate coverage; running it as
# a pre-push step would dirty the working tree it is meant to protect.
#
# Usage: scripts/tests/run_ci_gates_local.sh [filter-substring]

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
WORKFLOW=".github/workflows/lean_action_ci.yml"
[ -x ".lake/build/bin/concrete" ] || { echo "error: build first" >&2; exit 2; }
FILTER="${1:-}"

# The nightly fuzz steps derive SEED from the date in CI; the extracted fuzz
# commands reference "$SEED", which would trip `set -u` here. Mirror CI.
SEED="${SEED:-$(date -u +%Y%m%d)}"

PASS=0; FAIL=0; FAILED=""
while read -r cmd; do
  # normalize ./ prefix after an explicit interpreter
  cmd="${cmd/bash .\//bash }"
  if [ -n "$FILTER" ] && [[ "$cmd" != *"$FILTER"* ]]; then continue; fi
  if eval "$cmd" >/dev/null 2>&1; then
    PASS=$((PASS+1))
  else
    FAIL=$((FAIL+1)); FAILED="$FAILED\n  FAIL $cmd"
    echo "  FAIL $cmd"
  fi
done < <(grep -oE '([A-Z_][A-Z0-9_]*=[^ ;|&]+[[:space:]]+)*((bash|python3|sh)[[:space:]]+)?(\./)?scripts/[^ ;|&]*\.(sh|py)[^;|&]*' "$WORKFLOW" \
         | grep -v 'check_gate_mutation_coverage\.sh' \
         | sed 's/[[:space:]]*$//' \
         | sort -u)

echo
echo "CI-GATES-LOCAL: PASS=$PASS FAIL=$FAIL"
[ -n "$FAILED" ] && echo -e "Failures:$FAILED"
[ "$FAIL" -eq 0 ]
