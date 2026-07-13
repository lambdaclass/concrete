#!/usr/bin/env bash
# Phase 6C #8: the observability validation artifact.
#
# Ties the Phase 6C tooling together over a small program and asserts the whole
# observability surface is live and self-consistent: telemetry (#1), complexity
# slopes (#2), pipeline trace (#3), counterexample reduction (#4), gate
# mutation-testing (#5, sampled — see note), pass-output replay hashes (#6), and
# the cache-free incremental shadow + edit corpus (#7). Publishes a one-line
# census per component. It observes/repls facts owned by earlier phases; it does
# NOT enable caching or add a semantic truth source.
#
# NOTE on #5: the full gate-mutation sweep rebuilds the compiler per mutation
# (~15-20 min) and is a nightly/manual gate (`make test-gate-mutation-coverage`).
# Here we run only the two GREP-ONLY families (constructor coverage, source-maps),
# which need no rebuild, as a fast liveness sample; the full sweep is the nightly.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
[ -x ".lake/build/bin/concrete" ] || { echo "error: build first" >&2; exit 2; }
PASS=0; FAIL=0
run(){ # <label> <cmd...>
  local label="$1"; shift
  if "$@" >/tmp/6c_sub.log 2>&1; then echo "  ok   $label"; PASS=$((PASS+1));
  else echo "  FAIL $label"; sed 's/^/        /' /tmp/6c_sub.log | tail -4; FAIL=$((FAIL+1)); fi
}

echo "=== Phase 6C observability capstone ==="
run "#1 telemetry (--emit-trace-json schema)"        bash scripts/tests/check_pipeline_telemetry.sh
run "#2 anti-superlinear complexity guard"           bash scripts/tests/check_compiler_complexity.sh
run "#3 pipeline trace (first failing stage)"        bash scripts/tests/check_trace_pipeline.sh
run "#4 counterexample reduction + replay"           bash scripts/tests/check_counterexample_reduction.sh
run "#5 gate mutation sample (grep-only families)"   bash -c 'FAMILY=5 bash scripts/tests/check_gate_mutation_coverage.sh && FAMILY=6 bash scripts/tests/check_gate_mutation_coverage.sh'
run "#6 pass-output replay hashes"                   bash scripts/tests/check_pass_hashes.sh
run "#7 incremental shadow manifest + edit corpus"   bash scripts/tests/check_incremental_shadow.sh

echo
echo "--- census ---"
echo "  observability components live: $PASS/$((PASS+FAIL))"
echo "  #5 full mutation sweep (10 families, rebuild-per-mutation): nightly via 'make test-gate-mutation-coverage'"
echo
echo "PHASE-6C-OBSERVABILITY: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
