#!/usr/bin/env bash
# Phase 2 VC/discharge VALIDATION ARTIFACT (ROADMAP Phase 2 #3).
#
# The single umbrella gate for the whole VC / external-SMT surface. It runs every
# sub-gate so one command proves the Phase 2 evidence story holds end to end:
#   - the VC schema and kernel-checked discharge (omega / bv_decide);
#   - the opt-in external-SMT path, its determinism/provenance, the release-policy
#     gate, the Lean-replay artifact, and the negative/honesty boundaries;
#   - the worked VC-status matrix and the SMT teaching group;
#   - the red-team gate (adversarial inputs stay non-proofs).
# Every evidence class has a worked reference; ordinary linear/bv facts stay on
# omega/bv_decide and never drift into external SMT; counterexamples and
# solver_trusted are visible but never confused with kernel evidence.

set -uo pipefail
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }

SUBGATES=(
  check_vc_schema.sh              # VC schema v1 + kernel discharge classification
  check_vc_discharge_examples.sh  # one worked reference per VC status
  check_smt_path.sh               # opt-in SMT, emission, default purity
  check_smt_negatives.sh          # honesty boundaries (every class is a non-proof)
  check_smt_policy.sh             # release-policy gate on solver_trusted
  check_smt_replay.sh             # Lean-replay artifact + crisp boundary
  check_smt_examples.sh           # teaching group (when useful / when refused)
  check_smt_redteam.sh            # adversarial: nothing mis-proved, no crash
  check_vc_examples.sh            # end-of-Phase-2 examples (one VC/SMT surface each)
)

# CONCURRENT, REPORTED IN ORDER (R-0547). The sub-gates are independent processes with their own
# sandboxes, and a full pass already runs them side by side; in series they made this umbrella one of
# the longest gates of a parallel pass (361 s at JOBS=64). Each sub-gate's output is printed whole, in
# the listed order, and its verdict is its own recorded exit status — a missing status is a failure.
P2_TMP="$(mktemp -d)"; trap 'rm -rf "$P2_TMP"' EXIT
for g in "${SUBGATES[@]}"; do
  ( rc=0; bash "scripts/tests/$g" >"$P2_TMP/$g.log" 2>&1 || rc=$?; echo "$rc" > "$P2_TMP/$g.rc" ) &
done
wait
PASS=0; FAIL=0
for g in "${SUBGATES[@]}"; do
  echo "=== $g ==="
  cat "$P2_TMP/$g.log" 2>/dev/null
  if [ "$(cat "$P2_TMP/$g.rc" 2>/dev/null)" = "0" ]; then PASS=$((PASS+1)); else echo "  >>> $g FAILED"; FAIL=$((FAIL+1)); fi
  echo ""
done

echo "============================================================"
echo "PHASE2-VC: sub-gates PASS=$PASS  FAIL=$FAIL"
if command -v z3 >/dev/null 2>&1; then echo "(Z3 present: solver-trusted / counterexample / determinism rows exercised)"
else echo "(Z3 absent: solver rows degrade honestly to solver_error; emission + boundary rows still run)"; fi
[ "$FAIL" -eq 0 ]
