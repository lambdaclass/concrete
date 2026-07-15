#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
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

PASS=0; FAIL=0
for g in "${SUBGATES[@]}"; do
  echo "=== $g ==="
  if bash "scripts/tests/$g"; then PASS=$((PASS+1)); else echo "  >>> $g FAILED"; FAIL=$((FAIL+1)); fi
  echo ""
done

echo "============================================================"
echo "PHASE2-VC: sub-gates PASS=$PASS  FAIL=$FAIL"
if command -v z3 >/dev/null 2>&1; then echo "(Z3 present: solver-trusted / counterexample / determinism rows exercised)"
else echo "(Z3 absent: solver rows degrade honestly to solver_error; emission + boundary rows still run)"; fi
[ "$FAIL" -eq 0 ]
