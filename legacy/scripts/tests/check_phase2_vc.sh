#!/usr/bin/env bash
# Phase 2 VC/discharge VALIDATION ARTIFACT (ROADMAP Phase 2 #3).
#
# The single umbrella gate for the whole VC / external-SMT surface. It checks that every sub-gate
# below is its own CI step (delegated, not re-run: R-0546), so the Phase 2 evidence story is whole:
#   - the VC schema and kernel-checked discharge (omega / bv_decide);
#   - the opt-in external-SMT path, its determinism/provenance, the release-policy
#     gate, the Lean-replay artifact, and the negative/honesty boundaries;
#   - the worked VC-status matrix and the SMT teaching group;
#   - the red-team gate (adversarial inputs stay non-proofs).
# Every evidence class has a worked reference; ordinary linear/bv facts stay on
# omega/bv_decide and never drift into external SMT; counterexamples and
# solver_trusted are visible but never confused with kernel evidence.

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

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

# DELEGATED, NOT RE-RUN (R-0546, scripts/tests/lib/delegated_gates.sh). Every sub-gate is its own CI
# step in this workflow run and its own command in the local full set; running all nine again here
# was most of this gate's 8 CI minutes and asserted nothing the steps do not. What the umbrella keeps
# is the composition: a sub-gate whose step is removed or disabled turns this gate red.
source "$ROOT_DIR/scripts/tests/lib/delegated_gates.sh"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
echo "=== the Phase 2 sub-gates each run once, as their own CI steps ==="
delegated_gates_control ok no
require_delegated ok no "${SUBGATES[@]}"
echo ""

echo "============================================================"
echo "PHASE2-VC: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
