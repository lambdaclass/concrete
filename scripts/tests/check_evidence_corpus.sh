#!/usr/bin/env bash
# Source-contract integrity gate over the evidence-class corpus.
#
# Snapshots pin exact report TEXT; this gate pins the load-bearing FACT each
# subexample exists to demonstrate, and the cross-checks snapshots can't express:
#   - a `proved_*` subexample's linked theorem actually kernel-checks;
#   - the `stale_proof` subexample is genuinely detected stale (negative case);
#   - each class is reported as the right class.
# Robust to cosmetic report changes; catches real regressions in the
# source-contract path (proof-status / contracts / check-proofs / spec-drift).

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
EC="examples/evidence_classes"
if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER. Run 'make build' first." >&2
  exit 2
fi

PASS=0; FAIL=0

# want <label> <report> <subexample> <substring>
want() {
  local label="$1" report="$2" sub="$3" needle="$4"
  local out; out="$("$COMPILER" "$EC/$sub/src/main.con" --report "$report" 2>&1)"
  if printf '%s' "$out" | grep -qF -- "$needle"; then
    echo "  ok   $label"; PASS=$((PASS + 1))
  else
    echo "  FAIL $label — '$report' missing '$needle'"
    printf '%s\n' "$out" | sed 's/^/      /' | head -20; FAIL=$((FAIL + 1))
  fi
}

echo "=== evidence-class corpus: each class reports correctly ==="
want "proved_by_lean → proved [point]"        proof-status proved_by_lean "proved [point]"
want "omega → kernel decision"                contracts    proved_by_kernel_decision_omega "proved_by_kernel_decision"
want "omega → engine omega"                   contracts    proved_by_kernel_decision_omega "omega"
want "bv → engine bv_decide"                  contracts    proved_by_kernel_decision_bv "bv_decide"
want "partial → partial status"               contracts    partial_contract "partial — one direction proved_by_lean"
want "assumed → assumed_at_entry"             contracts    assumed_boundary "assumed_at_entry"
want "assumed → proved_at_callsite"           contracts    assumed_boundary "proved_at_callsite"
want "trusted → trusted/bypassed"             proof-status trusted_boundary "trusted"
want "runtime → omega-safe access"            contracts    runtime_checked "proved_by_kernel_decision (omega)"
want "runtime → constant in bounds"           contracts    runtime_checked "checked: in bounds"
want "runtime → catches OOB"                  contracts    runtime_checked "VIOLATION"
want "runtime → unproven access"              contracts    runtime_checked "unproven"
want "runtime → div omega-safe"               contracts    runtime_checked "divisor nonzero, no runtime check needed"
want "runtime → div catches /0"               contracts    runtime_checked "VIOLATION: division by zero"
want "runtime → overflow omega-safe"         contracts    runtime_checked "cannot overflow, no runtime check needed"
want "runtime → overflow unproven"           contracts    runtime_checked "bound the operands"

echo ""
echo "=== loop-invariant feeds runtime-safety (bounds from #[invariant]) ==="
rc="$("$COMPILER" "$EC/runtime_checked/src/main.con" --report contracts 2>&1)"
# sum_loop: body a[i] proved from the loop invariant + guard, no #[requires].
if printf '%s' "$rc" | grep -A2 -E '^demo\.sum_loop$' | grep -q "proved_by_kernel_decision (omega)"; then
  echo "  ok   loop-invariant bounds → proved from #[invariant]"; PASS=$((PASS + 1))
else
  echo "  FAIL loop-invariant bounds not proved from #[invariant]"; FAIL=$((FAIL + 1))
fi
# sum_loop_unsound: index mutated before the access → invariant hypothesis
# dropped → access correctly stays unproven (no false green).
if printf '%s' "$rc" | grep -A2 -E '^demo\.sum_loop_unsound$' | grep -q "unproven"; then
  echo "  ok   mutated index → stays unproven (soundness guard)"; PASS=$((PASS + 1))
else
  echo "  FAIL mutated-index access should stay unproven"; FAIL=$((FAIL + 1))
fi

echo ""
echo "=== integrity cross-checks (beyond snapshots) ==="
# proved_by_lean: the linked theorem must actually kernel-check.
pl="$("$COMPILER" "$EC/proved_by_lean/src/main.con" --report check-proofs 2>&1)"
if printf '%s' "$pl" | grep -qE "0 failed"; then
  echo "  ok   proved_by_lean — check-proofs reports 0 failed"; PASS=$((PASS + 1))
else
  echo "  FAIL proved_by_lean — check-proofs did not report 0 failed"
  printf '%s\n' "$pl" | sed 's/^/      /' | tail -8; FAIL=$((FAIL + 1))
fi
# stale_proof: staleness MUST be detected (negative case — a regression here is
# a silent-acceptance bug, the worst kind).
sp="$("$COMPILER" "$EC/stale_proof/src/main.con" --report proof-status 2>&1)"
if printf '%s' "$sp" | grep -qiE "stale"; then
  echo "  ok   stale_proof — drift detected (reported stale)"; PASS=$((PASS + 1))
else
  echo "  FAIL stale_proof — drift NOT detected (silent acceptance!)"; FAIL=$((FAIL + 1))
fi
# tested_by_oracle: the reference produces a full vector set (cheap; the native
# differential run is on-demand via oracle/run_oracle.sh).
n=$("$EC/tested_by_oracle/oracle/reference.py" 0 2>/dev/null | grep -c '|')
if [ "$n" -ge 100 ]; then
  echo "  ok   tested_by_oracle — reference emits $n vectors"; PASS=$((PASS + 1))
else
  echo "  FAIL tested_by_oracle — reference emitted only $n vectors"; FAIL=$((FAIL + 1))
fi

echo ""
echo "EVIDENCE-CORPUS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
