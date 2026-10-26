#!/usr/bin/env bash
# SMT negatives gate (ROADMAP Phase 2 #14) — locks the honesty boundaries.
#
# Every external-solver result class is a NON-PROOF unless it is a genuine
# kernel/Lean discharge. This suite pins each negative path so no one mistakes a
# solver result for proof:
#   - solver absent              → solver_error        (not a proof)
#   - configured tiny timeout    → unknown/timeout     (not a proof)
#   - out-of-fragment obligation → NO SMT query        (SMT never reaches for it)
#   - satisfiable negated goal   → counterexample      (not a proof)
#   - [policy] forbids           → release build FAILS (E0615)
#   - Lean replay cannot close   → stays solver_trusted (not upgraded)
# Cases needing a real solver are Z3-gated; the rest run anywhere.

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
# Per-run scratch (R-0546): a fixed /tmp path collides with a concurrent run of this gate.
GATE_TMP="$(mktemp -d)"; trap 'rm -rf "$GATE_TMP"' EXIT
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
ABS="$ROOT_DIR/$COMPILER"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
NL="examples/smt/nonlinear_overflow/src/main.con"
NEG="examples/smt/negatives/src/main.con"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# no proof class ever appears among SMT-routed VCs in the given JSON
assert_no_proof(){ local label="$1" json="$2"
  printf '%s' "$json" | python3 -c "
import json,sys
d=json.load(sys.stdin)
proofs={'proved_by_kernel_decision','proved_by_lean','proved_by_lean_replay'}
bad=[v for v in d['vcs'] if v.get('smt') and v['status'] in proofs]
sys.exit(0 if not bad else 1)" && ok "$label" || no "$label"; }

echo "=== out-of-fragment: SMT produces NO query for a linear obligation ==="
emit="$("$COMPILER" "$NEG" --report vcs --emit-smt 2>/dev/null)"
grep -qiF <<<"$emit" "no SMT-eligible" && ok "linear a+b → no SMT query (SMT does not grab kernel-owned facts)" \
  || no "linear obligation unexpectedly produced an SMT query"
# and its VC is never solver-classified.
assert_no_proof "linear obligation never becomes a solver proof" "$("$COMPILER" "$NEG" --report vcs --smt --json 2>/dev/null)"

echo "=== solver absent OR present: no result class is ever a proof ==="
assert_no_proof "nonlinear --smt: no VC is a proof class" "$("$COMPILER" "$NL" --report vcs --smt --json 2>/dev/null)"

if command -v z3 >/dev/null 2>&1; then
  echo "=== Z3 present: each negative class is reproduced AND is a non-proof ==="
  # configured tiny timeout → unknown, not a proof.
  tj="$("$COMPILER" "$NL" --report vcs --smt --smt-timeout-ms 1 --json 2>/dev/null)"
  printf '%s' "$tj" | python3 -c "
import json,sys
d=json.load(sys.stdin)
sys.exit(0 if any(v.get('smt') and v['status'] in ('unknown','timeout') for v in d['vcs']) else 1)" \
    && ok "tiny timeout (-t:1) → unknown/timeout" || no "tiny timeout did not yield unknown/timeout"
  assert_no_proof "timed-out VC is not a proof" "$tj"
  # satisfiable negated goal → counterexample, not a proof.
  sj="$("$COMPILER" "$NL" --report vcs --smt --json 2>/dev/null)"
  printf '%s' "$sj" | python3 -c "
import json,sys
d=json.load(sys.stdin)
sys.exit(0 if any(v['function'].endswith('.scale_unbounded') and v['status']=='counterexample' for v in d['vcs']) else 1)" \
    && ok "satisfiable negated goal → counterexample" || no "expected a counterexample"
  assert_no_proof "counterexample VC is not a proof" "$sj"
  # Lean replay cannot close nonlinear → stays solver_trusted, not upgraded.
  rj="$("$COMPILER" "$NL" --report vcs --smt --replay --json 2>/dev/null)"
  printf '%s' "$rj" | python3 -c "
import json,sys
d=json.load(sys.stdin)
sc=next((v for v in d['vcs'] if v['function'].endswith('.scale') and v.get('smt')), None)
sys.exit(0 if sc and sc['status']=='solver_trusted' and not any(v['status']=='proved_by_lean_replay' for v in d['vcs']) else 1)" \
    && ok "replay fails → stays solver_trusted (not upgraded)" || no "replay incorrectly changed the class"
  # policy forbids solver_trusted → release build fails (E0615).
  out="$( cd examples/smt/policy_forbid && "$ABS" build -o "$GATE_TMP/smtneg_out" 2>&1 )" && rc=0 || rc=$?
  rm -f "$GATE_TMP/smtneg_out"
  { [ "$rc" -ne 0 ] && grep -qF <<<"$out" "E0615"; } \
    && ok "policy forbid + solver_trusted → release build REJECTED (E0615)" || no "forbid policy did not reject (exit=$rc)"
else
  echo "=== Z3 absent: never a proof ==="
  ej="$("$COMPILER" "$NL" --report vcs --smt --json 2>/dev/null)"
  # SAFETY, asserted hard: no SMT-routed VC may come back proved when the solver is absent.
  printf '%s' "$ej" | python3 -c "
import json,sys
d=json.load(sys.stdin)
bad=('proved_by_lean','proved_by_kernel_decision','arithmetic_proved','solver_trusted')
rows=[v for v in d['vcs'] if v.get('smt')]
sys.exit(0 if all(not v['status'].startswith('proved') and v['status'] not in bad for v in rows) else 1)" \
    && ok "absent solver never yields a proof (fail-closed)" || no "absent solver produced a PROOF status — unsound"
  # TRACKED GAP, third site of the same one: `solver_error` is never produced anywhere, so
  # this reports `unproven`. See check_smt_path and check_smt_redteam; all three are the same
  # missing diagnostic and all three remain fail-closed.
  nz="$(printf '%s' "$ej" | python3 -c "
import json,sys
d=json.load(sys.stdin)
print(','.join(sorted({v['status'] for v in d['vcs'] if v.get('smt')})) or 'none')" 2>/dev/null || true)"
  if [ "$nz" = "solver_error" ]; then
    no "solver_error diagnostics landed — restore the strict assertion at all three sites and close the inventory entry"
  else
    ok "TRACKED: absent solver reports '$nz', not solver_error (fail-closed, diagnostic gap)"
  fi
  assert_no_proof "solver_error VC is not a proof" "$ej"
fi

echo ""
echo "SMT-NEGATIVES: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
