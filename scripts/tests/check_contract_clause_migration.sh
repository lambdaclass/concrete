#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Contract-clause migration gate (ROADMAP Phase 3 #10).
#
# The #[requires]/#[ensures]/#[invariant]/#[variant] clause statuses now live in
# the ONE ledger, not just the text contracts report:
#   • a malformed clause (unknown name / non-total construct) → a ledger
#     obligation of kind `invalid_contract_expression`, status `ineligible`;
#   • a clause calling a capability-requiring function → `impure_contract_call`,
#     status `ineligible`;
#   • a one-direction postcondition proof → `partial` (not a full proved_by_lean) —
#     the ledger now AGREES with the text report instead of overclaiming;
#   • a well-formed-but-unpreserved invariant is NOT a clause diagnostic (it stays
#     O2 `unproven`) — the migration must not manufacture a false diagnostic.
#
# Verified additive against the pre-#10 binary: only the new diagnostic rows
# appear, plus the single honest `proved_by_lean → partial` refinement.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
CN="examples/contract_negatives"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

ck(){ local label="$1" file="$2" expr="$3"
  "$COMPILER" "$file" --report vcs --json 2>/dev/null | python3 -c "
import json,sys
d=json.load(sys.stdin); V=d['vcs']; byid={v['id']:v for v in V}
def has(i): return i in byid
def st(i): return byid[i]['status']
def kind(i): return byid[i]['kind']
def anyk(k): return any(v['kind']==k for v in V)
sys.exit(0 if ($expr) else 1)
" 2>/dev/null && ok "$label" || no "$label"; }

echo "=== malformed clause → invalid_contract_expression (ineligible) ==="
ck "invalid #[requires] surfaces as ineligible diagnostic" \
  "$CN/invalid_contract_expression/src/main.con" \
  "has('cn.bad#req_diag1') and kind('cn.bad#req_diag1')=='invalid_contract_expression' and st('cn.bad#req_diag1')=='ineligible'"

echo "=== impure clause → impure_contract_call (ineligible) ==="
ck "impure #[ensures] surfaces as impure_contract_call" \
  "$CN/spec_ghost_totality/src/main.con" \
  "has('cn.bad#ens_diag1') and kind('cn.bad#ens_diag1')=='impure_contract_call' and st('cn.bad#ens_diag1')=='ineligible'"

echo "=== one-direction proof → partial (ledger no longer overclaims proved_by_lean) ==="
ck "weakened postcondition is partial, not proved_by_lean" \
  "$CN/weakened_postcondition/src/main.con" \
  "st('cn.weak#ensures0')=='partial'"

echo "=== negative control: a valid-but-unpreserved invariant is NOT a clause diagnostic ==="
ck "invalid_invariant emits NO invalid/impure diagnostic (it is an O2 failure)" \
  "$CN/invalid_invariant/src/main.con" \
  "not anyk('invalid_contract_expression') and not anyk('impure_contract_call') and st('cn.bad@10#O2')=='unproven'"

echo "=== the diagnostics are in the unified ledger view too ==="
"$COMPILER" "$CN/invalid_contract_expression/src/main.con" --report obligation-ledger --json 2>/dev/null \
  | python3 -c "
import json,sys
d=json.load(sys.stdin); byid={o['id']:o for o in d['obligations']}
sys.exit(0 if ('cn.bad#req_diag1' in byid and byid['cn.bad#req_diag1']['kind']=='invalid_contract_expression') else 1)
" 2>/dev/null && ok "obligation-ledger carries the clause diagnostic" || no "diagnostic missing from obligation-ledger"

echo ""
echo "CONTRACT-CLAUSE-MIGRATION: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
