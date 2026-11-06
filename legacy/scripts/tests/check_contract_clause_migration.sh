#!/usr/bin/env bash
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
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
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

# THE SURFACING MECHANISM CHANGED (2026-08-07). These two clauses used to compile and be
# carried as INELIGIBLE VC diagnostics; contract TYPE CHECKING now rejects them at check
# time, so the program never reaches VC generation and there is no VC to inspect.
#
# The legs are rewritten to the new mechanism rather than deleted, because the property
# they defend is unchanged and still worth defending: AN INVALID OR IMPURE CLAUSE MUST
# SURFACE, never be silently accepted. Rejection at check time satisfies that more strongly
# than an ineligible VC did — it is earlier, and it cannot be overlooked in a ledger.
#
# What IS lost is the ledger's record of these two cases; that trade is recorded below.
rejects(){ local label="$1" file="$2" pat="$3"
  local out; out="$("$COMPILER" "$file" 2>&1)"
  if [ -n "$out" ] && has_match "$pat" <<<"$out"; then ok "$label"
  else no "$label (expected a check error matching: $pat)"; fi; }

echo "=== malformed clause → rejected at check time ==="
rejects "invalid #[requires] is rejected, naming the unknown identifier" \
  "$CN/invalid_contract_expression/src/main.con" \
  "contract on 'bad': unknown identifier 'nonexistent'"

echo "=== impure clause → rejected at check time ==="
rejects "impure #[ensures] is rejected, naming the impure call and the rule" \
  "$CN/spec_ghost_totality/src/main.con" \
  "contract on 'bad': impure call 'tick' — spec/ghost must be pure and total"

echo "=== one-direction proof → unproven (bug 089: a coverage label discharges nothing) ==="
ck "weakened postcondition is unproven, not partial or proved_by_lean" \
  "$CN/weakened_postcondition/src/main.con" \
  "st('cn.weak#ensures0')=='unproven'"

echo "=== negative control: a valid-but-unpreserved invariant is NOT a clause diagnostic ==="
ck "invalid_invariant emits NO invalid/impure diagnostic (it is an O2 failure)" \
  "$CN/invalid_invariant/src/main.con" \
  "not anyk('invalid_contract_expression') and not anyk('impure_contract_call') and st('cn.bad@10#O2')=='unproven'"

# THE LEDGER NO LONGER CARRIES THESE, and that is a consequence worth stating rather than
# quietly dropping the leg. A program rejected at check time produces no obligations, so
# there is nothing for the ledger to hold. The diagnostic is not lost — it is a hard error
# — but a consumer reading only the ledger will no longer see that this function HAD a
# malformed clause; it will see no entry for the function at all.
#
# Asserted in that direction: the ledger must REFUSE rather than report a clean function,
# which is the failure that would matter (a malformed contract reading as absent).
if "$COMPILER" "$CN/invalid_contract_expression/src/main.con" --report obligation-ledger --json >/dev/null 2>&1; then
  no "a program with a malformed contract produced an obligation ledger — it must be rejected before obligations exist, or a bad clause reads as an absent one"
else
  ok "a malformed clause is rejected before the ledger exists, rather than reported as a clean function"
fi

echo ""
echo "CONTRACT-CLAUSE-MIGRATION: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
