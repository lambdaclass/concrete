#!/usr/bin/env bash
# Contract-stability gate (Phase 1 hardening, #6).
#
# A function's #[requires]/#[ensures]/#[invariant] are part of its published API.
# `concrete snapshot` records them as `contract` facts; `concrete diff` classifies
# a change between versions, SOUNDLY at the conjunctive clause-set level:
#   - precondition strengthened (a requires clause ADDED)   → BREAKING (weakened)
#   - postcondition weakened    (an ensures clause REMOVED) → BREAKING (weakened)
#   - precondition weakened     (a requires clause REMOVED) → COMPATIBLE (strengthened)
#   - public invariant changed                              → flagged (weakened)
# This gate pins those classifications so the API-stability rule "stays done".

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0

V1="examples/contract_stability/v1.con"
V2="examples/contract_stability/v2.con"
"$COMPILER" snapshot "$V1" -o "$TMP/v1.json" >/dev/null 2>&1
"$COMPILER" snapshot "$V2" -o "$TMP/v2.json" >/dev/null 2>&1

# JSON drift per function (machine-checked).
drift_json="$("$COMPILER" diff "$TMP/v1.json" "$TMP/v2.json" --json 2>/dev/null)"
assert_drift(){ local fn="$1" want="$2"
  local got; got="$(printf '%s' "$drift_json" | python3 -c "
import json,sys
d=json.load(sys.stdin)
es=d if isinstance(d,list) else d.get('changes',d.get('entries',[]))
print(next((e.get('drift') for e in es if e.get('function')=='$fn' and e.get('kind')=='contract'), '<absent>'))" 2>/dev/null)"
  if [ "$got" = "$want" ]; then echo "  ok   $fn → $want"; PASS=$((PASS+1));
  else echo "  FAIL $fn → expected '$want', got '$got'"; FAIL=$((FAIL+1)); fi; }

echo "=== contract API drift classification (v1 → v2) ==="
assert_drift "api.strengthen_pre" "weakened"      # added a requires clause
assert_drift "api.weaken_post"    "weakened"      # removed an ensures clause
assert_drift "api.relax_pre"      "strengthened"  # removed a requires clause
assert_drift "api.stable"         "<absent>"      # unchanged → no entry

echo "=== exit code: any breaking change → non-zero ==="
"$COMPILER" diff "$TMP/v1.json" "$TMP/v2.json" >/dev/null 2>&1 && rc=0 || rc=$?
if [ "$rc" -eq 1 ]; then echo "  ok   diff exits 1 on a weakened contract"; PASS=$((PASS+1));
else echo "  FAIL diff should exit 1 on a weakened contract (got $rc)"; FAIL=$((FAIL+1)); fi

echo "=== self-diff is clean (no false drift) ==="
"$COMPILER" diff "$TMP/v1.json" "$TMP/v1.json" >/dev/null 2>&1 && rc=0 || rc=$?
if [ "$rc" -eq 0 ]; then echo "  ok   v1 vs v1 → no changes, exit 0"; PASS=$((PASS+1));
else echo "  FAIL self-diff should be clean (got $rc)"; FAIL=$((FAIL+1)); fi

echo ""
echo "CONTRACT-STABILITY: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
