#!/usr/bin/env bash
# Contract-negatives gate (Phase 1 hardening).
#
# examples/contract_negatives/ holds the source-contract cases that could make a
# green proof misleading. Each must be caught HONESTLY. This gate pins those
# diagnostics so the hardening "stays done".
#
# Currently covers: unmet precondition at a call site (caller-side #[requires]
# checking). Omega-discharged cases need the Lean toolchain, so they are guarded
# by `command -v lake`; the constant-violation and honest-gap cases do not.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
CN="examples/contract_negatives"
PASS=0; FAIL=0

# assert_block <label> <caller-anchor> <expected-substring> <file>
# Scopes to the "Call-site obligations" section first (a caller with its own
# #[requires] also appears in "Source Contracts"), then checks the lines from the
# caller anchor up to the next blank line contain the substring.
assert_block(){ local l="$1" anchor="$2" needle="$3" file="$4"
  local out; out="$("$COMPILER" "$file" --report contracts 2>/dev/null \
    | sed -n '/=== Call-site obligations/,/^=== /p' \
    | awk -v a="$anchor" 'index($0,a){f=1} f{print} f&&/^$/{exit}')"
  if printf '%s' "$out" | grep -qF -- "$needle"; then echo "  ok   $l"; PASS=$((PASS+1));
  else echo "  FAIL $l — '$anchor' block missing '$needle'"; printf '%s\n' "$out"|sed 's/^/      /'; FAIL=$((FAIL+1)); fi; }

echo "=== precondition_callsite (unmet precondition at call site) ==="
F="$CN/precondition_callsite/src/main.con"
# always-on (constant fold / honest gap — no Lean needed):
assert_block "constant violation → failed_at_callsite" "cn.violation" "failed_at_callsite" "$F"
assert_block "genuine gap → unproven (caller does not establish)" "cn.unmet" "caller does not establish" "$F"
# omega-discharged (needs lake):
if command -v lake >/dev/null 2>&1; then
  assert_block "caller #[requires] establishes it → omega-proved" "cn.ok_via_requires" "engine:  omega" "$F"
  assert_block "enclosing guard establishes it → omega-proved"    "cn.ok_via_guard"    "engine:  omega" "$F"
else
  echo "  skip omega-discharged precondition checks (lake not on PATH)"
fi

# assert_contains <label> <needle> <cmd...>
assert_contains(){ local l="$1" n="$2"; shift 2; local o; o="$("$@" 2>&1)"
  if printf '%s' "$o" | grep -qF -- "$n"; then echo "  ok   $l"; PASS=$((PASS+1));
  else echo "  FAIL $l — missing '$n'"; printf '%s\n' "$o"|sed 's/^/      /'|head -6; FAIL=$((FAIL+1)); fi; }
# assert_absent <label> <needle> <cmd...>
assert_absent(){ local l="$1" n="$2"; shift 2; local o; o="$("$@" 2>&1)"
  if printf '%s' "$o" | grep -qF -- "$n"; then echo "  FAIL $l — unexpected '$n'"; FAIL=$((FAIL+1));
  else echo "  ok   $l"; fi; }

echo "=== missing_postcondition (#[ensures] with no proof) ==="
assert_contains "ensures reported missing, not proved" "missing (no in-source proof link" \
  "$COMPILER" "$CN/missing_postcondition/src/main.con" --report contracts

echo "=== invalid_attribute (malformed #[proof_fingerprint]) ==="
assert_contains "malformed attribute rejected at parse time" "expected a string literal" \
  "$COMPILER" "$CN/invalid_attribute/src/main.con"

echo "=== invalid_invariant (loop does not preserve the invariant) ==="
if command -v lake >/dev/null 2>&1; then
  # omega refuses the false preservation VC: O2's arithmetic step must NOT be proved.
  assert_absent "false invariant VC not omega-proved (no false green)" "arithmetic step:   proved_by_kernel_decision" \
    "$COMPILER" "$CN/invalid_invariant/src/main.con" --report contracts
else
  echo "  skip invalid_invariant omega check (lake not on PATH)"
fi

echo ""
echo "CONTRACT-NEGATIVES: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
