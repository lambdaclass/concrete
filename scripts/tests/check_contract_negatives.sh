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
assert_block "constant violation → counterexample" "cn.violation" "counterexample" "$F"
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
# assert_json <label> <pyexpr> <cmd...>
assert_json(){ local l="$1" e="$2"; shift 2; local o; o="$("$@" 2>/dev/null)"
  if printf '%s' "$o" | python3 -c "import json,sys; d=json.load(sys.stdin); sys.exit(0 if ($e) else 1)" 2>/dev/null; then echo "  ok   $l"; PASS=$((PASS+1));
  else echo "  FAIL $l — JSON/assert failed: $e"; FAIL=$((FAIL+1)); fi; }

echo "=== missing_postcondition (#[ensures] with no proof) ==="
assert_contains "ensures reported missing, not proved" "missing (no in-source proof link" \
  "$COMPILER" "$CN/missing_postcondition/src/main.con" --report contracts

echo "=== weakened_postcondition (only one direction proved) ==="
assert_contains "one_direction postcondition reported partial, not proved" \
  "partial — one direction proved_by_lean, converse outstanding" \
  "$COMPILER" "$CN/weakened_postcondition/src/main.con" --report contracts

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

echo "=== invalid_contract_expression (unknown identifier in contract) ==="
assert_contains "unknown identifier rejected in #[requires]" "invalid_contract_expression" \
  "$COMPILER" "$CN/invalid_contract_expression/src/main.con" --report contracts
assert_contains "unknown identifier named in diagnostic" "unknown identifier 'nonexistent'" \
  "$COMPILER" "$CN/invalid_contract_expression/src/main.con" --report contracts

echo "=== spec_ghost_totality (spec/ghost language must be pure & total) ==="
SGT="$CN/spec_ghost_totality/src/main.con"
assert_contains "impure (effectful) call in contract rejected" \
  "impure call 'tick' — spec/ghost must be pure and total" \
  "$COMPILER" "$SGT" --report contracts
# positive control: a contract calling a PURE helper is NOT over-rejected.
assert_block_absent() { local l="$1" anchor="$2" needle="$3" file="$4"
  local out; out="$("$COMPILER" "$file" --report contracts 2>/dev/null \
    | awk -v a="$anchor" 'index($0,a){f=1} f{print} f&&/^$/{exit}')"
  if printf '%s' "$out" | grep -qF -- "$needle"; then echo "  FAIL $l — unexpected '$needle'"; FAIL=$((FAIL+1));
  else echo "  ok   $l"; fi; }
assert_block_absent "pure-helper contract not over-rejected" "cn.good" "impure call" "$SGT"

echo "=== vacuous_contract (unsatisfiable preconditions / invariant) ==="
VAC="$CN/vacuous_contract/src/main.con"
# constant-false precondition (no omega needed):
assert_contains "requires(false) → vacuous, not proved" \
  "vacuous (precondition unsatisfiable" "$COMPILER" "$VAC" --report contracts
if command -v lake >/dev/null 2>&1; then
  # contradictory preconditions: omega refutes x>0 ∧ x<0 → the function is VACUOUS.
  assert_contains "contradictory requires → VACUOUS (omega)" "VACUOUS" "$COMPILER" "$VAC" --report contracts
  # #[invariant(false)] → invalid/vacuous, loop obligations meaningless.
  assert_contains "invariant(false) → invalid/vacuous" "invalid/vacuous" "$COMPILER" "$VAC" --report contracts
else
  echo "  skip omega vacuity checks (lake not on PATH)"
fi

echo "=== assert_obligation (assert(...) generates a proof obligation) ==="
AO="$CN/assert_obligation/src/main.con"
# block helper scoped to the assert/assume section.
assert_aa_block(){ local l="$1" anchor="$2" needle="$3" file="$4"
  local out; out="$("$COMPILER" "$file" --report contracts 2>/dev/null \
    | sed -n '/=== assert \/ assume/,/^=== /p' \
    | awk -v a="$anchor" 'index($0,a){f=1} f{print} f&&/^$/{exit}')"
  if printf '%s' "$out" | grep -qF -- "$needle"; then echo "  ok   $l"; PASS=$((PASS+1));
  else echo "  FAIL $l — '$anchor' block missing '$needle'"; printf '%s\n' "$out"|sed 's/^/      /'; FAIL=$((FAIL+1)); fi; }
# always-false assert is a VIOLATION (constant fold — no Lean needed):
assert_aa_block "assert(0>1) → VIOLATION (always false)" "cn.always_false" "VIOLATION: assert is always false" "$AO"
# unestablished assert is unproven, never silently accepted:
assert_aa_block "assert with no support → unproven" "cn.unproven" "unproven (assert not discharged" "$AO"
if command -v lake >/dev/null 2>&1; then
  # assert closed by omega from the function's #[requires]:
  assert_aa_block "assert closed by omega via #[requires] → proved" "cn.proved" "proved_by_kernel_decision" "$AO"
  # safety net: a false assert must NEVER be reported proved.
  assert_aa_block "assert(0>1) is NOT reported proved (no false green)" "cn.always_false" "VIOLATION" "$AO"
else
  echo "  skip omega assert checks (lake not on PATH)"
fi

echo "=== assume_taint (assume(...) is trust, not proof) ==="
AT="$CN/assume_taint/src/main.con"
# assume appears in the report with evidence class `assumed`, not proved.
assert_aa_block "assume → evidence class 'assumed' (not proved)" "cn.trusts" "assumed (trust, not proof" "$AT"
# the function opening the assume is marked TAINTED.
assert_aa_block "function with assume → TAINTED" "cn.trusts" "TAINTED" "$AT"
# a clean function in the same module is NOT tainted (taint is per-function).
assert_aa_block "clean sibling function not tainted" "cn.clean" "proved_by_kernel_decision" "$AT"
# release profile forbids the escape hatch: `concrete build` must fail with E0614.
ATDIR="$CN/assume_taint"
asm_out="$( cd "$ATDIR" && "$ROOT_DIR/$COMPILER" build 2>&1 )" && asm_exit=0 || asm_exit=$?
if [ "$asm_exit" -ne 0 ] && printf '%s' "$asm_out" | grep -qF "E0614"; then
  echo "  ok   forbid-assume policy rejects build (E0614)"; PASS=$((PASS+1));
else
  echo "  FAIL forbid-assume policy should reject build with E0614 (exit=$asm_exit)"; printf '%s\n' "$asm_out"|sed 's/^/      /'|head -4; FAIL=$((FAIL+1)); fi

echo "=== duplicate_links (two of the same proof-link attribute) ==="
assert_contains "duplicate #[spec] rejected at parse time" "duplicate #[spec(...)]" \
  "$COMPILER" "$CN/duplicate_links/src/main.con"

echo "=== fabricated_proof (nonexistent theorem name) ==="
FAB="$CN/fabricated_proof/src/main.con"
# documents the known limitation: proof-status trusts the fingerprint...
assert_contains "proof-status reports proved (known limitation)" "proof matches current body" \
  "$COMPILER" "$FAB" --report proof-status
# ...but --check is the safety net that catches it.
if command -v lake >/dev/null 2>&1; then
  assert_json "prove --check catches fabricated name → missing_theorem" \
    'd["all_checked"] is False and d["checks"][0]["status"]=="missing_theorem"' \
    "$COMPILER" prove "$FAB" cn.f --check --json
else
  echo "  skip fabricated --check (lake not on PATH)"
fi

echo "=== valid_complex_contract_scope (positive: resolver has zero false positives) ==="
# The companion to the negatives: a contract that legally mentions every name a
# contract CAN mention must produce NO false red. Over-eager scope-checking is as
# dishonest as a missed obligation.
POS="examples/contract_positive/valid_complex_contract_scope/src/main.con"
posrep="$("$COMPILER" "$POS" --report contracts 2>/dev/null)"
# no false positives of any class:
for bad in "invalid_contract_expression" "impure call" "unknown identifier" "unknown function/spec" "VACUOUS" "vacuous"; do
  if printf '%s' "$posrep" | grep -qF -- "$bad"; then
    echo "  FAIL positive fixture flagged '$bad' (false positive)"; printf '%s\n' "$posrep" | grep -F -- "$bad" | sed 's/^/      /'; FAIL=$((FAIL+1));
  else echo "  ok   no false '$bad'"; PASS=$((PASS+1)); fi
done
# and the legal names actually resolve to normal statuses (not silently dropped):
if printf '%s' "$posrep" | grep -qF "requires in_range(x, lo, hi)" \
   && printf '%s' "$posrep" | grep -qF "ensures result == clamp_spec(x, lo, hi)" \
   && printf '%s' "$posrep" | grep -qF "invariant 0 <= i && i <= span && acc <= LIMIT"; then
  echo "  ok   params/result/const/helper/spec/counter/ghost/local all resolve"; PASS=$((PASS+1));
else
  echo "  FAIL positive fixture: a legal contract name did not render"; FAIL=$((FAIL+1)); fi

echo "=== hmac_sha256 (mature source-link path — Phase 1 #8 regression anchor) ==="
# The crypto flagship carries source contracts alongside its in-source proof
# links. This pins BOTH the capability and the honesty of the call-site checker:
#  (a) block_to_words_at's #[requires(off+64<=384)] is discharged SYMBOLICALLY by
#      omega from sha256_compress_at's matching #[requires] (not a constant arg);
#  (b) the sha256_hash → sha256_compress_at call (block offset = blk*64, bounded
#      by a division-based block count omega can't model) stays honestly unproven
#      — no false green.
HMAC="examples/hmac_sha256/src/main.con"
if command -v lake >/dev/null 2>&1; then
  hmrep="$("$COMPILER" "$HMAC" --report contracts 2>/dev/null \
    | sed -n '/=== Call-site obligations/,/^=== /p')"
  cab="$(printf '%s' "$hmrep" | awk '/hmac_sha256.sha256_compress_at/{f=1} f{print} f&&/^$/{exit}')"
  if printf '%s' "$cab" | grep -qF "call block_to_words_at(buf, off)" \
     && printf '%s' "$cab" | grep -qF "omega (from caller's #[requires]"; then
    echo "  ok   block_to_words_at precond discharged symbolically by omega from caller #[requires]"; PASS=$((PASS+1));
  else echo "  FAIL hmac: expected symbolic omega discharge of block_to_words_at precond"; printf '%s\n' "$cab"|sed 's/^/      /'; FAIL=$((FAIL+1)); fi
  hab="$(printf '%s' "$hmrep" | awk '/hmac_sha256.sha256_hash/{f=1} f{print} f&&/^$/{exit}')"
  if printf '%s' "$hab" | grep -qF "unproven_at_callsite"; then
    echo "  ok   sha256_hash block-offset call stays honestly unproven (no false green)"; PASS=$((PASS+1));
  else echo "  FAIL hmac: sha256_hash call site should be honestly unproven"; printf '%s\n' "$hab"|sed 's/^/      /'; FAIL=$((FAIL+1)); fi
else
  echo "  skip hmac mature-path checks (lake not on PATH)"
fi

echo ""
echo "CONTRACT-NEGATIVES: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
