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
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
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
  if grep -qF <<<"$out" -- "$needle"; then echo "  ok   $l"; PASS=$((PASS+1));
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
  if grep -qF <<<"$o" -- "$n"; then echo "  ok   $l"; PASS=$((PASS+1));
  else echo "  FAIL $l — missing '$n'"; printf '%s\n' "$o"|sed 's/^/      /'|head -6; FAIL=$((FAIL+1)); fi; }
# assert_absent <label> <needle> <cmd...>
# An absence assertion over EMPTY output proves nothing: if the command produced no report at all
# — the file was rejected before the report ran, the binary crashed — then every needle is absent
# and the check passes. That is how a positive control in this very gate went green while proving
# nothing (see H27). Empty output is therefore a failure, not a pass.
assert_absent(){ local l="$1" n="$2"; shift 2; local o; o="$("$@" 2>&1)"
  if [ -z "$o" ]; then echo "  FAIL $l — VACUOUS: command produced no output, so absence proves nothing"; FAIL=$((FAIL+1)); return; fi
  if grep -qF <<<"$o" -- "$n"; then echo "  FAIL $l — unexpected '$n'"; FAIL=$((FAIL+1));
  else echo "  ok   $l"; PASS=$((PASS+1)); fi; }
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
# An "absent" assertion is vacuous whenever the block it searches is itself missing, and that is
# not hypothetical: promoting the impure-call defect from a report line to a check error made this
# very control pass by finding nothing. The whole fixture was rejected, its report was empty, and
# "no 'impure call' in the cn.good block" was true because there was no cn.good block.
#
# So require the anchor to EXIST before concluding anything from its contents.
assert_block_absent() { local l="$1" anchor="$2" needle="$3" file="$4"
  local rep; rep="$("$COMPILER" "$file" --report contracts 2>/dev/null)"
  if ! grep -qF <<<"$rep" -- "$anchor"; then
    echo "  FAIL $l — VACUOUS: anchor '$anchor' is not in the report, so this control proves nothing"
    FAIL=$((FAIL+1)); return
  fi
  local out; out="$(printf '%s\n' "$rep" \
    | awk -v a="$anchor" 'index($0,a){f=1} f{print} f&&/^$/{exit}')"
  if grep -qF <<<"$out" -- "$needle"; then echo "  FAIL $l — unexpected '$needle'"; FAIL=$((FAIL+1));
  else echo "  ok   $l"; PASS=$((PASS+1)); fi; }
# Points at a standalone positive fixture: the old target mixes this control with an impure-call
# negative, and that file is now rejected wholesale, which is what made the control vacuous.
PCC="examples/contract_positive/pure_contract_calls/src/main.con"
assert_block_absent "pure-helper contract not over-rejected" "pc.good" "impure call" "$PCC"
assert_block_absent "spec-fn contract not over-rejected" "pc.good2" "impure call" "$PCC"

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
  if grep -qF <<<"$out" -- "$needle"; then echo "  ok   $l"; PASS=$((PASS+1));
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
if [ "$asm_exit" -ne 0 ] && grep -qF <<<"$asm_out" "E0614"; then
  echo "  ok   forbid-assume policy rejects build (E0614)"; PASS=$((PASS+1));
else
  echo "  FAIL forbid-assume policy should reject build with E0614 (exit=$asm_exit)"; printf '%s\n' "$asm_out"|sed 's/^/      /'|head -4; FAIL=$((FAIL+1)); fi

echo "=== duplicate_links (two of the same proof-link attribute) ==="
assert_contains "duplicate #[spec] rejected at parse time" "duplicate #[spec(...)]" \
  "$COMPILER" "$CN/duplicate_links/src/main.con"

echo "=== fabricated_proof (nonexistent theorem name) ==="
FAB="$CN/fabricated_proof/src/main.con"
# CLOSED by bug 079. This leg used to pin the limitation: proof-status trusted the fingerprint and said
# "proof matches current body" for a theorem that does not exist. A theorem that does not exist
# evaluates no specification, so the claim is not shown to be about this body and is not proved.
assert_contains "proof-status refuses a nonexistent theorem (bug 079)" "is not shown to be what its theorem is about" \
  "$COMPILER" "$FAB" --report proof-status
# --check still names the missing theorem itself.
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
  if grep -qF <<<"$posrep" -- "$bad"; then
    echo "  FAIL positive fixture flagged '$bad' (false positive)"; printf '%s\n' "$posrep" | grep -F -- "$bad" | sed 's/^/      /'; FAIL=$((FAIL+1));
  else echo "  ok   no false '$bad'"; PASS=$((PASS+1)); fi
done
# and the legal names actually resolve to normal statuses (not silently dropped):
if grep -qF <<<"$posrep" "requires in_range(x, lo, hi)" \
   && grep -qF <<<"$posrep" "ensures result == clamp_spec(x, lo, hi)" \
   && grep -qF <<<"$posrep" "invariant 0 <= i && i <= span && acc <= LIMIT"; then
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
  if grep -qF <<<"$cab" "call block_to_words_at(buf, off)" \
     && grep -qF <<<"$cab" "omega (from caller's #[requires]"; then
    echo "  ok   block_to_words_at precond discharged symbolically by omega from caller #[requires]"; PASS=$((PASS+1));
  else echo "  FAIL hmac: expected symbolic omega discharge of block_to_words_at precond"; printf '%s\n' "$cab"|sed 's/^/      /'; FAIL=$((FAIL+1)); fi
  hab="$(printf '%s' "$hmrep" | awk '/hmac_sha256.sha256_hash/{f=1} f{print} f&&/^$/{exit}')"
  if grep -qF <<<"$hab" "unproven_at_callsite"; then
    echo "  ok   sha256_hash block-offset call stays honestly unproven (no false green)"; PASS=$((PASS+1));
  else echo "  FAIL hmac: sha256_hash call site should be honestly unproven"; printf '%s\n' "$hab"|sed 's/^/      /'; FAIL=$((FAIL+1)); fi
else
  echo "  skip hmac mature-path checks (lake not on PATH)"
fi

echo ""
# === contracts are REJECTED, not merely reported =============================================
# `--report contracts` has flagged unknown identifiers for a long time, but it is a report: the
# check pass accepted the program, so `#[requires(nosuchvar > 0)]` still put `nosuchvar` into the
# HYPOTHESES of every obligation in the function, including ones marked proved_by_kernel_decision.
# It fails closed (the call site cannot discharge a hypothesis over a name that does not exist),
# but the failure surfaces in another function rather than at the typo.
#
# Measured on merged main 2026-08-08: 1249 files, 2 newly rejected — both negative fixtures that
# exist to demonstrate the defect they trip. (Pre-merge it was 1248 files / 1 rejection; the
# second appeared when effectful contract calls became a check error.) 285 files are UNMEASURED:
# checking stops at the first error, so a contract defect can hide behind an unrelated one.
# Historical note, kept because the ratio held across the merge: 1248 files, exactly 1 newly
# rejected -- the fixture above, which documents itself as invalid. So rejection costs nothing
# real, and the assertions below are what keep it from over-rejecting later.
CTMP="$(mktemp -d)"; trap 'rm -rf "$CTMP"' EXIT
ct() { printf 'mod t {\n    const LIM: i32 = 10;\n%s\n    fn f(x: i32) -> i32 {\n        return x;\n    }\n}\n' "$1" > "$CTMP/t.con"; }
ct_reject() { ct "$2"; local o; o="$("$COMPILER" "$CTMP/t.con" --report vcs 2>&1 || true)"
  if grep -qF <<<"$o" "error[check]"; then echo "  ok   $1"; PASS=$((PASS+1));
  else echo "  FAIL $1 — accepted"; FAIL=$((FAIL+1)); fi; }
ct_accept() { ct "$2"; local o; o="$("$COMPILER" "$CTMP/t.con" --report vcs 2>&1 || true)"
  if grep -qF <<<"$o" "error[check]"; then echo "  FAIL $1 — rejected: $(grep -oE 'error\[check\]: .*' <<<"$o" | head -1)"; FAIL=$((FAIL+1));
  else echo "  ok   $1"; PASS=$((PASS+1)); fi; }

echo "=== contract checking (reject, not just report) ==="
ct_reject "unknown name in #[requires] is rejected at CHECK time" '    #[requires(zzz > 0)]'
ct_reject "an integer expression is not a proposition"            '    #[requires(x + 1)]'
# `result` is bound in `ensures` and NOWHERE else. Both directions asserted, because a single
# shared bound-name set would silently permit `result` in a precondition, where it has no meaning.
ct_reject "'result' in #[requires] is rejected (unbound there)"   '    #[requires(result > 0)]'
ct_accept "'result' in #[ensures] is accepted"                    '    #[ensures(result > 0)]'
# Over-rejection controls: the legal vocabulary must survive.
ct_accept "a parameter is accepted"                               '    #[requires(x > 0)]'
ct_accept "a module constant is accepted"                         '    #[requires(x < LIM)]'

# Loop contracts are checked too. An unbound name in an `#[invariant]` is universally quantified
# into the INITIATION obligation, which then reads `forall (bogus : Int), 0 <= 0 /\ 0 <= bogus`
# -- false, so the loop fails closed, but by luck of where the name landed rather than by rule.
cti() { printf 'mod t {\n    fn f(n: i32) -> i32 {\n        let mut i: i32 = 0;\n%s\n        while i < n {\n            i = i + 1;\n        }\n        return i;\n    }\n}\n' "$1" > "$CTMP/i.con"; }
cti_reject() { cti "$2"; local o; o="$("$COMPILER" "$CTMP/i.con" --report vcs 2>&1 || true)"
  if grep -qF <<<"$o" "error[check]"; then echo "  ok   $1"; PASS=$((PASS+1));
  else echo "  FAIL $1 — accepted"; FAIL=$((FAIL+1)); fi; }
cti_accept() { cti "$2"; local o; o="$("$COMPILER" "$CTMP/i.con" --report vcs 2>&1 || true)"
  if grep -qF <<<"$o" "error[check]"; then echo "  FAIL $1 — rejected: $(grep -oE 'error\[check\]: .*' <<<"$o" | head -1)"; FAIL=$((FAIL+1));
  else echo "  ok   $1"; PASS=$((PASS+1)); fi; }
cti_reject "unknown name in #[invariant] is rejected"  '        #[invariant(0 <= i && i <= bogusname)]'
cti_reject "an integer #[invariant] is not a proposition" '        #[invariant(i + 1)]'
# Over-rejection control: a loop invariant MUST be able to mention the loop local and the params.
cti_accept "an invariant over a local and a param is accepted" '        #[invariant(0 <= i && i <= n)]'
# A variant is an integer measure, so it must NOT be held to the proposition rule.
cti_accept "an integer #[variant] is accepted (measures are not propositions)" '        #[invariant(0 <= i)]
        #[variant(n - i)]'
# ...and a BOOLEAN variant is rejected, which is the other half of that rule. It was accepted
# because the proposition check simply skipped variants rather than holding them to their own
# requirement — absence of a rule reading as absence of a requirement. `i < n` does not decrease,
# it flips.
cti_reject "a boolean #[variant] is rejected (does not decrease)" '        #[invariant(0 <= i)]
        #[variant(i < n)]'

# TWO DISTINCT PROPERTIES, asserted separately. Rejecting the source is not the same claim as
# producing no obligation from it: a pass that reports the error but still emits records leaves an
# invented binder inside the obligation store, where a later consumer can pick it up. The second
# assertion is what makes "no arbitrary theorem variable was created" checkable rather than
# inferred from the first.
cti "        #[invariant(0 <= i && i <= bogusname)]"
INVOUT="$("$COMPILER" "$CTMP/i.con" --report vcs 2>&1 || true)"
grep -qF <<<"$INVOUT" "unknown identifier 'bogusname'" \
  && { echo "  ok   (1/2) the source program is rejected, naming the identifier"; PASS=$((PASS+1)); } \
  || { echo "  FAIL (1/2) rejection did not name 'bogusname'"; FAIL=$((FAIL+1)); }
grep -qF <<<"$INVOUT" "bogusname" && grep -qE <<<"$INVOUT" "conclusion|hypotheses" \
  && { echo "  FAIL (2/2) an obligation record still carries the invented binder"; FAIL=$((FAIL+1)); } \
  || { echo "  ok   (2/2) no obligation record contains the invented binder"; PASS=$((PASS+1)); }

# SHADOWING, which unknown-name rejection cannot catch by construction: once the identifier
# exists in two scopes every name resolves, so no diagnostic fires and resolution can silently
# pick the wrong valid binding. Shadowing IS expressible here -- a local may shadow a parameter,
# measured, not assumed -- so this is a live gap and not a hypothetical one.
cat > "$CTMP/shadow.con" <<'EOF'
mod sh {
    #[requires(x > 0)]
    #[ensures(result == x)]
    fn f(x: i32) -> i32 {
        let x: i32 = 99;
        return x;
    }
}
EOF
SHOUT="$("$COMPILER" "$CTMP/shadow.con" --report vcs 2>&1 || true)"
# The contract means the PARAMETER. Nothing in the pipeline records which `x` that is, so the
# guarantee today is only that the postcondition is not discharged. Pinning that keeps the
# refinement tier from quietly starting to discharge it while the ambiguity is unresolved (H25).
# Now REJECTED rather than merely undischarged. The earlier assertion pinned "no evidence is
# issued from it", which relied on the postcondition path being incomplete — the same
# safety-by-accident shape as H25. Rejecting establishes it here instead.
grep -qF <<<"$SHOUT" "shadowed by a local" \
  && { echo "  ok   a contract naming a SHADOWED param is rejected (which binding it means is recorded nowhere)"; PASS=$((PASS+1)); } \
  || { echo "  FAIL a shadowed-param contract is accepted despite unresolved binding identity"; FAIL=$((FAIL+1)); }

# Two over-rejection controls, because this restriction is easy to widen by accident. Shadowing
# is legal in the language; only shadowing of a name a CONTRACT mentions is ambiguous.
cat > "$CTMP/shadow_ok.con" <<'EOF'
mod so {
    #[ensures(result == x)]
    fn f(x: i32) -> i32 {
        let y: i32 = 99;
        return x + y - y;
    }
}
EOF
grep -qF <<<"$("$COMPILER" "$CTMP/shadow_ok.con" --report vcs 2>&1 || true)" "error[check]" \
  && { echo "  FAIL a contract with NO shadowing was rejected"; FAIL=$((FAIL+1)); } \
  || { echo "  ok   a contract over an unshadowed param is accepted"; PASS=$((PASS+1)); }
cat > "$CTMP/shadow_nocontract.con" <<'EOF'
mod sn {
    fn f(x: i32) -> i32 {
        let x: i32 = 99;
        return x;
    }
}
EOF
grep -qF <<<"$("$COMPILER" "$CTMP/shadow_nocontract.con" --report vcs 2>&1 || true)" "error[check]" \
  && { echo "  FAIL shadowing was rejected in a function with NO contract — the language rule changed"; FAIL=$((FAIL+1)); } \
  || { echo "  ok   shadowing without a contract is still legal (restriction is scoped to contracts)"; PASS=$((PASS+1)); }

# The loop-contract scope check is OVER-APPROXIMATE by construction: the bound set is every name
# bound anywhere in the function, not the names in scope at the loop. This pins that limit as a
# known, measured behaviour rather than leaving it as a comment nobody re-checks. When real
# scoping arrives with the typed contract record, this assertion should flip to a rejection.
cat > "$CTMP/overapprox.con" <<'EOF'
mod oa {
    fn f(n: i32) -> i32 {
        let mut i: i32 = 0;
        #[invariant(0 <= i && i <= later)]
        while i < n {
            i = i + 1;
        }
        let later: i32 = 5;
        return i + later;
    }
}
EOF
OAOUT="$("$COMPILER" "$CTMP/overapprox.con" --report vcs 2>&1 || true)"
grep -qF <<<"$OAOUT" "error[check]" \
  && { echo "  ok   KNOWN-LIMIT FLIPPED: loop scope is now precise — tighten this assertion"; PASS=$((PASS+1)); } \
  || { echo "  ok   known limit: a name bound only AFTER the loop is still admitted (over-approximate scope)"; PASS=$((PASS+1)); }

# WHAT THIS VALIDATION DOES NOT DO (H27). These pin the boundary as measured behaviour, so the
# gate says out loud that contracts are name-and-sort validated rather than type checked. Each is
# written to REPORT rather than fail, and to flip when the gap closes -- a known limit that
# nobody re-measures becomes a false impression.
gap() { ct "$2"; local o; o="$("$COMPILER" "$CTMP/t.con" --report vcs 2>&1 || true)"
  # The echo below is a CONDITIONAL runtime message — emitted only in the branch where the
  # compiler rejected the program, i.e. only once the gap no longer exists. It asserts
  # nothing about the hole's current status.
  #
  # check_hole_status_consistency scans source text and cannot see the enclosing `if`, so it
  # read the tripwire as a document contradicting KNOWN_HOLES. The line carries that gate's
  # sanctioned inline marker rather than being reworded: the wording is exactly right for
  # the moment it prints, and weakening it to satisfy a regex would blunt the signal.
  #
  # This comment avoids repeating the hole id next to a status word, because that is the
  # pattern the scanner matches — the first version of this note tripped the gate itself.
  if grep -qF <<<"$o" "error[check]"; then echo "  ok   H27 GAP CLOSED: $1 — tighten this assertion"; PASS=$((PASS+1)); # HOLE-STATUS-OK: conditional runtime message, not a status claim
  else echo "  ok   H27 known gap (not type checking): $1"; PASS=$((PASS+1)); fi; }
gap "operand width is unchecked"          '    #[requires(x < 9999999999)]'
gap "bool as arithmetic operand"          '    #[requires((x > 0) + 1 > 0)]'
cat > "$CTMP/impure.con" <<'EOF'
mod im {
    fn g(y: i32) -> i32 { return y; }

    #[requires(g(x) > 0)]
    fn f(x: i32) -> i32 { return x; }
}
EOF
# A PURE executable call stays legal — purity is the property that matters, not spec-only.
IMOUT="$("$COMPILER" "$CTMP/impure.con" --report vcs 2>&1 || true)"
grep -qF <<<"$IMOUT" "error[check]" \
  && { echo "  FAIL a contract calling a PURE executable fn was rejected (over-rejection)"; FAIL=$((FAIL+1)); } \
  || { echo "  ok   a contract calling a PURE executable fn is accepted"; PASS=$((PASS+1)); }
# ...while an IMPURE (capability-requiring) call is now rejected at CHECK time, not merely
# reported. Its meaning would depend on runtime effects, so it is not a proposition at all.
cat > "$CTMP/impure2.con" <<'EOF'
mod im2 {
    fn tick() with(Console) -> i32 { return 0; }

    #[ensures(result == tick())]
    fn bad(x: i32) -> i32 { return x; }
}
EOF
grep -qF <<<"$("$COMPILER" "$CTMP/impure2.con" --report vcs 2>&1 || true)" "impure call 'tick'" \
  && { echo "  ok   an IMPURE call in a contract is rejected at check time"; PASS=$((PASS+1)); } \
  || { echo "  FAIL an impure call in a contract is still only reported, not rejected"; FAIL=$((FAIL+1)); }

# TWO DEFINITIONS OF "IMPURE", HELD TOGETHER. `Check.impureFnsIn` and
# `ReportVC.impureFnNames` both mean "capability-requiring", and they are separate functions
# because Check must not depend on Report. Nothing made them agree — a comment in Check.lean
# claimed a gate assertion held them together, and the gate did not exist. Written now rather
# than the claim deleted, because the drift is real: if one starts treating (say) `#[trusted]`
# or an extern as impure and the other does not, a contract is rejected by one surface and
# accepted by the other, and which one you believe depends on which report you ran.
#
# Source-level, deliberately: the behavioural path cannot compare them, because Check rejects
# the program before Report ever runs. What is asserted is that both derive impurity from the
# SAME criterion — a non-empty capability set — so a change to one is visible as a divergence.
CRIT_CHECK="$(grep -c 'capSet.isEmpty' Concrete/Check/Check.lean || true)"
CRIT_REPORT="$(grep -c 'capSet.isEmpty' Concrete/Report/ReportVC.lean || true)"
if [ "$CRIT_CHECK" -ge 1 ] && [ "$CRIT_REPORT" -ge 1 ]; then
  echo "  ok   Check and ReportVC derive 'impure' from the same criterion (capSet)"; PASS=$((PASS+1))
else
  echo "  FAIL impurity criterion diverged — Check:$CRIT_CHECK ReportVC:$CRIT_REPORT occurrences of capSet.isEmpty"
  echo "       one surface would reject a contract the other accepts"
  FAIL=$((FAIL+1))
fi

echo "CONTRACT-NEGATIVES: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
