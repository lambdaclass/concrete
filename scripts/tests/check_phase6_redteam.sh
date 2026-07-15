#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Phase 6 #35a — red-team hardening gate.
#
# Targeted coverage for the failure classes that the dead-CI window hid and the
# 2026-06-24 resurrection exposed (see CHANGELOG "CI-fallout triage" and the
# ci-resurrection note). Each class below caused a real regression or a real
# interp/compiled disagreement that no gate was catching:
#
#   1. parser error recovery + duplicate proof-link attributes
#   2. match linear-consumption agreement across range/guard/OR/wildcard arms
#   3. match lowering correctness — interp == compiled, both correct — for
#      match-on-&T, guards, OR desugaring, width coercion, struct-update-in-arm
#   4. ProofCore fingerprint stability (semantic change ⇒ new fingerprint;
#      formatting-only change ⇒ same fingerprint)
#   5. deliberately-stale trust/proof fixtures stay stale
#
# This gate is also the reusable "new syntax feature red-team checklist": every
# future parser/lowering feature should add a fixture here covering positive
# parse, negative parse/check, a linear-ownership edge, a lowering execution
# oracle (interp == compiled), and a ProofCore/fingerprint assertion.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
write(){ printf '%s\n' "$2" > "$TMP/$1.con"; }

# Oracle: a self-checking program (returns 0 on success, nonzero on a mismatch)
# must compile, run to 0, AND interpret to 0 — interp and compiled must agree.
oracle(){ local n="$1"
  if ! "$C" "$TMP/$n.con" -o "$TMP/$n.bin" >"$TMP/$n.cerr" 2>&1; then
    no "$n: expected to compile"; sed 's/^/        /' "$TMP/$n.cerr" | head -3; return; fi
  "$TMP/$n.bin" >/dev/null 2>&1; local ce=$?
  "$C" "$TMP/$n.con" --interp >/dev/null 2>&1; local ie=$?
  if [ "$ce" = 0 ] && [ "$ie" = 0 ]; then ok "$n: interp == compiled == 0 (agree)"
  else no "$n: disagreement/failure (compiled=$ce interp=$ie)"; fi; }

# Negative: must be rejected, and the diagnostic must CONTAIN $must and NOT
# contain $forbidden (guards against recovery degrading a precise error).
reject_clean(){ local n="$1" must="$2" forbidden="$3"
  if "$C" "$TMP/$n.con" -o "$TMP/$n.bin" >"$TMP/$n.err" 2>&1; then
    no "$n: expected rejection"; return; fi
  if ! grep -qF "$must" "$TMP/$n.err"; then
    no "$n: missing expected diagnostic '$must'"; sed 's/^/        /' "$TMP/$n.err" | head -3; return; fi
  if [ -n "$forbidden" ] && grep -qF "$forbidden" "$TMP/$n.err"; then
    no "$n: diagnostic degraded — contains '$forbidden'"; sed 's/^/        /' "$TMP/$n.err" | head -4; return; fi
  ok "$n: rejected with '$must'${forbidden:+, no '$forbidden'}"; }

fp_of(){ "$C" "$1" --report extraction 2>/dev/null | grep -A6 "main.$2" | grep "fingerprint:" | head -1 | sed 's/^[[:space:]]*//'; }

echo "=== 1. parser recovery + duplicate proof-link attributes ==="
# Duplicate #[spec] must keep its precise diagnostic, not cascade to a generic
# "unexpected token }" (regression fixed 2026-06-24: recovery over-skipped).
write dup_spec 'mod m {
  #[spec(A.x)]
  #[spec(B.y)]
  fn f(x: i32) -> i32 { return x; }
}'
reject_clean dup_spec "duplicate #[spec(...)] on one function" "unexpected token"
write dup_proof 'mod m {
  #[proof_by(A.p)]
  #[proof_by(B.p)]
  fn f(x: i32) -> i32 { return x; }
}'
reject_clean dup_proof "duplicate #[proof_by(...)] on one function" "unexpected token"
# A malformed fn body inside a mod must yield ONE error, not a cascade onto the
# enclosing `}` (brace-depth-aware recovery).
write bad_body 'mod m {
  fn f(x: i32) -> i32 { return x + }
}'
if [ "$("$C" "$TMP/bad_body.con" 2>&1 | grep -c 'error\[parse\]')" = 1 ]; then
  ok "bad_body: single parse error (no brace-cascade)"
else no "bad_body: expected exactly one parse error"; "$C" "$TMP/bad_body.con" 2>&1 | head -4; fi

echo "=== 2. match linear-consumption agreement across arm forms ==="
# A linear resource consumed in EVERY arm (OR + range + guard + wildcard) is OK.
write consume_all 'struct R { v: Int }
fn take(r: R) -> Int { let R { v } = r; return v; }
fn f(x: Int) -> Int {
    let r: R = R { v: 1 };
    match x {
        0 | 1 => { let _a: Int = take(r); },
        2..=5 => { let _b: Int = take(r); },
        n if n > 100 => { let _c: Int = take(r); },
        _ => { let _d: Int = take(r); },
    }
    return 0;
}
pub fn main() -> Int { return f(3); }'
if "$C" "$TMP/consume_all.con" -o "$TMP/consume_all.bin" >"$TMP/ca.err" 2>&1; then
  ok "consume_all: consumed in every arm (OR/range/guard/_) compiles"
else no "consume_all: should compile"; sed 's/^/        /' "$TMP/ca.err" | head -3; fi
# Consumed in only some arms must be rejected (arms disagree on consumption).
write consume_some 'struct R { v: Int }
fn take(r: R) -> Int { let R { v } = r; return v; }
fn f(x: Int) -> Int {
    let r: R = R { v: 1 };
    match x {
        0 => { let _a: Int = take(r); },
        _ => { },
    }
    return 0;
}
pub fn main() -> Int { return f(3); }'
reject_clean consume_some "disagree on consumption" ""

echo "=== 3. match lowering correctness (interp == compiled, both correct) ==="
# match-on-&T with a guard binding — the 2026-06-24 interp deref fix.
write ml_ref_guard 'fn classify(p: &Int) -> i32 {
    match p {
        0 => { return 10; },
        1 | 2 => { return 20; },
        n if n > 5 => { return 30; },
        _ => { return 40; },
    }
}
pub fn main() -> Int {
    let a: Int = 2;
    let b: Int = 9;
    let c: Int = 0;
    if classify(&a) != 20 { return 1; }
    if classify(&b) != 30 { return 2; }
    if classify(&c) != 10 { return 3; }
    return 0;
}'
oracle ml_ref_guard
# OR-pattern desugaring + width coercion (i32 match value).
write ml_or_width 'fn pick(x: i32) -> i32 {
    match x {
        1 | 2 | 3 => { return 100; },
        _ => { return 200; },
    }
}
pub fn main() -> Int {
    if pick(2) != 100 { return 1; }
    if pick(9) != 200 { return 2; }
    return 0;
}'
oracle ml_or_width
# range-arm lowering.
write ml_range 'fn band(x: Int) -> Int {
    match x {
        0..=9 => { return 1; },
        10..=99 => { return 2; },
        _ => { return 3; },
    }
}
pub fn main() -> Int {
    if band(5) != 1 { return 1; }
    if band(50) != 2 { return 2; }
    if band(500) != 3 { return 3; }
    return 0;
}'
oracle ml_range

echo "=== 4. ProofCore fingerprint stability ==="
write fp_base 'fn g(x: Int) -> Int { return x + 1; }
pub fn main() -> Int { return 0; }'
write fp_reformat 'fn g(x: Int) -> Int {
    return x +    1;
}
pub fn main() -> Int { return 0; }'
write fp_semantic 'fn g(x: Int) -> Int { return x + 2; }
pub fn main() -> Int { return 0; }'
FPB="$(fp_of "$TMP/fp_base.con" g)"
FPR="$(fp_of "$TMP/fp_reformat.con" g)"
FPS="$(fp_of "$TMP/fp_semantic.con" g)"
if [ -n "$FPB" ] && [ "$FPB" = "$FPR" ]; then ok "fingerprint: formatting-only change keeps fingerprint"
else no "fingerprint: reformat changed fingerprint ('$FPB' vs '$FPR')"; fi
if [ -n "$FPB" ] && [ "$FPB" != "$FPS" ]; then ok "fingerprint: semantic change (+1→+2) changes fingerprint"
else no "fingerprint: semantic change did NOT change fingerprint ('$FPB')"; fi

echo "=== 5. deliberately-stale trust/proof fixture stays stale ==="
# examples/proof_pressure compute_checksum is intentionally stale (body has +1,
# fingerprint pins the original body). A blanket regeneration once un-staled it
# (fixed 2026-06-23). Guard that it reports stale.
PP="examples/proof_pressure/src/main.con"
if "$C" "$PP" --report proof-status 2>&1 | grep -q "1 stale"; then
  ok "proof_pressure: deliberately-stale compute_checksum still reports stale"
else no "proof_pressure: expected a stale proof (the deliberate fixture un-staled?)"; fi

echo ""
echo "PHASE6-REDTEAM: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
