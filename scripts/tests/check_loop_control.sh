#!/usr/bin/env bash
# Loop-control gate (ROADMAP Phase 6 #4).
#
# break / continue / labeled loops / while-as-expression are implemented; this
# gate pins their behavior so further control-flow work (defer, more patterns)
# builds on a fixed semantics. It locks four things:
#
#   1. CONTROL FLOW — unlabeled break exits the innermost loop only; labeled
#      `break 'l` exits the named loop; `for` supports break + continue.
#   2. WHILE-AS-EXPRESSION VALUE — `break <v>` and the `else { v }` block produce
#      the loop's value, correctly at non-i64 widths (regression: the value used
#      to be stored as i64 into a narrower result slot and read back as 0).
#   3. LINEAR CLEANUP — break/continue that would skip an unconsumed linear value
#      are rejected (E0210 / E0211); a linear consumed before the break is fine.
#   4. TYPE AGREEMENT — a while-expression's break value and else value must have
#      the same type (E0222).
#
# Fixtures live in tests/programs/loop_control/ (not picked up by the main suite,
# which only scans tests/programs/*.con). See docs/language/LOOP_CONTROL.md.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT_DIR/scripts/tests/lib/selfprint.sh"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
D="tests/programs/loop_control"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# run_expect <fixture> <expected-stdout>
run_expect(){
  local name="$1" exp="$2"
  gate_selfprint_wrap "$D/$name.con" "$TMP/$name.w.con"
  if ! "$C" "$TMP/$name.w.con" -o "$TMP/$name.bin" >"$TMP/$name.err" 2>&1; then
    no "$name: expected to compile, but it failed"; sed 's/^/        /' "$TMP/$name.err" | head -3; return
  fi
  local got; got="$("$TMP/$name.bin" 2>/dev/null)"
  [ "$got" = "$exp" ] && ok "$name -> $got" || no "$name: got '$got', want '$exp'"
}

# reject_with <fixture> <E-code>
reject_with(){
  local name="$1" code="$2"
  local out; out="$("$C" "$D/$name.con" -o "$TMP/$name.bin" 2>&1)"
  if grep <<<"$out" -qE 'error\['; then
    grep <<<"$out" -q "($code)" \
      && ok "$name rejected with $code" \
      || { no "$name rejected, but not with $code"; grep <<<"$out" -oE '\([A-Z0-9]+\)[^|]*' | head -1 | sed 's/^/        got: /'; }
  else
    no "$name: expected rejection ($code), but it compiled"
  fi
}

echo "=== 1. control flow: break scope, labeled break, for break/continue ==="
run_expect break_inner_only 3
run_expect labeled_break 1
run_expect for_break_continue 8

echo "=== 2. while-as-expression value (non-i64 width regression) ==="
run_expect loop_result_break_i32 7
run_expect loop_result_completes_i32 42

echo "=== 3. linear cleanup across break/continue ==="
run_expect break_after_consume_linear 0
reject_with neg_break_leaks_linear E0210
reject_with neg_continue_leaks_linear E0211

echo "=== 4. while-expression break/else type agreement ==="
reject_with neg_while_expr_removed E0001

echo "=== 5. the bounded-loop classifier is FAIL-CLOSED ==="
# The `predictable` profile advertises "bounded iteration, compiler-enforced"
# (docs/platform/PREDICTABLE_BOUNDARIES.md). It was enforcing something much weaker: the condition had
# to LOOK like a comparison and the step list had to be non-empty, with nothing tying the two
# together. Both loops below were classified `bounded`, the module passed
# `--check predictable`, and the effects report said `0 unbounded loops` -- for code that runs
# forever. This is a LIVENESS claim, so no safety obligation could ever have noticed it.
BIN_LC="$ROOT_DIR/.lake/build/bin/concrete"
if lake build >/dev/null 2>&1 && [ -x "$BIN_LC" ]; then
  TD_LC="$(mktemp -d)"; trap 'rm -rf "$TD_LC"' EXIT
  cat > "$TD_LC/loops.con" <<'CON'
mod loopcls {
    fn honest(n: i32) -> i32 {
        let mut z: i32 = 0;
        for (let mut i: i32 = 0; i < n; i = i + 1) { z = z + 1; }
        return z;
    }
    fn never_ends(n: i32) -> i32 {
        let mut z: i32 = 0;
        for (let mut i: i32 = 0; i < n; z = z + 1) { z = z + 1; }
        return z;
    }
    fn wrong_way(n: i32) -> i32 {
        let mut z: i32 = 0;
        for (let mut i: i32 = 0; i < n; i = i - 1) { z = z + 1; }
        return z;
    }
}
CON
  EFF="$("$BIN_LC" "$TD_LC/loops.con" --report effects 2>/dev/null)"
  NUNB="$(printf '%s' "$EFF" | grep -c 'loops: unbounded')"
  [ "$NUNB" = "2" ] \
    && ok "a step on an unrelated variable, and a step AWAY from the bound, both read unbounded" \
    || no "expected 2 unbounded loops, got $NUNB — a non-terminating loop reads as bounded"
  printf '%s' "$EFF" | grep -q 'loops: bounded' \
    && ok "and the honest i = i + 1 loop is still bounded (not merely rejecting everything)" \
    || no "a genuinely bounded loop is now misclassified — the rule is too strict to be useful"
  "$BIN_LC" "$TD_LC/loops.con" --check predictable >/dev/null 2>&1 \
    && no "--check predictable ADMITS a module containing two infinite loops" \
    || ok "--check predictable rejects the module"
else
  no "build failed or binary missing; the loop-classifier checks did not run"
fi

echo "=== 6. an INDIRECT call cannot claim no-recursion or a stack bound ==="
# The call graph records only DIRECT callees -- correct for extraction, wrong for two guarantees
# built on the same graph. A cycle through a function pointer produces no edge, so SCC found no
# cycle: `ping` calling `apply(ping, …)` reported `recursion: none`, PASSED
# `--check predictable`, and `--report stack-depth` stated `depth: 1, stack: 32 bytes` for a
# function that recurses arbitrarily deep. A false NUMBER, which is worse than a missing one
# because it is quotable.
# Must FAIL rather than skip when the build is broken: an earlier version guarded on TD_LC
# being set, which is exactly what an unbuildable tree leaves unset -- so a mutation that failed
# to compile silently bypassed this whole section instead of failing it.
if [ -z "${TD_LC:-}" ]; then
  no "build failed or temp dir unavailable; the indirect-call checks did NOT run"
else
  cat > "$TD_LC/indirect.con" <<'CON'
mod indirect {
    fn apply(f: fn(i32) -> i32, x: i32) -> i32 { return f(x); }
    fn ping(x: i32) -> i32 {
        if x <= 0 { return 0; }
        return apply(ping, x - 1);
    }
}
CON
  "$BIN_LC" "$TD_LC/indirect.con" --check predictable >/dev/null 2>&1 \
    && no "--check predictable ADMITS a module whose recursion goes through a function pointer" \
    || ok "an indirect call is refused by the predictable profile"
  SD="$("$BIN_LC" "$TD_LC/indirect.con" --report stack-depth 2>/dev/null)"
  printf '%s' "$SD" | grep -qE "Max stack bound: [1-9]" \
    && no "a byte-exact stack bound is still claimed where recursion cannot be ruled out" \
    || ok "no stack bound is claimed when the callee set is unknown"
  UNB="$(printf '%s' "$SD" | grep -c 'stack: unbounded')"
  [ "$UNB" = "2" ] \
    && ok "unboundedness propagates to the CALLER too (both functions, got $UNB)" \
    || no "expected both functions unbounded, got $UNB — a caller of an unbounded fn kept a bound"

  # Transitive case with ordinary direct recursion: the caller of a recursive function had a
  # finite bound because recursive callees were filtered out of the chain entirely.
  cat > "$TD_LC/transdepth.con" <<'CON'
mod transdepth {
    fn recurses(x: i32) -> i32 {
        if x <= 0 { return 0; }
        return recurses(x - 1);
    }
    fn caller(x: i32) -> i32 { return recurses(x); }
}
CON
  TD2="$("$BIN_LC" "$TD_LC/transdepth.con" --report stack-depth 2>/dev/null)"
  printf '%s' "$TD2" | grep -qE "Max stack bound: [1-9]" \
    && no "a caller of a directly-recursive function still gets a finite stack bound" \
    || ok "calling an unbounded function removes the caller's bound"
fi

echo "=== 7. the five profile gates agree on TRANSITIVITY ==="
# Audit result. Alloc and the blocking caps were already transitive for free: the compiler
# refuses `E0520: requires Alloc but caller has (none)`, so a caller must declare the capability
# and the effects report sees it on the signature. A fn-pointer type carries its capset, so
# passing an `with(Alloc)` function where a pure `fn(i32) -> i32` is expected is an E0220 type
# error -- the capability cannot be laundered through a combinator either.
#
# FFI had NO capability behind it, only a direct-callee check, so `f` calling `g` calling an
# extern reported `ffi: no` and `evidence: enforced` while transitively crossing FFI. Now closed
# over the call graph, which makes FFI agree with the two gates that already were.
if [ -z "${TD_LC:-}" ]; then
  no "build failed or temp dir unavailable; the transitivity checks did NOT run"
else
  cat > "$TD_LC/ffi.con" <<'CON'
mod ffitrans {
    trusted extern fn c_abs(x: i32) with() -> i32;
    fn direct(x: i32) -> i32 { return c_abs(x); }
    fn reaches(x: i32) -> i32 { return direct(x); }
}
CON
  FF="$("$BIN_LC" "$TD_LC/ffi.con" --report effects 2>/dev/null)"
  NFFI="$(printf '%s' "$FF" | grep -c 'ffi: yes')"
  [ "$NFFI" = "2" ] \
    && ok "a function that reaches an extern THROUGH another function crosses FFI (got $NFFI)" \
    || no "expected 2 FFI-crossing functions, got $NFFI — transitive FFI reads as ffi: no again"

  # Capability laundering through a fn pointer must stay a type error, or the alloc and blocking
  # gates lose the mechanism that makes them transitive in the first place.
  cat > "$TD_LC/launder.con" <<'CON'
mod capldr {
    fn needs_alloc(x: i32) with(Alloc) -> i32 { return x + 1; }
    fn apply_pure(f: fn(i32) -> i32, x: i32) -> i32 { return f(x); }
    fn launder(x: i32) -> i32 { return apply_pure(needs_alloc, x); }
}
CON
  "$BIN_LC" "$TD_LC/launder.con" --report effects >/dev/null 2>&1 \
    && no "an Alloc-requiring function can be passed as a PURE fn pointer — capability laundering" \
    || ok "a fn-pointer type carries its capset (laundering is a type error)"

  # And the capability itself must remain required of the caller.
  cat > "$TD_LC/captrans.con" <<'CON'
mod captrans {
    fn reads(x: i32) with(File) -> i32 { return x; }
    fn caller_nocap(x: i32) -> i32 { return reads(x); }
}
CON
  "$BIN_LC" "$TD_LC/captrans.con" --report effects >/dev/null 2>&1 \
    && no "a File-requiring function is callable without the capability — blocking gate is fooled" \
    || ok "capabilities are required of callers (E0520), so alloc/blocking stay transitive"
fi

echo "=== 8. recursion admission is TRANSITIVE (all five gates now agree) ==="
# "Predictable execution" is a property of RUNNING a function: if it calls something whose
# recursion cannot be ruled out, its own execution inherits that. The label used to be computed
# from each body in isolation, so a `main` calling a directly recursive `fib` was `enforced`.
# Alloc and blocking never had this problem (the compiler forces the caller to declare the
# capability); FFI was closed earlier. Recursion was the last gate asking "is it written here?"
# rather than "can you reach it?".
if [ -z "${TD_LC:-}" ]; then
  no "build failed or temp dir unavailable; the transitive-recursion checks did NOT run"
else
  cat > "$TD_LC/transrec.con" <<'CON'
mod transrec {
    fn recurses(x: i32) -> i32 {
        if x <= 0 { return 0; }
        return recurses(x - 1);
    }
    fn caller(x: i32) -> i32 { return recurses(x); }
    fn unrelated(x: i32) -> i32 { return x + 1; }
}
CON
  TR="$("$BIN_LC" "$TD_LC/transrec.con" --check predictable 2>/dev/null)"
  printf '%s' "$TR" | grep -q "2 function(s) failed" \
    && ok "a caller of a recursive function fails the profile too" \
    || no "transitive recursion is not rejected — a caller of unbounded recursion is admitted"
  # And it must not reject everything: a function reaching nothing recursive still passes.
  printf '%s' "$TR" | grep -q "1 passed" \
    && ok "an unrelated function still passes (the closure is not swallowing the module)" \
    || no "the transitive closure is over-broad — unrelated functions are being rejected"
  TRE="$("$BIN_LC" "$TD_LC/transrec.con" --report effects 2>/dev/null)"
  printf '%s' "$TRE" | grep -q "1 enforced" \
    && ok "exactly one function is enforced in the effects report" \
    || no "the effects report disagrees with the profile gate on who is enforced"
fi
# The gate that REJECTS builds must use the same closed sets. It did not, for FFI, between the
# commit that closed FFI and the one that added this: six consumers were updated and the policy
# path was not. `profileClosures` now returns both sets together so a consumer cannot take one
# without the other.
grep -q "Report.profileClosures projectModules pc" Concrete/Check/Policy.lean \
  && ok "the policy enforcement path uses the transitively-closed sets" \
  || no "policy enforcement is back on the direct-callee check — it rejects builds, so this matters most"
grep -qE "^def profileClosures" Concrete/Report/Report.lean \
  && ok "both closures come from one place (cannot obtain one without the other)" \
  || no "profileClosures is gone — consumers can drift apart again"

echo "=== 9. two properties, two names: admission vs proof eligibility ==="
# Making admission transitive changed what "passes the predictable profile" means -- but
# ProofCore computed its OWN version of that same-named property for proof ELIGIBILITY, and was
# left alone on purpose. The result was two commands printing opposite claims about one function:
#
#   --check predictable    caller — reaches a function whose recursion cannot be ruled out
#   --report proof-status  `caller` passes the predictable profile
#
# The logic was right in both places; the PHRASE was doing two jobs. They are genuinely different
# questions, and eligibility must NOT become transitive: `caller`'s own overflow obligation is
# provable whether or not `recurses` terminates. Overflow is a safety property, non-termination a
# liveness one. Making eligibility transitive would refuse a provable obligation for no soundness
# gain -- it would destroy proofs to tidy a name.
#
# So the durable invariant is: the two may DISAGREE, and neither may speak in the other's
# vocabulary.
if [ -z "${TD_LC:-}" ]; then
  no "build failed or temp dir unavailable; the vocabulary checks did NOT run"
else
  cat > "$TD_LC/twoprops.con" <<'CON'
mod twoprops {
    fn recurses(x: i32) -> i32 {
        if x <= 0 { return 0; }
        return recurses(x - 1);
    }
    fn caller(x: i32) -> i32 { return recurses(x); }
}
CON
  "$BIN_LC" "$TD_LC/twoprops.con" --check predictable >/dev/null 2>&1 \
    && no "admission is not transitive any more — caller of a recursive fn is admitted" \
    || ok "admission REJECTS the caller (transitive)"
  PS="$("$BIN_LC" "$TD_LC/twoprops.con" --report proof-status 2>/dev/null)"
  # RE-AIMED 2026-09-26, and the rename is in this check's own interest. The phrase used to
  # be "`caller` is eligible for proof", which is precisely the vocabulary this section
  # exists to keep apart: a reader takes "eligible for proof" to mean the claim would be
  # ADMITTED, while internally it meant only that an obligation can be EXTRACTED. It now
  # renders `obligation: extractable`, which names the per-body question and borrows
  # nothing from admission.
  #
  # The PROPERTY is unchanged and was verified directly before this grep was touched:
  # `caller` still produces an extractable obligation and `--check predictable` still
  # rejects it. Asserted on the qualName AND the state together, so a report that stopped
  # naming the function, or named it in some other state, fails here rather than matching
  # a bare phrase found anywhere in the output.
  printf '%s' "$PS" | grep -A2 'twoprops.caller`' | grep -q "obligation: extractable" \
    && ok "eligibility ACCEPTS the same function (per-body, and must stay so)" \
    || no "caller lost proof eligibility — a provable obligation is being refused"
  # The anti-conflation lock: the proof surface must not claim the admission property.
  printf '%s' "$PS" | grep -q "predictable profile" \
    && no "proof-status speaks of the 'predictable profile' again — one phrase, two properties" \
    || ok "the proof surface does not claim the admission property"
  # And the field holds what its name says.
  grep -q "eligibilityReasons" Concrete/Proof/ProofCore.lean \
    && ok "the reasons field is named for what it holds (was 'profileGates')" \
    || no "the eligibility reasons are called profile gates again"
fi

echo ""
echo "LOOP-CONTROL: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
