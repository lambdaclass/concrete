#!/usr/bin/env bash
# R-0436 / bug 056 — a function reference is a VALUE, not a register name.
#
# Lower used to represent a statically-known function as `SVal.reg
# "@fnref.<name>"`, and a call target as a `String` that was either a bare
# symbol or a `%`-prefixed register. Identity lived inside two string
# conventions that four passes decoded by hand, and it cost two defects:
#
#   1. `@fnref.X` is not a register, so the moment two branches bound the same
#      fn-typed variable the merge built a phi over a name no block defines and
#      SSAVerify refused it (E0709). Reassigning a fn pointer across an `if`, or
#      in a loop, could not compile — while the interpreter ran it correctly.
#   2. `replaceRegInInst` could rewrite the `%name` call target only into
#      another REGISTER. Folding a fn-pointer phi down to a known function left
#      the dead `%if.phi.N` in the call, and the dangling reference was caught
#      by llvm-as rather than by our own verifier — because SSAVerify's
#      `instUses` could not see a call target hidden in a string at all.
#
# `SVal.fnRef` and `SCallee.direct/indirect` make both a normal operand.
# Substitution, use-collection and verification reach them the way they reach
# every other operand, and devirtualization is a decision about a value's
# constructor rather than a string prefix.
#
# So this gate checks the two ends that matter: the programs run and agree with
# the interpreter, AND the common straight-line case still emits a DIRECT call.
# A fix that made everything indirect would pass a correctness gate while
# quietly deoptimizing every call through a fn-typed local.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="${COMPILER:-.lake/build/bin/concrete}"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# agrees <name> <expected-rc> <source> — compiles, runs to the expected status,
# and the interpreter reports the same value. Agreement is the load-bearing
# part: bug 056's signature was a program the interpreter ran and the compiler
# refused, so a compiled-only check would have called the old state healthy.
agrees() {
  local name="$1" want="$2" src="$3"
  local f="$TMP/$name.con"   # separate: see check_gate_hygiene's `local` rule
  printf '%s\n' "$src" > "$f"
  local out; out="$("$COMPILER" "$f" -o "$TMP/$name.out" 2>&1)"
  if [ ! -x "$TMP/$name.out" ]; then
    no "$name did not compile: $(printf '%s' "$out" | head -1)"; return
  fi
  local rc=0; bash -c '"$0" >/dev/null 2>&1' "$TMP/$name.out" 2>/dev/null || rc=$?
  local it; it="$("$COMPILER" "$f" --interp 2>&1 | tail -1)"
  if [ "$rc" != "$want" ]; then
    no "$name: compiled returned $rc, expected $want"
  elif [ "$it" != "$want" ]; then
    no "$name: interpreter said '$it', compiled said $rc — paths disagree"
  else
    ok "$name: compiled == interp == $want"
  fi
}

echo "=== a fn pointer may be rebound across control flow (bug 056) ==="
agrees fnptr_if 12 'fn a(x: Int) -> Int { return x + 1; }
fn b(x: Int) -> Int { return x + 2; }
fn pick(n: Int) -> Int {
  let mut f: fn(Int) -> Int = a;
  if n > 0 { f = b; } else { f = a; }
  return f(10);
}
fn main() -> Int { return pick(5); }'

agrees fnptr_if_const 12 'fn a(x: Int) -> Int { return x + 1; }
fn b(x: Int) -> Int { return x + 2; }
fn main() -> Int {
  let mut f: fn(Int) -> Int = a;
  if 1 > 0 { f = b; } else { f = a; }
  return f(10);
}'

agrees fnptr_loop 12 'fn a(x: Int) -> Int { return x + 1; }
fn b(x: Int) -> Int { return x + 2; }
fn main() -> Int {
  let mut f: fn(Int) -> Int = a;
  let mut i: Int = 0;
  while i < 3 { f = b; i = i + 1; }
  return f(10);
}'

echo "=== a phi may mix a loaded register with a known global ==="
# The `then` arm reaches the merge holding a value loaded from a struct field;
# the `else` arm reaches it holding a statically-known function. One phi, two
# different SVal shapes — impossible while one of them was a fake register.
agrees fnptr_field 12 'fn a(x: Int) -> Int { return x + 1; }
fn b(x: Int) -> Int { return x + 2; }
struct Ops { op: fn(Int) -> Int }
impl Destroy for Ops { pub fn destroy(&self) { } }
fn run(n: Int) -> Int {
  let mut g: fn(Int) -> Int = a;
  if n > 0 {
    let ops: Ops = Ops { op: b };
    g = ops.op;
    destroy(ops);
  }
  return g(10);
}
fn main() -> Int { return run(1); }'

echo "=== both arms statically known, and the taken one wins ==="
agrees fnptr_phi_else 11 'fn a(x: Int) -> Int { return x + 1; }
fn b(x: Int) -> Int { return x + 2; }
fn pick(n: Int) -> Int {
  let mut f: fn(Int) -> Int = a;
  if n > 0 { f = b; } else { f = a; }
  return f(10);
}
fn main() -> Int { return pick(0); }'

echo "=== devirtualization is preserved (must NOT regress to indirect) ==="
cat > "$TMP/direct.con" <<'CON'
fn a(x: Int) -> Int { return x + 1; }
fn main() -> Int {
  let f: fn(Int) -> Int = a;
  return f(10);
}
CON
IR="$("$COMPILER" "$TMP/direct.con" --emit-llvm 2>/dev/null)"
BODY="$(sed -n '/define .*@user_main/,/^}/p' <<<"$IR")"
if grep -qE 'call i64 @a\(' <<<"$BODY"; then
  ok "a statically-known target still compiles to a direct call"
else
  no "the straight-line case lost its direct call — devirtualization regressed:
$BODY"
fi
if grep -qE 'call i64 %' <<<"$BODY"; then
  no "the straight-line case emits an INDIRECT call through a register"
else
  ok "no indirect call in the straight-line case"
fi

# A phi that folds to one known function must devirtualize too — this is the
# exact shape whose call target used to be left dangling.
cat > "$TMP/folded.con" <<'CON'
fn a(x: Int) -> Int { return x + 1; }
fn b(x: Int) -> Int { return x + 2; }
fn main() -> Int {
  let mut f: fn(Int) -> Int = a;
  if 1 > 0 { f = b; } else { f = a; }
  return f(10);
}
CON
FBODY="$(sed -n '/define .*@user_main/,/^}/p' <<<"$("$COMPILER" "$TMP/folded.con" --emit-llvm 2>/dev/null)")"
if grep -qE 'call i64 @b\(' <<<"$FBODY"; then
  ok "a phi folded to one known target becomes a direct call"
else
  no "the folded phi did not devirtualize:
$FBODY"
fi
if grep -qE 'call i64 %if\.phi' <<<"$FBODY"; then
  no "the folded call still references the eliminated phi (bug 056's second half)"
else
  ok "no reference to the eliminated phi survives"
fi

echo "=== a genuinely undefined phi operand is still REJECTED ==="
# Removing the `@fnref.` exemptions from SSAVerify must not have blinded it.
# The roadmap asks for exactly this: the pass fails closed today, so prove the
# closure survived. Injected at the IR level because no source program should be
# able to produce an undefined phi operand.
probe() {
  local label="$1" want="$2" phiOperand="$3"
  cat > "$TMP/probe.lean" <<LEAN
import Concrete.IR.SSAVerify
open Concrete
def m : SModule := {
  name := "m", structs := [], enums := [], externFns := [], globals := [],
  functions := [{
    name := "bad", params := [], retTy := .int,
    blocks := [
      { label := "entry", insts := [], term := .condBr (.boolConst true) "l" "r" },
      { label := "l", insts := [], term := .br "mm" },
      { label := "r", insts := [], term := .br "mm" },
      { label := "mm",
        insts := [.phi "p" [($phiOperand, "l"), (.intConst 0 .int, "r")] .int],
        term := .ret (some (.reg "p" .int)) }
    ]
  }]
}
#eval match ssaVerifyProgram [m] with
  | .ok _ => "clean"
  | .error errs => "rejected:" ++ toString errs.length
LEAN
  local out; out="$(env LEAN_PATH=.lake/build/lib/lean lean "$TMP/probe.lean" 2>&1)"
  if grep -q "$want" <<<"$out"; then
    ok "$label"
  else
    no "$label — verifier said: $(printf '%s' "$out" | tr '\n' ' ' | head -c 300)"
  fi
}

# A phi naming a register that no block defines must still be refused. Removing
# the `@fnref.` exemptions must not have blinded the check.
probe "an undefined phi operand is still refused" "rejected:" '.reg "nowhere" .int'
# ...and a well-formed one must still pass, so the check above is not just
# "the verifier rejects everything".
probe "a constant phi operand is still accepted" "clean" '.intConst 1 .int'

# ...and the call TARGET must be checked the same way. This is the leg mutation
# #29 exposed: blinding the verifier to an indirect target breaks no valid
# program, so every correctness check above still passed while an invalid one
# would sail through to llvm-as. Only a probe that hands the verifier a bad
# target can see the difference.
callprobe() {
  local label="$1" want="$2" target="$3"
  cat > "$TMP/callprobe.lean" <<LEAN
import Concrete.IR.SSAVerify
open Concrete
def m : SModule := {
  name := "m", structs := [], enums := [], externFns := [], globals := [],
  functions := [{
    name := "bad", params := [], retTy := .int,
    blocks := [
      { label := "entry",
        insts := [.call (some "d") (.indirect $target) [] .int],
        term := .ret (some (.reg "d" .int)) }
    ]
  }]
}
#eval match ssaVerifyProgram [m] with
  | .ok _ => "clean"
  | .error errs => "rejected:" ++ toString errs.length
LEAN
  local out; out="$(env LEAN_PATH=.lake/build/lib/lean lean "$TMP/callprobe.lean" 2>&1)"
  if grep -q "$want" <<<"$out"; then
    ok "$label"
  else
    no "$label — verifier said: $(printf '%s' "$out" | tr '\n' ' ' | head -c 300)"
  fi
}

callprobe "an indirect call through an undefined register is refused" "rejected:" '(.reg "nowhere" (.fn_ [] (.concrete []) .int))'
# A `.fnRef` target is a GLOBAL, so it must NOT be reported as an undefined
# register — otherwise the fix would trade one false verdict for another.
callprobe "an indirect call through a fnRef global is accepted" "clean" '(.fnRef "bad" (.fn_ [] (.concrete []) .int))'

echo "=== the string encodings are gone, not merely unused ==="
# Structural, because every check above passes for a fix that keeps the
# conventions alive next to the new constructors — and a second representation
# is how the first one came back.
if grep -rn '"@fnref\.' Concrete/ --include='*.lean' | grep -v '^Concrete/IR/SSA.lean' | grep -q .; then
  no "a pass still spells a function reference as an @fnref. register name:
$(grep -rn '"@fnref\.' Concrete/ --include='*.lean' | grep -v '^Concrete/IR/SSA.lean')"
else
  ok "no pass constructs or decodes an @fnref. register name"
fi
if grep -rn 'startsWith "%"' Concrete/ --include='*.lean' | grep -q .; then
  no "a pass still decodes a call target from a %-prefixed string:
$(grep -rn 'startsWith "%"' Concrete/ --include='*.lean')"
else
  ok "no pass decodes a call target from a %-prefixed string"
fi
if grep -qE '^[[:space:]]*instance[^-]*Coe String SCallee' Concrete/IR/SSA.lean; then
  no "a String→SCallee coercion exists again — the bare-string form can compile silently"
else
  ok "no String→SCallee coercion (a call target cannot be spelled as a bare string)"
fi
if grep -qE '\| indirect \(target : SVal\)' Concrete/IR/SSA.lean \
   && grep -qE '\| fnRef \(name : String\) \(ty : Ty\)' Concrete/IR/SSA.lean; then
  ok "SVal.fnRef and SCallee.indirect carry the identity as values"
else
  no "the value-carrying constructors are gone — identity moved back into strings"
fi

echo
echo "FNPTR-VALUES: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
