#!/usr/bin/env bash
# R-0002 / bug 050 — a callee VALUE is never resolved by global name.
#
# Elab used to emit a call through a fn-typed local as `.call name args`, the
# same Core shape as a direct call. Every later pass that wanted to know "is this
# callee a local?" had to re-derive it: Lower and Interp probed their own
# variable scope and got it right, Mono had no scope and got it wrong. It
# resolved the name against the global fn map plus linker aliases, so a local
# fn-pointer whose name matched a generic function was rewritten into a direct
# call of that generic.
#
# Two observable consequences, both pinned below:
#   * silent wrong code — `pick(21)` returning the identity generic's 21 while
#     the interpreter returned 42;
#   * unbuildable projects — std.io's `Writer::write_raw` binds its target as a
#     local literally named `f`, so any program defining `fn f<T: Copy>` died in
#     SSAVerify with E0711.
#
# The callee's kind now travels with the call (`Callee.direct` / `.indirect`), so
# no pass re-decides it. This gate checks the property from the angles that would
# each catch a different way of losing it:
#
#   1. DISPATCH   — the value in the local is what runs, for name collisions with
#                   generic, non-generic, intrinsic, and renamed-import targets.
#   2. AGREEMENT  — interpreter == compiled on each, since the interpreter
#                   resolves indirectly by construction and is the oracle that
#                   originally disagreed.
#   3. NO REWRITE — the emitted IR contains no specialization of the shadowed
#                   generic for the indirect call, and the call goes through a
#                   register rather than a direct symbol.
#   4. STD        — std.io still works in a program that defines `f`.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="${COMPILER:-.lake/build/bin/concrete}"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# agrees <name> <expected-rc> <source>
agrees() {
  local name="$1" want="$2" src="$3" f="$TMP/$1.con"
  printf '%s\n' "$src" > "$f"
  local out rc irc
  if ! out="$("$COMPILER" "$f" -o "$TMP/$1.out" 2>&1)"; then
    no "$name did not compile: $(head -1 <<<"$out")"; return
  fi
  "$TMP/$1.out" >/dev/null 2>&1; rc=$?
  irc="$("$COMPILER" "$f" --interp 2>&1 | tail -1)"
  if [ "$rc" != "$want" ]; then
    no "$name compiled result $rc != expected $want (the callee value was not what ran)"
  elif [ "$irc" != "$want" ]; then
    no "$name interpreted result $irc != expected $want (compiled agreed)"
  else
    ok "$name = $want (compiled == interpreted == expected)"
  fi
}

echo "=== 1/2. dispatch through the local, with the interpreter as oracle ==="

# The historical witness: the local shadows a GENERIC function.
agrees shadows_generic 42 'mod t {
  fn pick<T: Copy>(x: T) -> T { return x; }
  fn double(x: i64) -> i64 { return x * 2; }
  pub fn main() -> i64 {
    let pick: fn(i64) -> i64 = double;
    return pick(21);
  }
}'

# Shadowing a NON-generic function: Mono only rewrote generics, but the call must
# still dispatch through the value, and nothing downstream may resolve the name.
agrees shadows_nongeneric 42 'mod t {
  fn helper(x: i64) -> i64 { return x + 1; }
  fn double(x: i64) -> i64 { return x * 2; }
  pub fn main() -> i64 {
    let helper: fn(i64) -> i64 = double;
    return helper(21);
  }
}'

# Shadowing an INTRINSIC name. Lower resolves intrinsics by name, so an indirect
# callee must be excluded there too or the call is intercepted. (`sizeof` cannot
# be used here: Check rejects it as "takes no value arguments" before shadowing is
# considered, i.e. that name is reserved rather than hijackable.)
agrees shadows_intrinsic 42 'mod t {
  fn double(x: i64) -> i64 { return x * 2; }
  pub fn main() -> i64 {
    let print_int: fn(i64) -> i64 = double;
    return print_int(21);
  }
}'

# A local shadowing a generic that IS ALSO used generically in the same function:
# the specialization must exist for the direct call and be ignored for the local.
agrees generic_also_used 45 'mod t {
  fn pick<T: Copy>(x: T) -> T { return x; }
  fn double(x: i64) -> i64 { return x * 2; }
  pub fn main() -> i64 {
    let direct: i64 = pick::<i64>(3);
    let pick: fn(i64) -> i64 = double;
    return pick(21) + direct;
  }
}'

# Passed as a PARAMETER rather than bound by let: parameters of fn type are the
# other way a callee becomes a value.
agrees fn_param 42 'mod t {
  fn pick<T: Copy>(x: T) -> T { return x; }
  fn double(x: i64) -> i64 { return x * 2; }
  fn apply(pick: fn(i64) -> i64, v: i64) -> i64 { return pick(v); }
  pub fn main() -> i64 { return apply(double, 21); }
}'

# A local shadowing a RENAMED IMPORT. Mono resolves a callee name through the
# linker-alias pool as well as the fn map (bug 044 widened that path), so the
# alias is a second way a binding name can be mistaken for a definition — for a
# plain import and for a generic one, whose alias resolution also specializes.
agrees shadows_renamed_import 42 'mod Helpers {
    pub fn twice(x: i64) -> i64 { return x * 2; }
    pub fn gen_id<T: Copy>(x: T) -> T { return x; }
}
mod Main {
    import Helpers.{ twice as pick, gen_id as tag };
    fn triple(x: i64) -> i64 { return x * 3; }
    pub fn main() -> i64 {
        let pick: fn(i64) -> i64 = triple;
        let tag: fn(i64) -> i64 = triple;
        return pick(7) + tag(7);
    }
}'

echo "=== 3. the emitted IR does not resolve the callee to a symbol ==="

cat > "$TMP/ir.con" <<'CON'
mod t {
  fn pick<T: Copy>(x: T) -> T { return x; }
  fn double(x: i64) -> i64 { return x * 2; }
  pub fn main() -> i64 {
    let pick: fn(i64) -> i64 = double;
    return pick(21);
  }
}
CON
IR="$("$COMPILER" "$TMP/ir.con" --emit-llvm 2>/dev/null)"

# Pre-fix, Mono specialized the shadowed generic and the call became a direct
# call of it. No specialization of `pick` may exist: nothing calls it generically.
if grep -qE "@pick_(for_)?[A-Za-z0-9_]+" <<<"$IR"; then
  no "a specialization of the shadowed generic was emitted — the indirect call was resolved by name"
else
  ok "no specialization of the shadowed generic exists"
fi

# The call must go through a register (indirect), not a direct @symbol.
if grep -qE "call .*@pick" <<<"$IR"; then
  no "the call targets @pick directly — the callee value was replaced by a symbol"
else
  ok "the call does not target a @pick symbol"
fi
# What must be true is which function the call targets: the one the local holds.
# Lower devirtualizes a statically-known fn reference to a direct call of that
# function, which is correct and must not be mistaken for the bug — the bug was
# targeting the SHADOWED NAME instead. (A pointer whose target is only known at
# runtime cannot be exercised here: bug 056 makes reassigning a fn-pointer across
# a branch fail SSAVerify, so that case joins this gate when R-0436 lands.)
if grep -qE "call i64 @double" <<<"$IR"; then
  ok "the call targets @double — the function the local holds"
else
  no "the call does not target @double; the callee value was not what got called"
fi

echo "=== 4. std.io survives a program that defines a generic named f ==="

# The exact collision: std.io's Writer::write_raw binds `let f: fn(...) = ...`.
# Pre-fix this failed to BUILD (E0711 from SSAVerify), so compiling is the check.
PROJ="tests/programs/regress_050_generic_f_std_io"
if [ -d "$PROJ" ]; then
  if out="$( cd "$PROJ" && "$ROOT_DIR/$COMPILER" build -o "$TMP/stdio" 2>&1 )"; then
    "$TMP/stdio" >/dev/null 2>&1
    rc=$?
    [ "$rc" -eq 0 ] && ok "std.io project defining 'fn f<T: Copy>' builds and runs" \
      || no "std.io project ran with exit $rc"
  else
    no "std.io project defining 'fn f<T: Copy>' failed to build: $(tail -1 <<<"$out")"
  fi
else
  no "$PROJ is missing — the std.io collision variant has no fixture"
fi

echo
echo "INDIRECT-CALL-IDENTITY: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
