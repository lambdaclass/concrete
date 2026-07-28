#!/usr/bin/env bash
# R-0005 / bug 053 — a trapping operation survives to trap, even discarded.
#
# `discard(-x)` at a signed MIN must abort. It did not: dead-code elimination
# had no `.unaryOp` case at all, so a discarded integer negation fell into a
# catch-all meaning "harmless" and was deleted, taking the documented MIN trap
# with it. The compiled program exited 0 while the interpreter aborted, and the
# emitted function contained no checked-negation call.
#
# The fix is not the missing arm — it is that FOUR consumers each decided
# independently whether `-x` can trap. The interpreter, the constant folder and
# EmitSSA agreed; DCE never asked. The folder's own comment, one screen away
# from the bug, said "leave the op live so the checked negation helper traps at
# runtime". `IntArith` now answers the question once (`evalIntUnaryOp`,
# `unaryOpCanTrap`) and every consumer reads it, which is the same single-source
# treatment `evalIntBinOp` already gave the binary family.
#
# So this gate checks the PROPERTY, not the arm: for each trapping shape, the
# compiled binary and the interpreter must agree, and the operation must still
# be present in the emitted IR. And for shapes that provably cannot trap, the
# optimizer must still be free to delete them — a fix that keeps everything
# alive would pass a trap gate while quietly costing every discarded operation.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="${COMPILER:-.lake/build/bin/concrete}"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# traps <name> <source> — compiled must abort AND the interpreter must report a
# trap. Agreement is the point: a silent compiled success against an aborting
# interpreter is exactly the bug-053 signature.
traps() {
  # Split, not one `local`: the builtin's arguments are expanded BEFORE it
  # assigns any of them, so `$name` here resolved against the OUTER (unset)
  # scope and `set -u` aborted the gate. It passed review because the sibling
  # gates spell the same line with `$1`, which is always set.
  local name="$1" src="$2"
  local f="$TMP/$name.con"
  printf '%s\n' "$src" > "$f"
  if ! "$COMPILER" "$f" -o "$TMP/$name.out" >/dev/null 2>&1; then
    no "$name did not compile"; return
  fi
  # Run through an inner shell whose stderr is discarded: the shell REPORTING a
  # signalled child is what prints "Abort trap: 6", so redirecting the child
  # alone does not silence it, and a gate nobody can read is a gate nobody runs.
  local rc=0; bash -c '"$0" >/dev/null 2>&1' "$TMP/$name.out" 2>/dev/null || rc=$?
  local it; it="$("$COMPILER" "$f" --interp 2>&1 | tail -1)"
  local compiled_trapped=0 interp_trapped=0
  [ "$rc" -ge 128 ] && compiled_trapped=1
  grep -qi "overflow\|trap\|abort" <<<"$it" && interp_trapped=1
  if [ "$compiled_trapped" = 1 ] && [ "$interp_trapped" = 1 ]; then
    ok "$name traps on both paths (compiled rc=$rc)"
  elif [ "$compiled_trapped" = 0 ] && [ "$interp_trapped" = 1 ]; then
    no "$name: interpreter trapped but COMPILED returned $rc — the trap was optimized away"
  elif [ "$compiled_trapped" = 1 ]; then
    no "$name: compiled trapped but interpreter did not ($it)"
  else
    no "$name: neither path trapped (compiled rc=$rc, interp '$it')"
  fi
}

# removable <name> <expected-rc> <source> — a provably non-trapping discarded op
# must still be deletable; the fix must not blanket-preserve dead work.
removable() {
  local name="$1" want="$2" src="$3"
  local f="$TMP/$name.con"
  printf '%s\n' "$src" > "$f"
  if ! "$COMPILER" "$f" -o "$TMP/$name.out" >/dev/null 2>&1; then
    no "$name did not compile"; return
  fi
  local rc=0; bash -c '"$0" >/dev/null 2>&1' "$TMP/$name.out" 2>/dev/null || rc=$?
  [ "$rc" = "$want" ] && ok "$name runs normally ($rc) — still removable" \
                      || no "$name returned $rc, expected $want"
}

echo "=== discarded checked negation at MIN traps, every signed width ==="
for spec in "i8:-128" "i16:-32768" "i32:-2147483648" "Int:-9223372036854775808"; do
  ty="${spec%%:*}"; minv="${spec##*:}"
  traps "neg_min_$ty" "mod t {
  pub fn main() -> Int {
    let x: $ty = $minv;
    discard(-x);
    return 7;
  }
}"
done

echo "=== the result being UNUSED is the whole point ==="
# The value is consumed here, so nothing may be eliminated — this shape trapped
# even before the fix, and its job is to prove the gate is not merely observing
# that negation traps in general.
traps "neg_min_used" 'mod t {
  pub fn main() -> Int {
    let x: i32 = -2147483648;
    let y: i32 = -x;
    return y as Int;
  }
}'

echo "=== provably non-trapping discards stay removable ==="
removable "neg_small_const" 7 'mod t {
  pub fn main() -> Int { let x: i32 = 5; discard(-x); return 7; }
}'
removable "bitnot_discarded" 7 'mod t {
  pub fn main() -> Int { let x: i32 = 5; discard(~x); return 7; }
}'
removable "float_neg_discarded" 7 'mod t {
  pub fn main() -> Int { let x: Float64 = 1.5; discard(-x); return 7; }
}'

echo "=== the operation survives into the emitted IR ==="
cat > "$TMP/ir.con" <<'CON'
mod t {
  pub fn main() -> Int {
    let x: i8 = -128;
    discard(-x);
    return 7;
  }
}
CON
IR="$("$COMPILER" "$TMP/ir.con" --emit-llvm 2>/dev/null)"
if sed -n '/define .*@user_main/,/^}/p' <<<"$IR" | grep -q "__cc_ssub"; then
  ok "the checked-negation helper is present in user_main"
else
  no "user_main contains no checked-negation call — the operation was deleted (bug 053's signature)"
fi

echo "=== the trap answer comes from ONE place ==="
# Structural, because the value checks above pass for any fix, including four
# consumers that happen to agree today and drift tomorrow. What made 053
# possible was the question being asked in four places.
if grep -q "unaryOpCanTrap" Concrete/IR/SSACleanup.lean; then
  ok "DCE consults IntArith.unaryOpCanTrap"
else
  no "DCE decides trapping locally again — the single source was bypassed"
fi
if grep -q "evalIntUnaryOp" Concrete/Interp/Interp.lean; then
  ok "the interpreter evaluates unary ops through IntArith"
else
  no "the interpreter re-derives unary semantics locally"
fi
if grep -qE "def (evalIntUnaryOp|unaryOpCanTrap)" Concrete/Semantics/IntArith.lean; then
  ok "IntArith owns the unary trap inventory"
else
  no "IntArith no longer defines the unary trap inventory"
fi

echo
echo "TRAP-INVENTORY: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
