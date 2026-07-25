#!/usr/bin/env bash
# Monomorphization name-collision regression gate (ROADMAP Phase 4 #44a — FIXED
# 2026-06-10).
#
# Mono used to mangle a specialization by the HEAD constructor of the type
# argument, discarding nested args, so `tag<Hold<Pair<i64>>>` and
# `tag<Hold<Pair<bool>>>` collapsed into one `tag_for_Pair` / one `%Hold_Pair`
# despite different layouts — a silent miscompile. `tyToSuffix` now keys on the
# FULL type (bracketed nested args), so distinct instantiations get distinct
# symbols and struct types.
#
# This gate now proves the fix HOLDS:
#   1. Two same-head/different-arg instantiations emit TWO distinct functions.
#   2. Array type-args (formerly all "unknown") are distinct.
#   3. Execution oracle: a field-touching body over the two distinct layouts
#      returns the correct value (would corrupt under the old merge).

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

F="examples/known_holes/mono_name_collision/src/main.con"

echo "=== distinct specializations for same-head / different-arg type args ==="
SSA="$("$COMPILER" "$F" --emit-ssa 2>&1)"
DEFS="$(printf '%s' "$SSA" | grep -cE "^define i64 @tag_for_")"
if [ "$DEFS" -ge 2 ]; then
  ok "two distinct tag specializations emitted (Pair<i64> and Pair<bool> no longer merge)"
else
  no "expected >=2 tag specializations, got $DEFS — name-collision miscompile may have regressed"
fi
rm -f examples/known_holes/mono_name_collision/src/main

echo "=== array type-args are distinct (formerly all 'unknown') ==="
cat > "$TMP/arr.con" <<'EOF'
struct Copy Box<T> { v: T }
fn tag<T: Copy>(b: Box<T>) -> i64 { return 1; }
fn main() -> i64 {
    let a: Box<[i64; 2]>  = Box::<[i64; 2]>  { v: [10, 20] };
    let b: Box<[bool; 2]> = Box::<[bool; 2]> { v: [true, false] };
    return tag(a) + tag(b);
}
EOF
ARR="$("$COMPILER" "$TMP/arr.con" --report mono 2>&1 | grep -E "Specializations:[[:space:]]+[0-9]" | grep -oE "[0-9]+")"
[ "${ARR:-0}" -ge 2 ] && ok "array type-args specialize separately ($ARR)" \
  || no "array type-args collapsed (got ${ARR:-0} specializations)"
rm -f "$TMP/arr"

echo "=== execution oracle: field access over two distinct layouts returns the right value ==="
cat > "$TMP/exec.con" <<'EOF'
struct Copy Pair<T> { a: T, b: T }
struct Copy Hold<T> { item: T }
fn first_i64(h: Hold<Pair<i64>>) -> i64 { return h.item.a; }
fn count<T: Copy>(h: Hold<T>, base: i64) -> i64 { return base; }
fn main() -> i64 {
    let big: Hold<Pair<i64>>    = Hold::<Pair<i64>>  { item: Pair::<i64>  { a: 100, b: 200 } };
    let small: Hold<Pair<bool>> = Hold::<Pair<bool>> { item: Pair::<bool> { a: true, b: false } };
    let x: i64 = count(big, 10);
    let y: i64 = count(small, 20);
    return first_i64(big) + x + y;
}
EOF
if "$COMPILER" "$TMP/exec.con" -o "$TMP/exec" >/dev/null 2>&1; then
  OUT="$("$TMP/exec" 2>/dev/null)"; RC=$?
  if [ "$OUT" = "130" ] || [ "$RC" = "130" ]; then
    ok "field-touching body over distinct layouts yields 130 (no ABI corruption)"
  else
    no "execution oracle gave stdout='$OUT' rc=$RC, expected 130 — possible layout corruption"
  fi
else
  no "execution oracle fixture failed to compile"
fi

echo "=== a specialization name that is already taken fails closed (E0809, bug 054) ==="

# `monoTypeName` is `base ++ "_" ++ suffixes`, built from source identifiers, so a
# hand-written name can occupy the name a specialization needs. Layout lookup is
# first-match-by-name, so the two then share ONE layout: with the hand-written
# declaration narrower, the specialization's fields are written past its end. The
# injective, unforgeable mangling that would remove the ambiguity is R-0007; until
# then this must be refused rather than resolved by declaration order.
rejects_e0809() {
  local name="$1" src="$2" f="$TMP/$1.con"
  printf '%s\n' "$src" > "$f"
  local out; out="$("$COMPILER" "$f" -o "$TMP/$1.out" 2>&1)"
  if grep -q "(E0809)" <<<"$out"; then ok "$name refused with E0809"
  else no "$name NOT refused with E0809 — got: $(head -1 <<<"$out")"; fi
}

# Hand-written type occupies the specialization's name, and is NARROWER.
rejects_e0809 user_shadows_specialization 'mod m {
  struct Copy Box<T> { v: T }
  struct Copy Box_Int { v: i8 }
  pub fn main() -> Int {
    let a: Box<Int> = Box::<Int> { v: 9223372036854775807 };
    let b: Box_Int = Box_Int { v: 3 };
    return (a.v / 1000000000000000000) + (b.v as Int);
  }
}'

# Two DISTINCT instantiations mangling to one name (the bug-054 second variant):
# `Pair<Int, Bool>` and `Pair_Int<Bool>` both produce `Pair_Int_Bool`.
rejects_e0809 two_instantiations_one_name 'mod m {
  struct Copy Pair<A, B> { a: A, b: B }
  struct Copy Pair_Int<B> { a: Int, b: B }
  pub fn main() -> Int {
    let x: Pair<Int, Bool> = Pair::<Int, Bool> { a: 7, b: true };
    let y: Pair_Int<Bool> = Pair_Int::<Bool> { a: 35, b: false };
    return x.a + y.a;
  }
}'

# The check must not fire on ordinary multi-argument generics.
cat > "$TMP/nocollide.con" <<'EOF'
mod m {
  struct Copy Pair<A, B> { a: A, b: B }
  pub fn main() -> Int {
    let x: Pair<Int, Bool> = Pair::<Int, Bool> { a: 7, b: true };
    let y: Pair<Int, i32> = Pair::<Int, i32> { a: 35, b: 1 };
    return x.a + y.a;
  }
}
EOF
if "$COMPILER" "$TMP/nocollide.con" -o "$TMP/nocollide" >/dev/null 2>&1; then
  "$TMP/nocollide" >/dev/null 2>&1
  [ "$?" = "42" ] && ok "distinct multi-arg instantiations still compile and run (42)" \
    || no "distinct multi-arg instantiations gave the wrong value"
else
  no "distinct multi-arg instantiations were rejected — E0809 is over-firing"
fi

echo ""
echo "MONO-NAME-COLLISION: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
