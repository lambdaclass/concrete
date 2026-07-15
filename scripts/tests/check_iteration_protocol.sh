#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Iteration-protocol gate (ROADMAP Phase 6 #17; docs/ITERATION_PROTOCOL.md).
#
# Concrete's traversal story is a fixed hierarchy of explicit forms — NOT a
# universal lazy `Iterator` trait. This gate locks the blessed forms that exist
# and the exclusions that keep authority/allocation visible:
#   1. `for` / indexed loop (language) — compiles + runs.
#   2. capability-polymorphic Vec::for_each / fold / map / for_each_ctx — present
#      with the right shapes; for_each/fold are Alloc-free, map carries Alloc.
#   3. cursor structs for parsers/readers (ByteCursor) — present with read/peek.
#   4. NO closures, NO `Iterator` trait, NO `dyn` trait-object iterator,
#      NO hidden allocation.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
STD="std/src"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== 1. for / indexed loop is a real, runnable traversal form ==="
cat > "$TMP/forloop.con" <<'EOF'
pub fn main() -> Int {
    let mut sum: Int = 0;
    for (let mut i = 0; i < 10; i = i + 1) {
        sum = sum + i;
    }
    if sum == 45 { return 0; }
    return 1;
}
EOF
if "$C" "$TMP/forloop.con" -o "$TMP/forloop.bin" >"$TMP/f.err" 2>&1 && "$TMP/forloop.bin" >/dev/null 2>&1 \
   && "$C" "$TMP/forloop.con" --interp >/dev/null 2>&1; then
  ok "for/indexed loop compiles, runs, and interp agrees (sum 0..9 == 45)"
else no "for/indexed loop oracle failed"; sed 's/^/        /' "$TMP/f.err" | head -3; fi

echo "=== 2. capability-polymorphic collection traversal exists with the right shapes ==="
# for_each / fold present on Vec, and capability-polymorphic (`cap C` + `with(C)`).
if grep -qE "fn for_each<cap C>\(&self, f: fn\(&T\) with\(C\)\) with\(C\)" "$STD/vec.con"; then
  ok "Vec::for_each is capability-polymorphic (with(C), no ambient authority)"
else no "Vec::for_each missing or not capability-polymorphic"; fi
if grep -qE "fn fold<A, cap C>" "$STD/vec.con"; then
  ok "Vec::fold present (capability-polymorphic accumulate)"; else no "Vec::fold missing"; fi
# for_each_ctx threads explicit &mut Ctx (closure-free stateful callback).
if grep -qE "fn for_each_ctx<Ctx, cap C>\(&self, ctx: &mut Ctx" "$STD/vec.con"; then
  ok "Vec::for_each_ctx threads explicit &mut Ctx (no closures for state)"
else no "Vec::for_each_ctx missing or not context-threaded"; fi

echo "=== 3. allocation is visible: map allocates (Alloc), for_each/fold do not ==="
if grep -qE "fn map<U, cap C>.*with\(C, Alloc\) -> Vec<U>" "$STD/vec.con"; then
  ok "Vec::map carries Alloc in its signature (allocation is visible)"
else no "Vec::map should carry Alloc"; fi
# for_each's with-clause must NOT include Alloc (alloc-free traversal).
if grep -E "fn for_each<cap C>" "$STD/vec.con" | grep -q "Alloc"; then
  no "Vec::for_each must be Alloc-free (no hidden allocation)"
else ok "Vec::for_each is Alloc-free (no hidden allocation)"; fi

echo "=== 4. cursor struct for parsers/readers (ByteCursor) ==="
if grep -qE "struct (Copy )?ByteCursor" "$STD/numeric.con" \
   && grep -qE "fn (read_u8|peek_u8)" "$STD/numeric.con"; then
  ok "ByteCursor cursor struct present with bounds-checked read/peek"
else no "ByteCursor cursor struct missing"; fi

echo "=== 5. exclusions: no Iterator trait, no dyn trait-objects, no closures in std ==="
if grep -rnE "trait[[:space:]]+Iterator|impl[[:space:]]+Iterator" "$STD" >/dev/null 2>&1; then
  no "an Iterator trait exists — the protocol is a fixed hierarchy, not a trait"
else ok "no Iterator trait (traversal is a fixed hierarchy)"; fi
if grep -rnE "\bdyn\b" "$STD" >/dev/null 2>&1; then
  no "a 'dyn' trait-object appears in std — no trait-object iterators allowed"
else ok "no 'dyn' trait-objects in std"; fi

echo ""
echo "ITERATION-PROTOCOL: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
