#!/usr/bin/env bash
# Enum union layout gate (audit 2026-07-16).
#
# The canonical builtin Option/Result union is shared by ALL instantiations of
# the program, so it must cover the worst ALIGNMENT-AWARE footprint
# (`end = alignUp 4 align + size`), not merely the worst tySize; and every
# alloca of an enum whose payloads need 8-byte alignment must carry an
# explicit `align 8` (the `{ i32, [N x i8] }` union declaration only carries
# 4-byte alignment, while payload loads/stores assume natural alignment).
# The byte-array storage itself is deliberate: aggregate load/store of a
# partially-initialized union is poison-safe only at byte granularity.
#
# Locks the class: footprint coverage, alloca alignment, and no align bloat
# for align-1-only programs.

set -uo pipefail
# knob-proof: fixtures here assert exit codes/stdout without the legacy echo
unset CONCRETE_ECHO_RESULT
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
LLI="$(command -v lli || true)"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

FIX="$ROOT_DIR/tests/programs/regress_enum_canonical_align.con"
"$C" "$FIX" --emit-llvm > "$TMP/fix.ll" 2>"$TMP/fix.err" || { no "fixture compiles ($(head -1 "$TMP/fix.err"))"; echo "ENUM-UNION-LAYOUT: PASS=$PASS FAIL=$FAIL"; exit 1; }

# 1. Footprint coverage: worst end is Option<[i64;3]> at 8+24=32 → [28 x i8].
if grep -q '%enum.Option = type { i32, \[28 x i8\] }' "$TMP/fix.ll"; then
  ok "union covers worst footprint ({ i32, [28 x i8] })"
else
  no "union footprint (want { i32, [28 x i8] }, got: $(grep '%enum.Option = type' "$TMP/fix.ll"))"
fi

# 2. Every %enum.Option alloca carries align 8 (payloads need it here).
total=$(grep -c 'alloca %enum.Option' "$TMP/fix.ll" || true)
aligned=$(grep -c 'alloca %enum.Option, align 8' "$TMP/fix.ll" || true)
if [ "$total" -gt 0 ] && [ "$total" = "$aligned" ]; then
  ok "all $total enum allocas carry align 8"
else
  no "alloca alignment ($aligned of $total enum allocas have align 8)"
fi

# 3. Behavioral: reads back the high-alignment payload correctly.
if [ -n "$LLI" ]; then
  out="$("$LLI" "$TMP/fix.ll")"
  if [ "$out" = "42007" ]; then ok "fixture runs: $out"; else no "fixture runs (want 42007, got $out)"; fi
else
  echo "  skip lli not found (behavioral check)"
fi

# 4. Align-1-only program: no align attribute, byte-exact old shape.
cat > "$TMP/lo.con" <<'EOF'
enum Option<T> {
    Some { value: T },
    None
}

fn main() with(Console) -> Int {
    let a: [u8; 24] = [7; 24];
    let arr: Option<[u8; 24]> = Option::Some { value: a };
    match arr {
        Option::Some { value } => { print_int(value[0] as Int); },
        Option::None {} => { print_int(-2); },
    }
    return 0;
}
EOF
"$C" "$TMP/lo.con" --emit-llvm > "$TMP/lo.ll" 2>"$TMP/lo.err" || { no "align-1 fixture compiles"; echo "ENUM-UNION-LAYOUT: PASS=$PASS FAIL=$FAIL"; exit 1; }
if grep -q '%enum.Option = type { i32, \[24 x i8\] }' "$TMP/lo.ll" \
   && ! grep -q 'alloca %enum.Option, align' "$TMP/lo.ll"; then
  ok "align-1-only: byte-exact union, no align attribute"
else
  no "align-1-only shape ($(grep '%enum.Option = type' "$TMP/lo.ll"); $(grep 'alloca %enum.Option' "$TMP/lo.ll" | head -1))"
fi

echo "ENUM-UNION-LAYOUT: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
