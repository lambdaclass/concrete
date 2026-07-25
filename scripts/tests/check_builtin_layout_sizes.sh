#!/usr/bin/env bash
# Hardcoded builtin layout constants must match the std declarations they describe.
#
# `Concrete/Check/Layout.lean` carries a size for each builtin generic whose
# layout the compiler does NOT derive from the declaration (they are excluded
# from struct monomorphization). Field OFFSETS still come from std's struct, but
# aggregate copies are sized from these constants — so an undercount truncates a
# by-value copy and everything past the cut reads garbage.
#
# That is not hypothetical. `hashmapSize` was 40 ("ptr + ptr + ptr + i64 + i64"),
# silently omitting HashMap's two fn-pointer fields, while the real struct was
# 56 bytes. Nothing failed only because no by-value path read past offset 40.
# Adding a `tombstones` field moved `cap` to offset 40, `HashMap::drop` (which
# takes `self` by value) then read a `cap` that had never been copied, its loop
# ran zero times, and every remaining key and value was leaked with no
# diagnostic — a silent H18 violation from a constant nobody had reason to
# re-read.
#
# So: derive the size from std and compare. A field added to one of these
# structs must fail here rather than in whatever by-value path first reads past
# the truncation.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# Every field of these structs is pointer-or-64-bit, so size = 8 * field count.
# The check asserts that too: a narrower field would break the assumption and is
# reported rather than silently mis-sized.
# struct_field_bytes <file> <StructName>
struct_field_bytes() {
  local file="$1" name="$2"
  awk -v want="$name" '
    $0 ~ ("pub struct " want "<") || $0 ~ ("pub struct " want " ") || $0 ~ ("pub struct " want "\\{") { inside=1; next }
    inside && /^[[:space:]]*\}/ { exit }
    inside {
      line=$0
      sub(/\/\/.*/, "", line)                       # strip comments
      if (line ~ /^[[:space:]]*$/) next
      if (line !~ /:/) next
      n++
      # flag any field that is not pointer-sized/64-bit
      if (line ~ /:[[:space:]]*(u8|i8|u16|i16|u32|i32|bool)[[:space:]]*,/) narrow++
    }
    END { if (narrow > 0) print "NARROW"; else print n * 8 }
  ' "$file"
}

lean_const() {
  grep -oE "def $1 : Nat := [0-9]+" Concrete/Check/Layout.lean | grep -oE '[0-9]+$'
}

echo "=== hardcoded builtin sizes match their std declarations ==="

check_pair() {
  local label="$1" file="$2" struct="$3" constname="$4"
  local derived declared
  derived="$(struct_field_bytes "$file" "$struct")"
  declared="$(lean_const "$constname")"
  if [ -z "$declared" ]; then
    no "$label — could not read Builtin.$constname from Layout.lean (renamed?)"; return
  fi
  if [ "$derived" = "NARROW" ]; then
    no "$label — $struct has a sub-64-bit field; the 8-bytes-per-field derivation no longer holds, so this gate cannot verify Builtin.$constname"
    return
  fi
  if [ -z "$derived" ] || [ "$derived" = "0" ]; then
    no "$label — found no fields for '$struct' in $file (declaration moved or renamed?)"; return
  fi
  if [ "$derived" = "$declared" ]; then
    ok "$label: $struct is $derived bytes, Builtin.$constname = $declared"
  else
    no "$label: $struct is $derived bytes in $file but Builtin.$constname = $declared — a by-value copy of $struct will be truncated to $declared bytes while field offsets use the real layout"
  fi
}

check_pair "HashMap" "std/src/map.con" "HashMap" "hashmapSize"
check_pair "Vec"     "std/src/vec.con" "Vec"     "vecSize"

echo
echo "BUILTIN-LAYOUT-SIZES: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
