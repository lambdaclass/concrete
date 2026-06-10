#!/usr/bin/env bash
# Struct mixed-width field-layout regression gate (ROADMAP Phase 4 #44e —
# FIXED 2026-06-10).
#
# The struct-literal store used to pack fields tightly (summing computeTySize)
# while field reads used `Layout.fieldOffset` (aligned). For any struct with a
# sub-word field followed by a wider one — e.g. `{a: u8, b: i64}` — `b` was
# stored at offset 1 but read from offset 8: a silent miscompile. The literal
# store now uses the same aligned `Layout` offsets as reads. Execution oracles
# below pin store/read agreement across mixed widths and nesting.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

run() {
  local name="$1" src="$2" exp="$3"
  printf '%s' "$src" > "$TMP/$name.con"
  if "$COMPILER" "$TMP/$name.con" -o "$TMP/$name" >/dev/null 2>&1; then
    local out rc val; out="$("$TMP/$name" 2>/dev/null)"; rc=$?; val="$out"; [ -z "$val" ] && val="$rc"
    [ "$val" = "$exp" ] && ok "$name = $exp" || no "$name: got '$val' expect $exp (layout mismatch?)"
  else
    no "$name failed to compile"
  fi
}

echo "=== struct-literal store offsets agree with field-read offsets ==="
run u8_then_i64 'struct Copy P { a: u8, b: i64 }
fn main() -> i64 { let m: P = P { a: 5, b: 99 }; return (m.a as i64) * 1000 + m.b; }' 5099
run mixed_three 'struct Copy M { a: u8, b: i64, c: u16 }
fn main() -> i64 { let m: M = M { a: 200, b: 1000, c: 50000 }; return (m.a as i64) + m.b + (m.c as i64); }' 51200
run middle_wide 'struct Copy M { a: u8, b: i64, c: u16 }
fn main() -> i64 { let m: M = M { a: 1, b: 2, c: 3 }; return m.b; }' 2
run trailing_narrow 'struct Copy M { a: u8, b: i64, c: u16 }
fn main() -> i64 { let m: M = M { a: 1, b: 2, c: 50000 }; return m.c as i64; }' 50000
run nested_mixed 'struct Copy I { a: u8, b: i64 } struct Copy O { i: I, t: u16 }
fn main() -> i64 { let mut o: O = O { i: I{a:1,b:10}, t: 5 }; o.i.b = 77; return o.i.b + (o.i.a as i64) + (o.t as i64); }' 83
run all_i64 'struct Copy T { a: i64, b: i64, c: i64 }
fn main() -> i64 { let m: T = T{a:200,b:1000,c:50000}; return m.a+m.b+m.c; }' 51200

echo ""
echo "STRUCT-FIELD-LAYOUT: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
