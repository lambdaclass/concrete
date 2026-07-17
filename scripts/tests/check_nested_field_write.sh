#!/usr/bin/env bash
# Nested place-write regression gate (ROADMAP Phase 4 #44c — FIXED 2026-06-10).
#
# `o.inner.v = x` and friends (a place expression deeper than one level) used to
# be silently dropped: Lower handled only single-level assignment targets and a
# compound base was lowered as a value copy whose mutation was discarded. The
# unified `storeToPlace` path now writes compound places in place by value
# writeback. This gate runs execution oracles over the whole family.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT_DIR/scripts/tests/lib/selfprint.sh"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

# run <name> <src> <expected>
run() {
  local name="$1" src="$2" exp="$3"
  printf '%s' "$src" > "$TMP/$name.con"
  gate_selfprint_wrap "$TMP/$name.con" "$TMP/$name.w.con"
  if "$COMPILER" "$TMP/$name.w.con" -o "$TMP/$name" >/dev/null 2>&1; then
    local out rc val; out="$("$TMP/$name" 2>/dev/null)"; rc=$?; val="$out"; [ -z "$val" ] && val="$rc"
    [ "$val" = "$exp" ] && ok "$name = $exp" || no "$name: got '$val' expect $exp (write dropped?)"
  else
    no "$name failed to compile"
  fi
}

echo "=== nested place writes take effect (execution oracles) ==="
# canonical fixture
gate_selfprint_wrap examples/known_holes/nested_field_write/src/main.con "$TMP/canon.w.con"
if "$COMPILER" "$TMP/canon.w.con" -o "$TMP/canon" >/dev/null 2>&1; then
  V="$("$TMP/canon" 2>/dev/null)"
  [ "$V" = "7709" ] && ok "nested field write fixture = 7709" || no "fixture: got '$V' expect 7709"
else no "canonical fixture failed to compile"; fi

run nested_field 'struct Copy I { v: i64 } struct Copy O { i: I, t: i64 }
fn main() -> i64 { let mut o: O = O { i: I{v:1}, t: 9 }; o.i.v = 77; return o.i.v*100 + o.t; }' 7709

run array_elem_field 'struct Copy P { x: i64, y: i64 }
fn main() -> i64 { let mut a: [P;2] = [P{x:1,y:2}, P{x:3,y:4}]; a[0].x = 50; return a[0].x + a[1].x; }' 53

run struct_array_elem 'struct Copy B { d: [i64;3], n: i64 }
fn main() -> i64 { let mut b: B = B { d: [1,2,3], n: 3 }; b.d[1] = 99; return b.d[1] + b.n; }' 102

run triple_nest 'struct Copy A { v: i64 } struct Copy B { a: A } struct Copy C { b: B }
fn main() -> i64 { let mut c: C = C { b: B { a: A { v: 1 } } }; c.b.a.v = 88; return c.b.a.v; }' 88

run two_d_array 'fn main() -> i64 { let mut m: [[i64;2];2] = [[1,2],[3,4]]; m[1][0] = 99; return m[1][0]; }' 99

run nested_via_mut_ref 'struct Copy I { v: i64 } struct Copy O { i: I }
fn setv(p: &mut O) -> i64 { p.i.v = 42; return 0; }
fn main() -> i64 { let mut o: O = O { i: I{v:1} }; borrow mut o as r in R { setv(r); } return o.i.v; }' 42

echo "=== single-level writes still work (no regression) ==="
run single_field 'struct Copy P { x: i64 }
fn main() -> i64 { let mut p: P = P{x:1}; p.x = 5; p.x = p.x + 10; return p.x; }' 15
run single_array 'fn main() -> i64 { let mut a: [i64;3] = [1,2,3]; a[2] = a[0]+a[1]; return a[2]; }' 3

echo ""
echo "NESTED-FIELD-WRITE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
