#!/usr/bin/env bash
# Raw-pointer / address-of-local aliasing regression gate
# (ROADMAP Phase 4 #44d — FIXED 2026-06-11).
#
# Locals used to be lowered as SSA register values, so `&mut x as *mut i64`
# (and `&mut x`) materialized a pointer to a COPY — a store through it did not
# reach `x`. Now address-of a local promotes it to a stable stack alloca, so
# the pointer aliases the variable; reads/writes of the local route through the
# alloca. This gate asserts the aliasing now holds.

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
    [ "$val" = "$exp" ] && ok "$name = $exp" || no "$name: got '$val' expect $exp (pointer not aliasing local?)"
  else
    no "$name failed to compile"
  fi
}

echo "=== address-of a local aliases its storage ==="
# canonical fixture (raw *mut through trusted)
if "$COMPILER" examples/known_holes/raw_ptr_to_local/src/main.con -o "$TMP/canon" >/dev/null 2>&1; then
  V="$("$TMP/canon" 2>/dev/null)"; [ -z "$V" ] && V=$?
  [ "$V" = "99" ] && ok "raw-ptr-to-local fixture = 99 (aliases)" || no "fixture: got '$V' expect 99"
else no "canonical fixture failed to compile"; fi

run raw_ptr_store 'trusted fn store(p: *mut i64, v: i64) -> i64 { *p = v; return 0; }
fn main() -> i64 { let mut x: i64 = 1; store(&mut x as *mut i64, 99); return x; }' 99
run mutref_via_fn 'fn setit(p: &mut i64) -> i64 { *p = 42; return 0; }
fn main() -> i64 { let mut x: i64 = 1; setit(&mut x); return x; }' 42
run repeated_mutate 'trusted fn bump(p: *mut i64) -> i64 { *p = *p + 10; return 0; }
fn main() -> i64 { let mut x: i64 = 5; bump(&mut x as *mut i64); bump(&mut x as *mut i64); return x; }' 25
run writes_around_addrof 'fn setit(p: &mut i64) -> i64 { *p = 42; return 0; }
fn main() -> i64 { let mut x: i64 = 1; x = x + 100; setit(&mut x); x = x + 1; return x; }' 43

echo "=== deref store/load consistent (kept from the original isolation) ==="
run deref_consistent 'trusted fn rw(p: *mut i64, v: i64) -> i64 { *p = v; return *p; }
fn main() -> i64 { let mut x: i64 = 1; return rw(&mut x as *mut i64, 99); }' 99

echo ""
echo "RAW-PTR-TO-LOCAL: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
