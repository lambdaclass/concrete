#!/usr/bin/env bash
# Array-bounds gate — KNOWN_HOLES H8 (runtime bounds checking).
#
# Raw `a[i]` / `a[i] = v` on a fixed array is now CHECKED: a dynamic index that is
# out of bounds traps (abort) at runtime, matching the interpreter, instead of
# silently reading/writing out-of-bounds memory. A single unsigned compare catches
# both negative and `>= len`. This gate locks:
#   - an in-bounds dynamic access still works and interp == compiled;
#   - an OOB READ aborts (compiled abort + interp error);
#   - an OOB WRITE aborts (the dangerous case — silent memory corruption before);
#   - a NEGATIVE index aborts (the single-unsigned-compare trick);
#   - a NESTED `m[i][j]` OOB aborts (the write/nested-place path is covered);
#   - `&mut a[i]` OOB aborts (the borrow/place path is covered).
# Indices are made dynamic via a loop so they are not proven-constant OOB (which is
# a separate hard COMPILE error, C7) — this gate is specifically the runtime path.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT_DIR/scripts/tests/lib/selfprint.sh"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# aborts <label> <source> : compiled must abort (nonzero) AND interp must error.
aborts(){ local n="$1"; printf '%s' "$2" > "$TMP/$n.con"
  if ! "$C" "$TMP/$n.con" -o "$TMP/$n.bin" >"$TMP/$n.err" 2>&1; then
    no "$n: expected to COMPILE (then trap at runtime)"; sed 's/^/        /' "$TMP/$n.err" | head -3; return; fi
  "$TMP/$n.bin" >/dev/null 2>&1; local ce=$?
  local ie; ie="$("$C" "$TMP/$n.con" --interp 2>&1 || true)"
  if [ "$ce" -ne 0 ] && grep <<<"$ie" -qi "bound\|index"; then
    ok "$n: OOB traps (compiled exit $ce, interp errors)"
  else no "$n: did NOT trap (compiled exit $ce, interp='$(echo "$ie" | head -1)')"; fi; }

# works <label> <source> <expected> : compiles, runs to <expected>, interp agrees.
works(){ local n="$1" e="$3"; printf '%s' "$2" > "$TMP/$n.con"
  gate_selfprint_wrap "$TMP/$n.con" "$TMP/$n.w.con"
  if ! "$C" "$TMP/$n.w.con" -o "$TMP/$n.bin" >"$TMP/$n.err" 2>&1; then
    no "$n: expected to compile"; sed 's/^/        /' "$TMP/$n.err" | head -3; return; fi
  local co io; co="$("$TMP/$n.bin" 2>/dev/null)"; io="$("$C" "$TMP/$n.con" --interp 2>/dev/null | tail -1)"
  if [ "$co" = "$e" ] && [ "$io" = "$e" ]; then ok "$n: in-bounds works (= $e, interp agrees)"
  else no "$n: got compiled='$co' interp='$io' want '$e'"; fi; }

echo "=== in-bounds dynamic access still works ==="
works inbounds 'mod m { fn main() -> Int { let a: [i32; 4] = [10,20,30,40]; let mut i: i32 = 0; while i < 2 { i = i + 1; } return a[i as Int] as Int; } }' 30

echo "=== out-of-bounds accesses trap (compiled + interp) ==="
aborts oob_read   'mod m { fn main() -> Int { let a: [i32; 4] = [10,20,30,40]; let mut i: i32 = 0; while i < 10 { i = i + 1; } return a[i as Int] as Int; } }'
aborts oob_write  'mod m { fn main() -> Int { let mut a: [i32; 4] = [10,20,30,40]; let mut i: i32 = 0; while i < 7 { i = i + 1; } a[i as Int] = 999; return a[0] as Int; } }'
aborts oob_neg    'mod m { fn main() -> Int { let a: [i32; 4] = [10,20,30,40]; let mut i: i32 = 0; while i > -3 { i = i - 1; } return a[i as Int] as Int; } }'
aborts oob_nested 'mod m { fn main() -> Int { let mut g: [[i32; 2]; 2] = [[1,2],[3,4]]; let mut j: i32 = 0; while j < 5 { j = j + 1; } g[1][j as Int] = 9; return g[0][0] as Int; } }'
aborts oob_mutref 'mod m { fn addv(r: &mut i32, d: i32) { *r = *r + d; } fn main() -> Int { let mut a: [i32; 4] = [1,2,3,4]; let mut i: i32 = 0; while i < 9 { i = i + 1; } addv(&mut a[i as Int], 5); return a[0] as Int; } }'

echo ""
echo "ARRAY-BOUNDS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
