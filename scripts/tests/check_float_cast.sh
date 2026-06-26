#!/usr/bin/env bash
# Checked float→int cast gate — ROADMAP #10 (closes former KNOWN_HOLES H2).
#
# `f as iN` / `f as uN` is a CHECKED conversion, profile-invariant:
#   - NaN, ±inf, or a value outside the target integer range → ABORT;
#   - an in-range value truncates toward zero.
# This matches the integer arithmetic decision (ordinary-looking operations
# never silently poison/wrap/saturate). Saturating or wrapping float→int, if
# ever needed, must be an explicitly named helper — never `as`.
#
# LIMITATION (documented, not silent): the interpreter does not support float
# literals yet, so this gate is COMPILED-ONLY — there is no interp==compiled
# oracle as there is for integer arithmetic. When float interp lands, upgrade
# the trap cases to interp==compiled agreement like check_arith_redteam.sh.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
write(){ printf '%s\n' "$2" > "$TMP/$1.con"; }

# compiled-only value check: must compile, run to success, print $want.
val(){ local n="$1" want="$2"
  if ! "$C" "$TMP/$n.con" -o "$TMP/$n.bin" >"$TMP/$n.e" 2>&1; then
    no "$n: expected to compile"; sed 's/^/        /' "$TMP/$n.e" | head -3; return; fi
  local out; out="$("$TMP/$n.bin" 2>/dev/null)"; local rc=$?
  if [ "$rc" -eq 0 ] && [ "$out" = "$want" ]; then ok "$n: $want"
  else no "$n: got out='$out' rc=$rc want '$want'"; fi; }

# compiled-only trap check: must compile, then ABORT (nonzero, no value).
trap_c(){ local n="$1"
  if ! "$C" "$TMP/$n.con" -o "$TMP/$n.bin" >"$TMP/$n.e" 2>&1; then
    no "$n: expected to compile then trap"; sed 's/^/        /' "$TMP/$n.e" | head -3; return; fi
  local out; out="$("$TMP/$n.bin" 2>/dev/null)"; local rc=$?
  if [ -z "$out" ] && [ "$rc" -ne 0 ]; then ok "$n: traps (checked cast, exit $rc)"
  else no "$n: expected a trap, got out='$out' rc=$rc"; fi; }

echo "=== in-range float→int truncates toward zero ==="
write inrange 'fn main() -> i64 { let f: f64 = 100.5; return (f as i32) as i64; }'
write negtrunc 'fn main() -> i64 { let f: f64 = -7.9; return (f as i32) as i64; }'
write atmin 'fn main() -> i64 { let f: f64 = -2147483648.0; return (f as i32) as i64; }'
write atmax 'fn main() -> i64 { let f: f64 = 2147483647.0; return (f as i32) as i64; }'
write u8ok 'fn main() -> i64 { let f: f64 = 200.0; return (f as u8) as i64; }'
val inrange 100
val negtrunc -7
val atmin -2147483648
val atmax 2147483647
val u8ok 200

echo "=== out-of-range / NaN / ±inf ABORT ==="
write over   'fn main() -> i64 { let f: f64 = 9999999999.0; return (f as i32) as i64; }'
write under  'fn main() -> i64 { let f: f64 = -9999999999.0; return (f as i32) as i64; }'
write u8over 'fn main() -> i64 { let f: f64 = 300.0; return (f as u8) as i64; }'
write u8neg  'fn main() -> i64 { let f: f64 = -1.0; return (f as u8) as i64; }'
write nan    'fn main() -> i64 { let z: f64 = 0.0; let f: f64 = z / z; return (f as i32) as i64; }'
write inf    'fn main() -> i64 { let z: f64 = 0.0; let o: f64 = 1.0; let f: f64 = o / z; return (f as i32) as i64; }'
for t in over under u8over u8neg nan inf; do trap_c "$t"; done

echo "=== the cast no longer lowers to a raw fptosi (it calls the checked helper) ==="
if "$C" "$TMP/over.con" --emit-llvm 2>/dev/null | grep -qE "@__cc_f64_to_i32"; then
  ok "lowering calls @__cc_f64_to_i32 (checked helper)"
else
  no "lowering is not the checked helper — H2 may have regressed to raw fptosi"
fi

echo ""
echo "FLOAT-CAST: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
