#!/usr/bin/env bash
# Numeric literal / cast gate (ROADMAP Phase 6 #6).
#
# Grows one section per landed #6 sub-fix. LANDED so far:
#   - OUT-OF-RANGE LITERALS REJECTED: an integer literal that cannot fit its
#     target type is a hard error (E0227), not a silent truncation. This closes a
#     "semantically dark construct" footgun (`let a: u8 = 300` used to run as 44).
#     In-range literals at every width still compile, and explicit `as` casts may
#     still truncate (that is the opt-in lossy path).
#
# Still open (#6): literal suffixes (7u8), lossy/narrowing cast diagnostics,
# runtime checked-overflow profile, signed/unsigned mixed-comparison rule.
#
# Fixtures: tests/programs/numeric/.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
D="tests/programs/numeric"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

reject_with(){ local n="$1" c="$2"
  local out; out="$("$C" "$D/$n.con" -o "$TMP/$n.bin" 2>&1)"
  if echo "$out" | grep -qE 'error\['; then
    echo "$out" | grep -q "($c)" && ok "$n rejected with $c" \
      || { no "$n rejected, wrong code"; echo "$out" | grep -oE '\([A-Z0-9]+\)' | head -1 | sed 's/^/        got: /'; }
  else no "$n: expected rejection ($c), compiled"; fi; }
run_expect(){ local n="$1" e="$2"
  if ! "$C" "$D/$n.con" -o "$TMP/$n.bin" >"$TMP/$n.err" 2>&1; then
    no "$n: expected to compile"; sed 's/^/        /' "$TMP/$n.err" | head -3; return; fi
  local g; g="$("$TMP/$n.bin" 2>/dev/null)"
  [ "$g" = "$e" ] && ok "$n -> $g" || no "$n: got '$g' want '$e'"; }

echo "=== out-of-range integer literals are rejected (not truncated) ==="
reject_with neg_u8_out_of_range E0227
reject_with neg_i32_out_of_range E0227
reject_with neg_negative_into_unsigned E0227   # negative literal into unsigned

echo "=== in-range / valid-negative literals compile; explicit as-cast still truncates ==="
run_expect in_range_literals 1
run_expect valid_negative_literals 1            # i8 = -128 (signed min), -var

echo ""
echo "NUMERIC-LITERALS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
