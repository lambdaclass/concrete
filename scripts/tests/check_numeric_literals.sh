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
  if grep <<<"$out" -qE 'error\['; then
    grep <<<"$out" -q "($c)" && ok "$n rejected with $c" \
      || { no "$n rejected, wrong code"; grep <<<"$out" -oE '\([A-Z0-9]+\)' | head -1 | sed 's/^/        got: /'; }
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

echo "=== numeric model invariants (docs/NUMERIC_MODEL.md) ==="
# inline probes: reject <label> <src>  /  accept <label> <src>
inl_reject(){ printf '%s' "$2" > "$TMP/m.con"
  "$C" "$TMP/m.con" -o "$TMP/m.bin" >"$TMP/m.out" 2>&1 \
    && no "$1: expected rejection, compiled" \
    || ok "$1: rejected ($(grep -oE '\([A-Z0-9]+\)' "$TMP/m.out" | head -1))"; }
inl_run(){ printf '%s' "$2" > "$TMP/m.con"
  if "$C" "$TMP/m.con" -o "$TMP/m.bin" >"$TMP/m.out" 2>&1; then
    local g; g="$("$TMP/m.bin" 2>/dev/null)"
    [ "$g" = "$3" ] && ok "$1 -> $g" || no "$1: got '$g' want '$3'"
  else no "$1: expected to compile"; sed 's/^/        /' "$TMP/m.out" | head -2; fi; }

# No implicit numeric conversion — even widening needs `as`.
inl_reject "implicit widen u8->u64" 'mod m { fn want(x: u64) -> u64 { return x; } fn main() -> Int { let a: u8 = 5; return want(a) as Int; } }'
# Comparison signedness is type-driven: (200:u8) < (100:u8) is false.
inl_run "u8 comparison is unsigned" 'mod m { fn main() -> Int { let a: u8 = 200; let b: u8 = 100; if a < b { return 1; } return 0; } }' 0
# Explicit `as` truncation is allowed (the opt-in lossy path).
inl_run "explicit as truncation" 'mod m { fn main() -> Int { let a: i64 = 300; let b: u8 = a as u8; return b as Int; } }' 44

echo ""
echo "NUMERIC-LITERALS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
