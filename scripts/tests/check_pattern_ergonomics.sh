#!/usr/bin/env bash
# Pattern-ergonomics gate (ROADMAP Phase 6 #5).
#
# #5 is a compound usability block, built incrementally. This gate grows one
# section per landed sub-feature. LANDED so far:
#
#   - Integer RANGE patterns: `lo..=hi` (inclusive) and `lo..hi` (exclusive) in
#     match arms. Endpoints, exclusion of the high bound, value-position arms,
#     negative bounds, and the exhaustiveness rule (a range is NOT a catch-all —
#     a range-only match still needs a `_` arm) are all locked.
#
# STILL OPEN (each becomes a new section here when it lands): match guards,
# OR patterns, if-let / while-let, struct-update, tuples, match-on-&T.
#
# Fixtures: tests/programs/patterns/. Example: examples/patterns/byte_ranges/.
# See docs/PATTERN_ERGONOMICS.md.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
D="tests/programs/patterns"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

run_expect(){ local n="$1" e="$2"
  if ! "$C" "$D/$n.con" -o "$TMP/$n.bin" >"$TMP/$n.err" 2>&1; then
    no "$n: expected to compile"; sed 's/^/        /' "$TMP/$n.err" | head -3; return; fi
  local g; g="$("$TMP/$n.bin" 2>/dev/null)"
  [ "$g" = "$e" ] && ok "$n -> $g" || no "$n: got '$g' want '$e'"; }

reject_with(){ local n="$1" c="$2"
  local out; out="$("$C" "$D/$n.con" -o "$TMP/$n.bin" 2>&1)"
  if echo "$out" | grep -qE 'error\['; then
    echo "$out" | grep -q "($c)" && ok "$n rejected with $c" \
      || { no "$n rejected, wrong code"; echo "$out" | grep -oE '\([A-Z0-9]+\)' | head -1 | sed 's/^/        got: /'; }
  else no "$n: expected rejection ($c), compiled"; fi; }

echo "=== range patterns: semantics ==="
run_expect range_inclusive 1        # lo..=hi includes both endpoints
run_expect range_exclusive 1        # lo..hi excludes the high endpoint
run_expect range_value_position 7   # range arms in match-as-expression
run_expect range_negative 1         # negative bounds

echo "=== range patterns: exhaustiveness (a range is not a catch-all) ==="
reject_with neg_range_no_default E0534

echo "=== range patterns: end-to-end example builds, runs, exits 0 ==="
ex="examples/patterns/byte_ranges"
if (cd "$ex" && "$C" build >"$TMP/ex.build" 2>&1) && [ -x "$ex/byte_ranges" ]; then
  "$ex/byte_ranges" >/dev/null 2>&1 && ok "byte_ranges example built and ran (exit 0)" \
                                    || no "byte_ranges ran non-zero"
else
  no "byte_ranges example failed to build"; sed 's/^/        /' "$TMP/ex.build" | head -5
fi

echo ""
echo "PATTERN-ERGONOMICS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
