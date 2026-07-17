#!/usr/bin/env bash
# Pattern-ergonomics gate (ROADMAP Phase 6 #5).
#
# #5 is closed for V1. This gate locks the built features: integer ranges,
# match guards, OR patterns, match-on-&T, struct update (via its own gate),
# `_` wildcard destructuring, and `if let` / `while let`. Tuples and nested
# patterns are explicit workload-gated decisions with separate gates/docs.
#
# Fixtures: tests/programs/patterns/. Example: examples/patterns/byte_ranges/.
# See docs/PATTERN_ERGONOMICS.md.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT_DIR/scripts/tests/lib/selfprint.sh"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
D="tests/programs/patterns"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

run_expect(){ local n="$1" e="$2"
  gate_selfprint_wrap "$D/$n.con" "$TMP/$n.w.con"
  if ! "$C" "$TMP/$n.w.con" -o "$TMP/$n.bin" >"$TMP/$n.err" 2>&1; then
    no "$n: expected to compile"; sed 's/^/        /' "$TMP/$n.err" | head -3; return; fi
  local g; g="$("$TMP/$n.bin" 2>/dev/null)"
  [ "$g" = "$e" ] && ok "$n -> $g" || no "$n: got '$g' want '$e'"; }

reject_with(){ local n="$1" c="$2"
  local out; out="$("$C" "$D/$n.con" -o "$TMP/$n.bin" 2>&1)"
  if grep <<<"$out" -qE 'error\['; then
    grep <<<"$out" -q "($c)" && ok "$n rejected with $c" \
      || { no "$n rejected, wrong code"; grep <<<"$out" -oE '\([A-Z0-9]+\)' | head -1 | sed 's/^/        got: /'; }
  else no "$n: expected rejection ($c), compiled"; fi; }

echo "=== range patterns: semantics ==="
run_expect range_inclusive 1        # lo..=hi includes both endpoints
run_expect range_exclusive 1        # lo..hi excludes the high endpoint
run_expect range_value_position 7   # range arms in match-as-expression
run_expect range_negative 1         # negative bounds

echo "=== range patterns: exhaustiveness (a range is not a catch-all) ==="
reject_with neg_range_no_default E0534

echo "=== OR patterns (A | B) ==="
run_expect or_literals 1   # 1 | 2 | 3
run_expect or_ranges 1     # ranges + mixed lit|range
run_expect or_enum 1       # E::A | E::B

echo "=== match guards (pattern if cond) ==="
run_expect guard_var_fallthrough 1   # failing var guard falls to next arm
run_expect guard_enum 1              # guarded enum arm falls to later Some
reject_with neg_guard_only_nonexhaustive E0534  # guarded-only match is non-exhaustive

echo "=== _ wildcard in destructuring bindings ==="
run_expect wildcard_destructure 1          # positional bind, others unaffected
reject_with neg_wildcard_read E0100        # `_` is not a readable variable

echo "=== match on &T (reference scrutinee) ==="
run_expect match_ref_scalar 1   # &i32: literal/range/var all see the value
run_expect match_ref_enum 1     # &enum: tag + payload through the reference

echo "=== if let / while let (desugar to match; Copy scrutinee only) ==="
run_expect if_let_some_else 7    # binds on Some, else on None (Copy enum)
run_expect if_let_no_else 5     # no-else leaves state unchanged on no-match (Copy enum)
run_expect while_let_drain 6     # loops while matching; re-evaluates scrutinee (Copy enum)
# By design (same rule as let-else): the desugared catch-all `_` arm may discard
# only Copy data, so if-let/while-let over a NON-Copy enum (e.g. Option<i32>) is
# rejected — write an explicit exhaustive match instead.
reject_with neg_if_let_noncopy E0288

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
