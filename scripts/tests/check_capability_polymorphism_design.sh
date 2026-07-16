#!/usr/bin/env bash
# Callable-values / capability-polymorphism design gate (ROADMAP Phase 5 #24a).
#
# Two invariants until docs/CALLABLE_VALUES_AND_CAPABILITIES.md exists:
#   1. The fn-pointer smuggling hole stays CLOSED: calling through a
#      capability-bearing fn type requires the caller to hold those caps.
#   2. The stdlib's higher-order-function surface is FROZEN at the recorded
#      baseline — no new public fn-pointer-taking API may land before the
#      callable-values design decides bare fn, bound fn, stateful context,
#      mutability/linearity, and with(C), or the combinatorial
#      map/map_file/map_alloc split calcifies.
# Once the design doc lands, invariant 2 is lifted (the doc governs instead);
# invariant 1 is permanent.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== fn-pointer capability smuggling stays rejected ==="
OUT="$("$COMPILER" tests/programs/adversarial_neg_cap_fnptr_smuggle.con 2>&1)"
grep <<<"$OUT" -q "requires capability 'Network' but 'smuggle' does not declare it" \
  && ok "pure caller invoking with(Network) fn pointer is rejected (E0240)" \
  || no "smuggling fixture no longer rejected — the #24a hole reopened"

"$COMPILER" tests/programs/cap_fnptr_declared.con >/dev/null 2>&1 \
  && ok "declared-capability fn-pointer call still compiles" \
  || no "positive fixture broke: declared caps should permit the call"
rm -f tests/programs/cap_fnptr_declared

echo "=== stdlib HOF surface frozen until the design doc lands ==="
if [ -f docs/CALLABLE_VALUES_AND_CAPABILITIES.md ]; then
  ok "callable-values design doc exists — HOF freeze lifted (doc governs new surface)"
else
  CURRENT=$(mktemp)
  for f in std/src/*.con; do
    grep -hoE "pub fn [a-z_]+[^{]*" "$f" | grep "fn(" | sed "s|^|$(basename "$f"): |"
  done | sed 's/ *$//' | sort > "$CURRENT"
  if diff -u scripts/tests/stdlib_hof_baseline.txt "$CURRENT" > /dev/null; then
    ok "no new fn-pointer-taking public stdlib API beyond the baseline"
  else
    no "stdlib HOF surface changed without docs/CALLABLE_VALUES_AND_CAPABILITIES.md:"
    diff -u scripts/tests/stdlib_hof_baseline.txt "$CURRENT" | sed 's/^/       /'
  fi
  rm -f "$CURRENT"
fi

echo ""
echo "CAP-POLY-DESIGN: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
