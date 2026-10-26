#!/usr/bin/env bash
# VC-generator differential gate.
#
# The calculus (`Concrete/Report/VCGen.lean`) is shadow-only: nothing consumes it. Its whole value
# is that it can be diffed against the eight hand-written walkers and leaves, which are the oracle
# now that their nine defects are fixed and gated. Without this gate that diff is a report someone
# has to remember to run, and a silent regression in either implementation goes unnoticed until
# the switchover — the worst possible time to discover it.
#
# WHY IT COUNTS VOLUME, NOT JUST AGREEMENT: the first corpus sweep reported agreement across 99
# files while comparing 0 obligations against 0. Both sides found nothing, and "agree" was
# vacuous. Three separate checks passed over an empty set on the day this was written, so a
# differential that cannot distinguish "they match" from "neither ran" is not evidence.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
BIN="./.lake/build/bin/concrete"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

lake build >/dev/null 2>&1 || { echo "FAIL build failed"; exit 1; }
[ -x "$BIN" ] || { echo "FAIL $BIN missing"; exit 1; }

echo "=== calculus vs hand-written walkers, across the corpus ==="
FILES=0; HERE=0; THERE=0; DIS=0; ONLY=0; DISLIST=""
for f in $(grep -rlE "/|<<|%|\[|\+|-|as " --include=*.con examples tests/programs 2>/dev/null); do
  OUT="$("$BIN" "$f" --report vcgen-diff 2>/dev/null || true)"
  # Files that do not PARSE produce no `compared` line. Skipping them is not leniency: the
  # diagnostics corpus contains deliberately-malformed fixtures, and counting "did not print
  # AGREE" as "disagreed" reported 39 false divergences the first time this was measured.
  printf '%s' "$OUT" | grep -q "compared" || continue
  FILES=$((FILES+1))
  H="$(printf '%s' "$OUT" | grep -oE 'compared [0-9]+' | grep -oE '[0-9]+')"
  T="$(printf '%s' "$OUT" | grep -oE 'against [0-9]+' | grep -oE '[0-9]+')"
  O="$(printf '%s' "$OUT" | grep -oE 'CALCULUS-ONLY \([0-9]+' | grep -oE '[0-9]+')"
  HERE=$((HERE+H)); THERE=$((THERE+T)); ONLY=$((ONLY+${O:-0}))
  printf '%s' "$OUT" | grep -q "AGREE" || { DIS=$((DIS+1)); DISLIST="$DISLIST $f"; }
done

[ "$DIS" -eq 0 ] \
  && ok "no disagreement across $FILES files" \
  || no "$DIS file(s) disagree:$DISLIST  (run --report vcgen-diff on one to see which side)"

[ "$HERE" = "$THERE" ] \
  && ok "obligation totals match exactly ($HERE)" \
  || no "totals differ: calculus $HERE vs walkers $THERE"

# NON-VACUITY. The two assertions above are satisfied by both sides finding nothing.
[ "$FILES" -ge 50 ] \
  && ok "compared $FILES files (not a vacuous corpus)" \
  || no "only $FILES files compared — the sweep is not exercising the corpus"
[ "$HERE" -ge 5000 ] \
  && ok "compared $HERE obligations (agreement is over real volume)" \
  || no "only $HERE obligations compared — 'agree' would mean little at this size"

# The calculus should be strictly AHEAD on kinds no walker can express. If this reaches zero,
# either the new rules were removed or the corpus stopped exercising them.
[ "$ONLY" -ge 100 ] \
  && ok "$ONLY sites use obligation kinds no walker can produce (neg-MIN, float→int cast)" \
  || no "only $ONLY calculus-only sites — the two new rule-table rows may have been lost"

echo ""
echo "VCGEN-DIFF: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
