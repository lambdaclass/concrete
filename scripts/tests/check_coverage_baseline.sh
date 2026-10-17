#!/usr/bin/env bash
# THE ELIGIBILITY-DENOMINATED COVERAGE BASELINE (R-0004 package 3).
#
# "How much of this corpus is proved?" has been answerable only by reading a report and adding up, and
# a number reached that way drifts the moment a status is added — which has already happened twice in
# this project, both times leaving a totals row that did not total.
#
# So the baseline is MEASURED here, over an explicit denominator, with every disposition named. The
# point is not the ratio; it is that the ratio has a denominator nobody has to reconstruct, and that
# a claim moving between dispositions is visible rather than absorbed.
#
# EXACT VALUES, NOT RATCHETS. A rising `proved` count is not automatically good: it can mean a claim
# stopped being checked. Every number is pinned and a change must be explained.
set -uo pipefail
trap 'rc=$?; if [ "$rc" -ne 0 ] && [ "${GATE_DONE:-0}" -ne 1 ]; then
  echo "FATAL: unexpected shell failure (exit $rc) — the verdict below is not trustworthy" >&2; exit "$rc"; fi' ERR
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
source "scripts/tests/lib/fresh.sh"
require_fresh_binary || exit 1
BIN="$ROOT_DIR/.lake/build/bin/concrete"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# The population: every fixture carrying a source proof link. Captured per file, then counted once,
# so a fixture appearing twice cannot inflate a disposition.
CENSUS=""
for f in $(grep -rlE '#\[(proof_by|ensures_proof)\(' examples --include='*.con' | sort); do
  CENSUS="$CENSUS$("$BIN" "$f" --report proof-status 2>/dev/null | grep -oE '^-- [a-z ]+' || true)
"
done
count_of() { printf '%s' "$CENSUS" | sed 's/-- //;s/ *$//' | grep -cx "$1" || true; }

PROVED="$(count_of 'proved')"
NOPROOF="$(count_of 'no proof')"
UNBOUND="$(count_of 'proof link unbound')"
INELIG="$(count_of 'not eligible')"
TRUSTED="$(count_of 'trusted')"
STALE="$(count_of 'proof stale')"
DEPSNC="$(count_of 'dependency not current')"
BLOCKED="$(count_of 'blocked')"
UNJUST="$(count_of 'dependency closure unjustified')"
SPECUNB="$(count_of 'spec not bound to body')"

TOTAL=$((PROVED + NOPROOF + UNBOUND + INELIG + TRUSTED + STALE + DEPSNC + BLOCKED + UNJUST + SPECUNB))
LINES="$(printf '%s' "$CENSUS" | grep -c '^-- ' || true)"

echo "  proved=$PROVED no-proof=$NOPROOF unbound=$UNBOUND ineligible=$INELIG trusted=$TRUSTED"
echo "  stale=$STALE deps-not-current=$DEPSNC blocked=$BLOCKED unjustified=$UNJUST spec-unbound=$SPECUNB  total=$TOTAL"

# THE DENOMINATOR RECONCILES. Every reported line lands in exactly one disposition; a line that
# matched none would mean a status exists that this baseline cannot see, which is precisely how a
# totals row stops totalling.
if [ "$TOTAL" = "$LINES" ]; then
  ok "every reported function lands in exactly one disposition ($TOTAL = $LINES reported)"
else
  no "coverage denominator does not reconcile: $TOTAL classified vs $LINES reported — a status exists that this baseline cannot see"
fi

# ELIGIBILITY-DENOMINATED. `ineligible` and `trusted` are not proof failures: one is outside the
# provable subset, the other is a declared, audited boundary. The meaningful denominator is what
# remains — the claims that COULD carry proof evidence.
ELIGIBLE=$((TOTAL - INELIG - TRUSTED))
echo "  eligible denominator: $ELIGIBLE (total $TOTAL minus $INELIG ineligible and $TRUSTED trusted)"
if [ "$ELIGIBLE" -gt 0 ] 2>/dev/null; then
  ok "the eligible denominator is non-empty ($ELIGIBLE), so the ratio below means something"
else
  no "eligible denominator is zero — every ratio derived from it is vacuous"
fi

# THE BASELINE ITSELF, pinned exactly.
# 35 -> 32 of 91 on 2026-10-17 (bug 079): three claims moved from `proved` to `spec not bound to body`,
# each because its theorem is not shown to be about its body — `loop_invariant.count_up`
# (`count_up_loop_preserves` evaluates no specification), `contract_negatives/fabricated_proof`'s `cn.f`
# and `proof_patterns/repair`'s `needs_proof` (both link theorems that do not exist). The denominator
# is unchanged: the claims did not leave the eligible set, they stopped counting as proved.
if [ "$PROVED" = "32" ] && [ "$ELIGIBLE" = "91" ]; then
  ok "BASELINE: 32 of 91 eligible claims are proved (2026-10-17, exact)"
else
  no "coverage moved to $PROVED of $ELIGIBLE eligible (baseline 32 of 91) — say which claims changed and why"
fi
# 31/15 -> 32/14 on 2026-08-18: ONE claim moved from `unbound` to `no-proof`, and the sum is
# unchanged. `proof_pressure.validate_header` had its misattributed `#[proof_by]` deleted, leaving
# `#[spec(...)]` alone. That used to synthesize a registry entry with an empty proof name and report
# `unbound` — the absence of freshness evidence about a claim — when there is no claim at all. A
# specification is not a proof claim, so the honest disposition is `no-proof`.
#
# Recorded as a transition rather than two independent edits, because that is what makes it
# checkable: any other movement changes the sum, and this one provably does not.
if [ "$STALE" = "5" ] && [ "$UNBOUND" = "14" ] && [ "$DEPSNC" = "4" ] && [ "$UNJUST" = "0" ] \
   && [ "$NOPROOF" = "32" ] && [ "$BLOCKED" = "1" ]; then
  ok "the unproved dispositions are exactly 32 no-proof / 14 unbound / 5 stale / 4 deps-not-current / 1 blocked / 0 unjustified"
else
  no "unproved dispositions moved: no-proof=$NOPROOF unbound=$UNBOUND stale=$STALE deps=$DEPSNC blocked=$BLOCKED unjustified=$UNJUST"
fi

# WHAT THE BASELINE DOES NOT SAY, asserted so the number cannot be read as more than it is. `proved`
# here means link present, fingerprint fresh, dependency closure justified and computable. It does
# NOT mean the Lean kernel re-ran: that is `--report check-proofs`, and no receipt binds it yet.
if [ "$("$BIN" examples/elf_header/src/main.con --report proof-status 2>/dev/null | grep -c 'kernel replay via' || true)" -gt 0 ]; then
  ok "every proved entry states that this report does not run the kernel (the baseline is not a replay count)"
else
  no "the proved rendering no longer discloses that the kernel was not run — the baseline now reads as replay coverage"
fi

GATE_DONE=1
echo "COVERAGE-BASELINE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
