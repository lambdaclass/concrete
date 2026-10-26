#!/usr/bin/env bash
# R-0004 slice 6 — the implementation manifest, MEASURED ON THE REAL CORPUS.
#
# WHY THIS GATE EXISTS. `implementationManifestResultOf` had no consumers, so its completeness
# conditions were pinned only by SYNTHETIC results in check_dependency_edges.sh. Synthetic probes
# prove the function refuses what it is handed; they cannot tell you whether real production is
# complete. And the producer this replaced could not have told us either: `filterMap` returned the
# subset it managed to build, which is byte-indistinguishable from a complete manifest.
#
# THE FIRST MEASUREMENT FOUND A REAL INCOMPLETENESS (2026-08-13): across the corpus, 64 identities
# are expected and 63 produce rows. One entry in examples/proof_pressure/src/main.con has no
# extracted proof-model body, so it is refused as `extracted-missing` and that file's manifest is
# correctly NOT usable. Under the old producer that file yielded a 4-row manifest reporting nothing
# wrong. The incompleteness was always there; only its visibility changed.
#
# SHADOW. Nothing consumes this manifest yet. The gate asserts what the producer reports, not that
# any verdict depends on it — and the fact that one file is not usable must NOT be "fixed" by
# loosening the condition. It is the true state.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
source "scripts/tests/lib/fresh.sh"
source "scripts/tests/lib/fingerprints.sh"
require_fresh_binary || exit 1

PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }

BIN=".lake/build/bin/concrete"

# ONE definition of the corpus, shared with the other manifest gates via lib/fingerprints.sh, so
# "the corpus" cannot mean two different sets in two gates.
mapfile -t SRCS < <(fp_files)
echo "=== implementation manifest over ${#SRCS[@]} sources ==="

if [ "${#SRCS[@]}" -eq 0 ]; then
  no "no corpus sources found — every assertion below would pass over an empty set"
  echo "IMPL-MANIFEST-GATE: PASS=$PASS FAIL=$FAIL"; exit 1
fi

TOT_EXPECTED=0; TOT_ROWS=0; TOT_REFUSED=0
NOT_USABLE=(); REASONS=""; MISSING_LINE=0

for src in "${SRCS[@]}"; do
  out="$("$BIN" "$src" --report impl-manifest 2>/dev/null || true)"
  line="$(printf '%s\n' "$out" | grep '^IMPL-MANIFEST ' || true)"
  if [ -z "$line" ]; then MISSING_LINE=$((MISSING_LINE+1)); continue; fi
  e="$(printf '%s' "$line" | grep -o 'expected=[0-9]*'  | cut -d= -f2)"
  r="$(printf '%s' "$line" | grep -o 'rows=[0-9]*'      | cut -d= -f2)"
  x="$(printf '%s' "$line" | grep -o 'refused=[0-9]*'   | cut -d= -f2)"
  a="$(printf '%s' "$line" | grep -o 'accounted=[0-9]*' | cut -d= -f2)"
  u="$(printf '%s' "$line" | grep -o 'usable=[a-z]*'    | cut -d= -f2)"

  # PER-FILE ACCOUNTING, checked here rather than only in aggregate: totals that balance can hide
  # one file over-counting while another under-counts.
  if [ "$a" != "$((r + x))" ]; then
    no "$src: accounted=$a but rows+refused=$((r + x)) — an identity was lost between the lists"
  fi
  if [ "$e" != "$a" ]; then
    no "$src: expected=$e but accounted=$a — an expected identity appears in neither list"
  fi

  TOT_EXPECTED=$((TOT_EXPECTED + e)); TOT_ROWS=$((TOT_ROWS + r)); TOT_REFUSED=$((TOT_REFUSED + x))
  [ "$u" = "no" ] && NOT_USABLE+=("$src")
  REASONS="$REASONS$(printf '%s\n' "$out" | grep '^IMPL-MANIFEST-REASONS' || true)"
done

if [ "$MISSING_LINE" -eq 0 ]; then
  ok "every source produced an accounting line (${#SRCS[@]}/${#SRCS[@]})"
else
  no "$MISSING_LINE source(s) produced NO accounting line — a silent absence, not a refusal"
fi

# NON-VACUITY. Every assertion below is over these numbers, so a corpus that reported zero
# identities would satisfy all of them while measuring nothing.
if [ "$TOT_EXPECTED" -gt 0 ]; then
  ok "the corpus expects $TOT_EXPECTED identities (non-vacuous denominator)"
else
  no "expected=0 across the whole corpus — the accounting is vacuous"
fi

# THE MEASURED STATE, asserted as exact values. A ratchet would be wrong here: `rows` rising is
# good, but `expected` rising while `rows` does not is a new incompleteness, and one number moving
# in each direction can leave a ratchet green.
# 64/63/1 -> 72/71/1 on 2026-10-18 (bug 083): the deleted name-keyed `provedFunctions` table's claims
# moved into source, so thesis_demo/src/main.con (3 subjects), main_drifted.con (2) and
# crypto_verify_drifted (3) joined the stored-link population, each with an extracted body.
# 72/71/1 -> 88/87/1 on 2026-10-21: toml, hexdump and png_chunks joined the stored-link population
# with 16 extracted subjects (7 claimed, 9 unclaimed helpers in the same files); the refusal is unchanged.
if [ "$TOT_EXPECTED" = "88" ] && [ "$TOT_ROWS" = "87" ] && [ "$TOT_REFUSED" = "1" ]; then
  ok "corpus accounting is exactly expected=88 rows=87 refused=1"
else
  no "corpus accounting moved: expected=$TOT_EXPECTED rows=$TOT_ROWS refused=$TOT_REFUSED (was 88/87/1) — if rows rose because an entry gained an extracted body, update this and say so"
fi

# The refusal must be NAMED, not merely counted. An unnamed refusal is a smaller number wearing a
# different label, which is the defect this whole type replaced.
if printf '%s' "$REASONS" | grep -q 'extracted-missing=1'; then
  ok "the single refusal is NAMED: extracted-missing"
else
  no "the refusal is not reported as extracted-missing — reasons seen: $(printf '%s' "$REASONS" | tr '\n' ' ')"
fi

# WHICH file is not usable, by name. "One file is not usable" would stay true if the incompleteness
# silently moved to a different file, which is a different fact.
if [ "${#NOT_USABLE[@]}" -eq 1 ] && [ "${NOT_USABLE[0]}" = "examples/proof_pressure/src/main.con" ]; then
  ok "exactly one source is not usable, and it is the known one (proof_pressure)"
else
  no "not-usable set changed: ${NOT_USABLE[*]:-none} (expected exactly examples/proof_pressure/src/main.con)"
fi

# ...and the complement, so the above cannot pass because NOTHING is usable. A producer that refused
# every file would leave a single named non-usable file true only by accident.
USABLE=$(( ${#SRCS[@]} - ${#NOT_USABLE[@]} ))
if [ "$USABLE" -gt 0 ]; then
  ok "$USABLE of ${#SRCS[@]} sources produce a usable manifest (refusal is targeted, not total)"
else
  no "NO source produces a usable manifest — the refusals are not targeted"
fi

# CROSS-CHECK AGAINST THE OTHER DENOMINATOR. The dependency-root work counts 64 subjects across this
# same corpus (62 rooted, 2 refused), and the manifest expects 64 identities. Measured 2026-08-13:
# the two agree FILE BY FILE across all 20 sources, not merely in total.
#
# WHAT THIS IS AND IS NOT. Both numbers currently derive from `pc.entries`, so agreement is what the
# code should produce — this is a DIVERGENCE TRIPWIRE, not independent confirmation that the two
# workstreams measure the same thing. Its value is that if either side later gains a filter (an
# "eligible subjects" predicate, a skipped entry kind), the denominators separate silently and two
# gates go on reporting confident numbers about different populations. Totals alone would not catch
# it: one file gaining an identity while another loses one keeps the sum at 64.
DIVERGED=0
for src in "${SRCS[@]}"; do
  me="$("$BIN" "$src" --report impl-manifest 2>/dev/null | grep -o 'expected=[0-9]*' | cut -d= -f2)"
  dr="$("$BIN" "$src" --report subject-facts 2>/dev/null | grep -c 'shadow depRoot:')"
  if [ "${me:-x}" != "${dr:-y}" ]; then
    no "$src: manifest expects ${me:-?} identities but ${dr:-?} depRoot lines — the two denominators have diverged"
    DIVERGED=$((DIVERGED+1))
  fi
done
if [ "$DIVERGED" -eq 0 ]; then
  ok "manifest and dependency-root denominators agree on all ${#SRCS[@]} sources, file by file"
fi

# CONTAINMENT: this is shadow. The manifest must not reach a status derivation.
if grep -rn "implementationManifestOf\|implementationManifestResultOf" Concrete/ --include=*.lean \
     | grep -v "^Concrete/Proof/ProofCore.lean:" \
     | grep -v "^Concrete/Report/Report.lean:" \
     | grep -v "^Concrete/Proof/BodyIdentity.lean:" | grep -q .; then
  no "the manifest producer reached a new consumer — check it is not a status derivation"
else
  ok "the manifest producer is consumed only by the shadow report — still decides nothing"
fi

echo "IMPL-MANIFEST-GATE: PASS=$PASS FAIL=$FAIL (expected=$TOT_EXPECTED rows=$TOT_ROWS refused=$TOT_REFUSED)"
[ "$FAIL" -eq 0 ]
