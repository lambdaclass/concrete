#!/usr/bin/env bash
# FROZEN CLAIM INVENTORY (R-0208): replay every Lean-backed claim and compare the rows with the
# committed reference. Since R-0546 this gate runs only inside the checker-acceptance step
# (scripts/tools/checker_acceptance_evidence.sh), which is its CI step and its command in the local
# full set; run it alone with `bash scripts/tests/check_claim_inventory.sh`.
#
# History: the acceptance step once ran only on CI, and it JUDGES — a corpus change that
# adds, removes or re-verdicts a claim turns it red — and twice (bug 079's first CI result; the bug 079
# follow-up fixtures at 78e6af25) a locally green push failed there. This gate is that judgement alone,
# runnable anywhere the compiler and the pinned checker are.
#
# Rows are compared, never the header: the header names the checker build, which differs per platform.
# A difference is fixed by explaining it and updating the reference, never by regenerating to make it go
# away (see scripts/tools/claim_inventory.sh).
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
trap 'rc=$?; if [ "$rc" -ne 0 ] && [ "${GATE_DONE:-0}" -ne 1 ]; then
  echo "FATAL: unexpected shell failure (exit $rc) — the verdict below is not trustworthy" >&2; exit "$rc"; fi' ERR
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
REF="docs/verification/claim-inventory-v4.34.1.tsv"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

replay_rc=0
bash scripts/tools/claim_inventory.sh > "$TMP/inv.tsv" 2> "$TMP/inv.err" || replay_rc=$?
grep -v '^#' "$REF" > "$TMP/ref.rows"
grep -v '^#' "$TMP/inv.tsv" > "$TMP/new.rows" || true
: > "$TMP/diff"
if [ "$replay_rc" -eq 0 ]; then ok "the replay exited cleanly"
else no "the replay exited $replay_rc:"; awk 'NR<=20 {print "       " $0}' "$TMP/inv.err"; fi

# An unidentified checker turns every row into env_failure: that is a broken environment, not a verdict.
if grep -q $'\tenv_failure\t' "$TMP/new.rows"; then
  no "the replay could not run the checker (env_failure rows; is the pinned lean on PATH?)"
else
  ok "the replay ran the pinned checker on every claim"
fi

_inv_same=0; [ -s "$TMP/new.rows" ] && diff "$TMP/ref.rows" "$TMP/new.rows" > "$TMP/diff" && _inv_same=1
# The acceptance step (scripts/tools/checker_acceptance_evidence.sh) runs this gate as its claim
# inventory judgement and publishes the replayed rows and their diff as evidence.
if [ -n "${CLAIM_INVENTORY_KEEP_DIR:-}" ]; then
  cp "$TMP/inv.tsv" "$TMP/diff" "$CLAIM_INVENTORY_KEEP_DIR/" 2>/dev/null \
    || no "could not keep the replayed inventory in $CLAIM_INVENTORY_KEEP_DIR"
fi
if [ "$_inv_same" = 1 ]; then
  ok "every row matches $REF ($(grep -c . "$TMP/new.rows") rows)"
else
  no "the replay differs from $REF:"
  awk 'NR<=40 {print "       " $0}' "$TMP/diff"
fi

# The header's counts describe the rows; a stale count is a reference that was edited by hand. A
# source with no checkable claim gets one sentinel row (`-` for function, theorem and kind); it counts
# as a source but not as a claim, exactly as claim_inventory.sh counts.
want="# sources: $(cut -f1 "$TMP/ref.rows" | sort -u | grep -c .)  claims: $(grep -vc $'\t-\t-\t-\t' "$TMP/ref.rows")"
if grep -qxF "$want" "$REF"; then ok "the reference header counts its rows"
else no "the reference header does not say '$want'"; fi

# Gate the gate: the same comparison must reject a dropped row and a changed verdict.
# Only diff's status 1 ("they differ") is a rejection; status 2 is diff itself failing.
rejects(){ local rc=0; diff -q "$TMP/ref.rows" "$1" >/dev/null 2>&1 || rc=$?; [ "$rc" -eq 1 ]; }
sed '1d' "$TMP/ref.rows" > "$TMP/drop.rows"
if rejects "$TMP/drop.rows"; then ok "control: a dropped claim is a difference"
else no "control: a dropped claim went unnoticed"; fi
awk -F'\t' 'BEGIN{OFS="\t"} NR==1{$7=($7=="minted"?"withheld:claim_not_proved":"minted")} {print}' \
  "$TMP/ref.rows" > "$TMP/flip.rows"
if rejects "$TMP/flip.rows"; then ok "control: a changed receipt verdict is a difference"
else no "control: a changed receipt verdict went unnoticed"; fi

GATE_DONE=1
echo "CLAIM-INVENTORY: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
