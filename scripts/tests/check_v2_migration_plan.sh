#!/usr/bin/env bash
# THE V1 -> V2 SUBJECT-DIGEST MIGRATION, DRY RUN (R-0004 package 3).
#
# Every stored `#[proof_fingerprint]` in the corpus is a V1 value: a body-only fingerprint answering
# a weaker question than the V2 subject digest, which also binds the declaration's signature,
# contracts, capabilities and constant environment. Activating V2 makes every stored V1 value NOT
# COMPARABLE at once — correctly — so activation without migration would turn the corpus
# `needs_recheck` in a single commit.
#
# THIS GATE IS ABOUT THE DENOMINATOR. A migration plan that lists only what it would change is
# indistinguishable from one that silently skips things, so the population must RECONCILE:
#
#     stored fingerprints in code  =  plan rows  +  every exclusion, named
#
# WHAT CANNOT BE MIGRATED MATTERS MORE THAN WHAT CAN. A stale link is pinned to a body that no longer
# exists; recording the CURRENT digest for it would assert that a proof was established against a
# body it was never checked against — manufacturing freshness, which is the exact forgery this
# package exists to prevent.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
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

FIXTURES="$(grep -rlE '#\[proof_fingerprint' examples --include='*.con' | sort)"

echo "=== the population reconciles ==="

# COUNTED FROM CODE, NOT FROM PROSE. A first attempt counted every textual occurrence and read 52,
# because fixture COMMENTS discuss `#[proof_fingerprint]` — the fourth time this repo has confused
# prose for code in a measurement. Comment lines are excluded.
CODE=0; ROWS=0; UNREACHED=""
for f in $FIXTURES; do
  n=$( { grep -v '^\s*//' "$f" || true; } | { grep -o '#\[proof_fingerprint' || true; } | wc -l)
  r=$( { "$BIN" "$f" --report migration 2>/dev/null || true; } | { grep -cE '^  \[' || true; } )
  CODE=$((CODE + n)); ROWS=$((ROWS + r))
  [ "$n" != "$r" ] && UNREACHED="$UNREACHED $f($n/$r)"
done
echo "  stored in code: $CODE   plan rows: $ROWS"

if [ "$CODE" -gt 0 ] && [ "$ROWS" -gt 0 ]; then
  ok "the census is live ($CODE stored, $ROWS planned) — the reconciliation below is not vacuous"
else
  no "census found nothing (stored=$CODE rows=$ROWS); every assertion below would be vacuous"
fi

# EXACTLY ONE fingerprint is unreachable by any obligation, and it is the fixture whose entire
# purpose is an invalid attribute. Pinned by NAME: "one is excluded" would be satisfied by a
# different one going missing.
# 44/43 -> 51/50 on 2026-10-18 (bug 083): the deleted name-keyed `provedFunctions` table's claims moved
# into source as v2 pins — thesis_demo/src/main.con (2), main_drifted.con (2), crypto_verify_drifted (3).
# 51/50 -> 58/57 on 2026-10-21 (workload proofs): seven new v2 pins, each reached by its obligation —
# examples/toml/src/main.con (4: is_ws, is_newline, is_digit, is_bare_key_char),
# examples/hexdump/src/main.con (2: gutter_byte, line_len), examples/png_chunks/src/main.con (1: png_sig_ok).
if [ "$CODE" = "58" ] && [ "$ROWS" = "57" ]; then
  ok "58 stored fingerprints = 57 plan rows + 1 unreachable"
else
  no "population moved: $CODE stored / $ROWS planned (expected 58 / 57)"
fi
if [ "$(tr -s ' ' '\n' <<<"$UNREACHED" | grep -c . || true)" = "1" ] \
   && grep -q 'contract_negatives/invalid_attribute' <<<"$UNREACHED"; then
  ok "the one unreachable fingerprint is contract_negatives/invalid_attribute, by name"
else
  no "the unreachable set changed — expected exactly contract_negatives/invalid_attribute, got:$UNREACHED"
fi

echo "=== every row has a disposition, and they reconcile ==="

DISP="$(for f in $FIXTURES; do
          { "$BIN" "$f" --report migration 2>/dev/null || true; } | { grep -oE '^  \[[a-z_0-9]+\]' || true; }
        done | sort | uniq -c)"
cnt(){ { grep -oE "[0-9]+ +\\[$1\\]" <<<"$DISP" || true; } | { grep -oE '^[0-9]+' || true; } | head -1; }
MIG="$(cnt migrate)"; ALREADY="$(cnt already_v2)"; STALE="$(cnt stale_no_honest_value)"
NODIG="$(cnt no_subject_digest)"
MIG="${MIG:-0}"; ALREADY="${ALREADY:-0}"; STALE="${STALE:-0}"; NODIG="${NODIG:-0}"
echo "  migrate=$MIG already_v2=$ALREADY stale_no_honest_value=$STALE no_subject_digest=$NODIG"

if [ "$(( MIG + ALREADY + STALE + NODIG ))" = "$ROWS" ]; then
  ok "every plan row carries exactly one disposition ($MIG + $ALREADY + $STALE + $NODIG = $ROWS)"
else
  no "dispositions do not sum to the plan: $MIG + $ALREADY + $STALE + $NODIG != $ROWS"
fi

# ACTIVATION IS COMPLETE, so there is nothing left to migrate. This is the regression leg: a v1
# value reappearing anywhere — a hand-written fingerprint, a reverted file, a new fixture — shows up
# here as `migrate` and fails, rather than silently sitting at a weaker digest.
if [ "$MIG" = "0" ] && [ "$ALREADY" = "$ROWS" ]; then
  ok "V2 activation is complete: all $ROWS plan rows already carry a v2 value, none left to migrate"
else
  no "$MIG link(s) still store a v1 value — activation is no longer complete"
fi

echo "=== a stale link carries a REAL digest, never a manufactured one ==="

# `elf_header_drifted` is a different program sharing every declaration name with `elf_header`; two
# of its functions drifted from what their proofs were pinned to. Their stored values are the GENUINE
# pre-drift v2 digests, obtained by reverting the drift, measuring, and restoring it — which is why
# this fixture still demonstrates STALENESS under V2 rather than degrading to "the record is old".
DRIFT_ST="$("$BIN" examples/elf_header_drifted/src/main.con --report proof-status 2>/dev/null || true)"
if [ "$( { grep -cE '^-- proof stale' <<<"$DRIFT_ST" || true; } )" = "2" ]; then
  ok "the drift fixture reports 2 STALE under v2 — a real digest mismatch, not an unreadable record"
else
  no "the drift fixture no longer reports 2 stale: $(grep -cE '^-- proof stale' <<<"$DRIFT_ST" || true)"
fi
# ...and TARGETED: its undrifted claims stay proved. A fixture that went entirely stale would satisfy
# the leg above while telling us nothing about discrimination.
if [ "$( { grep -cE '^-- proved' <<<"$DRIFT_ST" || true; } )" -ge 2 ]; then
  ok "the same fixture's undrifted claims stay proved — staleness is per-claim"
else
  no "nothing in the drift fixture is proved; staleness is blanket, not targeted"
fi
# NO SYNTHETIC VALUE MAY SIT WHERE A REAL ONE BELONGS. The two drifted links carry the exact digests
# `elf_header` records for the same declarations, because the v2 subject digest is package-independent
# — that equality is what makes them provably the pre-drift bodies rather than invented non-matches.
for d in a669dd14614bc56a33be35aa10226679 0d368796ab5dee78d031bac527ddeced; do
  if grep -q "v2:$d" examples/elf_header_drifted/src/main.con \
     && grep -q "v2:$d" examples/elf_header/src/main.con; then
    ok "drifted link pinned to the real pre-drift digest ${d:0:8} (identical in the undrifted program)"
  else
    no "digest ${d:0:8} is no longer shared between the drifted and undrifted programs — the pin is not a measured pre-drift value"
  fi
done
# No migrated row may carry a v1 value as its target, and every one must be a v2 value.
BADTARGET="$(for f in $FIXTURES; do
               { "$BIN" "$f" --report migration 2>/dev/null || true; } | { grep '\[migrate\]' || true; }
             done | { grep -vc -- '-> v2:' || true; })"
if [ "$BADTARGET" = "0" ]; then
  ok "every migrated row targets a v2-prefixed digest"
else
  no "$BADTARGET migrated row(s) do not target a v2 value"
fi

echo "=== the dry run writes nothing ==="

# A plan that mutated the corpus while reporting would be indistinguishable from an applied
# migration, and there is no undo for that.
BEFORE="$(git status --porcelain -- examples | sort)"
for f in $FIXTURES; do "$BIN" "$f" --report migration >/dev/null 2>&1 || true; done
AFTER="$(git status --porcelain -- examples | sort)"
if [ "$BEFORE" = "$AFTER" ]; then
  ok "running the plan leaves every fixture untouched"
else
  no "the dry run MODIFIED the corpus — it is not a dry run"
  diff <(printf '%s' "$BEFORE") <(printf '%s' "$AFTER") | head -5 | sed 's/^/      /'
fi

GATE_DONE=1
echo "V2-MIGRATION-PLAN: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
