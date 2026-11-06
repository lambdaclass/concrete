#!/usr/bin/env bash
# THE ATOMIC-FLIP ENTRANCE ASSERTION (R-0004 package 2).
#
# Before the `CallableId` evidence join is replaced by `DefinitionIdentity`, a specific set of
# conditions must hold TOGETHER. They were written in the roadmap as prose, which is exactly the
# shape that lets a flip proceed on a remembered version of the state rather than a measured one —
# and this project has already caught two prose conversion figures that had drifted.
#
# SEPARATE FROM `check_attestation_manifest.sh` ON PURPOSE. That gate reports progress, and it is
# deliberately GREEN while tables are pending: a pending table honestly reporting zero is not a
# failure. So its success can never be the entrance signal. This gate asserts the finished state,
# and is red until the flip is genuinely safe to perform.
#
# It measures; it does not restate. Every number comes from the manifest generator, the compiler, or
# the Lean environment, and no figure is written here that a producer could supply.
#
# STATE AS OF 2026-10-22. It went green (13/0) and the flip landed on 2026-08-15 (568886dc); since
# then it is the standing assertion that the entrance conditions still hold. It is RED again on
# condition 1 alone: bug 083 (274fb411) linked thesis_demo's `parse_byte`/`check_length` in source
# to `proofFns`, so that no-manifest table now has manifest rows and no attestation — a pending
# conversion (its references are already generated in GeneratedAttestations; binding them is R-0004
# work and would also move condition 3, which still asserts proofFns is evidence-ineligible). The
# mutation inventory declares this gate EXPECTED-RED by that exact FAIL line (`expect_red` in
# check_gate_mutation_coverage.sh), so its two families are reported PENDING rather than INVALID,
# any other red leg still invalidates them, and the run fails loudly once this gate turns green.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
source "scripts/tests/lib/fresh.sh"
require_fresh_binary || exit 1
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }

BIN="$ROOT_DIR/.lake/build/bin/concrete"
OUT="$(bash scripts/gen/attestation_manifest.sh 2>/dev/null)" || {
  echo "  FAIL the manifest generator refused — no entrance condition can be evaluated"
  echo "ATOMIC-FLIP-ENTRANCE: PASS=0 FAIL=1"; exit 1; }
field() { printf '%s' "$OUT" | grep -oE "^$1 *= *[0-9]+" | grep -oE '[0-9]+$' || true; }

leanq() {  # leanq <expected> <label> <expr>
  cat > "$TMP/p.lean" <<LEAN
import Concrete
import Examples
open Lean Meta Concrete Concrete.Proof
$3
LEAN
  local out; out="$(lake env lean "$TMP/p.lean" 2>&1 || true)"
  if grep -qE "error:|error\(lean" <<<"$out"; then
    no "$2 — probe did not elaborate: $(printf '%s' "$out" | tr '\n' ' ' | cut -c1-160)"
  elif grep -qF -- "$1" <<<"$out"; then ok "$2"
  else no "$2 — got: $(printf '%s' "$out" | tr '\n' ' ' | cut -c1-160)"; fi
}

echo "=== condition 1: pending conversion is zero ==="
# Measured through the same site probe the progress gate uses: a table with entries and no
# attestations is pending, whatever the roadmap says about it.
cat > "$TMP/sites.lean" <<'LEAN'
import Concrete
import Examples
open Concrete Concrete.Proof
def sites : List (String × FnTable) :=
  [ ("Concrete.Proof.FnTable.empty", FnTable.empty)
  , ("Concrete.Proof.proofFns", proofFns)
  , ("Concrete.Proof.proofFnsExt", proofFnsExt)
  , ("Concrete.Proof.cryptoFns", cryptoFns)
  , ("Concrete.Proof.elfFns", elfFns)
  , ("Concrete.Proof.parseValidateFns", parseValidateFns)
  , ("Concrete.Proof.fixedCapacityFns", fixedCapacityFns)
  , ("Concrete.Proof.ctTagFns", ctTagFns)
  , ("Concrete.Proof.pureCoreFns", pureCoreFns)
  , ("Examples.ProofPatterns.Proofs.combineFns", Examples.ProofPatterns.Proofs.combineFns)
  , ("Examples.HmacSha256.Proofs.shaFns", Examples.HmacSha256.Proofs.shaFns) ]
#eval show IO Unit from do
  for (n, t) in sites do
    IO.println s!"SITE {n} attested={t.attested.size} failures={t.attestationFailures} entries={t.entries.size}"
LEAN
SITES="$(lake env lean "$TMP/sites.lean" 2>&1)"
if grep -qE "error:|error\(lean" <<<"$SITES"; then
  no "the site probe did not elaborate — no condition can be measured"
  echo "ATOMIC-FLIP-ENTRANCE: PASS=$PASS FAIL=$((FAIL+1))"; exit 1
fi
PENDING=0; PENDING_NAMES=""
for tbl in $(printf '%s' "$OUT" | grep ' <- ' | grep -v EXCLUDED | awk '{print $1}' | sort -u); do
  att="$(printf '%s' "$SITES" | grep -E "^SITE $tbl " | grep -oE 'attested=[0-9]+' | cut -d= -f2)"
  ent="$(printf '%s' "$SITES" | grep -E "^SITE $tbl " | grep -oE 'entries=[0-9]+' | cut -d= -f2)"
  [ "${ent:-0}" = "0" ] && continue
  [ "${att:-0}" = "0" ] && { PENDING=$((PENDING+1)); PENDING_NAMES="$PENDING_NAMES $tbl"; echo "      pending: $tbl"; }
done
if [ "$PENDING" = "0" ]; then
  ok "no manifest-backed table with entries is unattested"
else
  # THE FAILURE NAMES ITS TABLES. The mutation inventory declares this gate expected-red by its exact
  # FAIL lines, so a count alone would let one table's conversion and another's regression trade
  # places under an unchanged line.
  no "$PENDING table(s) still pending conversion (${PENDING_NAMES# }) — the flip would migrate a join over tables that cannot describe their definitions"
fi
# No table may carry a broken generated reference: `needs_recheck` is not a state to flip on.
BROKEN="$(printf '%s' "$SITES" | grep -c 'failures=[1-9]' || true)"
if [ "$BROKEN" = "0" ]; then ok "no table carries a failed generated reference"
else no "$BROKEN table(s) carry failed generated references — needs_recheck, not entrance"; fi

echo "=== condition 2: the frozen subject snapshot is unchanged ==="
ROWS="$(field subject_rows)"; NOID="$(field subjects_no_identity)"
UNTABLED="$(field subjects_without_table)"; MAPPED="$(field table_attestations)"
# 86 = 0 + 38 + 48 -> 0 + 39 + 47 on 2026-08-15, by a DELIBERATE fixture repair rather than by the
# dependency population: `proof_pressure.validate_header`'s misattached `#[proof_by]` was deleted, so
# the subject no longer has a table. The 86 is unchanged, which is the point — the declaration still
# exists; what left is a false claim.
#
# 86 -> 105 = 0 + 49 + 56, by NEW PROOF-LINKED FIXTURES ONLY, never by the dependency population
# (dependency rows are still 41 and still split off at the generator's door). Nothing ran this gate
# in CI, so the pin went stale silently and was found as RED ON CLEAN in the mutation shards. Each
# fixture entered the generator's scan (`#[proof_by]`/`#[ensures_proof]`) for the first time; every
# subject row in it counts, linked or not:
#   274fb411 (bug 083) thesis_demo   +3 = 2 tabled (parse_byte, check_length -> proofFns) + 1 untabled (validate)
#   0b3314b7           toml          +12 = 4 tabled (FnTable.empty) + 8 untabled
#   9c9971ca           hexdump       +2 = 2 tabled (FnTable.empty)
#   d3cfaa13           png_chunks    +2 = 1 tabled (FnTable.empty) + 1 untabled (is_png_sig)
# Sum +19 = +10 untabled + +9 tabled. The thesis_demo rows are also why condition 1 is red: proofFns
# now has manifest rows and no attestation.
if [ "$ROWS" = "105" ] && [ "$NOID" = "0" ] && [ "$UNTABLED" = "49" ] && [ "$MAPPED" = "56" ]; then
  ok "subject accounting is exactly 105 = 0 + 49 + 56"
else
  no "subject accounting moved: $ROWS = $NOID + $UNTABLED + $MAPPED — the dependency population must not touch it"
fi

echo "=== condition 3: no-manifest tables are evidence-ineligible ==="
# They evaluate as mathematical models and must justify nothing. `proofFns` holds entries and no
# attestation, so it REFUSES; the other two hold no entries, so their membership is empty. Both
# dispositions justify nothing, and neither may acquire a name-keyed fallback.
leanq "true" "proofFns refuses as legacyUnattested (no manifest row, no fallback)" \
'#eval match scopedEntryEvidence proofFns with
   | .error ScopedMembershipRefusal.legacyUnattested => true
   | _ => false'
leanq "true" "proofFnsExt and pureCoreFns yield empty membership" \
'#eval (scopedEntryEvidence proofFnsExt).toOption == some []
   && (scopedEntryEvidence pureCoreFns).toOption == some []'
# POSITIVE CONTROL: they must still EVALUATE. Evidence-ineligible is not the same as unusable, and a
# flip that broke evaluation would be a different (and worse) change than the one intended.
leanq "true" "the no-manifest tables still evaluate (ineligible for evidence, not broken)" \
'#eval (proofFns.globals "parse_byte").isSome'

echo "=== condition 4: correspondence and roots are unmoved ==="
# THE CONTROL IS THE DRIFT FIXTURE NOW. `proof_pressure`'s misattachment was repaired deliberately on
# 2026-08-15 (its false `#[proof_by]` deleted), so it no longer claims anything. `main_drifted` is
# the permanent case: a DIFFERENT PROGRAM sharing every declaration name with `elf_header`, whose
# four body edges the scoped join refuses and the name-keyed one accepted.
CORR="$("$BIN" examples/elf_header_drifted/src/main.con --report subject-facts 2>/dev/null | grep -c 'shadow correspondence: matched=0 missing=4' || true)"
if [ "$CORR" -ge 1 ] 2>/dev/null; then
  ok "the cross-program substitution still refuses (matched=0 missing=4)"
else
  no "elf_header/main_drifted no longer refuses — a different program's edges are being justified again"
fi

echo "=== condition 8: every dependency request ends attested or named-refused ==="
DEPREQ="$(field dependency_requests)"; DEPATT="$(field dependency_attestations)"
DEPREF="$(field dependency_refusals)"; DEPNOID="$(field dependency_without_identity)"
if [ -n "$DEPREQ" ] && [ "$DEPREQ" = "$(( DEPATT + DEPREF ))" ] && [ "$DEPREQ" -gt 0 ] 2>/dev/null; then
  ok "dependency requests reconcile: $DEPREQ = $DEPATT attested + $DEPREF refused"
else
  no "dependency requests do not reconcile: $DEPREQ vs $DEPATT + $DEPREF"
fi
if [ "$DEPNOID" = "0" ]; then
  ok "no dependency request lacks a scoped identity"
else
  no "$DEPNOID dependency request(s) have no scoped identity — an edge the flip could not key"
fi
# Every refusal must NAME its reason. A count of refusals says a wall exists; the reason says which.
UNNAMED="$(printf '%s' "$OUT" | grep ' !! ' | grep -vc 'refusal=[a-zA-Z]' || true)"
if [ "$UNNAMED" = "0" ]; then ok "every dependency refusal names its reason"
else no "$UNNAMED dependency refusal(s) carry no named reason"; fi

echo "=== condition 9: every bound reference is load-bearing ==="
# A reference the site selected must be one the table's scoped membership actually answers with.
# Selection that no lookup consults would satisfy condition 1 without carrying anything.
leanq "true" "each converted table's scoped membership equals its bound attestation count" \
'#eval
  let tbls : List (String × FnTable) :=
    [("cryptoFns", cryptoFns), ("elfFns", elfFns), ("ctTagFns", ctTagFns),
     ("fixedCapacityFns", fixedCapacityFns), ("parseValidateFns", parseValidateFns),
     ("combineFns", Examples.ProofPatterns.Proofs.combineFns),
     ("shaFns", Examples.HmacSha256.Proofs.shaFns)]
  tbls.all (fun (_, t) =>
    match scopedEntryEvidence t with
    | .error _ => false
    | .ok rows => rows.length == t.attested.size)'
# ...and every bound identity is FINDABLE by the scoped lookup, which is the operation the flipped
# join performs. Equal counts alone would pass if the rows described something else entirely.
leanq "true" "every bound identity is findable by scoped lookup" \
'#eval
  let tbls : List FnTable :=
    [cryptoFns, elfFns, ctTagFns, fixedCapacityFns, parseValidateFns,
     Examples.ProofPatterns.Proofs.combineFns, Examples.HmacSha256.Proofs.shaFns]
  tbls.all (fun t =>
    match scopedEntryEvidence t with
    | .error _ => false
    | .ok rows => t.attested.toList.all (fun (_, d) => scopedEvidenceContains rows d))'

echo "=== condition 11: no converted table has an equation lemma ==="
leanq "true" "no proof term depends on a converted table's definitional shape" \
'#eval show MetaM Bool from do
   let env ← getEnv
   let tables := [`Concrete.Proof.cryptoFns, `Concrete.Proof.ctTagFns, `Concrete.Proof.elfFns,
                  `Concrete.Proof.fixedCapacityFns, `Concrete.Proof.parseValidateFns,
                  `Examples.ProofPatterns.Proofs.combineFns, `Examples.HmacSha256.Proofs.shaFns]
   return tables.all (fun t => !(env.contains (t ++ `eq_1)) && !(env.contains (t ++ `eq_def)))'

echo "ATOMIC-FLIP-ENTRANCE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
