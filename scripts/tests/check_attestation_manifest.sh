#!/usr/bin/env bash
# The attestation manifest, measured on the real corpus.
#
# The manifest decides which SUBJECT each evidence-bearing proof table is attested to, and the table
# conversion will be driven by it. So its numbers are pinned here rather than trusted from a run: a
# manifest that silently mapped fewer tables, or excluded a fixture it should have mapped, would
# produce a conversion that looks correct and attests the wrong implementations.
#
# EXACT VALUES, not ratchets. A mapping count rising is not automatically good — it can mean a drift
# fixture stopped being excluded — so every number is asserted and a change must be explained.
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

echo "=== attestation manifest ==="
OUT="$(bash scripts/gen/attestation_manifest.sh 2>/dev/null)"
RC=$?

# The generator REFUSES rather than emitting a partial manifest, so a non-zero exit is a hard failure
# here — not a condition to work around.
if [ "$RC" -eq 0 ]; then
  ok "the manifest generator completes without refusing"
else
  no "the manifest generator REFUSED (exit $RC) — run it directly to read the reasons"
  echo "ATTESTATION-MANIFEST: PASS=$PASS FAIL=$FAIL"; exit 1
fi

field() { printf '%s' "$OUT" | grep -oE "^$1 *= *[0-9]+" | grep -oE '[0-9]+$' || true; }

SCANNED="$(field fixtures_scanned)"
DRIFT_EX="$(field fixtures_excluded_drift)"
ROWS="$(field subject_rows)"
NOID="$(field subjects_no_identity)"
UNTABLED="$(field subjects_without_table)"
MAPPED="$(field table_attestations)"
DUPES="$(field duplicate_mappings)"
COLLAPSE="$(field package_collapse_keys)"
SURPLUS="$(field surplus_tables)"

echo "  scanned=$SCANNED drift_excluded=$DRIFT_EX rows=$ROWS no_identity=$NOID untabled=$UNTABLED mapped=$MAPPED"

# TYPED RECONCILIATION. Every subject row must end in exactly one bucket. This is the property that
# makes the denominator meaningful rather than two counters that happen to agree.
if [ -n "$ROWS" ] && [ "$ROWS" = "$(( NOID + UNTABLED + MAPPED ))" ]; then
  ok "every subject row is accounted for exactly once ($ROWS = $NOID + $UNTABLED + $MAPPED)"
else
  no "subject rows do not reconcile: $ROWS != $NOID + $UNTABLED + $MAPPED"
fi

# THE MEASURED STATE. Exact, because a rise can mean a drift fixture stopped being excluded.
# 48 -> 47 ON 2026-08-15, DELIBERATELY: `proof_pressure.validate_header` carried a MISATTACHED
# `#[proof_by]` naming a theorem about `elf_header`'s identically-named function. The claim was
# deleted rather than repointed — no theorem proves that body — so the subject moved from
# "has a table" to "has no table". The 86 total is unchanged because the declaration still exists;
# what left is a claim that was false.
# 86/47/39 -> 89/49/40 ON 2026-10-20 (bug 083): examples/thesis_demo's claims were made by the removed
# name-keyed provedFunctions table and carried no attribute, so the fixture was outside the manifest.
# They now live in source: its three declarations enter as subjects, parse_byte and check_length
# attested under Concrete.Proof.proofFns and validate untabled (it is not claimed).
# 89/49/40 -> 105/56/49 ON 2026-10-21 (workload proofs): examples/toml, examples/hexdump and
# examples/png_chunks gained their first proof links, so all three fixtures enter the manifest with
# every extracted declaration as a subject (16 rows). Attested +7, each under FnTable.empty:
# toml_parser.{is_ws,is_newline,is_digit,is_bare_key_char}, hexdump.{gutter_byte,line_len},
# png_chunks.png_sig_ok. Untabled +9, declarations with no claim: toml_parser.{mk_string_val,
# mk_int_val,mk_bool_val,mk_array_val,mk_table_val,mk_error_val,is_error,err_result} and
# png_chunks.is_png_sig.
if [ "$ROWS" = "105" ] && [ "$MAPPED" = "56" ] && [ "$UNTABLED" = "49" ] && [ "$NOID" = "0" ]; then
  ok "corpus accounting is exactly rows=105 attested=56 untabled=49 no-identity=0"
else
  no "corpus accounting moved: rows=$ROWS attested=$MAPPED untabled=$UNTABLED no-identity=$NOID (was 105/56/49/0) — say which subjects changed and why"
fi

# NO SILENT DEFECTS. Each of these has been zero since the collapse was closed; a non-zero value is a
# real finding, not noise.
for pair in "duplicate_mappings:$DUPES" "package_collapse_keys:$COLLAPSE" "surplus_tables:$SURPLUS"; do
  k="${pair%%:*}"; v="${pair##*:}"
  if [ "$v" = "0" ]; then ok "$k = 0"; else no "$k = $v — a real defect, not noise"; fi
done

# DRIFT IS EXCLUDED, AND NAMED. "Some fixture was excluded" would stay true if the WRONG one were.
if printf '%s' "$OUT" | grep -q 'excluded = ".*main_drifted.con"'; then
  ok "drift fixtures are excluded by name, not silently filtered"
else
  no "no named drift exclusion in the manifest — the classifier stopped matching"
fi
if [ "$DRIFT_EX" -ge 1 ] 2>/dev/null; then
  ok "$DRIFT_EX drift fixture(s) excluded from attestation"
else
  no "no drift fixture was excluded — a drifted implementation could be attested"
fi

# REUSE IS REPRESENTED, NOT COLLAPSED.
#
# THE INVARIANT IS THE ONE-TO-ONE MAPPING, not the number. Every consuming fixture must yield exactly
# one scoped package identity, and no two fixtures may share one. That property holds whatever the
# corpus contains: it fails if two programs collapse into a package (the defect closed in b3007c64)
# and it fails if one program splits across several. It is asserted FIRST for that reason.
EMPTY_PKGS="$(printf '%s' "$OUT" | grep '^Concrete.Proof.FnTable.empty <-' | awk '{print $3}' | cut -d/ -f1 | sort -u | grep -c . || true)"
EMPTY_SRCS="$(printf '%s' "$OUT" | grep '^Concrete.Proof.FnTable.empty <-' | sed -E 's/.*\(([^)]*)\)$/\1/' | sort -u | grep -c . || true)"
if [ "$EMPTY_PKGS" = "$EMPTY_SRCS" ] && [ "$EMPTY_SRCS" -gt 1 ]; then
  ok "INVARIANT: a shared table's $EMPTY_SRCS consuming fixtures map one-to-one onto scoped packages"
else
  no "one-to-one broken: $EMPTY_SRCS consuming fixtures, $EMPTY_PKGS package identities — fixtures are collapsing into one package or splitting across several"
fi

# TODAY'S CORPUS DENOMINATOR, which is a different kind of claim. 13 is what this corpus currently
# contains; it is NOT architectural truth, and it moves whenever a fixture is added or removed. It is
# pinned so such a move is stated rather than absorbed — not because 13 means anything by itself.
#
# (It was 8 in my own earlier note, taken from a map built on the `#[proof_by]` population alone;
# `evidence_classes/proved_by_lean` and `hmac_sha256` reach this table through IN-REPO theorems.)
# 10 -> 13 ON 2026-10-21: the toml, hexdump and png_chunks packages (workload proofs, all FnTable.empty).
if [ "$EMPTY_PKGS" = "13" ]; then
  ok "corpus denominator: FnTable.empty is attested to 13 packages (today's measurement, not an invariant)"
else
  no "FnTable.empty denominator moved to $EMPTY_PKGS (was 13) — say which fixture was added or removed"
fi

# RECOMPUTED AGAINST CURRENT FACTS. The manifest is derived, not stored, so its identities are only
# as trustworthy as the derivation is REPRODUCIBLE. A second run over the same tree must be
# byte-identical: if identity computation ever became order-dependent, environment-dependent or
# otherwise nondeterministic, the manifest would still look well-formed while attesting different
# identities on different runs — and a conversion driven by one run would disagree with the compiler
# on the next.
OUT2="$(bash scripts/gen/attestation_manifest.sh 2>/dev/null)"
if [ "$OUT" = "$OUT2" ]; then
  ok "a second derivation is byte-identical (identities are reproducible, not merely produced)"
else
  no "two derivations of the manifest DIFFER — identity computation is not deterministic, so no conversion driven by it can be trusted"
fi

# POSITIVE CONTROL FOR LEGITIMATE REUSE. Every refusal above is about rejecting bad mappings, and a
# manifest that rejected everything would satisfy all of them. So: one model, reused across several
# scoped packages, must be ACCEPTED — the same model attested to different implementations is exactly
# what scoped identity exists to permit, and refusing it would reimpose the CallableId collapse.
#
# Asserted on the shared table with the most reuse, and on the manifest completing at all: if reuse
# were being refused, the generator would exit non-zero and this count would be 1.
REUSED_OK="$(printf '%s' "$OUT" | grep -c '^Concrete.Proof.FnTable.empty <-' || true)"
if [ "$REUSED_OK" -ge 10 ] 2>/dev/null; then
  # ROWS, not packages. $EMPTY_PKGS above is the package count; this count is attestation rows, and
  # they differ because a fixture may attest several declarations. Saying "packages" here was a
  # miscount that reached two commit messages and a source comment before it was caught.
  ok "a reused model is ACCEPTED across $REUSED_OK attestation rows / $EMPTY_PKGS scoped packages (reuse is permitted, not refused)"
else
  no "the most-reused table produced only $REUSED_OK attestations — legitimate reuse is being refused, which reimposes the CallableId collapse"
fi

# THE REPAIRED MISATTACHMENT STAYS REPAIRED.
# 2 -> 1 ON 2026-08-15: the fixture WAS repaired. `proof_pressure` borrowed theorems from two
# packages; the `elf_header` borrowing was a misattachment and its claim is deleted. The remaining
# borrowing is legitimate — its `check_nonce` really is the function `Examples.CryptoVerify` proves,
# byte-identical body and all — so one borrowed table is the correct state, not a residue.
PP_TABLES="$(printf '%s' "$OUT" | grep 'proof_pressure' | awk '{print $1}' | sort -u | grep -c . || true)"
if [ "$PP_TABLES" = "1" ]; then
  ok "proof_pressure attests 1 table (the legitimate cryptoFns reuse; the misattachment is gone)"
else
  no "proof_pressure attests $PP_TABLES table(s) (expected 1) — a borrowed-theorem link changed"
fi

# === CONVERSION RECONCILIATION (package 2) ======================================================
#
# The manifest says which attestations a table SHOULD carry; the table sites say which it DOES. Those
# are two producers and nothing has been joining them, so a conversion could attest four of six rows
# and look exactly like a finished one — the table reports `attested`, `scopedEntryEvidence` returns
# rows, every existing probe passes, and the two missing definitions are simply not described.
#
# So the join is asserted here, per table, and every gap must be a NAMED exclusion. This is also what
# keeps the remaining conversions honest: a table nobody has converted yet must report zero, not a
# partial set that reads as progress.
echo "=== conversion reconciliation ==="

cat > "$TMP/attested.lean" <<'LEAN'
import Concrete
import Examples
open Concrete Concrete.Proof

/-- Every `FnTable` in the build, named as the manifest names it. A table missing from this list
    would be silently exempt from the reconciliation below, which is the failure mode the
    one-producer discipline exists to prevent — so the ONE-PRODUCER gate pins the list too. -/
def attestationSites : List (String × FnTable) :=
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
  for (n, t) in attestationSites do
    IO.println s!"SITE {n} attested={t.attested.size} failures={t.attestationFailures} entries={t.entries.size}"
    -- The identities a site ACTUALLY selected, so a check can ask what was omitted rather than only
    -- what was declared. Package and declaration are enough to name a manifest row.
    for (_, d) in t.attested do
      IO.println s!"BOUND {n} {d.scope} {d.declarationIdentity}"
    -- The declarations this table actually MODELS. A reference for a declaration the table has no
    -- model of cannot be bound at all — `withAttestations` needs a `PFnDef` to bind — and that is
    -- normal: a proof table models the callees a proof unfolds, and the SUBJECT of that proof need
    -- not be among them. Emitting the model names lets the reconciliation separate "cannot bind"
    -- from "chose not to bind", which are different facts with different consequences.
    for d in t.entries do
      match d.identity.id? with
      | some cid => IO.println s!"MODEL {n} {cid.declName}"
      | none     => IO.println s!"MODEL {n} «no-identity»"
LEAN

SITES="$(lake env lean "$TMP/attested.lean" 2>&1)"
if grep -qE "error:|error\(lean" <<<"$SITES"; then
  no "the table-site probe did not elaborate: $(printf '%s' "$SITES" | tr '\n' ' ' | cut -c1-200)"
  SITES=""
else
  ok "every table site reports its attested/entry counts"
fi

site_field() { printf '%s' "$SITES" | grep -E "^SITE $1 " | grep -oE "$2=[0-9]+" | grep -oE '[0-9]+$' || true; }

# NAMED EXCLUSIONS: a manifest row that is deliberately NOT attested, one line each, with the reason.
# The arithmetic below refuses any gap that is not on this list, so an exclusion is a decision that
# has to be written down rather than a number that happens to be smaller.
#
#   elfFns / proof_pressure / validate_header — the known misattachment. `proof_pressure` carries
#   `#[proof_by(Examples.ElfHeader.Proofs.validate_header_correct)]`, so the manifest offers the row,
#   but its `validate_header` calls `check_nonce` and `elfFns` does not hold it: this table's model
#   does not model that implementation. Attestation BINDS and does not check faithfulness, so
#   selecting the reference would make the table describe a function it does not describe, and
#   nothing downstream would catch it.
#
#   ctTagFns / evidence_classes/stale_proof / ct_compare — a DRIFTED implementation. Same linked
#   theorem, but the body starts `diff` at 1 instead of 0, and the compiler says so itself:
#   `--report proof-status` reports SPEC DRIFT, "the theorem is about a different function than the
#   source". Attesting it would bind the model to an implementation it does not model, and nothing
#   would catch it — `scopedEntryEvidence` recomputes the digest of the MODEL's body, never the
#   implementation's. The manifest still offers the row (see the drift-classifier check below).
# The `elfFns` / `proof_pressure` / `validate_header` exclusion was REMOVED on 2026-08-15 — not
# because the rule relaxed, but because the fixture was repaired: its misattached `#[proof_by]` is
# gone, so the manifest no longer offers the row and an exclusion for it would be stale. This is the
# path the "declared exclusion must be a REAL manifest row" check exists to force.
EXCLUSIONS="Concrete.Proof.ctTagFns:f2ce224db4c57094cad39e2b62023610:ct_compare"

for ex in $EXCLUSIONS; do
  extbl="${ex%%:*}"; rest="${ex#*:}"; expkg="${rest%%:*}"; exdecl="${rest##*:}"
  if printf '%s' "$OUT" | grep -q "^$extbl <- $expkg/[^ ]*\.$exdecl "; then
    ok "declared exclusion $extbl/$exdecl is a REAL manifest row (not a stale entry masking a gap)"
  else
    no "declared exclusion $extbl/$exdecl matches no manifest row — either the fixture was repaired (drop the exclusion) or the manifest stopped offering it"
  fi
done

# CONVERTED SET, EXACT. Converting a table is a deliberate step, so it is stated here; a table that
# starts reporting attestations without this list being updated is an unreviewed conversion.
CONVERTED="Concrete.Proof.cryptoFns Concrete.Proof.ctTagFns Concrete.Proof.elfFns Concrete.Proof.fixedCapacityFns Concrete.Proof.parseValidateFns Examples.HmacSha256.Proofs.shaFns Examples.ProofPatterns.Proofs.combineFns"

if [ -n "$SITES" ]; then
  # ANCHORED ON `SITE`. Unanchored, this also matched the `BOUND` lines added later — one line per
  # attestation — so every converted table appeared once per attestation and the set comparison
  # failed with a list that looked like corruption. A filter that happens to work because of what
  # the input does not yet contain is a defect waiting for the input to grow, which it did within
  # the hour.
  ACTUALLY_ATTESTED="$(printf '%s' "$SITES" | grep '^SITE ' | grep -vE 'attested=0 ' | awk '{print $2}' | sort | tr '\n' ' ')"
  if [ "$ACTUALLY_ATTESTED" = "$(printf '%s\n' $CONVERTED | sort | tr '\n' ' ')" ]; then
    ok "the converted set is exactly: $CONVERTED"
  else
    no "converted set is [$ACTUALLY_ATTESTED], expected [$CONVERTED] — an unreviewed conversion, or one that was reverted"
  fi

  for tbl in $(printf '%s' "$OUT" | grep ' <- ' | grep -v EXCLUDED | awk '{print $1}' | sort -u); do
    # THE DISTINCT REFERENCE SET, over BOTH populations. A table site selects generated symbols, and
    # a symbol exists per (table, package, module, declaration, implementation) whether the row that
    # produced it was a subject or a dependency. So the load-bearing invariant is that every
    # reference a table has is either BOUND at its site or a NAMED exclusion — an unbound reference
    # is a definition the table could describe exactly and does not.
    #
    # Counting subject rows against attestations was wrong the moment dependency references were
    # bound: `parseValidateFns` holds 8 attestations against 3 subject rows, which is correct.
    ROWS_T="$(printf '%s' "$OUT" | grep "^$tbl <- " | awk '{print $3, $4}' | sort -u | grep -c . || true)"
    SUBJ_T="$(printf '%s' "$OUT" | grep "^$tbl <- " | grep -vc 'binding_role=dependency' || true)"
    DEP_T="$(printf '%s' "$OUT" | grep "^$tbl <- " | grep -c 'binding_role=dependency' || true)"
    ATT_T="$(site_field "$tbl" attested)"
    ENT_T="$(site_field "$tbl" entries)"
    FAIL_T="$(site_field "$tbl" failures)"
    EXC_T="$(printf '%s\n' $EXCLUSIONS | grep -c "^$tbl:" || true)"
    # STRUCTURALLY UNBINDABLE: a reference for a declaration this table holds no model of. Nothing
    # can be bound to it — `withAttestations` needs a `PFnDef` — and that is not a gap: a proof table
    # models the callees a proof unfolds, and the subject of that proof need not be among them.
    # `calls.combine` is the case: `combineFns` models `inc` and `dbl`, never `combine` itself.
    NOMODEL_T=0
    while IFS= read -r r; do
      [ -n "$r" ] || continue
      rdecl="${r##*.}"
      printf '%s' "$SITES" | grep -qxF "MODEL $tbl $rdecl" || NOMODEL_T=$((NOMODEL_T+1))
    done <<EOF_REFS
$(printf '%s' "$OUT" | grep "^$tbl <- " | awk '{print $3}' | sort -u)
EOF_REFS
    if [ -z "$ATT_T" ]; then
      no "$tbl is in the manifest but not in the table-site list — it would be silently exempt from this reconciliation"
    elif [ "$ENT_T" = "0" ]; then
      # Vacuously attested: no entries, so there is nothing to bind and membership is empty for every
      # callee. The manifest still lists its rows, and they are correctly unattestable — one shared
      # `def` cannot carry one scoped identity per consuming package.
      if [ "$ATT_T" = "0" ]; then
        ok "$tbl has no entries: $ROWS_T reference(s) are vacuously attested, nothing to bind"
      else
        no "$tbl has no entries but carries $ATT_T attestation(s) — it is attesting models it does not hold"
      fi
    elif [ "$ATT_T" = "0" ]; then
      ok "$tbl is PENDING conversion (0 of $ROWS_T references bound; it refuses as legacy, which is honest)"
    elif [ "$FAIL_T" != "0" ]; then
      no "$tbl has $FAIL_T generated reference(s) that failed validation — needs_recheck, not a conversion"
    elif [ "$ROWS_T" = "$(( ATT_T + EXC_T + NOMODEL_T ))" ]; then
      ok "$tbl reconciles: $ROWS_T distinct references ($SUBJ_T subject rows, $DEP_T dependency rows) = $ATT_T bound + $EXC_T named exclusion(s) + $NOMODEL_T with no model in this table"
    else
      no "$tbl does NOT reconcile: $ROWS_T distinct references, $ATT_T bound, $EXC_T named exclusion(s), $NOMODEL_T with no model — an unbound reference to a model the table HOLDS is a definition it could describe exactly and does not"
    fi
  done
fi

# EVERY SUBJECT ROW HAS EXACTLY ONE DISPOSITION, and the four add up to the frozen 48. Conversion
# progress was being described in prose as a per-table list, which drifted the moment a table moved:
# a roadmap paragraph read 14 selected / 1 excluded / 13 vacuous / 20 pending after
# `parseValidateFns` landed, when the measurement was 17/1/13/17.
#
# DECIDED PER ROW BY WHAT THE SITE ACTUALLY BOUND, not by comparing counts. Counting attestations
# against subject rows broke as soon as a table bound DEPENDENCY references too: `parseValidateFns`
# holds 8 attestations against 3 subject rows and read as over-attested, which is not a defect but a
# second population. A row's disposition is a property of that row.
SEL=0; EXC=0; VAC=0; PEND=0; NOMODEL=0
while IFS= read -r row; do
  [ -n "$row" ] || continue
  rtbl="${row%% <-*}"; rrest="${row#* <- }"; rpkg="${rrest%%/*}"
  rmoddecl="${rrest%% *}"; rmoddecl="${rmoddecl#*/}"; rdecl="${rmoddecl#*.}"
  rent="$(site_field "$rtbl" entries)"
  if [ "${rent:-0}" = "0" ]; then VAC=$((VAC+1))
  elif printf '%s\n' $EXCLUSIONS | grep -qx "$rtbl:$rpkg:$rdecl"; then EXC=$((EXC+1))
  elif ! printf '%s' "$SITES" | grep -qxF "MODEL $rtbl $rdecl"; then NOMODEL=$((NOMODEL+1))
  elif printf '%s' "$SITES" | grep -qE "^BOUND $rtbl $rpkg $rdecl$"; then SEL=$((SEL+1))
  else PEND=$((PEND+1)); fi
done <<EOF_ROWS
$(printf '%s' "$OUT" | grep ' <- ' | grep -v EXCLUDED | grep -v 'binding_role=dependency')
EOF_ROWS
if [ "$(( SEL + EXC + VAC + NOMODEL + PEND ))" = "$MAPPED" ]; then
  ok "subject-row disposition reconciles: $SEL selected + $EXC excluded + $VAC vacuous + $NOMODEL no-model + $PEND pending = $MAPPED"
else
  no "subject-row disposition does NOT reconcile: $SEL + $EXC + $VAC + $NOMODEL + $PEND != $MAPPED — a row has two dispositions or none"
fi

# AND NO NAMED EXCLUSION MAY BE A RESTATEMENT OF A STRUCTURAL FACT. An exclusion is a DECISION about
# a reference the table could have bound; if the table holds no model of that declaration, there was
# never anything to decide, and calling it an exclusion would dress a structural impossibility up as
# a judgement — which is how a real misattachment could later hide among them.
for ex in $EXCLUSIONS; do
  extbl="${ex%%:*}"; exdecl="${ex##*:}"
  if printf '%s' "$SITES" | grep -qxF "MODEL $extbl $exdecl"; then
    ok "exclusion $extbl/$exdecl is a real decision: the table HOLDS that model and does not bind it"
  else
    no "exclusion $extbl/$exdecl restates a structural fact — the table has no such model, so nothing was excluded"
  fi
done

# === A DRIFTED IMPLEMENTATION MUST NEVER BE ATTESTED ============================================
#
# The manifest excludes drift fixtures by GREPPING THEIR HEADERS for "DRIFTED variant". That is a
# second, weaker producer of a fact the compiler already computes, and it has a live miss:
# `examples/evidence_classes/stale_proof` declares "the body has DRIFTED", does not match the
# pattern, and its row IS offered for attestation. Attesting it would bind a model to an
# implementation it does not model — and no downstream check would catch that, because
# `scopedEntryEvidence` recomputes the digest of the MODEL's body, never the implementation's.
#
# So the verdict is re-derived from the compiler here: any manifest row whose fixture reports SPEC
# DRIFT must be a DECLARED exclusion at its table site. This does not repair the manifest's
# classifier — that is a package-1 decision — it stops the conversion path from consuming its miss.
echo "=== drifted implementations are never attested ==="

DRIFTED_SRCS=""
for src in $(printf '%s' "$OUT" | grep ' <- ' | grep -v EXCLUDED | sed -E 's/.*\(([^)]*)\)$/\1/' | sort -u); do
  # CAPTURE FIRST, THEN GREP. `if binary … | grep -q` reads correctly and is WRONG under
  # `set -o pipefail`: the compiler exits non-zero on a fixture with a proof defect — which is
  # every fixture this check is looking for — so pipefail returns that failure and the branch is
  # never taken. The first version of this check reported "no fixture reports spec drift" while the
  # same grep matched by hand. A check that answers "nothing found" because it stopped looking is
  # exactly the defect this gate was written for, one level up.
  ps_out="$("$ROOT_DIR/.lake/build/bin/concrete" "$src" --report proof-status 2>&1)"
  case "$ps_out" in *"spec drift"*) DRIFTED_SRCS="$DRIFTED_SRCS $src" ;; esac
done

if [ -n "$DRIFTED_SRCS" ]; then
  ok "the compiler reports spec drift for:$DRIFTED_SRCS (so this check has a live case)"
else
  no "no manifest fixture reports spec drift — either the corpus lost its drift fixture or the report changed, and this check is now vacuous"
fi

for src in $DRIFTED_SRCS; do
  while read -r row; do
    [ -n "$row" ] || continue
    rtbl="${row%% <-*}"; rrest="${row#* <- }"; rpkg="${rrest%%/*}"
    rmoddecl="${rrest%% *}"; rmoddecl="${rmoddecl#*/}"; rdecl="${rmoddecl#*.}"
    # DECLARED and OMITTED are different facts, and only the second is the one that matters. An
    # earlier version of this check asked only whether the row appeared in EXCLUSIONS, which a table
    # could satisfy while ALSO selecting the reference — declared excluded and bound anyway. So the
    # binding is read from the table site itself.
    if printf '%s' "$SITES" | grep -qE "^BOUND $rtbl $rpkg $rdecl$"; then
      no "drifted row $rtbl/$rdecl ($src) IS BOUND at the table site — the model is attested to an implementation the compiler says it does not describe"
    elif printf '%s\n' $EXCLUSIONS | grep -qx "$rtbl:$rpkg:$rdecl"; then
      ok "drifted row $rtbl/$rdecl ($src) is omitted at the table site AND declared as an exclusion"
    else
      no "drifted row $rtbl/$rdecl ($src) is unbound but undeclared — say why it is excluded, or the next conversion will silently pick it up"
    fi
  done <<EOF_ROWS
$(printf '%s' "$OUT" | grep ' <- ' | grep -v EXCLUDED | grep -F "($src)")
EOF_ROWS
done

# === SUBJECT ROWS AND DEPENDENCY REQUESTS =======================================================
#
# The manifest carries TWO populations, kept apart by `binding_role`. `subject` rows are declarations
# a fixture links a proof to; `dependency` rows are the requested BODY-edge callees of those
# subjects — the population correspondence actually joins over, which is not the same set.
#
# THE SUBJECT SECTION IS FROZEN AND BYTE-STABLE. Dependency rows have their own counters and must
# never reach `table_attestations`, or the 86/48 denominator stops meaning anything.
#
# THIS REPLACED A SECOND PRODUCER. An earlier version re-derived the callee population in shell, by
# running the compiler per fixture and grepping `shadow edgeKinds` — the two-answers defect the
# manifest generator exists to avoid, one layer up. It also keyed on (fixture, callee), which is too
# weak: `proof_pressure.check_nonce` has a good identity under `cryptoFns` while the theorem claiming
# `validate_header` requests `elfFns`, so a row elsewhere read as satisfying this request. The key is
# now (consumer, table, callee) and the compiler produces it.
echo "=== subject rows and dependency requests ==="

DEPREQ="$(field dependency_requests)"
DEPATT="$(field dependency_attestations)"
DEPREF="$(field dependency_refusals)"
DEPDUP="$(field dependency_duplicate_mappings)"
DEPCON="$(field dependency_conflicts)"
DEPNOID="$(field dependency_without_identity)"

if [ -n "$DEPREQ" ] && [ "$DEPREQ" -gt 0 ] 2>/dev/null; then
  ok "$DEPREQ dependency requests emitted (the population is live, not an empty section)"
else
  no "no dependency requests — the compiler stopped emitting the dependency role, or the role split broke"
fi

# TYPED RECONCILIATION, exactly as for subject rows: every request ends attested or NAMED-refused.
if [ "$DEPREQ" = "$(( DEPATT + DEPREF ))" ]; then
  ok "every dependency request is accounted for exactly once ($DEPREQ = $DEPATT attested + $DEPREF refused)"
else
  no "dependency requests do not reconcile: $DEPREQ != $DEPATT + $DEPREF — a request was dropped"
fi

# 42 = 41 + 1 -> 41 = 41 + 0. The single refusal was `proof_pressure.validate_header -> elfFns ->
# check_nonce`, and it disappeared with the misattached claim that produced it. The refusal PATH is
# now controlled synthetically in `check_dependency_edges.sh`: a branch whose only corpus case has
# been repaired needs a control, or the next regression restores it silently.
if [ "$DEPREQ" = "41" ] && [ "$DEPATT" = "41" ] && [ "$DEPREF" = "0" ]; then
  ok "dependency population is exactly 41 requests = 41 attested + 0 refused"
else
  no "dependency population moved: $DEPREQ = $DEPATT + $DEPREF, was 41 = 41 + 0 — say which edge appeared or disappeared"
fi

# MODEL-PRESENCE PROVENANCE, THE HONEST VERSION — and it is model presence, not scoped membership,
# which is the evidence join's question. "Attested" is one word for three different claims, and
# the weakest of them — an exact identity whose table could not be read at all — must not be reported
# with the same word as membership verified against a table the compiler holds and recomputed.
#
# `unresolved` is 0 TODAY because `shaFns` was added to the classification generator's external
# tables, so every table a theorem names can now answer membership. That is a real improvement rather
# than a typed excuse: the ignorance was removed instead of being labelled. The path itself stays
# controlled by the unknown-table probes in `check_dependency_edges.sh`, which is where a synthetic
# unreadable table lives — this gate measures the corpus, and the corpus no longer has one.
DEPVC="$(field dependency_model_present_compiler)"
DEPVG="$(field dependency_model_present_asserted)"
DEPUN="$(field dependency_model_unresolved)"
if [ "$DEPATT" = "$(( DEPVC + DEPVG + DEPUN ))" ]; then
  ok "every attestation carries a model-presence state ($DEPATT = $DEPVC compiler-linked + $DEPVG generator-asserted + $DEPUN unresolved)"
else
  no "attestations do not reconcile by model state: $DEPATT != $DEPVC + $DEPVG + $DEPUN"
fi
if [ "$DEPVC" = "18" ] && [ "$DEPVG" = "23" ] && [ "$DEPUN" = "0" ]; then
  ok "model-presence states are exactly 18 compiler-linked / 23 generator-asserted / 0 unresolved"
else
  no "model-presence states moved: $DEPVC / $DEPVG / $DEPUN, was 18/23/0 — a table changed provenance, or one stopped being readable"
fi
# BOTH VERIFIED KINDS MUST BE LIVE. If generator-asserted fell to zero the distinction would be
# untested; if compiler-linked did, the strongest evidence class would have quietly disappeared.
if [ "$DEPVC" -gt 0 ] 2>/dev/null && [ "$DEPVG" -gt 0 ] 2>/dev/null; then
  ok "both model-presence provenances are live, so the distinction is exercised rather than declared"
else
  no "one model-presence provenance has no case ($DEPVC compiler-linked, $DEPVG generator-asserted) — the distinction is untested"
fi

for pair in "dependency_duplicate_mappings:$DEPDUP" "dependency_conflicts:$DEPCON" "dependency_without_identity:$DEPNOID"; do
  k="${pair%%:*}"; v="${pair##*:}"
  if [ "$v" = "0" ]; then ok "$k = 0"; else no "$k = $v — a real defect, not noise"; fi
done

# THE MISATTACHMENT IS REPAIRED, AND ITS ABSENCE IS ASSERTED. `proof_pressure.validate_header` no
# longer claims `Examples.ElfHeader.Proofs.validate_header_correct`; if that link ever returns, the
# manifest offers an `elfFns` row for a `check_nonce` edge again and this catches it.
if printf '%s' "$OUT" | grep -q 'elfFns.*check_nonce'; then
  no "the elfFns/check_nonce request is back — proof_pressure's misattached proof link has returned"
else
  ok "no elfFns/check_nonce request exists (the misattached claim stays deleted)"
fi

# AND AN EXACT REFERENCE IS NOT A JUSTIFICATION. `composition_unlinked_helper`'s `calls.dbl` IS
# attested — exact implementation selection is available to an unlinked helper — while its subject
# must keep refusing, because selection says which implementation an edge points at and never that
# the edge is justified.
if printf '%s' "$OUT" | grep -q 'combineFns <- .*calls.dbl.*binding_role=dependency'; then
  ok "an unlinked helper's callee DOES receive exact selection (identity is available to it)"
else
  no "composition_unlinked_helper/calls.dbl has no dependency attestation — this control lost its case"
fi
# THE CONTROL MOVED TO THE DRIFT FIXTURE, because the misattachment it used to watch was repaired.
# `main_drifted` is a DIFFERENT PROGRAM sharing every declaration name with `elf_header`; every one
# of its four body edges has an exact dependency reference, and none of them corresponds. Exact
# selection says which implementation an edge points at and never that the edge is justified.
DRIFT_SUBJECT_FACTS="$("$ROOT_DIR/.lake/build/bin/concrete" examples/elf_header_drifted/src/main.con --report subject-facts 2>/dev/null || true)"
if printf '%s' "$DRIFT_SUBJECT_FACTS" | grep -q 'shadow correspondence: matched=0 missing=4'; then
  ok "exact dependency selection did NOT make a different program's edges correspond (identity is not justification)"
else
  no "elf_header/main_drifted changed disposition — an identity was taken for a justification"
fi

# EVERY ATTESTATION ROW IS SELECTABLE. References are generated from BOTH attestation sections and
# deduped on (table, package, module, declaration, implementation), so the emitted symbol count must
# equal the number of distinct keys. A shorter file means some row has no reference an author could
# select — the silent gap the generated surface exists to prevent — and a longer one means the
# dedupe is dropping a real distinction.
KEYS="$(printf '%s' "$OUT" | grep ' <- ' | grep -v EXCLUDED | awk '{print $1, $3, $4}' | sort -u | grep -c . || true)"
REFS="$(grep -c '^def ' Concrete/Proof/GeneratedAttestations.lean || true)"
if [ "$REFS" = "$KEYS" ]; then
  ok "generated references cover every distinct (table, package, module, decl, implementation) key: $REFS"
else
  no "generated references are $REFS but the manifest has $KEYS distinct keys — regenerate scripts/gen/attestation_refs.sh"
fi
# NON-VACUITY of the dedupe: the two populations DO overlap, so refs must be fewer than rows. If they
# were equal, either nothing is shared (and the dedupe is untested) or the dedupe stopped running.
ROWS_BOTH="$(printf '%s' "$OUT" | grep -c ' <- ' || true)"
if [ "$KEYS" -lt "$ROWS_BOTH" ] 2>/dev/null; then
  ok "the populations overlap as expected: $ROWS_BOTH attestation rows collapse to $KEYS distinct references"
else
  no "no overlap between subject and dependency attestations ($ROWS_BOTH rows, $KEYS keys) — the dedupe is untested"
fi

echo ""
echo "=== the tracked generated file is what its generator produces ==="
# FRESHNESS, not arity. The check above compares HOW MANY references exist. It cannot see a reference
# whose digest has gone stale, and staleness is the whole failure mode here: every component in that
# file is a hash of something that moves, so the count can stay exactly right while every value in it
# describes implementations that no longer exist. A proof author would then select a reference that
# validates, compiles, and attests the wrong body.
#
# THIS IS WHAT PAYS FOR THE FILE BEING TRACKED AT ALL. `check_callable_identity.sh` refuses tracked
# generated artifacts, because a committed artifact that reads as hand-written erases the line between
# what a human asserted and what a tool emitted — and every provenance claim rests on that line.
# `GeneratedAttestations.lean` is the single narrow exception, for a reason that does not generalise:
# proof authors must `import` and SELECT from it, and Lean cannot import a file that is not on disk.
# The exception is conditional, and this is the condition — the file is re-derived and compared. An
# allowance without it would be the hole, not the gate.
REFS_FILE="Concrete/Proof/GeneratedAttestations.lean"
cp "$REFS_FILE" "$TMP/refs.committed"
# NOTHING HERE WRITES THE SHARED TREE (R-0546). This gate used to regenerate $REFS_FILE in place and
# to append a probe function to examples/hmac_sha256/src/main.con, restoring both afterwards. In a
# parallel pass that is a race, not a restore: while the probe was in place, concurrent gates read a
# mutated example ("'__attestation_probe' is a reserved identifier"), and while the regenerated .lean
# differed, every concurrent `require_fresh_binary` rebuilt the shared compiler from it. The generator
# now writes to ATTESTATION_REFS_OUT, and the perturbation runs in a private copy of the checkout.

# THE GENERATOR'S OUTPUT IS THE REFERENCE VALUE FOR EVERY LEG BELOW — captured once, here, as
# `refs.fresh`. The committed file is a CLAIM about that output and is only used by the staleness leg
# that judges it. Earlier this gate compared the restoration leg against the COMMITTED file, so a
# stale commit failed twice: once truthfully ("the file is stale") and once misleadingly ("restoring
# the source did not restore the references", which was false — the digest was a perfect function of
# content, and the baseline was simply the wrong one). One defect must produce one finding, or the
# count stops meaning anything and a reader starts discounting the gate's arithmetic.
REFS_FRESH_OK=0
if ATTESTATION_REFS_OUT="$TMP/refs.fresh" bash scripts/gen/attestation_refs.sh >/dev/null 2>&1 \
   && [ -s "$TMP/refs.fresh" ]; then
  REFS_FRESH_OK=1
  if cmp -s "$TMP/refs.committed" "$TMP/refs.fresh"; then
    ok "$REFS_FILE is byte-identical to a fresh regeneration"
  else
    DELTA="$(diff "$TMP/refs.committed" "$TMP/refs.fresh" 2>/dev/null | grep -c '^[<>]' || true)"
    no "STALE $REFS_FILE — $DELTA line(s) differ from a fresh regeneration; run scripts/gen/attestation_refs.sh. Until then a tracked reference may attest an implementation that no longer exists."
  fi
else
  no "the reference generator REFUSED — freshness is UNPROVEN, which is not the same as fresh"
fi

# NON-VACUITY. Without this leg, a generator that emitted a constant file, or one whose input query
# had silently gone empty, would satisfy the comparison above forever. A real subject body is
# perturbed so its implementation digest must move, and the emitted file must move with it.
#
# Both legs below compare against `refs.fresh`, so they measure the GENERATOR and stay meaningful
# whether or not the committed file happens to be current. They are skipped, loudly, when the first
# regeneration failed — with no reference value there is nothing to compare against, and reporting
# them as passes would be the vacuity this gate exists to refuse.
PROBE="examples/hmac_sha256/src/main.con"
if [ "$REFS_FRESH_OK" -ne 1 ]; then
  no "the generator refused, so the non-vacuity and restoration controls could not run — their absence is not a pass"
elif [ -f "$PROBE" ]; then
  # A PRIVATE COPY of the checkout (build included, so the copy's compiler is this compiler), in which
  # the probe is applied and reverted; the generator run there writes only to $TMP.
  PCOPY="$TMP/probe-repo"
  cp -a "$ROOT_DIR/." "$PCOPY" 2>/dev/null || true
  rm -rf "$PCOPY/.gate.lock"   # the copy is private: it holds no lock of the run it was copied from
  [ -f "$PCOPY/$PROBE" ] && [ -f "$PCOPY/scripts/gen/attestation_refs.sh" ] \
    || no "could not make a private copy of the checkout for the non-vacuity probe"
  cp "$PCOPY/$PROBE" "$TMP/probe.orig"
  # A COMMENT WOULD NOT DO. Implementation identity is a digest of the compiled body, not of the file
  # text, so a perturbation that the compiler discards proves nothing. This changes a real statement.
  printf '\nfn __attestation_probe(x: u32) -> u32 { return x + 1; }\n' >> "$PCOPY/$PROBE"
  rm -f "$TMP/refs.perturbed"
  ATTESTATION_REFS_OUT="$TMP/refs.perturbed" bash "$PCOPY/scripts/gen/attestation_refs.sh" >/dev/null 2>&1
  if [ ! -s "$TMP/refs.perturbed" ]; then
    no "the generator produced nothing in the private copy — the non-vacuity probe is unproven"
  elif cmp -s "$TMP/refs.fresh" "$TMP/refs.perturbed"; then
    no "the generated references did NOT move when a subject's package content changed — the comparison above is vacuous"
  else
    ok "changing a subject's package content changes the generated references (the comparison is live)"
  fi
  cp -f "$TMP/probe.orig" "$PCOPY/$PROBE"
  # ...and the perturbation is fully reversed: the generator's output returns to its own earlier output.
  rm -f "$TMP/refs.restored"
  ATTESTATION_REFS_OUT="$TMP/refs.restored" bash "$PCOPY/scripts/gen/attestation_refs.sh" >/dev/null 2>&1
  if [ -s "$TMP/refs.restored" ] && cmp -s "$TMP/refs.fresh" "$TMP/refs.restored"; then
    ok "restoring the source restores the references — the digest is a function of content alone"
  else
    no "the references did not return to the generator's own earlier output after restoring the probe — the digest depends on something other than content"
  fi
  rm -rf "$PCOPY"
else
  no "the non-vacuity probe fixture $PROBE is missing — the freshness comparison above is unproven"
fi


echo ""
echo "=== injected live cases: two defensive branches the corpus cannot exercise ==="
# TWO MUTATIONS SURVIVED THIS GATE because the checks they delete have no live case here:
#   manifest-source-population — the scan covers `proof_by|ensures_proof`, but NO fixture carries
#     ensures_proof WITHOUT proof_by, so narrowing the scan finds an identical set
#   manifest-surplus-refused   — surplus_tables is 0, so deleting the refusal changes nothing
#
# Both are negative assertions with no positive control. The cases are INJECTED INTO A DISPOSABLE
# COPY rather than added to the corpus: a real ensures-only fixture or a real surplus would move
# pinned identity denominators for no extra evidence. The injection runs the PRODUCTION generator
# over a REAL input boundary.
INJ="$TMP/inj"; rm -rf "$INJ"; mkdir -p "$INJ"
# Compare the root targets against THEIR BYTES AT INJECTION START. `git diff --quiet` cannot
# answer whether an injection escaped: it also reports an intentional change that predated this
# gate (for example, a freshly regenerated classification table). Pin the two root locations the
# injections could reach instead, so an already-dirty but stable tree is not blamed on this test.
ROOT_CT="$ROOT_DIR/Concrete/Proof/ClassificationTable.lean"
ROOT_PROBE="$ROOT_DIR/examples/zz_postcond_probe"
ROOT_PROBE_ABSENT=0
cp -f "$ROOT_CT" "$INJ/root-classification.before" 2>/dev/null || true
[ ! -e "$ROOT_PROBE" ] && ROOT_PROBE_ABSENT=1
cp -a "$ROOT_DIR/." "$INJ/repo" 2>/dev/null || true
IW="$INJ/repo"
if [ ! -d "$IW/examples" ] || [ ! -f "$IW/scripts/gen/attestation_manifest.sh" ]; then
  no "could not stage an injection copy — both live-case controls below would be vacuous"
else
  # EXIT CODES ARE ASSERTED THROUGHOUT. This gate does not run under `set -e`, so a generator that
  # failed would otherwise continue and its partial output could satisfy the deltas below.
  BASE_OUT="$(cd "$IW" && bash scripts/gen/attestation_manifest.sh 2>"$INJ/base.err")"; BASE_RC=$?
  gf(){ grep -oE "^$1 *= *[0-9]+" <<<"$2" | grep -oE '[0-9]+$' || true; }
  BASE_SCAN="$(gf fixtures_scanned "$BASE_OUT")"; BASE_ROWS="$(gf subject_rows "$BASE_OUT")"
  BASE_UNT="$(gf subjects_without_table "$BASE_OUT")"; BASE_SURP="$(gf surplus_tables "$BASE_OUT")"
  if [ "$BASE_RC" -eq 0 ] && [ "${BASE_SCAN:-0}" -gt 0 ] && [ "${BASE_SURP:-1}" -eq 0 ]; then
    ok "injection baseline: generator exit 0, $BASE_SCAN fixtures scanned, 0 surplus"
  else
    no "injection baseline is not clean (exit=$BASE_RC scanned='$BASE_SCAN' surplus='$BASE_SURP') — deltas below would not be attributable"
  fi

  # ---- 1. an ensures-only subject ---------------------------------------------------------------
  if grep -rqE '#\[ensures_proof\(' "$ROOT_DIR/examples" --include='*.con' 2>/dev/null; then
    ok "injection anchor is fresh: #[ensures_proof(...)] still appears in the real corpus"
  else
    no "no #[ensures_proof(...)] remains in the corpus — the probe imitates nothing real"
  fi
  # THE DIRECTORY NAME MUST NOT CONTAIN "ensures". The mutation narrows the scan with
  # `grep -v ensures`, so a probe under `zz_ensures_only_probe/` would be dropped BY ITS PATH and
  # this control would pass while proving nothing about the attribute. Hence `zz_postcond_probe`,
  # and the narrowed scan below filters on the ATTRIBUTE ONLY.
  PROBE_DIR="$IW/examples/zz_postcond_probe"
  mkdir -p "$PROBE_DIR/src"
  printf '[package]\nname = "zz_postcond_probe"\n' > "$PROBE_DIR/Concrete.toml"
  cat > "$PROBE_DIR/src/main.con" <<'PROBE'
// INJECTED PROBE — a postcondition proof link with NO #[proof_by]. Never a corpus member.
mod main {
    #[ensures_proof(Examples.ProofPatterns.Proofs.addThree_correct)]
    #[ensures(result >= x)]
    pub fn probe(x: i32) -> i32 { if x < 0 { return 0; } return x; }
}
PROBE
  FOUND="$(cd "$IW" && grep -rlE '#\[(proof_by|ensures_proof)\(' examples --include='*.con' 2>/dev/null | grep -c zz_postcond_probe || true)"
  NARROW="$(cd "$IW" && grep -rlE '#\[(proof_by)\(' examples --include='*.con' 2>/dev/null | grep -c zz_postcond_probe || true)"
  if [ "${FOUND:-0}" -eq 1 ] && [ "${NARROW:-1}" -eq 0 ]; then
    ok "the probe discriminates the scan patterns by ATTRIBUTE (production finds it, proof_by-only does not)"
  else
    no "the probe does not discriminate by attribute (production=$FOUND, proof_by-only=$NARROW)"
  fi
  INJ_OUT="$(cd "$IW" && bash scripts/gen/attestation_manifest.sh 2>"$INJ/inj.err")"; INJ_RC=$?
  INJ_SCAN="$(gf fixtures_scanned "$INJ_OUT")"; INJ_ROWS="$(gf subject_rows "$INJ_OUT")"
  INJ_UNT="$(gf subjects_without_table "$INJ_OUT")"
  # DISPOSITION, not merely a scan count. A bumped `fixtures_scanned` proves only that a file was
  # walked; these assert the generator ACCEPTED the subject and placed it in a bucket.
  if [ "$INJ_RC" -eq 0 ] && [ "${INJ_SCAN:-0}" -eq "$(( BASE_SCAN + 1 ))" ] \
     && [ "${INJ_ROWS:-0}" -eq "$(( BASE_ROWS + 1 ))" ] && [ "${INJ_UNT:-0}" -eq "$(( BASE_UNT + 1 ))" ]; then
    ok "the generator accepts the ensures-only subject and buckets it (exit 0; scanned $BASE_SCAN->$INJ_SCAN, rows $BASE_ROWS->$INJ_ROWS, untabled $BASE_UNT->$INJ_UNT)"
  else
    no "the ensures-only subject was not accepted as a row (exit=$INJ_RC scanned $BASE_SCAN->$INJ_SCAN rows $BASE_ROWS->$INJ_ROWS untabled $BASE_UNT->$INJ_UNT)"
  fi
  rm -rf "$PROBE_DIR"

  # ---- 2. surplus, as a LOWER-LEVEL RECONCILIATION attack ---------------------------------------
  # Deliberately distinct from freshness. This does not ask whether the generated file is stale; it
  # makes the COMPILED table the binary attests disagree with the SOURCE rows the manifest greps —
  # the shape a mis-shaped or partially-edited classification produces. Conflating the two would let
  # a freshness pass stand in for surplus handling.
  CT="$IW/Concrete/Proof/ClassificationTable.lean"
  EXPECT_ROWS=2
  ACTUAL_ROWS="$(grep -cE '\("Concrete\.Proof\.ctTagFns", "[0-9a-f]{32}"\)' "$CT" 2>/dev/null || true)"
  if [ "${ACTUAL_ROWS:-0}" -ne "$EXPECT_ROWS" ]; then
    no "expected $EXPECT_ROWS ctTagFns classification row(s), found ${ACTUAL_ROWS:-0} — the surplus injection is not the one described and is vacuous"
  else
    ok "surplus injection anchor is fresh: exactly $EXPECT_ROWS ctTagFns row(s) to rewrite"
    REPL_OK=0
    python3 - "$CT" "$EXPECT_ROWS" <<'PY' && REPL_OK=1
import sys, re, pathlib
p = pathlib.Path(sys.argv[1]); want = int(sys.argv[2]); s = p.read_text()
s2, n = re.subn(r'\("Concrete\.Proof\.ctTagFns", "([0-9a-f]{32})"\)', r'("Zz.Injected.Absent", "\1")', s)
if n != want:
    sys.stderr.write(f"replaced {n}, expected {want}\n"); sys.exit(1)
p.write_text(s2)
PY
    if [ "$REPL_OK" -ne 1 ]; then
      no "the surplus rewrite did not change exactly $EXPECT_ROWS rows — the injection is not the one described"
    else
      ok "the surplus rewrite changed exactly $EXPECT_ROWS row(s)"
      SURP_OUT="$(cd "$IW" && bash scripts/gen/attestation_manifest.sh 2>"$INJ/surp.err")"; SURP_RC=$?
      SURP_N="$(gf surplus_tables "$SURP_OUT")"
      if [ "$SURP_RC" -ne 0 ]; then
        ok "the generator REFUSES (exit $SURP_RC) when a table is attested with no classification row"
      else
        no "the generator exited 0 despite an injected surplus — it emitted a manifest authorising material nothing requested"
      fi
      if grep -qF "table 'Concrete.Proof.ctTagFns' is attested but referenced by no classification row (surplus)" "$INJ/surp.err"; then
        ok "the refusal names the exact table and reason"
      else
        no "the exact named surplus refusal did not appear — the detector is not reporting what it found"
      fi
      if [ "${SURP_N:-0}" -eq 1 ]; then
        ok "surplus_tables reports exactly 1 — the injected table only"
      else
        no "surplus_tables = ${SURP_N:-?}, expected exactly 1 — the injection missed or the generator rejects indiscriminately"
      fi
      if grep -q '^Concrete.Proof.elfFns <- ' <<<"$SURP_OUT"; then
        ok "an unaffected table still maps under the injection (reject-all would fail this)"
      else
        no "no unaffected table mapped — the generator rejected more than the injected row"
      fi
    fi
  fi

  # ---- 3. the real injection targets are untouched ----------------------------------------------
  if [ -f "$INJ/root-classification.before" ] \
     && cmp -s "$INJ/root-classification.before" "$ROOT_CT" \
     && [ "$ROOT_PROBE_ABSENT" -eq 1 ] && [ ! -e "$ROOT_PROBE" ]; then
    ok "the real injection targets are byte-identical afterwards (both injections stayed in the copy)"
  else
    no "a real injection target changed — an injection escaped its copy or its baseline was not clean"
  fi
  rm -rf "$INJ"
fi

echo "ATTESTATION-MANIFEST: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
