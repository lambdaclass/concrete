#!/usr/bin/env bash
# IDENTITY ROWS — R-0482's frozen canonical rows, and proof that freezing rows (not counts) matters.
#
# `check_identity_freeze.sh` pins populations. This gate pins ASSOCIATIONS: which subject is attested
# under which table, which model each consumer pairs with which reference, which callee each body edge
# names, which fingerprint each stored link carries (`scripts/tools/identity_rows.sh`). The R-0482
# migration must reproduce these rows or account for each one in its typed old-to-new map.
#
# Three controls run in a scratch copy, so the working tree is never touched:
#   A. swapping two consumers' models keeps every frozen COUNT and must still move the rows — the
#      reason rows exist;
#   B. an unused std edit (with the consumer renames today's scheme forces) must move NO row — the
#      rows exclude the over-bound package bytes, so a std edit cannot train people to regenerate;
#   C. the same std edit without those renames must be REFUSED — a consumer selecting a stale symbol
#      is never silently absorbed.
#
# A legitimate change (a proved body, theorem, table or fingerprint moves) regenerates the frozen file
# in the same commit and explains every changed row:
#   bash scripts/tools/identity_rows.sh > docs/verification/identity-rows-pre-r0482.tsv
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
FROZEN="docs/verification/identity-rows-pre-r0482.tsv"
BIN="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$BIN" ] || { echo "error: build first ($BIN missing)" >&2; exit 2; }
[ -f "$FROZEN" ] || { echo "FATAL: $FROZEN missing" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== the frozen rows ==="
if bash scripts/tools/identity_rows.sh > "$TMP/now.tsv" 2> "$TMP/now.err"; then
  ok "the row producer ran without refusal"
else
  no "the row producer refused: $(tail -3 "$TMP/now.err" | tr '\n' ' ')"
fi
for k in subject:80 dependency:40 reference:60 selection:40 link:60 scope:20; do
  kind="${k%%:*}"; floor="${k##*:}"
  n="$(grep -c "^$kind	" "$FROZEN" || true)"
  [ "$n" -ge "$floor" ] && ok "frozen file holds $n $kind rows" \
    || no "frozen file holds only $n $kind rows (floor $floor) — the freeze is vacuous"
done
if diff "$FROZEN" "$TMP/now.tsv" > "$TMP/drift.diff"; then
  ok "every row matches the freeze ($(grep -vc '^#' "$FROZEN") rows)"
else
  no "rows moved ($(grep -c '^[<>]' "$TMP/drift.diff") diff lines). A legitimate change regenerates the freeze in the same commit and explains each row; the R-0482 migration maps each one:"
  head -20 "$TMP/drift.diff" | sed 's/^/         /'
fi

# --- scratch copy: tracked + untracked-unignored files, plus the executable ----------------------------
W="$TMP/repo"; mkdir -p "$W/.lake/build"
git ls-files -co --exclude-standard -z | (cd "$ROOT_DIR" && xargs -0 tar cf -) | (cd "$W" && tar xf -) \
  || { no "could not copy the repository"; echo "IDENTITY-ROWS: PASS=$PASS FAIL=$FAIL"; exit 1; }
# A COPY of the executable, not a link: the compiler finds std relative to `IO.appPath`, so a linked
# binary would read the ORIGINAL tree's std and the std-edit controls would measure nothing.
# `cp -c` clones on APFS; elsewhere it is a plain copy.
mkdir -p "$W/.lake/build/bin"
cp -c "$BIN" "$W/.lake/build/bin/concrete" 2>/dev/null || cp "$BIN" "$W/.lake/build/bin/concrete"
snapshot(){ (cd "$W" && tar cf "$TMP/pristine.tar" Concrete/Proof/Proof.lean Concrete/Proof/GeneratedAttestations.lean std/src/base64.con proofs); }
restore(){ (cd "$W" && tar xf "$TMP/pristine.tar"); }
snapshot

echo ""
echo "=== control A: a model swap keeps every count and must move the rows ==="
perl -0pi -e 's/AttestedPFnDef\.of checkNonceFn(\s+GeneratedAttestations\.cryptoFns_([0-9a-f]{8})_check_nonce)(.*?)AttestedPFnDef\.of computeTagFn(\s+GeneratedAttestations\.cryptoFns_\2_compute_tag)/AttestedPFnDef.of computeTagFn$1$3AttestedPFnDef.of checkNonceFn$4/s' "$W/Concrete/Proof/Proof.lean"
if cmp -s "$W/Concrete/Proof/Proof.lean" "$ROOT_DIR/Concrete/Proof/Proof.lean"; then
  no "control A could not apply its swap — the anchor moved, so the control is vacuous"
else
  (cd "$W" && bash scripts/tests/check_identity_freeze.sh > "$TMP/a.freeze" 2>&1)
  FREEZE_A=$?
  (cd "$W" && bash scripts/tools/identity_rows.sh > "$TMP/a.tsv" 2>/dev/null)
  MOVED_A="$(diff "$FROZEN" "$TMP/a.tsv" | grep -c '^[<>]' || true)"
  if [ "$FREEZE_A" -eq 0 ] && [ "$MOVED_A" -gt 0 ]; then
    ok "the swap passes every count in IDENTITY-FREEZE yet moves $MOVED_A row lines — rows see what counts cannot"
  elif [ "$FREEZE_A" -ne 0 ]; then
    no "the swap moved a count, so it no longer demonstrates the count-blind case: $(tail -1 "$TMP/a.freeze")"
  else
    no "a model swap moved NO row — the selection rows do not bind the model/reference pairing"
  fi
fi
restore

echo ""
echo "=== controls B and C: an unused std edit ==="
cp "$W/Concrete/Proof/GeneratedAttestations.lean" "$TMP/gen.before"
(cd "$W" && ATTESTATION_JOIN_ROWS_OUT="$TMP/raw.before" bash scripts/gen/attestation_manifest.sh > /dev/null 2>&1)
perl -0pi -e 's/if v == 62 \{ return 43; \}/if v == 62 { return 43; } if v == 63 { return 47; }/' "$W/std/src/base64.con"
(cd "$W" && bash scripts/gen/attestation_refs.sh > /dev/null 2>&1)
(cd "$W" && ATTESTATION_JOIN_ROWS_OUT="$TMP/raw.after" bash scripts/gen/attestation_manifest.sh > /dev/null 2>&1)
ids(){ grep -oE '^def [A-Za-z0-9_]+' "$1" | awk '{print $2}' | sort; }
RENAMED="$(comm -23 <(ids "$TMP/gen.before") <(ids "$W/Concrete/Proof/GeneratedAttestations.lean") | grep -c . || true)"
if [ "$RENAMED" -eq 0 ]; then
  no "the std edit renamed no generated symbol — control C would be vacuous (if R-0482 has landed, convert B/C to their post-migration form)"
else
  if (cd "$W" && bash scripts/tools/identity_rows.sh > /dev/null 2>&1); then
    no "C: consumers still select the pre-edit symbols, yet the row producer accepted them"
  else
    ok "C: with consumers left on stale symbols the row producer refuses (it never absorbs them)"
  fi
  # The renames today's scheme forces: old 8-hex package prefix -> new, per symbol, by matching the
  # symbol with its prefix removed. Pairing is by the symbol's other parts, which the edit does not move.
  # The renames today's scheme forces, paired BY SOURCE FIXTURE from the compiler's own rows (the
  # pairing the recorded migrations used). Pairing by symbol shape is ambiguous: `cryptoFns_@_check_nonce`
  # exists in two packages, and one of them has no other symbol to disambiguate it by.
  pkgs(){ awk -F'\t' '$1=="subject" && $3!="-" {print $NF "\t" substr($3, 1, 8)}' "$1" | sort -u; }
  join -t$'\t' <(pkgs "$TMP/raw.before") <(pkgs "$TMP/raw.after") \
    | awk -F'\t' '$2 != $3 {print "_" $2 "_\t_" $3 "_"}' | sort -u > "$TMP/prefixes.tsv"
  if [ "$(cut -f1 "$TMP/prefixes.tsv" | uniq -d | grep -c . || true)" -gt 0 ]; then
    no "B: an old package prefix maps to two new ones — the rename map is ambiguous"
  fi
  while IFS=$'\t' read -r op np; do
    grep -rl -- "$op" "$W/Concrete/Proof/Proof.lean" "$W/proofs" 2>/dev/null | xargs perl -pi -e "s/$op/$np/g"
  done < "$TMP/prefixes.tsv"
  if (cd "$W" && bash scripts/tools/identity_rows.sh > "$TMP/b.tsv" 2> "$TMP/b.err"); then
    MOVED_B="$(diff "$FROZEN" "$TMP/b.tsv" | grep -c '^[<>]' || true)"
    if [ "$MOVED_B" -eq 0 ]; then
      ok "B: the std edit renamed $RENAMED symbols across $(wc -l < "$TMP/prefixes.tsv" | tr -d ' ') packages and moved NO row"
    else
      no "B: an unused std edit moved $MOVED_B row lines — over-bound bytes have leaked into the rows"
    fi
  else
    no "B: after the forced renames the row producer still refused: $(tail -2 "$TMP/b.err" | tr '\n' ' ')"
  fi
fi
restore

echo ""
echo "IDENTITY-ROWS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
