#!/usr/bin/env bash
# IDENTITY ROWS — the canonical full rows R-0482's migration must reproduce (or map, row by row).
#
# WHY ROWS, NOT COUNTS. `check_identity_freeze.sh` pins populations (63 references, 21 packages, 55
# keys). A rewrite can keep every `sort -u` count while moving a reference to another declaration,
# re-pointing a consumer at another package, or swapping two implementations — the counts cannot see
# an association change. These rows can: every row names what is attested, where, under which table,
# and who selects it.
#
# WHY NO PACKAGE DIGESTS IN THE ROWS. Under today's scheme an unused std edit moves 7 package digests
# (`check_identity_overbinding.sh`), and so every definition digest and every generated symbol that
# embeds them. Freezing those bytes would turn every std edit into a freeze update and train people to
# regenerate without reading. Instead each package digest is replaced by the LABEL of the one fixture
# that produces it, and the tool REFUSES unless that correspondence is a bijection — so the rows stay
# exact about associations while the over-bound bytes stay out. A label is a repository path: it is a
# test-inventory key, never an identity.
#
# WHAT A DIFF MEANS. Rows move legitimately when a proved body, theorem, table or fingerprint changes;
# those commits regenerate this file and explain each changed row. The R-0482 migration must not move
# any row except through its typed old-to-new map.
#
# Usage: scripts/tools/identity_rows.sh            (rows to stdout; refusals to stderr, exit 1)
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
export LC_ALL=C
[ -x .lake/build/bin/concrete ] || { echo "FATAL: build first (.lake/build/bin/concrete missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
REFUSED=0
refuse(){ echo "REFUSE: $1" >&2; REFUSED=$((REFUSED+1)); }

# The compiler's rows, through the manifest's own corpus enumeration (one scan, one drift classifier).
ATTESTATION_JOIN_ROWS_OUT="$TMP/raw.tsv" bash scripts/gen/attestation_manifest.sh > /dev/null 2> "$TMP/man.err" \
  || { cat "$TMP/man.err" >&2; echo "FATAL: the attestation manifest refused" >&2; exit 1; }
[ -s "$TMP/raw.tsv" ] || { echo "FATAL: the manifest produced no compiler rows" >&2; exit 1; }

# package digest <-> fixture label, from subject rows. Must be a bijection.
awk -F'\t' '$1=="subject" && $3!="-" {print $3"\t"$NF}' "$TMP/raw.tsv" | sort -u > "$TMP/pkgmap.tsv"
while read -r p; do refuse "package $p is produced by more than one fixture"; done \
  < <(cut -f1 "$TMP/pkgmap.tsv" | uniq -d)
while read -r l; do refuse "fixture $l produces more than one package digest"; done \
  < <(cut -f2 "$TMP/pkgmap.tsv" | sort | uniq -d)

label(){ # label <pkg digest> -> fixture label, or a visible UNMAPPED marker (refused)
  local l
  l="$(awk -F'\t' -v p="$1" '$1==p {print $2; exit}' "$TMP/pkgmap.tsv")"
  if [ -n "$l" ]; then printf '%s' "$l"; else printf 'UNMAPPED:%s' "$1"; fi
}

{
  # scope rows: one per fixture that produced a package
  cut -f2 "$TMP/pkgmap.tsv" | sort -u | awk '{print "scope\t" $0}'

  # subject rows: every claiming subject, including those with no table or no identity
  awk -F'\t' -v OFS='\t' '$1=="subject" {
      reason = (NF >= 10) ? $9 : "-"
      print "subject", $NF, $4, $5, $6, $7, $8, reason
    }' "$TMP/raw.tsv"

  # dependency rows: consumer by NAME (its digest embeds the package digest), callee by label
  while IFS=$'\t' read -r -a f; do
    n=${#f[@]}; src="${f[$((n-1))]}"
    cpkg="${f[8]:--}"
    if [ "$cpkg" = "-" ]; then clabel="-"; else clabel="$(label "$cpkg")"; fi
    printf 'dependency\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
      "$src" "${f[3]}" "${f[4]}" "${f[5]}" "${f[6]}" "$clabel" "${f[9]:--}" "${f[10]:--}" "${f[11]:--}"
  done < <(awk -F'\t' '$1=="dependency"' "$TMP/raw.tsv")
} > "$TMP/rows.tsv"

# reference rows: every generated symbol, its package digest replaced by its fixture label
REFS="Concrete/Proof/GeneratedAttestations.lean"
awk '
  /^def [A-Za-z0-9_]+ :/ { sym = $2; next }
  sym != "" && /DefinitionIdentity\.of\?/ {
    n = split($0, q, "\""); print sym "\t" q[2] "\t" q[4] "\t" q[6] "\t" q[8]; sym = ""
  }' "$REFS" > "$TMP/refs.tsv"
: > "$TMP/symmap.tsv"
while IFS=$'\t' read -r sym pkg mod decl impl; do
  l="$(label "$pkg")"
  tmpl="${sym/_${pkg:0:8}_/_@_}"
  [ "$tmpl" != "$sym" ] || refuse "symbol $sym does not embed its package prefix ${pkg:0:8}"
  printf '%s\t%s\t%s\n' "$sym" "$tmpl" "$l" >> "$TMP/symmap.tsv"
  printf 'reference\t%s\t%s\t%s\t%s\t%s\n' "$tmpl" "$l" "$mod" "$decl" "$impl" >> "$TMP/rows.tsv"
done < "$TMP/refs.tsv"
[ "$(wc -l < "$TMP/refs.tsv")" -ge 40 ] || refuse "parsed only $(wc -l < "$TMP/refs.tsv") generated references"

# selection rows: every consumer site pairing a MODEL with a generated reference, with the Lean
# declaration that holds the pairing. The pairing is the association a migration could silently
# change (model A attested under B's reference) while every count stays the same.
for f in Concrete/Proof/Proof.lean $(grep -rl 'GeneratedAttestations\.' proofs 2>/dev/null | sort); do
  awk -v file="$f" '
    /^(private |protected |noncomputable )*(def|theorem|abbrev|instance) / {
      n = split($0, w, /[ \t:(]+/); for (i = 1; i <= n; i++) if (w[i] ~ /^(def|theorem|abbrev|instance)$/) { encl = w[i+1]; break } }
    {
      line = $0
      while (match(line, /AttestedPFnDef\.of +[A-Za-z0-9_.]+ +[A-Za-z.]*GeneratedAttestations\.[A-Za-z0-9_]+/)) {
        m = substr(line, RSTART, RLENGTH); line = substr(line, RSTART + RLENGTH)
        split(m, t, / +/); sym = t[3]; sub(/.*GeneratedAttestations\./, "", sym)
        print file "\t" encl "\t" t[2] "\t" sym
      }
    }' "$f"
done | while IFS=$'\t' read -r file encl model sym; do
  m="$(awk -F'\t' -v s="$sym" '$1==s {print $2 "\t" $3; exit}' "$TMP/symmap.tsv")"
  if [ -z "$m" ]; then refuse "$file selects $sym, which is not a generated reference"; m="UNRESOLVED:$sym	-"; fi
  printf 'selection\t%s\t%s\t%s\t%s\n' "$file" "$encl" "$model" "$m"
done >> "$TMP/rows.tsv"
N_SEL_SITES="$(grep -rhoE 'GeneratedAttestations\.[A-Za-z0-9_]+' Concrete/Proof/Proof.lean proofs 2>/dev/null | grep -c . || true)"
N_SEL_ROWS="$(grep -c '^selection' "$TMP/rows.tsv" || true)"
[ "$N_SEL_SITES" = "$N_SEL_ROWS" ] || refuse "$N_SEL_SITES selection sites but $N_SEL_ROWS model pairings parsed — a selection outside AttestedPFnDef.of is unaccounted"

# link rows: stored proof links (subject-keyed; the migration must not touch them)
grep -rlE --include='*.con' 'proof_fingerprint\("' examples tests std 2>/dev/null | sort | while read -r f; do
  awk -v file="$f" '
    /proof_fingerprint\("/ { match($0, /proof_fingerprint\("[^"]*"\)/); fp = substr($0, RSTART+19, RLENGTH-21); next }
    fp != "" && /(^|[ \t])fn [A-Za-z_][A-Za-z0-9_]*/ {
      match($0, /fn [A-Za-z_][A-Za-z0-9_]*/); print "link\t" file "\t" substr($0, RSTART+3, RLENGTH-3) "\t" fp; fp = "" }' "$f"
done >> "$TMP/rows.tsv"

grep -q UNMAPPED "$TMP/rows.tsv" && refuse "a row names a package digest no fixture produces"
# refusals raised inside the selection pipeline run in a subshell, so re-derive them from the rows
grep -q "UNRESOLVED:" "$TMP/rows.tsv" && refuse "a consumer selects a symbol that is not a generated reference"
[ "$REFUSED" -eq 0 ] || { echo "IDENTITY-ROWS: refused ($REFUSED)" >&2; exit 1; }

echo "# identity-rows v1 — generated by scripts/tools/identity_rows.sh; do not edit by hand"
echo "# kinds: scope<label> | subject<label,module,decl,impl,theorem,tables,refusal>"
echo "#        dependency<label,consumer,table,disposition,kind,callee-label,module,decl,impl>"
echo "#        reference<symbol(@=package),label,module,decl,impl>"
echo "#        selection<file,enclosing-decl,model,symbol,label>"
echo "#        link<file,fn,fingerprint>"
sort "$TMP/rows.tsv"
