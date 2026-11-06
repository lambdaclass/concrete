#!/usr/bin/env bash
# IDENTITY ROWS — the canonical rows R-0482's migration must reproduce or map, row by row.
#
# TWO STAGES, TWO FILES.
#   raw        every identity row with its digest bytes intact: package digests, definition digests,
#              generated symbol names. The exact migration map is written against THESE bytes, so the
#              baseline is retained (`docs/verification/identity-raw-baseline-pre-r0482.tsv`), tied to
#              its commit. It is a record of the pre-migration scheme, not a property of later trees.
#   normalized the same rows with each package digest replaced by the LABEL of the fixture producing
#              it (`docs/verification/identity-rows-pre-r0482.tsv`). This is what the gate holds every
#              tree to: it is exact about ASSOCIATIONS but immune to today's over-binding, where an
#              unused std edit moves 7 package digests and every symbol embedding them.
#
# WHY ROWS, NOT COUNTS. `check_identity_freeze.sh` pins populations. A rewrite can keep every
# `sort -u` count while re-pairing a model with another reference or moving a reference to another
# declaration; rows see that.
#
# THE BIJECTION IS A BASELINE PROPERTY. Normalization requires each package digest to come from
# exactly one fixture and each fixture to produce one digest — true of the pre-migration scheme and the
# precondition for labelling. It is NOT a requirement on migrated identities: under the ratified
# scheme, legitimately shared scopes may make it false, and the migration map is judged against the
# raw baseline instead.
#
# A label is a repository path: a test-inventory key, never an identity.
#
# Usage:
#   scripts/tools/identity_rows.sh                  normalized rows of this tree
#   scripts/tools/identity_rows.sh --raw            raw rows of this tree
#   scripts/tools/identity_rows.sh --normalize F    normalized rows from a raw file F
# Refusals go to stderr with exit 1.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
export LC_ALL=C
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
REFUSED=0
refuse(){ echo "REFUSE: $1" >&2; REFUSED=$((REFUSED+1)); }

# --- stage 1: raw rows -----------------------------------------------------------------------------
raw_rows() {
  [ -x .lake/build/bin/concrete ] || { echo "FATAL: build first (.lake/build/bin/concrete missing)" >&2; exit 2; }
  # The compiler's rows, through the manifest's own corpus enumeration (one scan, one drift classifier).
  ATTESTATION_JOIN_ROWS_OUT="$TMP/join.tsv" bash scripts/gen/attestation_manifest.sh > /dev/null 2> "$TMP/man.err" \
    || { cat "$TMP/man.err" >&2; echo "FATAL: the attestation manifest refused" >&2; exit 1; }
  [ -s "$TMP/join.tsv" ] || { echo "FATAL: the manifest produced no compiler rows" >&2; exit 1; }

  {
    awk -F'\t' -v OFS='\t' '$1=="subject" && $3!="-" {print "fixture", $NF, $3}' "$TMP/join.tsv" | sort -u
    # subject: label, definition digest, package digest, module, decl, implementation, theorem, tables, refusal
    awk -F'\t' -v OFS='\t' '$1=="subject" {
        reason = (NF >= 10) ? $9 : "-"
        print "subject", $NF, $2, $3, $4, $5, $6, $7, $8, reason }' "$TMP/join.tsv"
    # dependency: label, consumer digest, consumer name, table, disposition, kind,
    #             callee definition digest, callee package digest, module, decl, implementation
    awk -F'\t' -v OFS='\t' '$1=="dependency" {
        src = $NF
        for (i = 8; i <= 12; i++) if (i >= NF || $i == "") $i = "-"
        print "dependency", src, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12 }' "$TMP/join.tsv"
  } > "$TMP/raw.tsv"

  # reference: generated symbol, package digest, module, decl, implementation
  awk -v OFS='\t' '
    /^def [A-Za-z0-9_]+ :/ { sym = $2; next }
    sym != "" && /DefinitionIdentity\.of\?/ { split($0, q, "\""); print "reference", sym, q[2], q[4], q[6], q[8]; sym = "" }
  ' Concrete/Proof/GeneratedAttestations.lean > "$TMP/refs.tsv"
  [ "$(wc -l < "$TMP/refs.tsv")" -ge 40 ] || refuse "parsed only $(wc -l < "$TMP/refs.tsv") generated references"
  cat "$TMP/refs.tsv" >> "$TMP/raw.tsv"

  # selection: file, holding declaration, model, generated symbol — every AttestedPFnDef.of pairing
  for f in Concrete/Proof/Proof.lean $(grep -rl 'GeneratedAttestations\.' proofs 2>/dev/null | sort); do
    awk -v file="$f" -v OFS='\t' '
      /^(private |protected |noncomputable )*(def|theorem|abbrev|instance) / {
        n = split($0, w, /[ \t:(]+/); for (i = 1; i <= n; i++) if (w[i] ~ /^(def|theorem|abbrev|instance)$/) { encl = w[i+1]; break } }
      {
        line = $0
        while (match(line, /AttestedPFnDef\.of +[A-Za-z0-9_.]+ +[A-Za-z.]*GeneratedAttestations\.[A-Za-z0-9_]+/)) {
          m = substr(line, RSTART, RLENGTH); line = substr(line, RSTART + RLENGTH)
          split(m, t, / +/); sym = t[3]; sub(/.*GeneratedAttestations\./, "", sym)
          print "selection", file, encl, t[2], sym
        }
      }' "$f"
  done >> "$TMP/raw.tsv"
  local sites rows
  sites="$(grep -rhoE 'GeneratedAttestations\.[A-Za-z0-9_]+' Concrete/Proof/Proof.lean proofs 2>/dev/null | grep -c . || true)"
  rows="$(grep -c '^selection' "$TMP/raw.tsv" || true)"
  [ "$sites" = "$rows" ] || refuse "$sites selection sites but $rows model pairings parsed — a selection outside AttestedPFnDef.of is unaccounted"

  # link: file, function, stored fingerprint (subject-keyed; the migration must not touch them)
  grep -rlE --include='*.con' 'proof_fingerprint\("' examples tests std 2>/dev/null | sort | while read -r f; do
    awk -v file="$f" '
      /proof_fingerprint\("/ { match($0, /proof_fingerprint\("[^"]*"\)/); fp = substr($0, RSTART+19, RLENGTH-21); next }
      fp != "" && /(^|[ \t])fn [A-Za-z_][A-Za-z0-9_]*/ {
        match($0, /fn [A-Za-z_][A-Za-z0-9_]*/); print "link\t" file "\t" substr($0, RSTART+3, RLENGTH-3) "\t" fp; fp = "" }' "$f"
  done >> "$TMP/raw.tsv"

  [ "$REFUSED" -eq 0 ] || { echo "IDENTITY-ROWS: refused ($REFUSED)" >&2; exit 1; }
  echo "# identity-raw v1 — generated by scripts/tools/identity_rows.sh --raw; do not edit by hand"
  echo "# kinds: fixture<label,package> | subject<label,definition,package,module,decl,impl,theorem,tables,refusal>"
  echo "#        dependency<label,consumer-definition,consumer,table,disposition,kind,definition,package,module,decl,impl>"
  echo "#        reference<symbol,package,module,decl,impl> | selection<file,holding-decl,model,symbol>"
  echo "#        link<file,fn,fingerprint>"
  grep -v '^#' "$TMP/raw.tsv" | sort
}

# --- stage 2: normalize (package digest -> producing fixture) ----------------------------------------
normalize() { # normalize <raw file>
  local raw="$1"
  grep -v '^#' "$raw" > "$TMP/in.tsv"
  awk -F'\t' '$1=="fixture" {print $3 "\t" $2}' "$TMP/in.tsv" | sort -u > "$TMP/pkgmap.tsv"
  [ -s "$TMP/pkgmap.tsv" ] || { echo "FATAL: no fixture rows in $raw" >&2; exit 1; }
  while read -r p; do refuse "package $p is produced by more than one fixture (baseline bijection)"; done \
    < <(cut -f1 "$TMP/pkgmap.tsv" | uniq -d)
  while read -r l; do refuse "fixture $l produces more than one package digest (baseline bijection)"; done \
    < <(cut -f2 "$TMP/pkgmap.tsv" | sort | uniq -d)

  awk -F'\t' -v OFS='\t' '
    FNR == NR { lab[$1] = $2; next }
    function L(p) { return (p == "-") ? "-" : ((p in lab) ? lab[p] : "UNMAPPED:" p) }
    function T(sym, p,   t) { t = sym; if (!sub("_" substr(p, 1, 8) "_", "_@_", t)) t = "NOPREFIX:" sym; return t }
    $1 == "fixture"    { print "scope", $2 }
    $1 == "subject"    { print "subject", $2, $5, $6, $7, $8, $9, $10 }
    $1 == "dependency" { print "dependency", $2, $4, $5, $6, $7, L($9), $10, $11, $12 }
    $1 == "reference"  { tm[$2] = T($2, $3); tl[$2] = L($3); print "reference", tm[$2], tl[$2], $4, $5, $6 }
    $1 == "selection"  { sel[++ns] = $0 }
    $1 == "link"       { print }
    END {
      for (i = 1; i <= ns; i++) {
        split(sel[i], f, "\t")
        if (f[5] in tm) print "selection", f[2], f[3], f[4], tm[f[5]], tl[f[5]]
        else            print "selection", f[2], f[3], f[4], "UNRESOLVED:" f[5], "-"
      }
    }' "$TMP/pkgmap.tsv" "$TMP/in.tsv" > "$TMP/rows.tsv"

  grep -q 'UNMAPPED:'   "$TMP/rows.tsv" && refuse "a row names a package digest no fixture produces"
  grep -q 'NOPREFIX:'   "$TMP/rows.tsv" && refuse "a generated symbol does not embed its package prefix"
  grep -q 'UNRESOLVED:' "$TMP/rows.tsv" && refuse "a consumer selects a symbol that is not a generated reference"
  [ "$REFUSED" -eq 0 ] || { echo "IDENTITY-ROWS: refused ($REFUSED)" >&2; exit 1; }

  echo "# identity-rows v1 — generated by scripts/tools/identity_rows.sh; do not edit by hand"
  echo "# kinds: scope<label> | subject<label,module,decl,impl,theorem,tables,refusal>"
  echo "#        dependency<label,consumer,table,disposition,kind,callee-label,module,decl,impl>"
  echo "#        reference<symbol(@=package),label,module,decl,impl>"
  echo "#        selection<file,enclosing-decl,model,symbol,label>"
  echo "#        link<file,fn,fingerprint>"
  sort "$TMP/rows.tsv"
}

case "${1:-}" in
  --raw)       raw_rows ;;
  --normalize) [ -r "${2:-}" ] || { echo "usage: $0 --normalize <raw file>" >&2; exit 2; }; normalize "$2" ;;
  "")          raw_rows > "$TMP/self.raw" || exit $?; normalize "$TMP/self.raw" ;;
  *)           echo "usage: $0 [--raw | --normalize <raw file>]" >&2; exit 2 ;;
esac
