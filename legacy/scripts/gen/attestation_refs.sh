#!/usr/bin/env bash
# Emit `Concrete/Proof/GeneratedAttestations.lean` from the attestation manifest.
#
# These are the OPAQUE TYPED REFERENCES a proof author selects. Nobody transcribes a package identity
# or a digest: hand-copied digests go stale silently, and a hand-written package name is the collision
# scoped identity exists to remove.
#
# `Except`-typed, deliberately. `DefinitionIdentity.of?` validates and its constructor is private, so
# a reference that fails validation stays a REFUSAL rather than becoming a value — the `needs_recheck`
# disposition, not a default.
#
# One symbol per (table, package, declaration), because a table reused across packages needs one
# attestation PER PACKAGE and collapsing them is what the scoped identity exists to prevent.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

# TEST SEAMS, both unset in normal use, so the default path is exactly what it was.
# `check_attestation_refs_order.sh` needs to run THIS generator — not a reimplementation
# of its emit logic — over two permutations of one manifest and compare the results. It
# cannot do that without supplying the input and diverting the output, and a gate that
# reimplements the producer tests the reimplementation. Reading a manifest from a file
# does not weaken the real path: the refusal below still applies when the seam is unused.
MAN_FILE="${ATTESTATION_MANIFEST_FILE:-}"
if [ -n "$MAN_FILE" ]; then
  [ -r "$MAN_FILE" ] || { echo "FATAL: ATTESTATION_MANIFEST_FILE is not readable: $MAN_FILE" >&2; exit 1; }
  MAN="$(cat "$MAN_FILE")"
else
  MAN="$(bash scripts/gen/attestation_manifest.sh 2>/dev/null)" || {
    echo "FATAL: the manifest refused; refusing to emit references from it" >&2; exit 1; }
fi

OUT="${ATTESTATION_REFS_OUT:-Concrete/Proof/GeneratedAttestations.lean}"
{
  echo "import Concrete.Proof.DefinitionIdentity"
  echo
  echo "/-!"
  echo "# Generated attestation references — do not edit"
  echo
  echo "Emitted by \`scripts/gen/attestation_refs.sh\` from the attestation manifest, which is itself"
  echo "derived from compiler-produced subject facts. A proof author SELECTS one of these symbols; the"
  echo "components are never written by hand."
  echo
  echo "\`Except\`-typed because \`of?\` validates and the constructor is private: a reference that fails"
  echo "validation stays a refusal rather than becoming a value."
  echo "-/"
  echo
  echo "namespace Concrete.Proof.GeneratedAttestations"
  echo
  # BOTH ATTESTATION SECTIONS. Subject rows and dependency attestations are different populations
  # with different meanings — one is a proof-linked declaration, the other is the exact implementation
  # a body edge points at — but an author selects a reference the same way for either, so the surface
  # is one file. Refusal rows (`!!`) are not references and are not emitted: a request the manifest
  # refused has nothing to select.
  #
  # DEDUPED on (table, package, module, declaration, implementation). The same callee is legitimately
  # both a subject of its table and the target of a body edge, and emitting the symbol twice would not
  # compile. Deduping on anything less would silently drop a real distinction: two packages, or two
  # implementations of one declaration, are different references and must both exist.
  # SORTED BEFORE DEDUPE, under `LC_ALL=C`, for the same reason `build_identity.sh`
  # sorts its inventory: the emitted file is compared BYTE FOR BYTE by
  # `check_attestation_manifest.sh`, so an order inherited from the manifest's
  # enumeration makes that gate a function of the environment rather than of content.
  # It did: one machine ordered `block_to_words_at` before `block_to_words` and the
  # gate reported a correct tree as STALE, advising a regeneration that would have
  # committed that machine's order and broken CI.
  #
  # BEFORE the dedupe, not after. `awk` keeps the FIRST row per key, so an unsorted
  # input decides WHICH row survives as well as where it lands. Sorting afterwards
  # would leave that selection environment-dependent while making the output look
  # canonical — the worse failure, because it is invisible.
  printf '%s\n' "$MAN" | grep ' <- ' | grep -v EXCLUDED \
    | LC_ALL=C sort \
    | awk '{ key = $1 FS $3 FS $4; if (!(key in seen)) { seen[key] = 1; print } }' \
    | while read -r line; do
    tbl="${line%% <-*}"
    rest="${line#* <- }"
    pkgdecl="${rest%% *}"
    pkg="${pkgdecl%%/*}"
    moddecl="${pkgdecl#*/}"
    mod="${moddecl%%.*}"
    decl="${moddecl#*.}"
    impl="$(printf '%s' "$rest" | grep -oE 'impl=[0-9a-f]+' | cut -d= -f2)"
    [ -n "$impl" ] || continue
    # Symbol names the TABLE, the SCOPE and the DECLARATION (R-0482: `$pkg` is the owning package's
    # scope digest). A name omitting the package would
    # collide across packages for a reused table, which is the collapse this exists to prevent.
    tblshort="$(printf '%s' "$tbl" | sed 's/.*\.//')"
    sym="$(printf '%s_%s_%s' "$tblshort" "${pkg:0:8}" "$decl" | tr -c 'A-Za-z0-9_' '_')"
    echo "def $sym : Except DefinitionIdentityRefusal DefinitionIdentity :="
    echo "  DefinitionIdentity.of? \"$pkg\" \"$mod\" \"$decl\" \"$impl\""
  done
  echo
  echo "end Concrete.Proof.GeneratedAttestations"
} > "$OUT.tmp"

# D5 (R-0482): the symbol keeps `<table>_<scope8>_<decl>`, so generation REFUSES what the truncation
# could hide — two distinct scopes sharing an 8-hex prefix, and two distinct full keys rendering one
# symbol. A collision would otherwise surface as a Lean redefinition error at best, or, if the two
# rows ever deduplicated, as one reference silently answering for another scope's definition.
# (Sorted with `sort -u` under the exported C locale, deliberately not spelled like the emit
# pipeline's canonical sort, which check_attestation_refs_order.sh removes to build its control.)
export LC_ALL=C
COLL="$(awk '/^def /{sym=$2} /DefinitionIdentity.of\?/{print sym "\t" $2 $3 $4 $5}' "$OUT.tmp" \
  | sort -u | awk -F'\t' 'seen[$1]++ {print $1}')"
PFX="$(grep -oE 'DefinitionIdentity.of\? "[0-9a-f]{32}"' "$OUT.tmp" | grep -oE '[0-9a-f]{32}' | sort -u \
  | awk '{p=substr($0,1,8); if (p in seen && seen[p] != $0) print p; seen[p]=$0}')"
# One (table, scope/module.declaration) with two implementations is also two full keys for one
# symbol: the emit dedupe keeps the first and would drop the other in silence.
IMPLS="$(printf '%s\n' "$MAN" | grep ' <- ' | grep -v EXCLUDED \
  | awk '{ k = $1 FS $3; i = ""; for (f = 4; f <= NF; f++) if ($f ~ /^impl=/) i = $f
           if (i == "") next; if ((k in seen) && seen[k] != i) print k; seen[k] = i }' | sort -u)"
[ -n "$IMPLS" ] && COLL="$COLL $IMPLS"
if [ -n "$COLL$PFX" ]; then
  rm -f "$OUT.tmp"
  [ -n "$PFX" ] && echo "REFUSE: distinct scopes share the 8-hex symbol prefix: $PFX" >&2
  [ -n "$COLL" ] && echo "REFUSE: distinct full keys render one symbol: $COLL" >&2
  exit 1
fi
mv "$OUT.tmp" "$OUT"

echo "wrote $OUT ($(grep -c '^def ' "$OUT") references)"
