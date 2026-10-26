#!/usr/bin/env bash
# IDENTITY OVER-BINDING — the separation's positive controls (R-0482, converted from the entrance gate).
#
# Before R-0482 this gate PINNED A DEFECT: one `PackageIdentity` bound the whole build's content, std
# included, and scoped every `DefinitionIdentity` by it. A two-line std edit moved 7 of 21 package
# identities (168 lines of the attestation surface), and a sibling edit renamed a different function's
# attestation symbol. Those legs failed when the separation landed, which was their purpose; each is
# now asserted in its positive form:
#
#   O1  an unused std edit moves NO scope (it moves std's artifact and so each dependent's resolution
#       CONTEXT, which is where dependency content belongs);
#   O2  a sibling edit in a manifestful package moves its ARTIFACT, not its scope, and the sibling's
#       definition identity is unchanged;
#   O2s the stated standalone exception: a sibling edit in a manifestless fixture moves its standalone
#       scope (which contains its own artifact), while a std edit does not;
#   O3  editing the subject's own body moves its implementation digest (unchanged in both schemes).
#
# NO BUILD REQUIRED: identity is computed from source content at REPORT time, so editing a copy and
# running the existing binary against it is sufficient. The copy means the working tree is never
# touched. The binary is COPIED into the scratch tree, never linked: the compiler finds std relative to
# `IO.appPath`, so a linked binary would read the original std and the std legs would measure nothing.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
BIN="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$BIN" ] || { echo "error: build first ($BIN missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

W="$TMP/repo"; mkdir -p "$W"
cp -a "$ROOT_DIR/." "$W/" 2>/dev/null || { echo "error: could not copy the repository" >&2; exit 2; }
WBIN="$W/.lake/build/bin/concrete"
rm -f "$WBIN"; cp -c "$BIN" "$WBIN" 2>/dev/null || cp "$BIN" "$WBIN"
BIN="$WBIN"
# pid <example> <field> — one line of `--report package-identity` (scope | artifact | context).
pid() { "$BIN" "$W/examples/$1/src/main.con" --report package-identity 2>/dev/null | awk -F'\t' -v f="$2" '$1 == f {print $2; exit}'; }
# defid <example> <decl> — the definition identity digest of one subject.
defid() {
  "$BIN" "$W/examples/$1/src/main.con" --report attestation-join 2>/dev/null \
    | awk -F'\t' -v d="$2" '$1 == "subject" && $5 == d {print $2; exit}'
}

# pkgid <root> <example> — the package identity the compiler reports for that example's subjects.
pkgid() {
  "$BIN" "$1/examples/$2/src/main.con" --report attestation-join 2>/dev/null \
    | awk -F'\t' '$1 == "subject" {print $3; exit}'
}
# declid <root> <example> <decl> — the implementation digest for one declaration.
declid() {
  "$BIN" "$1/examples/$2/src/main.con" --report attestation-join 2>/dev/null \
    | awk -F'\t' -v d="$3" '$1 == "subject" && $5 == d {print $6; exit}'
}

BASE_PKG="$(pkgid "$W" crypto_verify)"
BASE_TAG="$(declid "$W" crypto_verify compute_tag)"
if [ -n "$BASE_PKG" ] && [ -n "$BASE_TAG" ]; then
  ok "baseline read: crypto_verify package $BASE_PKG, compute_tag impl $BASE_TAG"
else
  no "could not read a baseline identity (pkg='$BASE_PKG' impl='$BASE_TAG') — every leg below would be vacuous"
  echo "IDENTITY-OVERBINDING: PASS=$PASS FAIL=$FAIL"; exit 1
fi

echo ""
echo "=== O1: an unused std edit moves no scope, only resolution context ==="
GEN="$W/Concrete/Proof/GeneratedAttestations.lean"
cp "$GEN" "$TMP/gen.before"
CV_CTX0="$(pid crypto_verify context)"; CV_SCOPE0="$(pid crypto_verify scope)"
SA_SCOPE0="$(pid proof_patterns/composition scope)"
perl -0pi -e 's/if v == 62 \{ return 43; \}/if v == 62 { return 43; } if v == 63 { return 47; }/' "$W/std/src/base64.con" 2>/dev/null
if cmp -s "$W/std/src/base64.con" "$ROOT_DIR/std/src/base64.con"; then
  no "O1: the std edit did not apply — the anchor moved, so every O1 leg would be vacuous"
else
  ( cd "$W" && bash scripts/gen/attestation_refs.sh >/dev/null 2>&1 )
  ids_of(){ grep -oE 'of\? "[0-9a-f]{32}"' "$1" | sort -u; }
  MOVED="$(comm -23 <(ids_of "$TMP/gen.before") <(ids_of "$GEN") | grep -c . || true)"
  TOTAL="$(ids_of "$TMP/gen.before" | grep -c . || true)"
  if [ "${TOTAL:-0}" -lt 10 ]; then
    no "O1: only $TOTAL scopes parsed — the measurement is broken"
  elif [ "${MOVED:-0}" -eq 0 ]; then
    ok "O1: a two-line std code edit moved 0 of $TOTAL scopes (pre-R-0482: 7 of 21, 168 surface lines)"
  else
    no "O1: a std edit moved $MOVED of $TOTAL scopes — std content has reached PackageScopeIdentity"
  fi
  CV_CTX1="$(pid crypto_verify context)"; CV_SCOPE1="$(pid crypto_verify scope)"
  [ -n "$CV_SCOPE0" ] && [ "$CV_SCOPE0" = "$CV_SCOPE1" ] && ok "O1: crypto_verify's scope is unchanged ($CV_SCOPE0)" \
    || no "O1: crypto_verify's scope moved or was unreadable ('$CV_SCOPE0' -> '$CV_SCOPE1')"
  [ -n "$CV_CTX0" ] && [ "$CV_CTX0" != "$CV_CTX1" ] && ok "O1: its resolution context moved ($CV_CTX0 -> $CV_CTX1) — std's artifact is where the edit is recorded" \
    || no "O1: crypto_verify's context did not move under a std edit ('$CV_CTX0' -> '$CV_CTX1') — dependency content is not bound anywhere"
  SA_SCOPE1="$(pid proof_patterns/composition scope)"
  [ -n "$SA_SCOPE0" ] && [ "$SA_SCOPE0" = "$SA_SCOPE1" ] && ok "O2s: a std edit leaves the standalone composition scope unchanged" \
    || no "O2s: a std edit moved a standalone scope ('$SA_SCOPE0' -> '$SA_SCOPE1')"
fi
cp "$ROOT_DIR/std/src/base64.con" "$W/std/src/base64.con"; cp "$TMP/gen.before" "$GEN"

echo ""
echo "=== O2: a sibling edit moves the ARTIFACT, not the scope ==="
SIB="$W/examples/proof_pressure/src/main.con"
if [ -f "$SIB" ]; then
  S0="$(pid proof_pressure scope)"; A0="$(pid proof_pressure artifact)"; D0="$(defid proof_pressure check_nonce)"
  printf '\n// transient probe: sibling-edit identity coupling\n' >> "$SIB"
  S1="$(pid proof_pressure scope)"; A1="$(pid proof_pressure artifact)"; D1="$(defid proof_pressure check_nonce)"
  cp "$ROOT_DIR/examples/proof_pressure/src/main.con" "$SIB"
  [ -n "$S0" ] && [ "$S0" = "$S1" ] && ok "O2: proof_pressure's scope survives a sibling edit ($S0)" \
    || no "O2: a sibling edit moved proof_pressure's scope ('$S0' -> '$S1')"
  [ -n "$A0" ] && [ "$A0" != "$A1" ] && ok "O2: the sibling edit moved its artifact ($A0 -> $A1)" \
    || no "O2: a sibling edit did not move the artifact ('$A0' -> '$A1') — own content is not bound"
  [ -n "$D0" ] && [ "$D0" = "$D1" ] && ok "O2: check_nonce's definition identity is unchanged ($D0), so its symbol is too" \
    || no "O2: a sibling edit moved check_nonce's definition identity ('$D0' -> '$D1')"
else
  no "examples/proof_pressure/src/main.con is missing — O2 could not run"
fi

echo ""
echo "=== O2s: the standalone exception — own edits move a standalone scope ==="
SA="$W/examples/proof_patterns/composition/src/main.con"
if [ -f "$SA" ] && [ ! -f "$W/examples/proof_patterns/composition/Concrete.toml" ]; then
  T0="$(pid proof_patterns/composition scope)"
  printf '\n// transient probe: standalone own-content edit\n' >> "$SA"
  T1="$(pid proof_patterns/composition scope)"
  cp "$ROOT_DIR/examples/proof_patterns/composition/src/main.con" "$SA"
  [ -n "$T0" ] && [ "$T0" != "$T1" ] && ok "O2s: an own-content edit moved the standalone scope ($T0 -> $T1), as D3 states" \
    || no "O2s: an own-content edit did not move the standalone scope ('$T0' -> '$T1')"
else
  no "examples/proof_patterns/composition is missing or gained a manifest — O2s could not run"
fi

echo ""
echo "=== control 3: editing the SUBJECT's own implementation ==="
# The leg that must keep passing in BOTH schemes, and the reason the other two are defects rather
# than the price of doing business: a real change to the body being attested must move that
# declaration's identity. If this ever fails, identity has stopped tracking what it names.
SUBJ="$W/examples/crypto_verify/src/main.con"
if grep -q 'return key \* message + nonce;' "$SUBJ" 2>/dev/null; then
  perl -0pi -e 's/return key \* message \+ nonce;/return key * message + nonce + 1;/' "$SUBJ"
  EDIT_TAG="$(declid "$W" crypto_verify compute_tag)"
  git -C "$W" checkout -- examples/crypto_verify/src/main.con 2>/dev/null || true
  if [ -n "$EDIT_TAG" ] && [ "$EDIT_TAG" != "$BASE_TAG" ]; then
    ok "editing the subject's own body moves its implementation digest ($BASE_TAG -> $EDIT_TAG)"
  else
    no "editing compute_tag's body did NOT move its implementation digest — identity has stopped tracking the thing it names"
  fi
else
  no "the compute_tag body anchor is gone from crypto_verify — control 3 could not run"
fi

echo ""
echo "=== deferred by decision ==="
# P1 (accepted 2026-10-13): std definitions are now scoped under the std scope, but a claim whose
# closure reaches std stays fail-closed (`unscopedEdge`), as before. "A reachable std edit moves
# exactly the affected roots" therefore has no claim to measure; it lands with the slice that admits
# std nodes into report roots. Origin sensitivity (G) moved to check_identity_rows.sh.
ok "reachable-std sensitivity deferred to the std-in-roots slice (P1) — recorded, not skipped"

echo ""
echo "IDENTITY-OVERBINDING: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
