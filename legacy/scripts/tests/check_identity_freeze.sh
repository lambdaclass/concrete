#!/usr/bin/env bash
# IDENTITY FREEZE — the pre-migration population, pinned exactly.
#
# The five-way identity separation replaces one over-bound `PackageIdentity` with
# PackageScopeIdentity / PackageArtifactIdentity / ResolutionContextIdentity, and rescopes
# `DefinitionIdentity` onto the first of those. That migration must be ATOMIC and must reproduce the
# COMPLETE reference population — so the population has to be a measured number before anything
# moves, not a number recovered afterwards from whatever survived.
#
# WHY A GATE RATHER THAN A NOTE. The last identity change moved every package in the repository and
# stranded 168 lines of attestations; the count of references never changed, so a count-only check
# would have called it healthy. What makes a freeze useful is that a DIFFERENCE fails loudly while
# the old scheme is still in place, so "the migration reproduced the population" is checkable rather
# than asserted.
#
# These numbers are expected to change exactly once, in the migration commit, together with the
# generator, the references, the consumers and the deletion of the old symbols. A change to any of
# them at any other time is drift and should fail here.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

REFS="Concrete/Proof/GeneratedAttestations.lean"
[ -f "$REFS" ] || { echo "FATAL: $REFS missing — nothing to freeze" >&2; exit 2; }

echo "=== the generated attestation surface ==="

# Every `of?` line is `DefinitionIdentity.of? "<package>" "<module>" "<decl>" "<implementation>"`.
mapfile -t OF < <(grep -oE 'DefinitionIdentity\.of\? "[0-9a-f]+" "[^"]+" "[^"]+" "[0-9a-f]+"' "$REFS")
N_OF="${#OF[@]}"
N_DEF="$(grep -c '^def ' "$REFS" || true)"

# NON-VACUITY FIRST: every count below is derived from these lines.
if [ "$N_OF" -ge 40 ]; then
  ok "parsed $N_OF identity constructions from the generated surface"
else
  no "parsed only $N_OF identity constructions — the parse is broken and every count below is vacuous"
  echo "IDENTITY-FREEZE: PASS=$PASS FAIL=$FAIL"; exit 1
fi

# One symbol per construction: a mismatch means the file is malformed, not merely moved.
if [ "$N_DEF" = "$N_OF" ]; then
  ok "each of the $N_DEF generated symbols constructs exactly one identity"
else
  no "$N_DEF symbols but $N_OF identity constructions — the generated file is malformed"
fi

pkgs()  { printf '%s\n' "${OF[@]}" | awk '{print $2}' | sort -u; }
keys()  { printf '%s\n' "${OF[@]}" | awk '{print $3, $4, $5}' | sort -u; }
impls() { printf '%s\n' "${OF[@]}" | awk '{print $5}' | sort -u; }

N_PKG="$(pkgs | grep -c . || true)"
N_KEY="$(keys | grep -c . || true)"
N_IMPL="$(impls | grep -c . || true)"

# ---------------------------------------------------------------------------
# THE FROZEN NUMBERS (2026-08-20, pre-migration).
FREEZE_REFS=63     # generated reference symbols
FREEZE_PKGS=21     # distinct package identities appearing in them
FREEZE_KEYS=55     # distinct (module, declaration, implementation) triples
FREEZE_IMPLS=55    # distinct implementation digests — equal to the key count, so an implementation
                   # digest identifies its (module, declaration) uniquely in this corpus today

# A reference ADDED after the freeze (a new attested scope, explained as an `A` row in the amendments
# that check_identity_rows.sh verifies) is not a migrated symbol: the frozen numbers plus the explained
# additions are what must hold. Each added reference must be a new distinct key and implementation — the
# duplication gap below stays pinned, so an addition that duplicated a key would still fail there — and
# each added `scope` row is one new package identity.
AMEND_FILE="docs/verification/identity-rows-amendments.tsv"
ADD_REFS="$(grep -cE "^A	reference	" "$AMEND_FILE" 2>/dev/null || true)"
ADD_PKGS="$(grep -cE "^A	scope	" "$AMEND_FILE" 2>/dev/null || true)"
FREEZE_REFS=$(( FREEZE_REFS + ${ADD_REFS:-0} ))
FREEZE_PKGS=$(( FREEZE_PKGS + ${ADD_PKGS:-0} ))
FREEZE_KEYS=$(( FREEZE_KEYS + ${ADD_REFS:-0} ))
FREEZE_IMPLS=$(( FREEZE_IMPLS + ${ADD_REFS:-0} ))

check(){ # name got want
  if [ "$2" = "$3" ]; then ok "$1: $2 (frozen)"
  else no "$1 moved: $2, frozen at $3 — if this is the migration, update the freeze in the SAME commit as the generator, references, consumers and deletion of old symbols"; fi
}
check "generated references"        "$N_DEF"  "$FREEZE_REFS"
check "distinct package identities" "$N_PKG"  "$FREEZE_PKGS"
check "distinct (module, decl, impl) keys" "$N_KEY" "$FREEZE_KEYS"
check "distinct implementation digests"    "$N_IMPL" "$FREEZE_IMPLS"

# THE OVER-BINDING ITSELF, measured. References outnumber keys because the same declaration is
# attested once PER PACKAGE — which is the coupling the separation removes. Recording the gap makes
# the migration's effect visible: under PackageScopeIdentity these should collapse toward the key
# count, and a migration that does not change this ratio has not changed the thing it was for.
GAP=$(( N_DEF - N_KEY ))
if [ "$GAP" -eq 8 ]; then
  ok "package-scoped duplication is $GAP references beyond the distinct keys (the coupling the separation removes)"
else
  no "package-scoped duplication moved to $GAP (frozen at 8) — the identity scheme changed without this freeze being updated"
fi

echo "=== dependent populations ==="

# Consumers that SELECT a reference. A migration renames every symbol, so this is the count of call
# sites that must be rewritten atomically with it.
N_SEL="$(grep -rho 'GeneratedAttestations\.[A-Za-z0-9_]*' Concrete/Proof/Proof.lean proofs 2>/dev/null | grep -c . || true)"
if [ "$N_SEL" = "44" ]; then
  ok "44 consumer selections must migrate atomically with the symbols"
else
  no "consumer selections moved to $N_SEL (frozen at 44) — every one is a call site the migration must rewrite in the same commit"
fi

# Stored proof links, per corpus. The migration must not disturb these: they are keyed by subject
# digest, not by package symbol, so a change here means the migration reached further than intended.
for pair in "examples:43" "tests:11" "std:11"; do
  c="${pair%%:*}"; want="${pair##*:}"
  # A link ADDED after the freeze (a new fixture, explained as an `A` row in the amendments that
  # check_identity_rows.sh verifies) is not a migrated subject: the frozen count plus the explained
  # additions under this corpus is what must hold.
  added="$(grep -cE "^A	link	$c/" docs/verification/identity-rows-amendments.tsv 2>/dev/null || true)"
  want=$(( want + ${added:-0} ))
  got="$(grep -rhoE 'proof_fingerprint\("[^"]*"\)' "$c" --include='*.con' 2>/dev/null | grep -c . || true)"
  if [ "$got" = "$want" ]; then ok "stored links in $c/: $got (frozen)"
  else no "stored links in $c/ moved to $got (frozen at $want) — the identity migration must not touch stored subjects"; fi
done

echo "IDENTITY-FREEZE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
