#!/usr/bin/env bash
# Namespace guard for the proof-theorem migration.
#
# Example proof THEOREMS live in their per-example module
# `Concrete.Examples.<Ex>.Proofs` (namespace `Examples.<Ex>.Proofs`).
# `Concrete.Proof` keeps proof INFRASTRUCTURE (the PExpr/eval model, the loop/
# array "ladder" lemmas, ProofKit) plus the registered spec PExprs / eval
# scaffolding / `specs` + `provedFunctions` tables (those are `def`s, not
# theorems). This gate keeps the migration "done" by failing when:
#
#   1. a file under Concrete/Examples/ re-enters `namespace Concrete.Proof`;
#   2. Concrete/Proof/Proof.lean declares a theorem/lemma not on the allowlist
#      (scripts/tests/proof_namespace_allowlist.txt) — i.e. a new example proof
#      snuck into the compiler namespace;
#   3. one of the migrated flagship theorems reappears in any Concrete.Proof file.
#
# To allow a genuinely new INFRASTRUCTURE lemma in Concrete.Proof, add its name to
# the allowlist (a deliberate, reviewable change) — same discipline as snapshots.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

PROOF="Concrete/Proof/Proof.lean"
ALLOW="scripts/tests/proof_namespace_allowlist.txt"
FAIL=0

# 1. No example module may declare the compiler proof namespace.
if grep -rln '^namespace Concrete\.Proof\b' Concrete/Examples/ 2>/dev/null | grep -q .; then
  echo "FAIL: a file under Concrete/Examples/ declares 'namespace Concrete.Proof':"
  grep -rln '^namespace Concrete\.Proof\b' Concrete/Examples/ | sed 's/^/    /'
  echo "  → example proofs must use 'namespace Examples.<Ex>.Proofs'."
  FAIL=1
fi

# 2. Allowlist subset check on Concrete/Proof/Proof.lean theorems/lemmas.
current="$(grep -oE "^(theorem|lemma) [A-Za-z0-9_']+" "$PROOF" | awk '{print $2}' | sort -u)"
allow="$(grep -vE '^[[:space:]]*#|^[[:space:]]*$' "$ALLOW" | sort -u)"
newones="$(comm -23 <(printf '%s\n' "$current") <(printf '%s\n' "$allow"))"
if [ -n "$newones" ]; then
  echo "FAIL: theorem/lemma(s) in $PROOF not on the allowlist:"
  printf '%s\n' "$newones" | sed 's/^/    /'
  echo "  → if example-correctness, define under Concrete.Examples.<Ex>.Proofs;"
  echo "  → if genuine infrastructure, add the name to $ALLOW."
  FAIL=1
fi

# 3. Migrated flagship theorems must not reappear in any Concrete.Proof file.
MIGRATED=(
  count_up_loop_preserves
  validate_version_correct validate_header_fields_success parse_header_too_short
  compute_tag_correct verify_tag_correct check_nonce_correct verify_message_composed_correct
  ring_new_correct ring_push_then_contains_correct compute_tag_zero_correct
  ct_compare_same_tag_correct ct_compare_different_tag_correct ct_compare_equal_zeros_correct
  check_magic_correct check_class_correct check_data_correct check_version_correct validate_header_correct
  sha256_init_correct ch_selects_high hmac_sha256_refines_spec sha256_hash_refines_spec
  ch_refines round_refines_list
)
PROOF_NS_FILES=("$PROOF" Concrete/ProofKit/*.lean)
for n in "${MIGRATED[@]}"; do
  if grep -qE "^(theorem|lemma) ${n}\b" "${PROOF_NS_FILES[@]}" 2>/dev/null; then
    echo "FAIL: migrated example theorem '$n' reappeared in a Concrete.Proof file"
    FAIL=1
  fi
done

if [ "$FAIL" -eq 0 ]; then
  echo "PROOF-NAMESPACE: PASS — Concrete.Proof holds only allowlisted (infrastructure/grandfathered) theorems; example proofs stay under Concrete.Examples.*"
  exit 0
else
  exit 1
fi
