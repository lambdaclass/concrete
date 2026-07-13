#!/usr/bin/env bash
# Operational VC auto-discharge forcing-probe gate (ROADMAP Phase 9 #16a).
#
# Locks the result of the forcing probe that decided #16a: can a FIXED,
# MECHANICAL tactic (eval-unfold + named-spec unfold + reusable
# Int<->Nat<->BitVec collapse + mechanical conjunction/guard split, then route
# the leaf to bv_decide/omega/rfl) close the OPERATIONAL VC of obligations that
# today require a hand-written Lean bridge theorem? No per-obligation human
# input is allowed.
#
# Two halves, two opposite assertions:
#
#   1. POSITIVE (closes.lean) MUST type-check (exit 0). It proves that the
#      common operational-VC classes are mechanically dischargeable TODAY:
#        - pure bitwise word functions (ch, maj)        -> bv_decide
#        - straight-line loop-body preservation (count_up) -> omega
#        - branching postconditions with a guard split (validate_version)
#      If this breaks (toolchain bump, lemma drift, eval-model change), the
#      80/20 the roadmap item is predicated on has regressed.
#
#   2. BOUNDARY (boundary.lean) MUST FAIL to type-check (nonzero exit). It is
#      the open-gap tripwire: the cast-normalization fragment V1 must build is
#      exactly what these two cases (rotr shift-amount casts; unsplit guard)
#      need. When a real auto-discharge implementation lands, these will START
#      to close, this assertion will flip, and the flip is the signal to
#      promote them into closes.lean and update the roadmap + design note.
#
# Design note: research/proof-evidence/operational-vc-auto-discharge.md
# Fixtures:    scripts/tests/fixtures/operational_vc_autodischarge/
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

FIX="scripts/tests/fixtures/operational_vc_autodischarge"
CLOSES="$FIX/closes.lean"
BOUNDARY="$FIX/boundary.lean"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

# The Concrete oleans the fixtures import must be built.
if [ ! -f .lake/build/lib/lean/Examples/HmacSha256/Proofs.olean ]; then
  echo "error: build first (Concrete proof oleans missing — run 'lake build')" >&2
  exit 2
fi

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== positive: common operational VCs close with a fixed mechanical tactic ==="
if lake env lean "$CLOSES" >"$TMP/closes.out" 2>&1; then
  ok "closes.lean type-checks (ch, maj, loop-preservation, branching+split all auto-close)"
else
  no "closes.lean failed to type-check — the mechanical-discharge baseline regressed"
  sed 's/^/        /' "$TMP/closes.out" | grep -i "error" | head -8
fi

echo "=== boundary: cast-normalization / guard-split gap still open (tripwire) ==="
if lake env lean "$BOUNDARY" >"$TMP/boundary.out" 2>&1; then
  no "boundary.lean UNEXPECTEDLY type-checked — the auto-discharge fragment may have landed."
  echo "        -> promote these cases into closes.lean and update ROADMAP #16a + the design note."
else
  # Confirm it failed for the expected reason (a real tactic failure, not an
  # import/build error), so the tripwire stays meaningful. Match tactic-failure
  # signatures only (no backticks — they would trigger command substitution).
  if grep -qi "error:" "$TMP/boundary.out" \
     && grep -qiE "failed|unsolved goals|not definitionally equal" "$TMP/boundary.out"; then
    ok "boundary.lean fails to auto-close (rotr shift-cast + unsplit guard) — gap is open, as recorded"
  else
    no "boundary.lean failed, but NOT with a tactic error (import/build problem?)"
    sed 's/^/        /' "$TMP/boundary.out" | grep -i "error" | head -8
  fi
fi

echo ""
echo "passed: $PASS  failed: $FAIL"
[ "$FAIL" -eq 0 ] || exit 1
echo "operational-VC-auto-discharge probe gate OK"
