#!/usr/bin/env bash
# ProofKit arithmetic-bridge gate (ROADMAP Phase 2 #7).
#
# The arithmetic bridge lemmas (Int/Nat/BitVec conversions, byte masking, the
# signed-division bridge) are centralized in Concrete/ProofKit/Arith.lean. This
# gate pins the success criterion: the library exists, it is actually USED by the
# proof corpus, and the lemmas are NOT re-duplicated as one-offs inside examples.
# (The kernel-check that the lemmas hold is `lake build`; this gate guards the
# *centralization* from regressing.)

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
ARITH="Concrete/ProofKit/Arith.lean"
PASS=0; FAIL=0

want_def(){ local name="$1"
  if grep -qE "theorem $name\b" "$ARITH"; then echo "  ok   library defines $name"; PASS=$((PASS+1));
  else echo "  FAIL $ARITH missing theorem $name"; FAIL=$((FAIL+1)); fi; }

echo "=== Arith.lean defines the centralized bridges ==="
[ -f "$ARITH" ] || { echo "  FAIL $ARITH missing"; echo "PROOFKIT-ARITH: PASS=$PASS FAIL=1"; exit 1; }
want_def ofNat64_eq_setWidth32
want_def ofNat32_msb_false
want_def and255_lo
want_def sdiv_ofNat_eq_natDiv

echo "=== the library is actually USED by the proof corpus ==="
# HMAC's padding block-count bridge must be a corollary of the general lemma.
if grep -q "sdiv_ofNat_eq_natDiv" proofs/Examples/HmacSha256/Proofs.lean; then
  echo "  ok   HmacSha256 uses sdiv_ofNat_eq_natDiv (signed-division bridge)"; PASS=$((PASS+1));
else
  echo "  FAIL HmacSha256 does not use the centralized sdiv bridge"; FAIL=$((FAIL+1)); fi

echo "=== the centralized lemmas are NOT re-defined inside examples (no one-offs) ==="
for name in ofNat64_eq_setWidth32 ofNat32_msb_false and255_lo sdiv_ofNat_eq_natDiv; do
  hits="$(grep -rEl "theorem $name\b" proofs/Examples/ 2>/dev/null || true)"
  if [ -z "$hits" ]; then echo "  ok   no example redefines $name"; PASS=$((PASS+1));
  else echo "  FAIL $name re-defined in: $hits"; FAIL=$((FAIL+1)); fi
done

echo ""
echo "PROOFKIT-ARITH: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
