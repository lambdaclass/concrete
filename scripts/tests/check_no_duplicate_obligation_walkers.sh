#!/usr/bin/env bash
# No-duplicate-obligation-walkers guard (ROADMAP Phase 3 #18d).
#
# Phase 3 consolidated every obligation family onto one scoped collector
# (`scopedWalk`), one shared lowering layer, one discharge-adapter firewall, and
# one ObligationCore hub that the reports render through. This guard FAILS CI if
# that consolidation regresses — i.e. if a family-specific scoped collector is
# reintroduced, a per-family leaf disappears, or a VC report goes back to
# rendering raw `collectVCs` output instead of flowing through the hub. It is a
# source-structure gate (greps the compiler), the standing lock on "one ledger,
# everything else is a view".

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
R="Concrete/Report.lean"; M="Main.lean"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== the old family-specific scoped walkers stay deleted ==="
old="$(grep -cE "def scopedCallsS|def scopedBoundsS|def scopedDivS|def scopedArithS|def scopedAssertsS" "$R" || true)"
[ "$old" = "0" ] && ok "no per-family scoped*S collector reintroduced" \
  || no "a family-specific scoped collector came back ($old found) — route it through scopedWalk"

echo "=== the unified collector + per-family leaves are the only collection path ==="
for d in scopedWalkS scopedWalkB callLeaf boundsLeaf divLeaf arithLeaf assertLeaf; do
  grep -qE "def $d\b" "$R" && ok "present: $d" || no "missing unified-collector piece: $d"
done

echo "=== VC reports render THROUGH the ObligationCore hub (no raw recompute) ==="
# the hub-routed render calls must be the ones in use ...
grep -q "Report.vcsJson vcView" "$M" && grep -q "Report.vcsReport vcView" "$M" \
  && grep -q "vcAuditSummary auditVCView" "$M" \
  && ok "--report vcs and audit render from ledger→toVCView views" \
  || no "a VC report is no longer hub-routed"
# ... and the raw forms (rendering collectVCs output directly) must be gone.
if grep -qE "vcsReport dvcs|vcsJson dvcs|vcAuditSummary auditVCs\b" "$M"; then
  no "a VC report renders raw collectVCs output (bypasses the hub)"
else
  ok "no raw collectVCs rendering in Main (hub is in the data path)"
fi
# the round-trip lossless-ness proof must remain in the hub.
grep -q "toVCView (ofVC v)" Concrete/ObligationCore.lean \
  && ok "round-trip identity proof (toVCView ∘ ofVC = id) retained" \
  || no "lossless round-trip proof removed from ObligationCore"

echo ""
echo "NO-DUPLICATE-WALKERS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
