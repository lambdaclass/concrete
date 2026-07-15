#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
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
# The report layer was split (2026-07-07): the obligation collectors live in
# Concrete/Report/ReportObligations.lean, so search the whole Report/ dir to
# keep the "no duplicate walkers ANYWHERE in the report layer" guarantee.
R="Concrete/Report"; M="Main.lean"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== the old family-specific scoped walkers stay deleted ==="
old="$(grep -rhE "def scopedCallsS|def scopedBoundsS|def scopedDivS|def scopedArithS|def scopedAssertsS" "$R" | wc -l | tr -d ' ')"
[ "$old" = "0" ] && ok "no per-family scoped*S collector reintroduced" \
  || no "a family-specific scoped collector came back ($old found) — route it through scopedWalk"

echo "=== the unified collector + per-family leaves are the only collection path ==="
for d in scopedWalkS scopedWalkB callLeaf boundsLeaf divLeaf arithLeaf assertLeaf; do
  grep -rqE "def $d\b" "$R" && ok "present: $d" || no "missing unified-collector piece: $d"
done

echo "=== VC reports render the ONE obligation schedule directly (post-merge) ==="
# after the #18d model merge there is one record type, so the VC reports render
# the obligation schedule (`dvcs`/`auditVCs`) directly — no conversion glue.
grep -q "Report.vcsJson dvcs" "$M" && grep -q "Report.vcsReport dvcs" "$M" \
  && grep -q "vcAuditSummary auditVCs" "$M" \
  && ok "--report vcs and audit render the obligation schedule directly" \
  || no "a VC report no longer renders the obligation schedule"
# the conversion glue is gone.
grep -q "toVCView" "$M" Concrete/Proof/ObligationCore.lean \
  && no "toVCView conversion glue reintroduced (records are unified — no view needed)" \
  || ok "no toVCView conversion glue (records unified)"

echo ""
echo "NO-DUPLICATE-WALKERS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
