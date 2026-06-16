#!/usr/bin/env bash
# Single-truth-source guard (ROADMAP Phase 3 #18d, step 5).
#
# After the model merge there is ONE obligation record type. This guard FAILS CI
# if the second model creeps back in any form:
#   - `Report.VC` reverting to an independent `structure` (storage) instead of an
#     `abbrev` of the one record;
#   - a second obligation ledger `structure Obligation`;
#   - the `toVCView` conversion glue returning;
#   - the proof-status surface NOT flowing into the one record via `ofProofStatus`;
#   - a family-specific scoped collector returning;
#   - a VC report recomputing instead of rendering the obligation schedule.
# It is a source-structure gate (greps the compiler) — the standing lock on
# "one ledger, one record, everything else a view".

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
R="Concrete/Report.lean"; O="Concrete/ObligationCore.lean"; M="Main.lean"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== there is exactly ONE obligation record type ==="
# the canonical record is a single `structure Obligation` in Report ...
nstruct="$(grep -cE "^structure Obligation\b" "$R" || true)"
[ "$nstruct" = "1" ] && ok "exactly one 'structure Obligation' (in Report)" \
  || no "expected exactly one 'structure Obligation' in Report, found $nstruct"
# ... ObligationCore aliases it, never redefines it.
grep -q "^abbrev Obligation := Report.Obligation" "$O" \
  && ok "ObligationCore.Obligation is an abbrev of Report.Obligation" \
  || no "ObligationCore no longer aliases the one record"
grep -qE "^structure Obligation\b" "$O" \
  && no "ObligationCore redefined a second Obligation struct" \
  || ok "no second Obligation struct in ObligationCore"

echo "=== Report.VC is a compatibility alias, NOT independent storage ==="
grep -q "^abbrev VC := Obligation" "$R" \
  && ok "VC is an abbrev of the one record" \
  || no "VC is not an abbrev of Obligation"
grep -qE "^structure VC\b" "$R" \
  && no "Report.VC came back as an independent storage struct" \
  || ok "no 'structure VC' (VC is not a second model)"

echo "=== no conversion glue between the (now unified) records ==="
grep -q "toVCView" "$O" "$M" \
  && no "toVCView conversion glue reintroduced" \
  || ok "no toVCView glue"

echo "=== proof-status flows INTO the one record (not a parallel ledger) ==="
grep -q "def ofProofStatus" "$O" && grep -q "def proofLinkLedger" "$O" \
  && ok "proof-status projects into the record via ofProofStatus/proofLinkLedger" \
  || no "proof-status no longer projects into the one record"
grep -q "proofLinkLedger" "$M" \
  && ok "the obligation-ledger unions VCs + the proof-link projection" \
  || no "obligation-ledger no longer unions the proof-link family"

echo "=== no family-specific scoped collector (one walker) ==="
old="$(grep -cE "def scopedCallsS|def scopedBoundsS|def scopedDivS|def scopedArithS|def scopedAssertsS" "$R" || true)"
[ "$old" = "0" ] && ok "no per-family scoped*S collector" || no "a family-specific collector returned ($old)"

echo "=== VC reports render the obligation schedule (no recompute path) ==="
grep -q "Report.vcsJson dvcs" "$M" && grep -q "vcAuditSummary auditVCs" "$M" \
  && ok "--report vcs / audit render the discharged obligation schedule" \
  || no "a VC report is not rendering the obligation schedule"

echo "=== --report contracts renders the ledger, not a private discharge (Phase 3 #18e) ==="
# renderContracts must read computeVCsDischarged (the one ledger) and must NOT run
# its own per-family discharge (kernelDischargeLoopVCs / bvDischarge*). Extract just
# its body (def → next top-level def) and assert the shim has not crept back.
rc_body="$(awk '/^def renderContracts /{f=1;print;next} /^def [A-Za-z]/{f=0} f{print}' "$M")"
if printf '%s' "$rc_body" | grep -q "computeVCsDischarged" \
   && ! printf '%s' "$rc_body" | grep -qE "kernelDischargeLoopVCs|bvDischargeCallSites|bvDischargeOverflow"; then
  ok "renderContracts consumes the ledger (no private kernel/bv discharge path)"
else
  no "renderContracts re-introduced a private discharge path (#18e shim returned)"
fi

echo ""
echo "SINGLE-TRUTH-SOURCE: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
