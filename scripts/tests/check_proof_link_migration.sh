#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Proof-link freshness migration gate (ROADMAP Phase 3 #11).
#
# The proof-status model (proved / stale / missing / blocked / ineligible /
# trusted) is now projected into the ONE ObligationCore ledger instead of living
# as a separate report. Each registered/eligible function becomes a proof-link
# obligation with a canonical kind:
#   proved      → source_proof_link    (proved_by_lean)
#   stale       → spec_drift           (stale)         — fingerprint drift
#   missing     → missing_theorem      (missing)
#   blocked     → blocked_proof        (unproven)
#   ineligible  → ineligible_construct (ineligible)
#   trusted     → trusted_boundary     (trusted)
# so a release gate reads proof staleness from the same ledger as the runtime /
# contract obligations. This is purely additive to --report obligation-ledger
# (the VC subset is unchanged — see check_obligation_core.sh).

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# ck <label> <file> <python-bool-expr over ledger byid/kindset>
ck(){ local label="$1" file="$2" expr="$3"
  "$COMPILER" "$file" --report obligation-ledger --json 2>/dev/null | python3 -c "
import json,sys
d=json.load(sys.stdin); O=d['obligations']; byid={o['id']:o for o in O}
def has(i): return i in byid
def kind(i): return byid[i]['kind']
def st(i): return byid[i]['status']
def pol(i): return byid[i].get('policy_impact','')
def links(): return [o for o in O if o['id'].endswith('#prooflink')]
sys.exit(0 if ($expr) else 1)
" 2>/dev/null && ok "$label" || no "$label"; }

TD="examples/thesis_demo/src/main.con"
echo "=== fresh proof links and missing/ineligible facts are ledger obligations ==="
ck "proved fn → source_proof_link (proved_by_lean)" "$TD" \
  "has('main.parse_byte#prooflink') and kind('main.parse_byte#prooflink')=='source_proof_link' and st('main.parse_byte#prooflink')=='proved_by_lean'"
ck "eligible-but-unproved fn → missing_theorem (missing)" "$TD" \
  "has('main.validate#prooflink') and kind('main.validate#prooflink')=='missing_theorem' and st('main.validate#prooflink')=='missing'"
ck "ineligible fn → ineligible_construct (ineligible)" "$TD" \
  "any(o['kind']=='ineligible_construct' and o['status']=='ineligible' for o in links())"
ck "every proof-link obligation uses a canonical proof kind" "$TD" \
  "all(o['kind'] in ('source_proof_link','spec_drift','missing_theorem','blocked_proof','ineligible_construct','trusted_boundary') for o in links())"

SP="examples/evidence_classes/stale_proof/src/main.con"
echo "=== fingerprint drift surfaces as spec_drift (stale) — release-gate signal ==="
ck "stale proof → spec_drift (stale)" "$SP" \
  "any(o['kind']=='spec_drift' and o['status']=='stale' for o in links())"
ck "spec_drift carries the stale-proof policy impact" "$SP" \
  "any(o['kind']=='spec_drift' and 'stale' in o.get('policy_impact','') for o in links())"

echo ""
echo "PROOF-LINK-MIGRATION: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
