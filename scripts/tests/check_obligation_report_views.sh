#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Reports-as-views gate (ROADMAP Phase 3 #15).
#
# The obligation surfaces — `--report vcs`, `--report obligation-ledger`,
# `--report proof-status`, and the `--report audit` VC summary — are all
# projections of the SAME underlying discharge (collectVCs / dischargeVCs /
# proofStatusEntries). This gate proves they are consistent VIEWS: the same stable
# ids, the same single status vocabulary, and no report recomputing a status that
# disagrees with the ledger. Output parity itself is enforced by the existing
# snapshot gate, which this gate also runs.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
HMAC="examples/hmac_sha256/src/main.con"
TD="examples/thesis_demo/src/main.con"
VOCAB="proved_by_lean,proved_by_kernel_decision,proved_by_lean_replay,arithmetic_proved,solver_trusted,tested_by_oracle,runtime_checked,enforced,assumed,trusted,partial,stale,vacuous,missing,unproven,planned,counterexample,unknown,timeout,solver_error,ineligible"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== --report vcs is a faithful view: ids+statuses == the ledger's VC subset ==="
vmap="$("$COMPILER" "$HMAC" --report vcs --json 2>/dev/null | python3 -c "import json,sys;d=json.load(sys.stdin);print('|'.join(sorted(v['id']+':'+v['status'] for v in d['vcs'])))")"
lmap="$("$COMPILER" "$HMAC" --report obligation-ledger --json 2>/dev/null | python3 -c "import json,sys;d=json.load(sys.stdin);print('|'.join(sorted(o['id']+':'+o['status'] for o in d['obligations'] if not o['id'].endswith('#prooflink'))))")"
[ -n "$vmap" ] && [ "$vmap" = "$lmap" ] && ok "vcs ids+statuses == ledger VC subset" || no "vcs drifted from the ledger"

echo "=== single status vocabulary across vcs + ledger ==="
"$COMPILER" "$HMAC" --report vcs --json 2>/dev/null | python3 -c "
import sys,json
ok=set('$VOCAB'.split(','))
d=json.load(sys.stdin)
sys.exit(0 if set(v['status'] for v in d['vcs'])<=ok else 1)" && ok "every --report vcs status ∈ canonical vocabulary" || no "vcs status outside vocabulary"
"$COMPILER" "$HMAC" --report obligation-ledger --json 2>/dev/null | python3 -c "
import sys,json
ok=set('$VOCAB'.split(','))
d=json.load(sys.stdin)
sys.exit(0 if set(o['status'] for o in d['obligations'])<=ok else 1)" && ok "every ledger status ∈ canonical vocabulary" || no "ledger status outside vocabulary"

echo "=== --report proof-status totals == the ledger's proof-link projection ==="
TOT="$("$COMPILER" "$TD" --report proof-status 2>/dev/null | grep -i 'Totals:')"
"$COMPILER" "$TD" --report obligation-ledger --json 2>/dev/null | python3 -c "
import json,sys,collections,re
d=json.load(sys.stdin)
links=[o for o in d['obligations'] if o['id'].endswith('#prooflink')]
c=collections.Counter(o['kind'] for o in links)
m={'proved':c.get('source_proof_link',0),'stale':c.get('spec_drift',0),'unproved':c.get('missing_theorem',0),
   'blocked':c.get('blocked_proof',0),'ineligible':c.get('ineligible_construct',0),'trusted':c.get('trusted_boundary',0)}
tot='''$TOT'''
ok_all=all(int(re.search(rf'(\d+) {k}',tot).group(1))==v for k,v in m.items())
sys.exit(0 if ok_all else 1)" && ok "proof-status proved/stale/unproved/blocked/ineligible/trusted == ledger #prooflink kinds" \
  || no "proof-status totals disagree with the ledger proof-link projection"

echo "=== --report audit VC summary == --report vcs (audit is a view, not a recompute) ==="
declare -A vcnt
while IFS= read -r line; do
  s=$(echo "$line" | cut -d: -f1); n=$(echo "$line" | cut -d: -f2)
  vcnt[$s]=$n
done < <("$COMPILER" "$HMAC" --report vcs --json 2>/dev/null | python3 -c "
import json,sys,collections
d=json.load(sys.stdin)
for k,v in collections.Counter(x['status'] for x in d['vcs']).items(): print(f'{k}:{v}')")
audit_mismatch=0
for cls in proved_by_kernel_decision proved_by_lean proved_by_lean_replay solver_trusted; do
  aud=$("$COMPILER" "$HMAC" --report audit 2>/dev/null | grep -oE "${cls}: *[0-9]+" | head -1 | grep -oE "[0-9]+\$")
  exp=${vcnt[$cls]:-0}
  [ "${aud:-0}" = "$exp" ] || { echo "      $cls: audit=${aud:-0} vcs=$exp"; audit_mismatch=1; }
done
[ "$audit_mismatch" -eq 0 ] && ok "audit per-class VC counts == --report vcs counts" || no "audit VC summary drifted from --report vcs"

echo "=== existing snapshots stay green (output parity) ==="
if bash scripts/tests/check_snapshots.sh >/tmp/rv_snap.log 2>&1; then ok "snapshot gate green"; else no "snapshot gate failed"; tail -5 /tmp/rv_snap.log|sed 's/^/      /'; fi

echo ""
echo "OBLIGATION-REPORT-VIEWS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
