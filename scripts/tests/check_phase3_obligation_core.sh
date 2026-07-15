#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Phase 3 capstone: ObligationCore is the single truth source (ROADMAP #19).
#
# Runs the obligation_core_probe — one project exercising every migrated
# obligation family — and proves the unified ledger is the ONLY truth source:
# the same stable ids and statuses appear across --report vcs, --report
# obligation-ledger, --report audit, --report proof-status, and `concrete prove`.
# Then it composes the Phase-3 consolidation gates as the final umbrella.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
P="examples/obligation_core_probe/src/main.con"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== the probe exercises every obligation family in ONE ledger ==="
"$COMPILER" "$P" --report obligation-ledger --json 2>/dev/null | python3 -c "
import json,sys
d=json.load(sys.stdin); kinds=set(o['kind'] for o in d['obligations'])
need={'precondition','postcondition','assert','assume','vacuity','array_bounds','div_nonzero',
      'no_overflow','invariant_init','invariant_preservation','variant_nonnegative','variant_decreases',
      'missing_theorem','ineligible_construct'}
missing=need-kinds
sys.exit(0 if not missing else (sys.stderr.write('missing kinds: %s\n'%missing) or 1))" \
  && ok "all runtime-safety / contract / loop / proof-link families present" \
  || no "the probe ledger is missing an obligation family"

echo "=== single truth source: every surface agrees on the same stable ids/statuses ==="
vmap="$("$COMPILER" "$P" --report vcs --json 2>/dev/null | python3 -c "import json,sys;d=json.load(sys.stdin);print('|'.join(sorted(v['id']+':'+v['status'] for v in d['vcs'])))")"
lmap="$("$COMPILER" "$P" --report obligation-ledger --json 2>/dev/null | python3 -c "import json,sys;d=json.load(sys.stdin);print('|'.join(sorted(o['id']+':'+o['status'] for o in d['obligations'] if not o['id'].endswith('#prooflink'))))")"
[ -n "$vmap" ] && [ "$vmap" = "$lmap" ] && ok "vcs ids+statuses == ledger VC subset" || no "vcs ≠ ledger VC subset"

# proof-status totals == ledger #prooflink projection.
TOT="$("$COMPILER" "$P" --report proof-status 2>/dev/null | grep -i 'Totals:')"
"$COMPILER" "$P" --report obligation-ledger --json 2>/dev/null | python3 -c "
import json,sys,collections,re
d=json.load(sys.stdin)
c=collections.Counter(o['kind'] for o in d['obligations'] if o['id'].endswith('#prooflink'))
m={'proved':c.get('source_proof_link',0),'stale':c.get('spec_drift',0),'unproved':c.get('missing_theorem',0),
   'blocked':c.get('blocked_proof',0),'ineligible':c.get('ineligible_construct',0),'trusted':c.get('trusted_boundary',0)}
tot='''$TOT'''
sys.exit(0 if all(int(re.search(rf'(\d+) {k}',tot).group(1))==v for k,v in m.items()) else 1)" \
  && ok "proof-status totals == ledger #prooflink projection" || no "proof-status ≠ ledger proof-links"

# audit VC counts == vcs counts.
amis=0
declare -A vc
while IFS=: read -r k v; do vc[$k]=$v; done < <("$COMPILER" "$P" --report vcs --json 2>/dev/null | python3 -c "import json,sys,collections;d=json.load(sys.stdin);[print(f'{k}:{v}') for k,v in collections.Counter(x['status'] for x in d['vcs']).items()]")
for cls in proved_by_kernel_decision proved_by_lean; do
  a=$("$COMPILER" "$P" --report audit 2>/dev/null | grep -oE "${cls}: *[0-9]+" | head -1 | grep -oE "[0-9]+\$")
  [ "${a:-0}" = "${vc[$cls]:-0}" ] || amis=1
done
[ "$amis" = "0" ] && ok "audit VC summary == --report vcs counts" || no "audit ≠ vcs counts"

# prove --show-obligation == ledger for a loop obligation.
sp="$("$COMPILER" prove "$P" probe.count --show-obligation O1 --json 2>/dev/null | python3 -c "import json,sys;d=json.load(sys.stdin);print(d['id']+'|'+d['status'])")"
lp="$("$COMPILER" "$P" --report obligation-ledger --json 2>/dev/null | python3 -c "import json,sys;d=json.load(sys.stdin);o=next(o for o in d['obligations'] if 'probe.count' in o['id'] and o['id'].endswith('#O1'));print(o['id']+'|'+o['status'])")"
[ -n "$sp" ] && [ "$sp" = "$lp" ] && ok "prove --show-obligation O1 == ledger ($sp)" || no "prove ≠ ledger: prove=$sp ledger=$lp"

echo "=== umbrella: the Phase-3 consolidation gates are all green ==="
for g in check_obligation_core check_obligation_report_views check_obligation_policy_views \
         check_obligation_prove_views check_obligation_single_truth_source \
         check_no_duplicate_obligation_walkers check_discharge_adapters check_obligation_lowering; do
  if bash "scripts/tests/$g.sh" >/tmp/p3_$g.log 2>&1; then ok "$g"; else no "$g"; tail -3 /tmp/p3_$g.log|sed 's/^/      /'; fi
done

echo ""
echo "PHASE3-OBLIGATION-CORE: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
